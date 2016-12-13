//===---------------- pass3_dblBuffer.g - double buffering ----------------===//
//
//  Double buffer and prefetch the briquettes; remove double buffering (of big
//  temporaries) in the input which are no longer needed after pipelining
//
//===----------------------------------------------------------------------===//
//
//  Developed by Jagan Jayaraj and Pei-Hung Lin
//  Copyright 2007-2013, Regents of the University of Minnesota
//
//  Copyright 2014 Sandia Corporation. Under the terms of Contract
//  DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
//  certain rights in this software.
//
//  Derived from the ANTLR 2 grammar for Fortran 77 by Olivier Dragon      
//  Copyright (C) 2007  Olivier Dragon  (v2 ANTLR grammar) 
//      In turn derived from Terence Parr's PCCTS grammar.
//
//  This file is part of CFDbuilder.
//
//  CFDbuilder is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  CFDbuilder is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with CFDbuilder.  If not, see <http://www.gnu.org/licenses/>.
//
//  Contacts: {jaganj,phlin}@cs.umn.edu, paul@lcse.umn.edu
//
//===----------------------------------------------------------------------===//

tree grammar pass3_dblBuffer;

/* 09/29/2012:
   In this pass, we double buffer and de-double buffer arrays based on directives.
   We need double buffering to support prefetching data from the global arrays in
   the main memory to the stack variables in cache.  The double buffering is 
   invoked by cPPM$ DOUBLEBUFFER <arrayname>.
   
   We also need de-double buffering to support the "grid-block-update" pattern.  
   We bring in a block of the problem state into the L3 cache and performa a 3-D
   update with the help of many threads before we move on to the next block.  The
   ddtemp array is a circular buffer by itself.  We don't any further double buffering.
   However, if we operate on the ddtemp briquettes in FW style, we will overwrite and
   corrupt it.  <do icget = icube-nghostcubes, icube+nghostcubes>  We didn't have 
   this problem with the "grid-briquette-update" pattern because we have a ddold and 
   ddnu.  Because ddtemp acts as both ddold and ddnu, we will corrupt the data if
   we don't pipeline the computation.  To facilitate writing FW, we work around this
   problem by double buffering ddtemp in FW.  We create two new temporaries called
   ddtempold and ddtempnu, and equivalence them to the double buffered ddtemp.  
   The translator then eliminates the double-buffering on ddtemp when the code has 
   been fully pipelined.  We need the programmer to identify ddtemp with the help
   of cPPM$ DE-DBL-BUF <ddtemp> directive.
   An example usage is:
       cPPM$ DE-DBL-BUF DDTEMP
            dimension   ddtemp(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                           ntempbqupdate+4,2)
           
            dimension   ddtempold(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                             ntempbqupdate+4)
            dimension    ddtempnu(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                             ntempbqupdate+4)
            equivalence (ddtemp(1,0,0,1,1),ddtempold)
            equivalence (ddtemp(1,0,0,1,2),ddtempnu)
            
            call ppmmf (    ....
           &                ddtempold(1,1,jbqmod,kbqtemp),
           &                ddtempnu(1,1,jbqmod,kbqtemp), ....)
                                                   
     It will be transformed into,       
             dimension   ddtemp(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                           ntempbqupdate+4)
           
            dimension   ddtempold(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                             ntempbqupdate+4)
            dimension    ddtempnu(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                             ntempbqupdate+4)
            equivalence (ddtemp(1,0,0,1),ddtempold)
            equivalence (ddtemp(1,0,0,1),ddtempnu)
            
            call ppmmf (    ....
           &                ddtempold(1,1,jbqmod,kbqtemp),
           &                ddtempnu(1,1,jbqmod,kbqtemp), ....)  
           
       ddtempold, ddtempnu are now equivalent.            
     
   
   01/25/2011:
   Update: I am rewriting it to convert only the DO loops preceeded by cPPM$ LONGIT directive to 
   DDOLOOP in the transform region.  It still has issues with the scalar statements.  I am 
   allowing them momentarily, but will fix it soon.  Refer to fuse.g for a more detailed 
   explanation.
   12/14/2010:
   In the second phase of pipelining, we double buffer and re-index temporaries.  We re-index
   temporaries occuring in a select section of the code we call the transform region.  See the
   comment below from 10/28 for more on re-indexing temporaries.  The second phase is further 
   split into two parts, pass3_dblBuffer.g and pipeline3.g.  Re-indexing is done exclusively in 
   pipeline3.g.  However, we check if the transform region is conformal here.  We only allow 
   DO loops and enclosing IF statements in the transform region.  The DO loops are over the 
   1-D sweep direction, and will be pipelined.  Any non-loop construct (scalar statements) between
   such DO loops may break the pipeline semantics, and, therefore, are not allowed in the
   transform region.  The only exemption are the IF statements enclosing the pipelineable DO
   loops.  
   eg: <transform region begin>
          do ..
            do ..
            enddo
          enddo
          if (ypass .gt. 0)
            do ..  
            enddo
          else
            do ..
            enddo
          enddif
          do ..
          enddo     
       <transform region end>      
   The initial thought was to have two parts for convenience.  The rules for conformity checks were
   very simple.  Once you tag the pipelineable DO loops as DDOLOOP, it is very straight-forward to 
   look for just these patterns, and transform them.  However, the transformation involves adding 
   new statements.  The tree visitor gets all confused when the number of children change dynamically.
   It loops for the initial number of children.  When we add more, the new number of children doesn't
   become the upper bound for the tree visitor loop.  It works on the next few children till it hits 
   the original upper bound and stops.  It doesn't explore a whole bunch of valid children.  The only
   solution is to perform the transformations after we identified all DDOLOOPs in a transform region.       
   This means the rules (not the actions) for conformity check and transformation are almost the same.
   It makes sense to fold them into a single file.  Perform the conformity check topdown, and the 
   transformation bottomup, as we always did.  However, it affects other transformations like double-
   buffering.  Variables outside the transform region may not get double-buffered, as we add more 
   statements to do pipelining.  The same issue as before.  The current solution is to have all
   re-writes which don't add new nodes in one pass, and transformations which add new nodes in separate
   passes.  By current design, we can't deal with more than one transform region belonging to one parent
   ie. a subroutine.  I don't expect such a situation.   
   
   10/28/2010: 
   Re-indexing the temporaries saves space, but is very complex to code and maintain.  A very simple
   alternative is to use a map between the original subscripts (indexes) and the re-indexed quantities.
   To begin with, we use a single array of size nx+nghostcells as a map, and hence all the temporaries
   have the same size.  They could be shrinked later, but through the analysis.  In the case of GPUs,
   the memory reduction from this further analysis will not buy us anything because GPUs are terrible 
   with multitude of indexes, and all the temporaries could never fit in their on-chip memory anyways.
   Just to standardize, the upper limit of the map array is made 0, and therefore the array can only 
   be indexed negative subscripts.  The original subscript expressions are now passed through this map.
   For them to remain unchanged, the loop bounds of the original index are adjusted.  We use 'ii' as the
   map, and 'ibegin' as the lower bound for outer index.  If these names are already in use by the 
   subroutine, I can create new names in the future.
   
   A sample translation would look like,  
c      do 1100   i = 2-nbdy,n+nbdy
      ibegin = 1-nx
      ibegin = max(ibegin,2-nx*(icube+nghostcubes))
      do 1100   i = ibegin,0
!DEC$ VECTOR ALWAYS
c!DEC$ VECTOR ALIGNED
      do jk = 1,nssq
      cl(jk,ii(i)) = .5 * (c(jk,ii(i)) + c(jk,ii(i-1)))
      ...
      
      i is being replaced by ii(i) and i-1 by ii(i-1).  I am set out to do just this and ibegin
      computations in this pass.   
   -JJ
   

 */          
    

options {
    language = Java;
    backtrack=true;          // allow backtracking if necessary
    output=AST;              // build ASTs from input AST
    tokenVocab=tokenFile;     
    ASTLabelType=FTree; // we're using FTree nodes
}


@header{
package pipeliner;
import lexer.*;
import symbol.*;
import translator.*;
import IntegerEvaluator.*;
import java.util.HashMap;
}
@members{   
   boolean bprefetch = false;  // True in the prefetch region
   boolean btransregion = false; // True in the transform region
   boolean bldoloop = false;  // Longitudinal DO Loop
   boolean bdblBufinSubprogBody = false; // Found a double buffer statement in the Subprogram Body 
   boolean bdedblBufinSubprogBody = false; // Found a de-double buffer statement in the Subprogram Body    
   
   HashMap dblBuffer = new HashMap();
   HashMap dedblBuffer = new HashMap();   
   
   Scope currentScope = null;    
   LabelTable lbltab = null;                    
   
   public pass3_dblBuffer(TreeNodeStream input, LabelTable lbltab){
      this(input);
      this.lbltab = lbltab;      
      //this.lblfregion = lbltab.getSize()+1; 
   }
   
   public Symbol retrieveSymbol(String name) {
        Symbol vs;
        
        /* I primarily use this subroutine to retrieve the symbols for predefined
           constants and variables like nbdy, nx, ii, ibegin.  Therefore, I am 
           explicitly setting isVectorLoop to false.  
         */  
        if(currentScope.resolve(name)==null){
            vs = new VariableSymbol(name,Symbol.getDataType(name),false);
            currentScope.define(vs);  
        }else{
            vs = currentScope.resolve(name);
            if(vs.scope != currentScope){
               vs.setArgument();
            }
            vs.setVectortype((BaseScope)currentScope,false);
        }
        return vs;
   }
} 

program :
  ^(CODEROOT (ppmdirectives | directives | executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit:
     functionSubprogram
  |  mainProgram
  |  subroutineSubprogram
  |  blockdataSubprogram
  ;

/* 2 */
mainProgram : /* Dont understand this */
  ^(MAINPROG programStatement subprogramBody)
  ;

/* 3 */
functionSubprogram :
  ^(FUNCTION functionStatement subprogramBody)
  ;

/* 4 */
subroutineSubprogram :
  ^(SUBROUTINE subroutineStatement subprogramBody)
  ;

/* 5 - blockDataSubprogram */
blockdataSubprogram :
  ^(BLOCKDATA blockdataStatement subprogramBody)
  ;

/* 6 */
otherSpecificationStatement :
      dimensionStatement
    | allocatableStatement
    | equivalenceStatement
    | intrinsicStatement
    | saveStatement
    | volatileStatement
  ;

/* 7 */
executableStatement :
    assignmentStatement
  | gotoStatement
  | ifStatement
  | doStatement
  | continueStatement
  | stopStatement
  | pauseStatement
  | readStatement
  | writeStatement
  | printStatement
  | rewindStatement
  | backspaceStatement
  | openStatement
  | closeStatement
  | endfileStatement
  | inquireStatement
  | callStatement
  | returnStatement
  ;

/* 8 */
programStatement :
  ^(T_PROGRAM NAME)
  ;

seos : NEWLINE ;

/* 9, 11, 13 */
/* 10 */
functionStatement :
  ^(T_FUNCTION (type)? functionName namelist?)
  ;

blockdataStatement :
  ^(T_BLOCK NAME)
  ;

/* 12 */
subroutineStatement:
  ^(T_SUBROUTINE ID=NAME namelist?)  {currentScope = $ID.symbol.scope;}
  ;
  
namelist :
  ^(SUBARG identifier+)
  ;

subprogramBody
@after{
  if(bdblBufinSubprogBody || bdedblBufinSubprogBody){
    CommonTreeNodeStream donode;
    FTree declBlk;
           
    declBlk = (FTree)retval.tree.getChild(0);    
    donode = new CommonTreeNodeStream(Translator.fTreeAdaptor, declBlk);    
    fixdblBufDefns fixdblBufDefs = new fixdblBufDefns(donode, dblBuffer, dedblBuffer);
    fixdblBufDefs.setTreeAdaptor(Translator.fTreeAdaptor);
    fixdblBufDefs.downup(declBlk, false);
    
    if (bdblBufinSubprogBody)
        bdblBufinSubprogBody = false;   
    if (bdedblBufinSubprogBody)
        bdedblBufinSubprogBody = false;      
  }
}:
  ^(SUBPROGRAMBLOCK declarationBlock executionBlock)
  ;

declarationBlock:
  ^(DECLARATIONBLOCK declarationStatement*)
  ;

executionBlock:
  ^(EXECUTIONBLOCK wholeStatement*)
  ;

declarationStatement:
    ppmdecldirectives  
/*     directives   ;   c$OMP MASTER which belongs to the execution block 
 *                      gets added to the declaration region if it is the
 *                      very first statement in the execution region.  
 *                      The bigArray gets defined after this OMP directive
 *                      which is incorrect.  Technically we don't have any
 *                      OMP directives in the declarationStatement yet.
 *                      If we have to add them in the future (after 09/24/12)
 *                      we have to draw a distinction between the declaration
 *                      block OMP statements and execution region OMP statements
 *                      All the passes need to be modified.
 */ 
  | includeStatement
  | implicitStatement
  | useStatement
  | parameterStatement
  | typeStatement
  | commonStatement
  | dataStatement
  | externalStatement
  | otherSpecificationStatement
  ;

macroStatement :
  MACRO (~EOS)+
  ;
  
wholeStatement  :
    ppmdirectives
  | directives
//  | statement
//  |^(LABEL statement) 
  |formatStatement
  |allocateStatement
//  |macroStatement!
  |statementFunctionStatement  
  |executableStatement
  ;

endStatement:
   ^(T_END (l=LABEL)?)
   ;

/* 15 */
dimensionStatement : 
  ^(T_DIMENSION arrayDeclarator+)
  ;
  
allocatableStatement :  
  ^(T_ALLOCATABLE entityList+)
  ;
  
entityList :  
  arrayName deferredShapeSpecList?
  ; 
  
deferredShapeSpecList : 
  T_COLON+
  ; 
  
allocateStatement : 
  ^(T_ALLOCATE arrayDeclarator+)
  ;

/* 16 */
arrayDeclarator :
  ^(ARRAY arrayName arrayDeclaratorExtents)
  ;

arrayDeclaratorExtents :
  ^(ADEXTS arrayDeclaratorExtent arrayDeclaratorExtent*)
  ;

arrayDeclaratorExtent: 
  ^(ADEXT expression+)
  ;

/* 17 */
equivalenceStatement : 
  ^(T_EQUIVALENCE equivEntityGroup+)
  ;

/* 18 */
equivEntityGroup : 
  ^(EQUIVGRP varRef+)
  ;

/* 19 */
commonStatement :
  ^(T_COMMON (commonBlock+ | commonItems))
  ;

commonName : 
  T_DIV (NAME T_DIV | T_DIV)
  ;

commonItem :
    NAME
  | arrayDeclarator
  ;

commonItems :
  commonItem+ 
  ;

commonBlock :
  commonName commonItems
  ;

dataStatement:
  ^(T_DATA dataBlock+)
  ;

dataBlock:
  NAME T_DIV (RCON | DCON | ICON) T_DIV
  ;
  
/* 20 */
// need to expand the typename rule to produce a better AST
// May need to work on it.
typeStatement :
  ^(TYPESTMT type typeStatementNameList)
  ;

typeStatementNameList :
  typeStatementName (typeStatementName)*
  ;
  
typeStatementName :
    NAME
  | arrayDeclarator
  ;
  
typename :
     T_REAL
   | T_PRECISION
   | T_INTEGER
   | T_LOGICAL
   | T_CHARACTER
  ;
  
type : 
  (typename typenameLen?)
  ;

typenameLen :
  ^(T_STAR lenSpecification) 
  ;

includeStatement :  
  ^(T_INCLUDE SCON)
  ;
  
/* 21 */
implicitStatement : 
  ^(T_IMPLICIT (implicitNone | implicitSpecs) )
  ;

implicitSpec : 
  ^(type implicitLetters+)
  ;

implicitSpecs : 
  implicitSpec+
  ;

implicitNone : 
  T_NONE
  ;

implicitLetter : 
  NAME
  ; 
  
implicitRange : 
  ^(IMPLICITRANGE implicitLetter implicitLetter?)
  ;

implicitLetters : 
  implicitRange+
  ;

/* 22 */
lenSpecification :  
    ICON
  | T_LPAREN expression T_RPAREN
  ;

useStatement:
  ^(T_USE NAME)
  ;

/* 23 */
parameterStatement : 
  ^(T_PARAMETER paramlist)
  ;

paramlist : paramassign ( paramassign )*
  ;

paramassign : 
  ^(T_ASSIGN NAME expression)
  ;

/* 24 */
externalStatement :
  ^(T_EXTERNAL namelist)
  ;

/* 25 */
intrinsicStatement : 
  T_INTRINSIC namelist
  ;

/* 26 */
saveStatement : 
  ^(T_SAVE (saveEntity+)?)
  ;

saveEntity :  
    NAME 
  | ^(T_DIV NAME T_DIV)
  ;

volatileStatement : 
  ^(T_VOLATILE NAME+)
  ;
  
/* 29 */
assignmentStatement : 
  ^(T_ASSIGN varRef expression)
  ;

/* 30 */
gotoStatement : 
  ^(GOTO unconditionalGoto )
  |^(GOTO assignedGoto)
  ;

/* 31 */
unconditionalGoto: 
  lblRef
  ;

lblRef:
  ^(LABELREF ICON)
  ;

labelList : 
  lblRef (lblRef)*
  ;

/* 33 */
assignedGoto : 
  NAME (labelList)?
  ;

/* 34 */
ifStatement :
  ^(T_IF expression blockIfStatement)
  | ^(T_IF expression logicalIfStatement)
  ;
  
/* 35 */
logicalIfStatement : 
  ^(THENBLOCK executableStatement)
  ;

/* 36 */
blockIfStatement : 
   firstIfBlock
  (elseIfStatement)*
  (elseStatement)?
  ;

firstIfBlock :
  ^(THENBLOCK wholeStatement*)
  ;

/* 37 */
elseIfStatement : 
  ^(ELSEIF expression ^(THENBLOCK wholeStatement*))
  
  // this action is used to ensure that in the AST there is a clear difference
  // between
  // ELSE IF ...
  // and
  // ELSE
  //    IF ... END IF
  // END IF
  // this is done by replacing the 'else if' by one 'elseif' inside the AST
  ;

/* 38 */
elseStatement :
  ^(ELSEBLOCK wholeStatement*)
  ;

/* 39 */
//endIfStatement : 
//  (T_ENDIF! | T_END! T_IF!) ;

/* 40 */
doStatement :
   /* If I don't reset bldoloop right away here after T_DO, the innermost do loop gets 
      converted to DDOLOOP.  
    */ 
   {bldoloop && btransregion}? ^(T_DO  {bldoloop = false;} (doWithEndDo|doWithLabel) ) -> ^(DDOLOOP doWithEndDo? doWithLabel?)
 | {!(bldoloop && btransregion)}? ^(T_DO (doWithEndDo|doWithLabel) )                                     
  ;

doVarArgs:
  ^(DOVARARG id=NAME e1=expression 
  e2=expression (e3=expression )?)
  ;

doWithLabel:
  (lblRef
   doVarArgs 
   doBody)
  ;

doBody :
  ^(DOBLOCK wholeStatement*)
  ;

doWithEndDo :
  doVarArgs
  doBody
  ;
  
enddoStatement : 
  (T_ENDDO | T_END T_DO| continueStatement)
  ;

/* 41 */
continueStatement: 
  ^(T_CONTINUE (LABEL)?)
  ;

/* 42 */
stopStatement : 
  ^(T_STOP ICON?)
  ;

/* 43 */
pauseStatement : 
  ^(T_PAUSE ICON?)
  ;

iocontrolArgs:
  T_LPAREN
  (ICON | varRef | T_STAR)
  (T_COMMA
  (lblRef | T_STAR))?
  T_RPAREN
;

/* 44 */
writeStatement : 
  ^(T_WRITE iocontrolArgs (~EOS)*)
  ;

/* 45 */
readStatement : 
  ^(T_READ iocontrolArgs (~EOS)*)
  ;

/* 46 */
printStatement : 
  ^(T_PRINT (~EOS)+)
  ;

/* 50 */
openStatement : 
  ^(T_OPEN (~EOS)+)
  ;

/* 51 */
closeStatement : 
  ^(T_CLOSE (~EOS)+)
  ;

/* 52 */
inquireStatement : 
  ^(T_INQUIRE (~EOS)+)
  ;

/* 53 */
backspaceStatement : 
  ^(T_BACKSPACE (~EOS)+)
  ;

/* 54 */
endfileStatement : 
  ^(T_ENDFILE (~EOS)+)
  ;

/* 55 */
rewindStatement : 
  ^(T_REWIND (~EOS)+)
  ;

/* 58-59 */
formatStatement: 
  ^(T_FORMAT (LABEL)? (~EOS)+)
  ;

/* 70 */
statementFunctionStatement : 
  ^(T_LET (~EOS)+)
  ;

/*Function_reference*/
functionReference :
  ^(FUNCREF functionName functionArgumentList)
  ;
  
functionArgumentList: 
  ^(FUNCARG functionArgument*)
  ;

functionArgument :
  expression
  ;
  
/* 71 */
callStatement : 
  ^(T_CALL subroutineCall)
  ;

subroutineCall :
  (NAME callArgumentList?)
  ;

callArgumentList : 
  ^(CALLARG callArgument+)
  ;
  
callArgument : 
  expression
  ;

/* 72 */
returnStatement : 
  ^(T_RETURN expression?)
  ;
  
/* 74 */
expression:  
    unsignedArithmeticConstant
  | SCON  
  | varRef 
  | functionReference
  | ^(T_LOR expression expression)
  | ^(T_LAND expression expression)
  | ^(T_LNOT expression expression)
  | ^(T_LT expression expression)
  | ^(T_LE expression expression)
  | ^(T_EQ expression expression)
  | ^(T_NE expression expression)
  | ^(T_GT expression expression)
  | ^(T_GE expression expression)
  | ^(T_PLUS expression expression)
  | ^(T_MINUS expression expression)
  | ^(T_STAR expression expression)
  | ^(T_DIV expression expression)
  | ^(NEG expression)    
  | ^(T_POWER expression expression)
  | ^(MADD expression expression expression)
  | ^(MSUB expression expression expression)  
  ;

concatOp :
  T_DIV T_DIV 
  ;

/* 88 */
arrayElementName : 
  ^(NAME expression+)
  ;

subscripts [boolean bdblBuff, boolean bdedblBuff]
@after{
     if ($bdblBuff){        
        FTree lastdim = null;
        //System.out.println("prefetch = "+bprefetch);
        if (bprefetch)
        {
          /* In the prefetch region.  Add ic1 as the last dimension. */
          lastdim = (FTree)adaptor.create(NAME,"ic1");
          lastdim.symbol = retrieveSymbol("ic1"); /* Note: Even if ic1 doesn't exist yet, it is created here */ 
          retval.tree.addChild(lastdim);            
        } else {
          /* Not in the prefetch region.  Add ic0 as the last dimension. */
          lastdim = (FTree)adaptor.create(NAME,"ic0");
          lastdim.symbol = retrieveSymbol("ic0");    /* Note: Even if ic0 doesn't exist yet, it is created here */
          retval.tree.addChild(lastdim);                    
        } 
     }
     if ($bdedblBuff){
      /* equivalence (ddtempold,ddtemp(....,1))
         equivalence (ddtempnu,ddtemp(.....,2))
         Eliminate the last dimension from ddtemp
       */
       int nchildren = retval.tree.getChildCount();
       if (nchildren > 1){
           retval.tree.deleteChild(nchildren-1);
       } else {
          /* Don't delete if the number of subscripts is just one.
           * I don't want to deal with converting an array symbol
           * into a non-array symbol.
           */
           FTree one = (FTree)adaptor.create(ICON,"1");
           retval.tree.replaceChildren(0,0,one);
       }  
     }
}:
  ^(SUBSCRIPT expression+);
 
varRef:      
      ^(ARRAYREF ID=NAME
                 {if ($ID.symbol.hasEquivalence() && dblBuffer.containsKey($ID.symbol.equivName) /* If it's equivalence is double-buffered */ 
                      && currentScope == dblBuffer.get($ID.symbol.equivName)      /*    in the right scope. */
                      && !dblBuffer.containsKey($ID.text))                                       /* It is not present in the double-buffer table. */
                  {
                      /* Double buffer the equivalence too */
                      dblBuffer.put($ID.text,currentScope);
                  }
                 }
                 s=subscripts[dblBuffer.containsKey($ID.text) && currentScope == dblBuffer.get($ID.text), 
                              dedblBuffer.containsKey($ID.text) && currentScope == dedblBuffer.get($ID.text)
                             ])
    |   ID=NAME
  ;  

/* 92 */
arrayName :  
  NAME
  ;

implicitfunctionName: 
   T_ACHAR
  |T_ACOS
  |T_ABS
  |T_ALOG
  |T_ATAN
  |T_ATAN2
  |T_COS
  |T_DBLE
  |T_EXP
  |T_FLOAT
  |T_ICHAR
  |T_INT
  |T_LOG
  |T_MAX 
  |T_MIN
  |T_MOD
  |T_SQRT
  |T_SIN
  |T_TAN
  |T_TANH
  |T_DABS
  |T_DACOS
  |T_DATAN
  |T_DATAN2
  |T_DCOS
  |T_DEXP
  |T_DLOG
  |T_DMAX 
  |T_DMIN
  |T_DMOD
  |T_DSIN
  |T_DSQRT
  |T_DTAN
  |T_DTANH
  ;
/* 98 */
functionName :
    implicitfunctionName
  | NAME
  ;

/* 101 */
unsignedArithmeticConstant: 
    i=ICON
  | RCON
  | DCON
  ;

/* 108 */
logicalConstant : 
  (T_TRUE | T_FALSE)
  ;

identifier:
  NAME
  ;

directives:
    ^(DIRECTIVE (T_AND)? (~EOS)+)
  ;
  
ppmdirectives:
//
//       ^(PPMDIRECTIVE INLINE)
//  |^(PPMDIRECTIVE INLINED)  
//  |^(PPMDIRECTIVE UNROLL ICON)
//   ^(PPMDIRECTIVE UPPERBOUND ic=ICON)
//  |^(PPMDIRECTIVE LOWERBOUND ic=ICON) 
   ^(PPMDIRECTIVE TRANSFORMBEGIN) {btransregion = true;}
  |^(PPMDIRECTIVE TRANSFORMEND) {btransregion = false;}  
  |^(PPMDIRECTIVE PREFETCHBEGIN) {bprefetch = true;}
  |^(PPMDIRECTIVE PREFETCHEND) {bprefetch = false;}
  |^(PPMDIRECTIVE n=LONGIT) {bldoloop = true;}
  |^(PD=PPMDIRECTIVE DOUBLEBUFFER ID=NAME)
    {
      /* Add an extra dimension at the end */
      ArraySymbol as = null;
      int nDim;
            
      currentScope = $PD.symbol.scope;      
      as = (ArraySymbol)currentScope.resolve($ID.text);
      if (as == null){
          //currentScope.printSymbols();
          System.out.println("Invalid arrayname "+$ID.text+" specified with the DOUBLEBUFFER directive!");
          System.exit(-1);
      }      
      dblBuffer.put($ID.text,currentScope);
      as.nDim++;
      nDim = as.nDim;            
      as.UBDim[nDim-1]="1";      
      as.UBDimTree[nDim-1] = (FTree)adaptor.create(ICON,"1");
      /* Calling setArraySize is a waste of time. I can perfectly handle it here. */
      as.LBDim[nDim-1] = "0";
      as.LBDimTree[nDim-1] = (FTree)adaptor.create(ICON,"0");
      as.sizeDim[nDim-1] = "2";
      as.itotalsize =  as.itotalsize * 2;
      as.totalsize = Integer.toString(as.itotalsize);
      /* Update the Symbol Table with the updated symbol */
      currentScope.define(as);
      /* The AST for the definition itself is modified only in pipeline4pbp.g.
         Things are too spread out in the pipeliner.  Clean it up!
       */  
      bdblBufinSubprogBody = true; 
    } 
  |^(PPMDIRECTIVE (n=~(PREFETCHBEGIN | PREFETCHEND | TRANSFORMBEGIN | TRANSFORMEND | LONGIT | DOUBLEBUFFER | DEDOUBLEBUFFER | EOS))+)  
  ;

ppmdecldirectives :
     ^(PD=PPMDIRECTIVE DEDOUBLEBUFFER ID=NAME)
    {
      /* Eliminate the extra dimension at the end */
      ArraySymbol as = null;
      int nDim;
            
      currentScope = $PD.symbol.scope;      
      as = (ArraySymbol)currentScope.resolve($ID.text);
      if (as == null){
          System.out.println("Invalid arrayname "+$ID.text+" specified with the DEDOUBLEBUFFER directive!");
          System.exit(-1);
      }      
      dedblBuffer.put($ID.text,currentScope);

      if (as.nDim > 1){
          as.nDim--;
      } else {
         /* Don't delete the last dimension if there is only one dimension.
          * I don't want to deal with converting an ArraySymbol into a 
          * non-array variable symbol.
          */
         as.UBDim[0]="1";      
         as.UBDimTree[0] = (FTree)adaptor.create(ICON,"1");

         as.LBDim[0] = "1";
         as.LBDimTree[0] = null;
         as.sizeDim[0] = "1";          
      }      
      /* Calling setArraySize is a waste of time. I can perfectly handle it here. */             
      as.itotalsize =  as.itotalsize / 2;
      as.totalsize = Integer.toString(as.itotalsize);
      /* Update the Symbol Table with the updated symbol */
      currentScope.define(as);
      bdedblBufinSubprogBody = true; 
    }
  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

  