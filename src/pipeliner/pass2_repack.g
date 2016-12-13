//===------------ pass2_repack.g - pack the result briquettes -------------===//
//
//  This code deals with the complexity of repacking the results into briquettes
//  when the boundary size is not a multiple of the briquette dimension
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

tree grammar pass2_repack;

/* 01/02/2011:
   Pattern matching ANTLRs are very elegant, but seems to be very error-prone.  Adding statements
   (nodes/children) seems to work, but deletion doesn't.  TreeVisitor visit method gets all 
   confused.  I performed a whole bunch of experiments from deleting statements in the reverse
   order to getting child index at the time of deletion.  The conclusion is deletion doesn't
   work with pattern matching grammars.  It forces me to use the more clumsy tree grammars, but,
   as demonstrated below, they atleast work.
      -JJ
       
  08/12/2014:
   Pattern matching grammar has few more problems:
   (a) More often than not, the rewrites must be performed only in a certain context.  
       The context, in our case, is the enclosing PPM directives which are guiding 
       the transformations.  The directives are at the same level as the statement
       of interest.  Essentially we are matching a list, but we are interested to
       transform only a small number of statements inside this list.  ANLTR doesn't
       allow you to replace this list with another list.  You can only replace this 
       entire list with a single tree.  All the context information is now lost.
   (b) The second option is to not use the rewrite facility but instead perform the 
       rewrite explicitly in the actions.  However, ANTLR stores only the first 
       statement from the list matched into retval.tree.  It's not possible to 
       manipulate the statement(s) of interest because they most likely are not
       inside retval.tree
   (c) We could also just match the statement of interest (so that its tree is in
       retval.tree, and try to figure out the context by parsing the token stream 
       explicitly in Java.  However this is rather difficult, and defeats the purpose 
       of using ANTLR and its features.
   Yet again, pattern matching proves inadequate for most of our needs.  Therefore, 
   we are left to match the whole tree and use global variables to remember the context. 
   The code bloat must be tolerated to avoid writing laborious code, and instead make 
   ANTLR write the code for us. 
        
   04/18/2012:
   The objective of this pass is to deal with the issues in repackaging the 
   data when nbdy is not an integer multiple of nsugar (nx).  We write out
   dnu and voxelnu to the main memory as full briquettes.  We do these  
   copies at the end of the pipeline after the loop over planes.  When nbdy
   is an integer multiple of nsugar, dnu and voxelnu briquettes are fully 
   constructed and ready for data transfers at the end of the loop over
   planes.  However when nbdy is not a multiple of nsugar, the briquettes 
   are only partially constructed at the end of loop over planes.  
   
   We have a couple of solutions.  The first solution is to double or triple
   buffer dnu.  That way we can write out a fully constructed dnu.  The other
   solution is to construct dnu late.  If we synchronize the dnu packaging with
   the loop over planes then a full dnu will be available at the end of the 
   loop over planes.  This involves storing the results like rhonu, and pnu, 
   longer.  We prefer the second approach because saving a few more planes of 
   the results is better than double buffering or triple buffering a big array 
   like dnu.  There is just isn't enough space.  We follow the same procedure
   for voxelnu as well.
   
   It is tempting to get rid off dnu briquette altogether and write out the
   results directly to the main memory.  Repacking the data involves 
   transposes.  Strided access patterns are usually costly.  However, if caches
   work properly then ddnu might be local and it wouldn't kill us.  It is worth
   while to try sending back data directly and bypass dnu.    
           
   Pipeline Phase 2:
   *  Adjusts the iteration space of repackaging loops.  
            eg: (1,nx) -> (1,nx-ioffset)
       where   nghostcubes * mynx - nbdy.  ifoffset is 0 when nbdy is an 
      integer multiple of mynx.      
   *  Rename the token REPACK to LONGIT.  From now on, we will treat the repack
      loop as a regular longitudinal loop.
      
     It would appear that we are performing fewer iterations.  However, iskipit
    will come to our rescue.  In the current logic, iinbegin can be 1 or 
    1+iinskip, but iinend is always equal to mynx.  This means we are performing
    more iincube iterations than necessary.  That is where iskipit comes into 
    picture.  When iskipit is ON, we only do repackaging, and no computation.  
    We can bypass all the other pipeline stages.       
    
     The above two transformations will produce the desired results.  However, 
    it is based on some very special transformations in the later passes, and is
    fragile.  The iteration space transformation only affects the longitudinal 
    loop index variables appearing in the last dimension.  The index variable 
    occuring at all other places in the loop will be renamed to iincube which is
    what we want.  The negative shift of the upper bound will make the loop a 
    later pipeline stage.  This is exactly what we want, delay the construction
    of dnu.  I would prefer the syntactic transformations to be not so spaghetti 
    like, but more modular.              
      
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
   boolean bpipelineouter = false; // True if it is the outer pipeline loop
   boolean bpipelinebody = false;  // True in the outer pipeline loop body
   boolean brepack = false; // Delete the redundant iterations.  True for such a loop. 
   Scope currentScope = null;
        
   private int ivalueOf(String parameter){
      Symbol vs;      
      
      vs = currentScope.resolve(parameter);        
      if(vs == null){
         System.out.println("pass2_repack.g : "+parameter+" is undefined");
         System.exit(-1);
      }     
                 
      return ((ParamSymbol)vs).ivalue;
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
  ^(T_SUBROUTINE subroutineName namelist?)
  ;
  
namelist :
  ^(SUBARG identifier+)
  ;

subprogramBody
:
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
  
wholeStatement :
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
  ^(LABELREF l=ICON)
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
doStatement
scope {
   Symbol idsymbol; // Symbol for the current doloop identifier   
}  
@init{
   boolean btmp1 = false, btmp2 = false;
}
@after {     
     bpipelineouter = btmp1;     
     if(bpipelineouter){         
        bpipelineouter = false;
        bpipelinebody  = false;
     }
}      
 : 
    ^(T_DO (lblRef)? doVarArgs { 
                                 btmp1 = bpipelineouter; 
                                 if (bpipelineouter) 
                                     bpipelinebody = true;
                                 bpipelineouter = false;
                                 brepack = false;
                               } 
                               d=doBody);

doVarArgs
@after
{  
  if (brepack && bpipelinebody){
     Symbol vs = null;
     FTree minusTree = null;
     int nbdy = -1, mynx = -1, nghostcubes = -1, ioffset = 0;
     
     currentScope = $id.symbol.scope;     
     //System.out.println("CurrentScope = "+currentScope);

     nghostcubes = ivalueOf("nghostcubes");
     nbdy        = ivalueOf("nbdy");
     mynx        = ivalueOf("mynx");
     
     /* ioffset = iinskip = nghostcubes * mynx - nbdy
        ioffset is 0 when nbdy is an integer multiple of mynx.
      */
     ioffset = nghostcubes * mynx - nbdy;             
                 
     if (ioffset > 0) {                 
       /* Replace expression e2 with e2 - ioffset */     
       minusTree = (FTree)adaptor.create(T_MINUS,"-");
       minusTree.addChild((FTree)adaptor.dupTree($e2.tree));
       minusTree.addChild((FTree)adaptor.create(ICON,Integer.toString(ioffset)));             
       retval.tree.replaceChildren(2,2,minusTree);
     }     
  }
}:
  ^(DOVARARG id=NAME e1=expression 
  e2=expression (e3=expression )?)
  ;

doWithLabel:
  (lblRef
   doVarArgs 
   doBody)
  ;

doBody:
  ^(DOBLOCK w=wholeStatement+   )
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
  (subroutineName callArgumentList?)
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
  | v=varRef 
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

subscripts:
  ^(SUBSCRIPT expression+);
   /* By using another temporary btmp, we account for subscripts occuring inside subscript 
      expression, and make sure that bsubscript is always on in the subscript region.
    */ 
 
varRef:      
      ^(ARRAYREF ID=NAME s=subscripts)
    |   ID=NAME
  ;  

/* 92 */
arrayName :  
  NAME
  ;

/* 97 */
subroutineName : 
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
  
ppmdirectives
@after{
   if (brepack) {
     FTree newTree = null;
     FTree ppmdirTree = null, attrTree = null;
     int index = -1;
   
     /* Add ^(PPMDIRECTIVE LONGIT) as a next statement */      
     ppmdirTree = (FTree)adaptor.create(PPMDIRECTIVE,"cppm$");
     attrTree = (FTree)adaptor.create(LONGIT,"LONGIT");
     ppmdirTree.addChild(attrTree);     
   
     newTree = (FTree)adaptor.nil();
     newTree.addChild(retval.tree);
     newTree.addChild(ppmdirTree);
   
     retval.tree = newTree;
   }
}:
   ^(PPMDIRECTIVE REPACK)  {brepack = true;} 
  |^(PPMDIRECTIVE PIPELINE)  {bpipelineouter = true;}
  |^(PPMDIRECTIVE ~(REPACK | PIPELINE | EOS)+)  
  ;

ppmdecldirectives :
   ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

  
