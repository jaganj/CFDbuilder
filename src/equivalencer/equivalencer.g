//===---------------- equivalencer.g - control data layout ----------------===//
//
//  Equivalence all the automatic arrays in the computational region to one big
//  array. This allows us to somewhat control the virtual addresses of the
//  temporaries to avoid set conflicts in L1D cache. Also, helps overcome any
//  compiler bugs in managing so many variables.
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

tree grammar equivalencer;

/* 01/02/2011:
   Pattern matching ANTLRs are very elegant, but seems to be very error-prone.  Adding statements
   (nodes/children) seems to work, but deletion doesn't.  TreeVisitor visit method gets all 
   confused.  I performed a whole bunch of experiments from deleting statements in the reverse
   order to getting child index at the time of deletion.  The conclusion is deletion doesn't
   work with pattern matching grammars.  It forces me to use the more clumsy tree grammars, but,
   as demonstrated below, they atleast work.
   
    - JJ     
 */
 
options {
    language = Java;
    backtrack=true;          // allow backtracking if necessary
    output=AST;              // build ASTs from input AST
    tokenVocab=tokenFile;     
    ASTLabelType=FTree; // we're using FTree nodes
}


@header{
package equivalencer;
import lexer.*;
import symbol.*;
import translator.*;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
}
@members{ 
   Scope currentScope = null;
   Tmps tmps = new Tmps();
      
   public void createBigArray(FTree declBlk){
       FTree newStmts = (FTree)adaptor.nil();
       
       int bigArraySize = tmps.bigArraySize;
       /* Round bigArraySize to a multiple of 16 */
       if (bigArraySize \% 16 > 0)
           bigArraySize = ((bigArraySize / 16) + 1) * 16;
         
       /*** Create the Big Array ***/
       /*  dimensionStatement:      ^(T_DIMENSION arrayDeclarator+)       
           arrayDeclarator:         ^(ARRAY arrayName arrayDeclaratorExtents)
           arrayName :                NAME
           arrayDeclaratorExtents : ^(ADEXTS arrayDeclaratorExtent arrayDeclaratorExtent*)  
           arrayDeclaratorExtent :  ^(ADEXT iexpr iexpr?)
           iexpr:                     ICON
        */       
       FTree adext = (FTree)adaptor.create(ADEXT,"ADEXT");
       adext.addChild((FTree)adaptor.create(ICON,Integer.toString(bigArraySize/4)));
       FTree adexts = (FTree)adaptor.create(ADEXTS,"ADEXTS");
       adexts.addChild(adext);
       
       FTree arrdecl = (FTree)adaptor.create(ARRAY,"ARRAY");
       arrdecl.addChild((FTree)adaptor.create(NAME,"bigArray"));
       arrdecl.addChild(adexts);
       
       FTree dimStmt = (FTree)adaptor.create(T_DIMENSION,"dimension");
       dimStmt.addChild(arrdecl);
       newStmts.addChild(dimStmt);
       
       int totaltmps = tmps.tmpstable.size();
       int ioffset = 1, tmpsize = 0;

       ArraySymbol basym;
       String name = "bigArray";
       currentScope = tmps.tmpstable.get(0).sym.scope;
       
       /*
          arrayDeclaratorExtents : ^(ADEXTS arrayDeclaratorExtent arrayDeclaratorExtent*);
          arrayDeclaratorExtent :  ^(ADEXT expression expression?)
        */
       FTree badeclext = (FTree)adaptor.create(ADEXT,"ADEXT");
       badeclext.addChild((FTree)adaptor.create(ICON,Integer.toString(bigArraySize/4)));
       FTree badeclexts = (FTree)adaptor.create(ADEXTS,"ADEXTS");
       badeclexts.addChild(badeclext);  
       
       basym = new ArraySymbol(name,badeclexts,(BaseScope)currentScope);
       currentScope.define(basym);                 
       
       FTree bigArrBase = (FTree)adaptor.create(ARRAYREF,"ARRAYREF");
       FTree baref = (FTree)adaptor.create(NAME,"bigArray");
       baref.symbol = basym; 
       bigArrBase.addChild(baref);       
       
       /* Create the equivalences */       
       for(int i=0; i<totaltmps; i++){
       /*  equivalenceStatement : ^(T_EQUIVALENCE equivEntityGroup+)
           equivEntityGroup :     ^(EQUIVGRP varRef varRef)  
           varRef :                 NAME  | ^(ARRAYREF NAME subscripts) 
           subscripts:            ^(SUBSCRIPT expression+)
        */
           FTree subscripts = (FTree)adaptor.create(SUBSCRIPT,"SUBSCRIPT");
           subscripts.addChild((FTree)adaptor.create(ICON,Integer.toString(ioffset)));
           
           FTree bigArr = (FTree)adaptor.dupTree(bigArrBase);
           bigArr.addChild(subscripts);
           
           FTree equivgrp = (FTree)adaptor.create(EQUIVGRP,"EQUIVGRP");
           Symbol tmpsym =  tmps.tmpstable.get(i).sym;

           FTree varref = (FTree)adaptor.create(NAME,tmpsym.name);
           varref.symbol = tmpsym;
           equivgrp.addChild(varref);
           equivgrp.addChild(bigArr);      
                
           FTree equivStmt = (FTree)adaptor.create(T_EQUIVALENCE,"equivalence"); 
           equivStmt.addChild(equivgrp);
           newStmts.addChild(equivStmt);           
            
           /* Update ioffset */ 
           tmpsize = ((ArraySymbol)tmpsym).itotalsize * tmpsym.typelen;
           /* Round tmpsize to a multiple of 16 bytes */
           if(tmpsize \% 16 > 0)
              tmpsize = ((tmpsize / 16) + 1) * 16;
              
           ioffset = ioffset + tmpsize / 4;                      
       }

       declBlk.addChild(newStmts);
   }      
} 

program :
  ^(CODEROOT (ppmdirectives | directives | executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit :
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
executableStatement:
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
subroutineStatement :
  ^(T_SUBROUTINE subroutineName namelist?)
  ;
  
namelist :
  ^(SUBARG identifier+)
  ;

subprogramBody
@after{
    if (tmps.tmpstable.size() > 0)
    {
       tmps.rearrange();
       createBigArray((FTree)retval.tree.getChild(0));
       tmps.reset();
    }
}:
  ^(SUBPROGRAMBLOCK declarationBlock executionBlock)
  ;

declarationBlock :
  ^(DECLARATIONBLOCK declarationStatement*)
  ;

executionBlock :
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
  
wholeStatement 
// s c o p e{
  //boolean bremoveend;
//}
//@init{
  //$wholeStatement::bremoveend = false;
//}
//@after{
  //if($wholeStatement::bremoveend){
    // int ibeginIndex = -1, iendIndex = -1;
     //FTree parent;
     
     //ibeginIndex = removebegin.getChildIndex();
     //iendIndex = removeend.getChildIndex();
     //parent = (FTree)removebegin.getParent();
     
     //for (int i=iendIndex; i>=ibeginIndex; i--){
       //  parent.deleteChild(i);
     //}
     //$wholeStatement::bremoveend = false;
  //}
//}
  :
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
  NAME T_DIV (DCON | RCON | ICON) T_DIV
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
  ^(T_DO (doWithEndDo|doWithLabel) )
  ;

doVarArgs
 :
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
expression :  
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
  | ^(T_PLUS e1=expression e2=expression)
  | ^(T_MINUS e1=expression e2=expression)
  | ^(T_STAR e1=expression e2=expression)
  | ^(T_DIV e1=expression e2=expression)
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

subscripts :
  ^(SUBSCRIPT (e=expression)+);
 
varRef :      
      ^(ARRAYREF ID=NAME s=subscripts
          {            
            tmps.add((ArraySymbol)$ID.symbol);
          })
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
  
ppmdirectives:
    ^(PPMDIRECTIVE (~EOS)+)
  ;
   
ppmdecldirectives :
     ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;
 

  
