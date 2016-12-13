//===------------- FortranTreeParser.g - Fortran AST reference ------------===//
//
//  This is a ANTLR v3 grammar file for the Fortran syntax expressed in AST. 
//  It is meant for reference only.
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

tree grammar FortranTreeParser;

options {
  language = Java;
  backtrack=true;
  output = AST;
  tokenVocab = tokenFile;
  ASTLabelType = CommonTree;
}

@header{package parser;}

program :  ^(CODEROOT (ppmdirectives|directives|executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit :
  functionSubprogram 
  | mainProgram 
  | subroutineSubprogram 
  | blockdataSubprogram
  ;

/* 2 */
mainProgram :
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
    |allocatableStatement 
//    |allocateStatement 
    |equivalenceStatement 
    |intrinsicStatement 
    |saveStatement
    |volatileStatement
//    |macroStatement
  ;

/* 7 */
executableStatement :
   assignmentStatement 
  |gotoStatement 
  |ifStatement 
  |doStatement 
//  |continueStatement 
  |stopStatement 
  |pauseStatement
  |readStatement
  |writeStatement
  |printStatement
  |rewindStatement
  |backspaceStatement
  |openStatement
  |closeStatement
  |endfileStatement
  |inquireStatement
  |callStatement
  |returnStatement
  ;

/* 8 */
programStatement :
  ^(T_PROGRAM NAME);

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
  ^(SUBARG identifier+) ;

//statement :
//   formatStatement 
//  |includeStatement 
//  |implicitStatement 
//  |useStatement
//  |parameterStatement 
//  |typeStatement 
//  |commonStatement 
//  |externalStatement 
//  |otherSpecificationStatement  
//  |statementFunctionStatement 
//  |executableStatement
//  ;

subprogramBody  :
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
  |includeStatement
  |implicitStatement
  |useStatement
  |parameterStatement 
  |typeStatement
  |commonStatement 
  |dataStatement
  |externalStatement
  |otherSpecificationStatement
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

endStatement :
   ^(T_END LABEL?)
   ;

/* 15 */
dimensionStatement : 
  ^(T_DIMENSION arrayDeclarator+);
  
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
  ^(T_ALLOCATE arrayDeclarator+);

/* 16 */
arrayDeclarator :
  ^(arrayName arrayDeclaratorExtents)
  ;

arrayDeclaratorExtents :
  arrayDeclaratorExtent+;

arrayDeclaratorExtent : 
  ^(ADEXT T_COLON? expression+)
  ;

/* 17 */
equivalenceStatement : 
  ^(T_EQUIVALENCE equivEntityGroup+);

/* 18 */
equivEntityGroup : 
  ^(EQUIVGRP varRef+);

/* 19 */
commonStatement :
  ^(T_COMMON (commonBlock+ |commonItems)) ;

commonName : 
  T_DIV (NAME T_DIV | T_DIV) ;

commonItem :
  NAME | arrayDeclarator ;

commonItems :
  commonItem+ ;

commonBlock :
  commonName commonItems ;

dataStatement:
  T_DATA dataBlock (T_COMMA dataBlock)* -> ^(T_DATA dataBlock+)
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
  NAME |  arrayDeclarator 
  ;
  
typename :
  (T_REAL
   |T_PRECISION
   |T_INTEGER
   |T_LOGICAL
   |T_CHARACTER)
  ;
  
type : 
  typename typenameLen?;

typenameLen :
  ^(T_STAR lenSpecification);

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
  implicitSpec+;

implicitNone : 
  T_NONE ;

implicitLetter : 
  n=NAME ; 
  
implicitRange : 
  ^(IMPLICITRANGE implicitLetter implicitLetter?);

implicitLetters : 
  implicitRange+ ;

/* 22 */
lenSpecification :  
  ICON |
  T_LPAREN expression T_RPAREN
  ;

useStatement:
  ^(T_USE NAME)
;

/* 23 */
parameterStatement : 
  ^(T_PARAMETER paramlist);

paramlist : paramassign ( paramassign )* ;

paramassign : 
  ^(T_ASSIGN NAME expression);

/* 24 */
externalStatement :
  ^(T_EXTERNAL namelist);

/* 25 */
intrinsicStatement : 
  T_INTRINSIC namelist 
  ;

/* 26 */
saveStatement : 
  ^(T_SAVE (s+=saveEntity+)?);

saveEntity :  
    NAME 
  | ^(T_DIV NAME T_DIV) ;

volatileStatement : 
  ^(T_VOLATILE NAME+)
  ;
  
/* 29 */
assignmentStatement : 
  ^(T_ASSIGN varRef expression)
  ;

/* 30 */
gotoStatement : 
  ^(T_GO
  (unconditionalGoto | assignedGoto))
  ;

/* 31 */
unconditionalGoto : 
  lblRef ;

lblRef :
  ^(LABELREF ICON)
  ;

labelList : 
  lblRef (lblRef)* ;

/* 33 */
assignedGoto : 
  NAME (labelList)? ;

/* 34 */
ifStatement :
  ^(T_IF expression blockIfStatement)
  | ^(T_IF expression logicalIfStatement)
//  | ^(T_IF expression arithmeticIfStatement)
  ;
  
//arithmeticIfStatement : 
//  lblRef  lblRef  lblRef ;

/* 35 */
logicalIfStatement : 
  ^(THENBLOCK executableStatement)
  ;

/* 36 */

firstIfBlock :
  ^(THENBLOCK wholeStatement+)
  ;

blockIfStatement : 
  firstIfBlock
  (elseIfStatement)*
  (elseStatement)?
//  endIfStatement
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
  ^(T_DO (doWithEndDo|doWithLabel) );

doVarArgs :
  ^(DOVARARG identifier expression 
  expression (expression )?)
  ;

doWithLabel :
  lblRef 
  doVarArgs 
  doBody
  //enddoStatement
  ;

doBody :
  ^(DOBLOCK wholeStatement*) 
  ;

doWithEndDo :
  doVarArgs
  doBody
//  enddoStatement
  ;
  
enddoStatement : 
  (T_ENDDO! | T_END! T_DO!| continueStatement) ;

/* 41 */
continueStatement : 
  ^(T_CONTINUE LABEL?) ;

/* 42 */
stopStatement : 
  ^(T_STOP ICON?);

/* 43 */
pauseStatement : 
  ^(T_PAUSE ICON?);

iocontrolArgs:
  T_LPAREN
  (ICON|varRef|T_STAR)
  (T_COMMA
  (lblRef|T_STAR))?
  T_RPAREN
;

/* 44 */
writeStatement : 
  ^(T_WRITE iocontrolArgs (~EOS)*);

/* 45 */
readStatement : 
  ^(T_READ iocontrolArgs (~EOS)*);

/* 46 */
printStatement : 
  ^(T_PRINT (~EOS)+);

/* 50 */
openStatement : 
  ^(T_OPEN (~EOS)+);

/* 51 */
closeStatement : 
  ^(T_CLOSE (~EOS)+);

/* 52 */
inquireStatement : 
  ^(T_INQUIRE (~EOS)+);

/* 53 */
backspaceStatement : 
  ^(T_BACKSPACE (~EOS)+);

/* 54 */
endfileStatement : 
  ^(T_ENDFILE (~EOS)+);

/* 55 */
rewindStatement : 
  ^(T_REWIND (~EOS)+);

/* 58-59 */
formatStatement : 
  ^(T_FORMAT LABEL? (~EOS)+);

/* 70 */
statementFunctionStatement : 
  ^(T_LET (~EOS)+);

/*Function_reference*/
functionReference :
  ^(FUNCREF functionName functionArgumentList);
  
functionArgumentList: 
  ^(FUNCARG functionArgument*);

functionArgument :
  expression;
  
/* 71 */
callStatement : 
  ^(T_CALL subroutineCall);

subroutineCall :
  (subroutineName callArgumentList?);

callArgumentList : 
  ^(CALLARG callArgument+);
  
callArgument : 
  expression;

/* 72 */
returnStatement : 
  ^(T_RETURN expression?);

/* 74 */
expression : 
  
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
  ;


concatOp :
  T_DIV T_DIV 
  ;

/* 88 */
arrayElementName : 
  ^(NAME expression+);

subscripts :
  ^(SUBSCRIPT expression+) ;

varRef :
  NAME
  ^(ARRAYREF NAME subscripts) ;

//substringApp :
//  ^(T_COLON lexpr2? lexpr2?);

/* 92 */
arrayName :  
  NAME;

/* 97 */
subroutineName : 
  NAME ;

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
  |NAME 
  ;

/* 101 */
unsignedArithmeticConstant : 
  ICON 
  | RCON
  | DCON
  ;

/* 108 */
logicalConstant : 
  (T_TRUE | T_FALSE)
  ;

identifier :
  NAME 
  ;

//to : 
//  n=NAME {$n.getText().compareToIgnoreCase("to") == 0}? {$n.setType(TO);} ;

directives :
    ^(DIRECTIVE (T_AND)? (~EOS)+)
  ;

ppmdirectives :
    ^(PPMDIRECTIVE n=(~EOS)+)
  ;

ppmdecldirectives :
    ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;
 