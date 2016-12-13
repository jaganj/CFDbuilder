//===--------------- FortranParser.g - Fortran parser grammar -------------===//
//
//  This is a ANTLR v3 grammar file for parsing (syntactic analysis of) the 
//  Fortran input.
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


parser grammar FortranParser;
options {
  output=AST;
  language=Java;     
  ASTLabelType=FTree;
  backtrack=true;
  tokenVocab=tokenFile;
}

@header{
package parser;
import lexer.*;
import translator.FTree;
import java.util.StringTokenizer;
import java.util.Stack;
import java.lang.StringBuffer;
import java.util.Hashtable;
import java.util.Set;
import java.util.Collection;
import java.util.Iterator;
import java.io.PrintStream;
}

@members{
  String currProcedure = "";  /* Name of the current procedure */
}

/* Start rule */
/*startprogram
    : program -> ^(CODEROOT program)
    ;
*/    
program
@after {
    $program.tree = (FTree)adaptor.becomeRoot(adaptor.create(CODEROOT, "CODEROOT"), $program.tree);
}
:
	((ppmdirectives)=>ppmdirectives | (directives)=>directives | (seos)=>seos! | executableUnit )+ // -> ^(CODEROOT $p)
	;

/* one unit of a fortran program */
executableUnit :
	(functionStatement)=>functionSubprogram 
	|mainProgram 
	|subroutineSubprogram 
	|blockdataSubprogram
	;

/* 2 */
mainProgram : /* Dont understand this */
	programStatement subprogramBody -> ^(MAINPROG programStatement subprogramBody)
	;

/* 3 */
functionSubprogram :
	functionStatement subprogramBody -> ^(FUNCTION functionStatement subprogramBody)
	;

/* 4 */
subroutineSubprogram :
	subroutineStatement subprogramBody -> ^(SUBROUTINE subroutineStatement subprogramBody)
	;

/* 5 - blockDataSubprogram */
blockdataSubprogram :
	blockdataStatement subprogramBody -> ^(BLOCKDATA blockdataStatement subprogramBody)
	;

/* 6 */
otherSpecificationStatement :
  dimensionStatement
  |allocatableStatement 
//  |allocateStatement 
  |equivalenceStatement 
	|intrinsicStatement 
	|saveStatement 
	|volatileStatement
//	|macroStatement!
	;

/* 7 */
executableStatement 
@init{
   if(!$doWithLabel.isEmpty()){
    //System.out.println("$$"+$doWithLabel::dolooplabel);
    //System.out.println(input.LT(1).getText() +":"+input.LT(2) );
      if(input.LA(1) == LABEL) {
        if($doWithLabel::dolooplabel == Integer.valueOf(input.LT(1).getText())){
          //System.out.println("CC:"+input.LT(1).getText());
          return retval;
        }
      }
   }
}
:
  assignmentStatement 
	|gotoStatement 
	|ifStatement
	|doStatement 
	|continueStatement 
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
programStatement:
	T_PROGRAM NAME seos -> ^(T_PROGRAM NAME)
	;

seos : 
	EOS+ -> NEWLINE 
	;

/* 9, 11, 13 */
/* 10 */
functionStatement :
	(type)? T_FUNCTION f=functionName {currProcedure = $f.text;} T_LPAREN (namelist)? T_RPAREN seos 
	 -> ^(T_FUNCTION type? functionName namelist?)
	;

blockdataStatement :
	T_BLOCK NAME seos -> ^(T_BLOCK NAME)
	;

/* 12 */
subroutineStatement :
	T_SUBROUTINE s=subroutineName {currProcedure = $s.text;} ( T_LPAREN (namelist)? T_RPAREN )? seos 
	 -> ^(T_SUBROUTINE subroutineName namelist?)
  ;
	
namelist :
	identifier ( T_COMMA identifier )*  -> ^(SUBARG identifier+)
	;

//statement :
//	formatStatement 
//	|includeStatement 
//	|implicitStatement 
//	|useStatement 
//	|parameterStatement 
//	|typeStatement 
//	|commonStatement 
//	|externalStatement 
//	|otherSpecificationStatement 	
//  |macroStatement!
//	|(statementFunctionStatement)=>statementFunctionStatement 
//	|executableStatement
//	;

/*
ArrayVarList will record all array variables in the list. 
The reason for this list is FunctionReference and array reference are identical 
in the parsing grammar. As in the parsing phase, symbol table isn't created yet.
The only way to distinguish function and array is though the array declaration.
Any variable in a(b,c) format but has no record in ArrayVarList will be treated
as FunctionReference. 
*/
subprogramBody	
scope{
ArrayList ArrayVarList;
}
@init{
$subprogramBody::ArrayVarList = new ArrayList();
}
@after{
$subprogramBody::ArrayVarList.clear();
$subprogramBody::ArrayVarList = null;
}
:
	declarationBlock executionBlock endStatement -> ^(SUBPROGRAMBLOCK declarationBlock executionBlock)
	;

declarationBlock:
  (declarationStatement)* -> ^(DECLARATIONBLOCK declarationStatement*)
  ;

executionBlock:
  ( wholeStatement )* -> ^(EXECUTIONBLOCK wholeStatement*)
  ;

declarationStatement:
    ppmdecldirectives seos! 
/*     directives seos!  ;   c$OMP MASTER which belongs to the execution block 
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
  | includeStatement seos!
  |implicitStatement seos!
  |useStatement seos!
  |parameterStatement seos! 
  |typeStatement seos!
  |commonStatement seos!
  |dataStatement seos!
  |externalStatement seos!
  |otherSpecificationStatement seos!
  |seos!
  ;

macroStatement :
//  MARCO 
//  (
//  T_DEFINE NAME ICON
//  | T_IF (~EOS)+
//  | T_ELSE (~EOS)+
//  | T_ENDIF
//  )
  MACRO (~EOS)+
  ;

wholeStatement :
  ppmdirectives
	| directives 
	| seos! 
//	| statement seos! 
//	| LABEL statement seos -> ^(LABEL statement) 
  |formatStatement seos!
  |allocateStatement seos!
  |macroStatement!
  |(statementFunctionStatement seos!)=>statementFunctionStatement 
  |executableStatement seos!
	 ;

endStatement :
	 (LABEL)? T_END seos -> ^(T_END LABEL?)
	 ;

/* 15 */
dimensionStatement : 
	T_DIMENSION arrayDeclarator (T_COMMA arrayDeclarator)* -> ^(T_DIMENSION arrayDeclarator+)
	;
	
allocatableStatement :	
	T_ALLOCATABLE^ entityList (T_COMMA! entityList)*
	;
	
entityList :	
	arrayName deferredShapeSpecList? 
	;	
	
deferredShapeSpecList :	
	T_LPAREN! T_COLON (T_COMMA! T_COLON)* T_RPAREN!
	;	
	
allocateStatement :	
	T_ALLOCATE T_LPAREN arrayDeclarator (T_COMMA arrayDeclarator)* T_RPAREN 
	 -> ^(T_ALLOCATE arrayDeclarator+)
	;

/* 16 */
arrayDeclarator 
@after{
$subprogramBody::ArrayVarList.add($ID.text);
}
:
	ID=arrayName T_LPAREN arrayDeclaratorExtents T_RPAREN 
	 -> ^(ARRAY arrayName arrayDeclaratorExtents)
	;

arrayDeclaratorExtents :
	arrayDeclaratorExtent (T_COMMA arrayDeclaratorExtent)* 
	 -> ^(ADEXTS arrayDeclaratorExtent arrayDeclaratorExtent*)
	;

arrayDeclaratorExtent : 
	iexpr (T_COLON iexpr)? -> ^(ADEXT iexpr iexpr?)
	;

/* 17 */
equivalenceStatement : 
	T_EQUIVALENCE equivEntityGroup (T_COMMA equivEntityGroup)* 
	 -> ^(T_EQUIVALENCE equivEntityGroup+)
	;

/* 18 */
equivEntityGroup : 
	T_LPAREN varRef T_COMMA varRef T_RPAREN -> ^(EQUIVGRP varRef varRef)
	;

/* 19 */
commonStatement :
	T_COMMON commonBlock (T_COMMA commonBlock)* -> ^(T_COMMON commonBlock+)
	|T_COMMON commonItems -> ^(T_COMMON commonItems) 
	;

commonName : 
	T_DIV (NAME T_DIV | T_DIV) 
	;

commonItem :
	NAME 
	| arrayDeclarator 
	;

commonItems :
	commonItem (T_COMMA! commonItem)* 
	;

commonBlock :
	commonName commonItems 
	;

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
	type typeStatementNameList -> ^(TYPESTMT type typeStatementNameList)
	;

typeStatementNameList :
	typeStatementName (T_COMMA! typeStatementName)*
	;
	
typeStatementName :
	NAME 
	|	arrayDeclarator 
	;
	
typename :
  T_REAL 
  |T_DOUBLE! T_PRECISION 
  |T_INTEGER 
  |T_LOGICAL 
  |T_CHARACTER
	;
	
type : 
	typename typenameLen?
	;

typenameLen :
	T_STAR lenSpecification -> ^(T_STAR lenSpecification)
	;

includeStatement :	
	T_INCLUDE SCON -> ^(T_INCLUDE SCON)
	;
	
/* 21 */
implicitStatement : 
	T_IMPLICIT^ (implicitNone | implicitSpecs) 
	;

implicitSpec : 
	type T_LPAREN implicitLetters (T_COMMA implicitLetters)* T_RPAREN -> ^(type implicitLetters+)
	;

implicitSpecs : 
	implicitSpec (T_COMMA! implicitSpec)*
	;

implicitNone : 
	T_NONE 
	;

implicitLetter : 
	NAME 
	; 
	
implicitRange : 
	implicitLetter (T_MINUS implicitLetter)? -> ^(IMPLICITRANGE implicitLetter+)
	;

implicitLetters : 
	implicitRange (T_COMMA! implicitRange)* 
	;

/* 22 */
lenSpecification : 	
	ICON |
	T_LPAREN expression T_RPAREN
	;
	
useStatement :
  T_USE^ NAME 
  ;

/* 23 */
parameterStatement : 
	T_PARAMETER T_LPAREN paramlist T_RPAREN -> ^(T_PARAMETER paramlist)
	;

paramlist : paramassign ( T_COMMA! paramassign )* 
;

paramassign : 
	NAME T_ASSIGN expression -> ^(T_ASSIGN NAME expression)
	;

/* 24 */
externalStatement :
	T_EXTERNAL namelist -> ^(T_EXTERNAL namelist)
	;

/* 25 */
intrinsicStatement : 
	T_INTRINSIC namelist 
	;

/* 26 */
saveStatement : 
	T_SAVE (saveEntity (T_COMMA saveEntity)*)? -> ^(T_SAVE saveEntity+)
	;

saveEntity :  
	NAME 
	| T_DIV NAME T_DIV -> ^(T_DIV NAME T_DIV) 
	;

volatileStatement : 
  T_VOLATILE (NAME (T_COMMA NAME)*)? -> ^(T_VOLATILE NAME+)
  ;

/* 29 */
assignmentStatement : 
	varRef T_ASSIGN expression -> ^(T_ASSIGN varRef expression)
	;

/* 30 */
gotoStatement : 
	(T_GOTO | T_GO to) (u=unconditionalGoto)	 -> ^(GOTO unconditionalGoto)
	 |(T_GOTO | T_GO to) ( assignedGoto)    -> ^(GOTO assignedGoto)
	;

/* 31 */
unconditionalGoto : 
	lblRef 
	;

lblRef :
	ICON -> ^(LABELREF ICON)
	;

labelList : 
	lblRef (T_COMMA! lblRef)* 
	;

/* 33 */
assignedGoto : 
	NAME ( (T_COMMA!)? T_LPAREN! labelList T_RPAREN! )? 
	;

/* 34 */
ifStatement :
	T_IF T_LPAREN expression T_RPAREN
	(b=blockIfStatement | l=logicalIfStatement /*| arithmeticIfStatement*/) 
		-> ^(T_IF expression $b? $l?)
	;

//arithmeticIfStatement : 
//	lblRef T_COMMA! lblRef T_COMMA! lblRef 
//	;

/* 35 */
logicalIfStatement : 
	executableStatement -> ^(THENBLOCK executableStatement)
	;

/* 36 */

firstIfBlock :
	T_THEN seos
	wholeStatement* -> ^(THENBLOCK wholeStatement*)
	;

blockIfStatement : 
	firstIfBlock
	(options { greedy=true; } :
		elseIfStatement)*
	(elseStatement)?
	endIfStatement
	;


/* 37 */
elseIfStatement : 
	(T_ELSEIF | T_ELSE T_IF)
	T_LPAREN expression T_RPAREN T_THEN seos
	wholeStatement* 
	 -> ^(ELSEIF expression ^(THENBLOCK wholeStatement*)) 
	
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
	T_ELSE seos 
	wholeStatement* 
	 -> ^(ELSEBLOCK wholeStatement*)
	;

/* 39 */
endIfStatement : 
	T_ENDIF! 
	| T_END! T_IF!
	;

/* 40 */
doStatement : 
	T_DO (doWithLabel | doWithEndDo) -> ^(T_DO doWithLabel? doWithEndDo?)
	;

doVarArgs :
	identifier T_ASSIGN expression T_COMMA expression ( T_COMMA expression )? 
	 -> ^(DOVARARG identifier expression expression expression?)
	;

doWithLabel
scope {
  int dolooplabel;
}
:
	l=lblRef{$doWithLabel::dolooplabel=Integer.valueOf($l.tree.getChild(0).getText());}
	doVarArgs
	seos!
	doBody
	enddoStatement!
	;

doBody :
	wholeStatement* -> ^(DOBLOCK wholeStatement*)	
	// this action ensures that the loop body's statements are children of a
	// CODEBLOCK node for the loop.
	;

doWithEndDo :
	doVarArgs
	seos!
	doBody
	enddoStatement!
	;
	
enddoStatement : 
	T_ENDDO! 
	| T_END! T_DO! 
	| continueStatement
  ;

/* 41 */
continueStatement returns[int continuelabel]
: 
	(l=LABEL {$continuelabel=Integer.valueOf($l.text);})? T_CONTINUE -> ^(T_CONTINUE LABEL?) 
	;

/* 42 */
stopStatement : 
	T_STOP (ICON)? -> ^(T_STOP ICON?)
	;

/* 43 */
pauseStatement : 
	T_PAUSE (ICON)? -> ^(T_PAUSE ICON?)
	;

iocontrolArgs
//@after{System.out.println("got IO control:"+retval.tree.toStringTree());}
:
  T_LPAREN
  (ICON|varRef|T_STAR)
  (T_COMMA
  (lblRef|T_STAR))?
  T_RPAREN
;


/* 44 */
writeStatement : 
	T_WRITE^ iocontrolArgs (~EOS)*
	;

/* 45 */
readStatement : 
	T_READ^ iocontrolArgs (~EOS)*
	;	

/* 46 */
printStatement : 
	T_PRINT^ (~EOS)+
	;

/* 50 */
openStatement : 
	T_OPEN^ (~EOS)+
	;

/* 51 */
closeStatement : 
	T_CLOSE^ (~EOS)+
	;

/* 52 */
inquireStatement : 
	T_INQUIRE^ (~EOS)+
	;

/* 53 */
backspaceStatement : 
	T_BACKSPACE^ (~EOS)+
	;

/* 54 */
endfileStatement : 
	T_ENDFILE^ (~EOS)+
	;

/* 55 */
rewindStatement : 
	T_REWIND^ (~EOS)+
	;

/* 58-59 */
formatStatement : 
	LABEL? T_FORMAT^ (~EOS)+
	;

/* 70 */
statementFunctionStatement : 
	T_LET^ (~EOS)+
	;

/*Function_reference*/
functionReference :
	functionName  T_LPAREN f=functionArgumentList T_RPAREN 
	 -> ^(FUNCREF functionName functionArgumentList)
	;
	
functionArgumentList:	
	functionArgument? (T_COMMA functionArgument)* 
	 -> ^(FUNCARG functionArgument*)
	;

functionArgument :
	expression
	;
	
/* 71 */
callStatement : 
	T_CALL subroutineCall -> ^(T_CALL subroutineCall)
	;

subroutineCall :
	subroutineName (T_LPAREN! (callArgumentList)? T_RPAREN!)?
	;

callArgumentList : 
	callArgument (T_COMMA callArgument)* -> ^(CALLARG callArgument+)
	;
	
callArgument : 
	expression
	;

/* 72 */
returnStatement : 
	T_RETURN ( integerExpr )? -> ^(T_RETURN integerExpr?)
	;

/* 74 */
expression : 
	lexpr2 (T_LOR^ lexpr2)* 
	;
	
lexpr2 : 
	lexpr3 (T_LAND^ lexpr3)* 
	;
	
lexpr3 : 
	T_LNOT^ lexpr4 
	| lexpr4 
	;

lexpr4 : 
	aexpr0 ( (T_LT^ | T_LE^ | T_EQ^ | T_NE^ | T_GT^ | T_GE^) aexpr0 )? 
	;

aexpr0 : 
	aexpr1 ((T_PLUS^ | T_MINUS^) aexpr1)* 
	;

aexpr1 : 
	aexpr2 ((T_STAR^ | T_DIV^)  aexpr2)* 
	;

aexpr2 : 
	T_MINUS aexpr3 -> ^(NEG aexpr3)
	| aexpr3
	;	 

aexpr3 : 
	aexpr4 ( T_POWER^ aexpr4 )* 
	;

aexpr4 : 
	(unsignedArithmeticConstant)=> unsignedArithmeticConstant 
	| SCON
	| logicalConstant  
	| {!($subprogramBody::ArrayVarList.contains(input.LT(1).getText()))}? functionReference
	| varRef 
	| T_LPAREN! expression T_RPAREN!		  
	;

/* integer expression */
iexpr :
	iexpr1 ((T_PLUS^ | T_MINUS^) iexpr1)* 
	;

/* integer expression with fpe return code. */
//iexprCode :
//	iexpr1 ((T_PLUS^ | T_MINUS^) iexpr1)* ;

iexpr1:
	iexpr2 ((T_STAR^ | T_DIV^) iexpr2)* 
	;

iexpr2: 
	iexpr3 (T_POWER^ iexpr3)? 
	;

iexpr3:
	ICON 
	| varRef 
	| T_LPAREN! iexpr T_RPAREN! 
	| T_MINUS iexpr3 -> ^(NEG iexpr3)
	;

/* 77 */
integerExpr : 
	iexpr 
	;

concatOp :
	T_DIV T_DIV 
	;

/* 88 */
arrayElementName : 
	NAME T_LPAREN integerExpr (T_COMMA integerExpr)* T_RPAREN -> ^(NAME integerExpr+)
	;

subscripts :
	T_LPAREN( expression (T_COMMA expression)* ) T_RPAREN -> ^(SUBSCRIPT expression+)
	;

varRef :
  NAME
	|NAME (subscripts ( substringApp )? ) -> ^(ARRAYREF NAME subscripts) 
	;

substringApp :
	T_LPAREN (l1=lexpr2)? T_COLON (l2=lexpr2)? T_RPAREN -> ^(T_COLON $l1? $l2?)
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
	|ID=NAME {System.out.println("Warning: \""+$ID.text+"\" will be parsed to functionName in "+currProcedure
	                             +"!\nPlease check the function declaration on your own!");} 
	;

/* 101 */
unsignedArithmeticConstant : 
	ICON 
	| RCON
	| DCON
  ;

/* 108 */
logicalConstant : 
	T_TRUE
	| T_FALSE
	;

// needed because Fortran doesn't have reserved keywords. Putting the rule
// "keyword" instead of a few select keywords breaks the parser with harmful
// non-determinisms
identifier :
	NAME 
	;

to : 
	n=NAME {$n.getText().compareToIgnoreCase("to") == 0}? {$n.setType(TO);} 
	;

directives :
		DIRECTIVE^ (T_AND)? (~EOS)+ seos! 
	;

/*
PHLIN: I tried to retain only PPM directives. The parser will check the first token that follows
!PPM$ and recognize that belongs to declaration block or execution block. Whenever new directive
is introduced, please add the checking here and TokenizePPMdir.g in translator package.
*/

ppmdirectives :
  PPMDIRECTIVE^ (n=~EOS)+ seos! //{System.out.println("I saw PPM directives:"+$n.toString());}
;


//ppmdecldirectives :
//  PPMDIRECTIVE^ (n=~EOS) if {System.out.println("I see doublebuffer");} (~EOS)+ seos! {System.out.println("I saw PPM decl directives:"+$n.toString());}
// ;

ppmdecldirectives :
  /* Specify all the declaration directives explicitly here.  Otherwise, if an 
   * execution block PPM directive appears right at the end of the declaration
   * section, this section tries to match it.
   */
 PPMDIRECTIVE^ (n=~EOS)
 {  $n.getText().toLowerCase().contentEquals("dedoublebuffer") 
  | $n.getText().toLowerCase().contentEquals("penlowerbound") 
  | $n.getText().toLowerCase().contentEquals("penupperbound") 
 }? 
    (~EOS)+   //{System.out.println("I saw PPM directives:"+$n.toString());} 
 ;

