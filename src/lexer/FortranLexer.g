//===---------------- FortranLexer.g - Fortran lexer grammar --------------===//
//
//  This is a ANTLR v3 grammar file for tokenizing (lexical analysis of) the 
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

lexer grammar FortranLexer;

options {
    language=Java;
    //tokenVocab=FortranLexer;
    backtrack=true;
    //filter=true;
}

tokens
{
	XCON;
	PCON;
	FCON;
	RCON;
}

@header{package lexer;}
/*#****************************************************************************
 * The Fortran Lexer
 *****************************************************************************/


DIRECTIVE
	:
	COMMENTPREFIX ('dec$' | 'DEC$' | 'dir$' | 'DIR$' | '$OMP')
	;	

/* Transform directives - not any of the regular Fortran directives */	
PPMDIRECTIVE
	:
	COMMENTPREFIX 'PPM$'	
	;	

//PPMDECLDIRECTIVE
//  :
//  COMMENTPREFIX 'ppd$'    
//  ;	
// Fortran 77 comments must start with the character on the first column
// we keep the comments inside the AST. See parser rules 'wholeStatement'.
// We however trim empty comment lines.

MACRO :
  ({getCharPositionInLine()==0}?=>T_SHARP)
  ;

LABEL	:	
  ({getCharPositionInLine()<5}?=>NUM)+ 
  ;

// Need 4 lookahead for logical operators (eg .NE. and .NEQV.)
T_AND          : '&'  ;
T_ASSIGN       : '='  ;
T_BACKSLASH    : '\\' ;
T_COLON        : ':'  ;
T_COMMA        : ','  ;
//CONCAT     : '//' ; // define in parser. Not all // are concat ops.
T_DIV          : '/'  ;
T_DOLLAR       : '$'  ;
T_DQUOTE       : '\"' ;
T_LPAREN       : '('  ;
T_MINUS        : '-'  ;
T_OR           : '|'  ;
T_POWER        :{getCharPositionInLine() > 0}?=> '**' ; // not a comment
T_PLUS         : '+'  ;
T_RPAREN       : ')'  ;
T_SHARP        : '#'  ;
T_STAR         :{getCharPositionInLine() > 0}?=> '*'  ; // not a comment
T_SQUOTE       : '\'' ;

T_LNOT         : '.not.'   ;
T_LAND         : '.and.'   ;
T_LOR          : '.or.'    ;
T_EQV          : '.eqv.'   ;
T_NEQV         : '.neqv.'  ;
T_XOR          : '.xor.'   ;
T_EOR          : '.eor.'   ;
T_LT           : '.lt.'    ;
T_LE           : '.le.'    ;
T_GT           : '.gt.'    ;
T_GE           : '.ge.'    ;
T_NE           : '.ne.'    ;
T_EQ           : '.eq.'    ;
T_TRUE         : '.true.'  ;
T_FALSE        : '.false.' ;

// Token list
T_INTEGER       :       'integer'       ;
T_REAL          :       'real'          ;
T_COMPLEX       :       'complex'   
                        {System.out.println("Complex data type not supported.  Please remove them."); System.exit(0); }    ;
T_CHARACTER     :       'character'     ;
T_LOGICAL       :       'logical'       ;
T_ABS	        :	'abs'	        ;
T_ABSTRACT      :       'abstract'      ;
T_ACOS          :       'acos'          ;

T_ACHAR		:      	'achar'		;
T_ALLOCATABLE   :       'allocatable'   ;
T_ALLOCATE      :       'allocate'      ;
T_ALOG          :       'alog'          ;
T_ASSIGNMENT    :       'assignment'    ;
// ASSIGN statements are not supported.
//T_ASSIGN        :       'ASSIGN'        ;
T_ASSOCIATE     :       'associate'     ;
T_ASYNCHRONOUS  :       'asynchronous'  ;
T_ATAN          :       'atan'          ;
T_ATAN2         :       'atan2'         ;
T_BACKSPACE     :       'backspace'     ;
T_BLOCK         :       'block'         ;
T_BLOCKDATA     :       'blockdata'     ;
T_CALL          :       'call'          ;
T_CASE          :       'case'          ;
T_CLASS         :       'class'         ;
T_CLOSE         :       'close'         ;
T_COMMON        :       'common'        ;
T_CONTAINS      :       'contains'      ;
T_CONTINUE      :       'continue'      ;
T_COS           :       'cos'           ;
T_CYCLE         :       'cycle'         ;
T_DATA          :       'data'          ;
T_DABS          :       'dabs'          ;
T_DACOS         :       'dacos'         ;
T_DATAN         :       'datan'         ;
T_DATAN2        :       'datan2'        ;
T_DBLE          :       'dble'          ;
T_DCOS          :       'dcos'          ;
T_DEFAULT       :       'default'       ;
//T_DEFINE        :       'define'        ;
T_DEALLOCATE    :       'deallocate'    ;
T_DEFERRED      :       'deferred'      ;
T_DEXP          :       'dexp'          ;
T_DLOG          :       'dlog'          ;
T_DMAX          :       'dmax'          ;
T_DMIN          :       'dmin'          ;
T_DMOD          :       'dmod'          ;
T_DO            :       'do'            ;
T_DOUBLE        :       'double'        ;
T_DOUBLEPRECISION:      'doubleprecision' ;
T_DOUBLECOMPLEX:        'doublecomplex' 
                        {System.out.println("Complex data type not supported.  Please remove them"); System.exit(0); }    ;
T_DSIN          :       'dsin'          ;
T_DSQRT         :       'dsqrt'         ;
T_DTAN          :       'dtan'          ;
T_DTANH         :       'dtanh'         ;
T_ELEMENTAL     :       'elemental'     ;
T_ELSE          :       'else'          ;
T_ELSEIF        :       'elseif'        ;
T_ELSEWHERE     :       'elsewhere'     ;
T_ENTRY         :       'entry'         ;
T_ENUM          :       'enum'          ;
T_ENUMERATOR    :       'enumerator'    ;
T_EQUIVALENCE   :       'equivalence'   ;
T_EXIT          :       'exit'          ;
T_EXP		:	'exp'	        ;
T_EXTENDS       :       'extends'       ;
T_EXTERNAL      :       'external'      ;
T_FILE          :       'file'          ;
T_FINAL         :       'final'         ;
T_FLOAT         :       'float'         ;
T_FLUSH         :       'flush'         ;
T_FORALL        :       'forall'        ;
T_FORMAT        :       'format'       	;
T_FORMATTED     :       'formatted'     ;
T_FUNCTION      :       'function'      ;
T_GENERIC       :       'generic'       ;
T_GO            :       'go'            ;
T_GOTO          :       'goto'          ;
T_ICHAR         :       'ichar'         ;
T_IF            :       'if'            ;
T_IMPLICIT      :       'implicit'      ;
T_IMPORT        :       'import'        ;
T_IN            :       'in'            ;
T_INT           :       'int'           ;
T_INCLUDE	      :	      'include'     	;
T_INOUT         :       'inout'         ;
T_INTENT        :       'intent'        ;
T_INTERFACE     :       'interface'     ;
T_INTRINSIC     :       'intrinsic'     ;
T_INQUIRE       :       'inquire'       ;
T_LET		        :	      'let'	         	;
T_LOG		        :	      'log'	         	;
T_MAX		        :	      'max'		        ;
T_MIN		        :      	'min'		        ;
T_MOD           :       'mod'           ;
T_MODULE        :       'module'        ;
T_NAMELIST      :       'namelist'      ;
T_NONE          :       'none'          ;
T_NON_INTRINSIC :       'non_intrinsic' ;
T_NON_OVERRIDABLE:      'non_overridable';
T_NOPASS        :       'nopass'        ;
T_NULLIFY       :       'nullify'       ;
T_ONLY          :       'only'          ;
T_OPEN          :       'open'          ;
T_OPERATOR      :       'operator'      ;
T_OPTIONAL      :       'optional'      ;
T_OUT           :       'out'           ;
T_PARAMETER     :       'parameter'     ;
T_PASS          :       'pass'          ;
T_PAUSE         :       'pause'         ;
T_POINTER       :       'pointer'       ;
T_PRINT         :       'print'         ;
T_PRECISION     :       'precision'     ;
T_PRIVATE       :       'private'       ;
T_PROCEDURE     :       'procedure'     ;
T_PROGRAM       :       'program'       ;
T_PROTECTED     :       'protected'     ;
T_PUBLIC        :       'public'        ;
T_PURE          :       'pure'          ;
T_READ          :       'read'          ;
T_RECURSIVE     :       'recursive'     ;
T_RESULT        :       'result'        ;
T_RETURN        :       'return'        ;
T_REWIND        :       'rewind'        ;
T_SAVE          :       'save'          ;
T_SELECT        :       'select'        ;
T_SELECTCASE    :       'selectcase'    ;
T_SELECTTYPE    :       'selecttype'    ;
T_SEQUENCE      :       'sequence'      ;
T_SIN           :       'sin'           ;
T_SQRT		      :	      'sqrt'	        ;
T_STOP          :       'stop'          ;
T_SUBROUTINE    :       'subroutine'    ;
T_TAN           :       'tan'           ;
T_TANH          :       'tanh'          ;
T_TARGET        :       'target'        ;
T_THEN          :       'then'          ;
//T_TO            :       'to'            ;
T_TYPE          :       'type'          ;
T_UNFORMATTED   :       'unformatted'   ;
T_USE           :       'use'           ;
//T_VALUE         :       'value'         ;
T_VOLATILE      :       'volatile'      ;
T_WAIT          :       'wait'          ;
T_WHERE         :       'where'         ;
T_WHILE         :       'while'         ;
T_WRITE         :       'write'         ;
T_ENDASSOCIATE  :       'endassociate'  ;
T_ENDBLOCK      :       'endblock'      ;
T_ENDBLOCKDATA  :       'endblockdata'  ;
T_ENDDO         :       'enddo'         ;
T_ENDENUM       :       'endenum'       ;
T_ENDFORALL     :       'endforall'     ;
T_ENDFILE       :       'endfile'       ;
T_ENDFUNCTION   :       'endfunction'   ;
T_ENDIF         :       'endif'         ;
T_ENDINTERFACE  :       'endinterface'  ;
T_ENDMODULE     :       'endmodule'     ;
T_ENDPROGRAM    :       'endprogram'    ;
T_ENDSELECT     :       'endselect'     ;
T_ENDSUBROUTINE :       'endsubroutine' ;
T_ENDTYPE       :       'endtype'       ;
T_ENDWHERE      :       'endwhere'      ;
T_END   	      : 	    'end'		        ;
T_DIMENSION     :       'dimension'     ;

//protected CONTINUATION : ~('0' | ' ') ;
EOS:
	('\r' (options {greedy=true;}:'\n')?) 
	| '\n'   // End Of Statement
	;

// Fortran 77 doesn't allow for empty lines. Therefore EOS (newline) is NOT
// part of ignored white spaces. It is only ignored for line continuations.
WS : WHITE { $channel=HIDDEN; } // White spaces or empty lines
	;

/*
PHLIN: any characters follow !, ! isn't at 1st column, will be treated as comment.
*/
LINE_COMMENT
    : {getCharPositionInLine() > 0}?=> '!' ~('\n'|'\r')*  {$channel=HIDDEN;}
    ;
    
// '' is used to drop the charater when forming the lexical token
// Strings are assumed to start with a single quote (') and two
// single quotes is meant as a literal single quote
SCON :
	(T_SQUOTE)	( ~(T_SQUOTE|T_DQUOTE|'\n'|'\r') )*	(T_SQUOTE)
	| (T_DQUOTE) ( ~(T_SQUOTE|T_DQUOTE|'\n'|'\r') )* (T_DQUOTE)
;
	
// numeral literal
ICON 
@init {int counter=0;}
   :
	// plain integer
	INTVAL {counter = Integer.parseInt($INTVAL.text);}
	
	(	
	// real
		 // avoid tokenizing the . from an operator (eg. '1.eq.2')
	 	('.' ( NUM | EXPON | ~('n'|'e'|'a'|'o'|'x'|'l'|'g'|'t'|'f'|'0'..'9')))=>
		'.' (NUM)* (EXPON)?   // 123.456 or 123.
		{ _type = RCON; }
		|
  // double
    '.' (NUM)* DEXPON   // 123.456d or 123.d
    { _type = DCON; }
    |		
	// X format descriptor
		'x'
		{ this.state.type = XCON; }
		|
	// P format descriptor
		'p'
		{ this.state.type = PCON; }
	)?
	;	

// real
RCON:
	'.' (NUM)* (EXPON)? ;     // .12345
	
DCON:	
  '.' (NUM)* DEXPON ;     // .12345d
  
// hexadecimal
ZCON:
	'z' '\'' (HEX)+	'\''
	{
		String str = getText();
		str = str.substring(2,str.length() - 1);
		setText(str);
	}
	;
	

// identifier (keyword or variable)
NAME :
	(('i'|'f'|'d'|'g'|'e') (NUM)+ '.') => FDESC
	{ this.state.type = FCON; } // format descriptor	
	| ALPHA (ALNUMEXT)*   // regular identifier
	;

fragment WHITE: (' ' | '\t') ;
fragment ALPHA: ('a'..'z' | 'A'..'Z') ; // case-insensitive
fragment NUM  : ('0'..'9') ;
fragment ALNUMEXT: (ALPHA | NUM | '_' | T_DOLLAR) ;
fragment HEX  : (NUM | 'a'..'f') ;
fragment SIGN : ('+' | '-') ;
fragment NOTNL: ~('\n'|'\r') ;
fragment INTVAL //returns [int val=0] Warning!! Lexers can't 'return' values
/* @init {int val=0;}  : (NUM)+ {val=Integer.parseInt(getText());} ; This line is equally egregious */
	: (NUM)+;
fragment FDESC: ('i'|'f'|'d') (NUM)+ '.' (NUM)+ |
	('e'|'g') (NUM)+ '.' (NUM)+  ('e' (NUM)+)? ;
fragment EXPON: ('e') (SIGN)? (NUM)+ 
//	{
//		String str = getText();
//		str = str.replaceAll("^[dD]", "0e");
//		str = str.replaceAll("[dD]", "e");
//		setText(str);
//	}
	;
fragment DEXPON: ('d') (SIGN)? (NUM)+
  ;
  	
/*
Warning: Phlin found there might be bug in getCharPositionInLine().
When program try to parse a = b*p, the *p might be parse incorrectly
it would be okay if use a = b * p. So far, I have no idea why this 
happens. To solve this, I put another getCharPositionInLine() before
T_STAR and T_POWER. That solves this issue for now. Keep this in
mind and we might need to figure it out later.
*/

fragment COMMENTPREFIX: 
	{getCharPositionInLine() == 0}?=>
	('c' | 'C' | '*' | '!' )
	;

