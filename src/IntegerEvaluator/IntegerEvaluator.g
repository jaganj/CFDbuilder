//===---------------- IntegerEvaluator.g - Integer Evalautor --------------===//
//
//  This code evaluates static integer expressions to aid other passes
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

grammar IntegerEvaluator;

options {
  language = Java;
}

tokens{
  NEG;
}

@header{
  package IntegerEvaluator;
  import symbol.*;
}
@lexer::header{ package IntegerEvaluator; } 

start returns [int output]:
  e=expression {output = $e.output;}
  ;

expression returns [int output]: 
  a=aexpr0 {output = $a.output;}
//  |T_MAX T_LPAREN e1=expression T_COMMA e2=expression T_RPAREN {if($e1.output > $e2.output) {output = $e1.output;}else{output = $e2.output;}}
//  |T_MIN T_LPAREN e1=expression T_COMMA e2=expression T_RPAREN {if($e1.output < $e2.output) {output = $e1.output;}else{output = $e2.output;}}
  ;

aexpr0 returns [int output]
@init{boolean isplus = false;}
:
  a1=aexpr1{output = $a1.output;} ((T_PLUS{isplus = true;} | T_MINUS) a2=aexpr1
  {
    if(isplus){
      output = output + $a2.output;
    }else{
      output = output - $a2.output;
    }
    isplus = false;
  })* 
  ;

aexpr1 returns [int output]
@init{boolean ismul = false;}
: 
  a1=aexpr2{output = $a1.output;} ((T_STAR{ismul = true;} | T_DIV)  a2=aexpr2
  {
    if($a2.output == 0 && (!ismul)) {
      System.err.println("Divide by zero in IntegerEvaluator"); 
      System.exit(-1);
    } 
    if(ismul){
        output = output * $a2.output;
      }else{
        output = output / $a2.output;
      }
      ismul = false;
  })* 
  ;

aexpr2 returns [int output]: 
  T_MINUS a=aexpr3 {output = 0 - $a.output;}
  | a=aexpr3 {output = $a.output;}
  ;  
  
aexpr3 returns [int output]:
  u=unsignedArithmeticConstant{output = Integer.parseInt($u.text);}
  |T_LPAREN a=aexpr0 T_RPAREN{output = $a.output;}
  |T_MAX T_LPAREN e1=aexpr0 T_COMMA e2=aexpr0 T_RPAREN {if($e1.output > $e2.output) {output = $e1.output;}else{output = $e2.output;}}
  |T_MIN T_LPAREN e1=aexpr0 T_COMMA e2=aexpr0 T_RPAREN {if($e1.output < $e2.output) {output = $e1.output;}else{output = $e2.output;}}
  
  ;
  
unsignedArithmeticConstant : 
  i=ICON;
  
varRef:
  n=NAME;
  
T_PLUS         : '+'  ;
T_MINUS        : '-'  ;
T_STAR         : '*'  ;
T_DIV          : '/'  ;
T_DOLLAR       : '$'  ;
T_LPAREN       : '('  ;
T_RPAREN       : ')'  ;
T_COMMA        : ','  ;
T_MAX          : 'max';
T_MIN          : 'min';
ICON           :INTVAL;
NAME :
  | ALPHA (ALNUMEXT)*   // regular identifier
  ;
WS : WHITE { $channel=HIDDEN; } // White spaces or empty lines
  ;
fragment WHITE: (' ' | '\t') ;
fragment ALPHA: ('a'..'z' | 'A'..'Z') ; // case-insensitive  
fragment ALNUMEXT: (ALPHA | NUM | '_' | T_DOLLAR) ;
fragment NUM  : ('0'..'9') ;
fragment INTVAL //returns [int val=0] Warning!! Lexers can't 'return' values
/* @init {int val=0;}  : (NUM)+ {val=Integer.parseInt(getText());} ; This line is equally egregious */
  : (NUM)+;