//===---------- updateFMA.g - update floating point multiply-add ----------===//
//
//  This is a ANTLR v3 grammar file for generating the multiply-add
//  instructions
//
//===----------------------------------------------------------------------===//
//
//  Developed by Jagan Jayaraj and Pei-Hung Lin
//  Copyright 2007-2013, Regents of the University of Minnesota
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

tree grammar updateFMA;
options {
    tokenVocab=tokenFile;
    output = AST;
    ASTLabelType=FTree; 
    filter=true; 
    backtrack=true;
}

@header{
package translator;
import lexer.*;
import symbol.*;
}

@members{
  SymbolTable symtab;
  Scope currentScope;
  public updateFMA(TreeNodeStream input, SymbolTable symtab) {
      this(input);
      this.symtab = symtab;
      currentScope = symtab.globals;
  }
}

bottomup
    : removeZero
    | madd
    | msub
    ;               

madd
:
   ^(T_PLUS ^(T_STAR (a=.) (b=.)) (c=.)) -> ^(MADD $a $b $c)
  |^(T_PLUS (c=.) ^(T_STAR (a=.) (b=.))) -> ^(MADD $a $b $c)
  ;

msub
:
   ^(T_MINUS ^(T_STAR (a=.) (b=.)) (c=.)) -> ^(MSUB $a $b $c)
  |^(T_MINUS (c=.) ^(T_STAR (a=.) (b=.))) -> ^(NEG ^(MSUB $a $b $c))
  ;
  
removeZero
:
  ^(T_STAR u=unsignedArithmeticConstant{$unsignedArithmeticConstant.isZero}? .) -> $u
  |^(T_STAR . u=unsignedArithmeticConstant{$unsignedArithmeticConstant.isZero}?) -> $u
  |^(T_DIV u=unsignedArithmeticConstant{$unsignedArithmeticConstant.isZero}? .) -> $u
  |^(T_DIV . u=unsignedArithmeticConstant{if($unsignedArithmeticConstant.isZero){System.err.println("Find divide by zero!!"); System.exit(1);}})
  |^(T_PLUS u=unsignedArithmeticConstant{$unsignedArithmeticConstant.isZero}? (a=.) ) -> $a
  |^(T_PLUS (a=.) u=unsignedArithmeticConstant{$unsignedArithmeticConstant.isZero}?) -> $a
  |^(T_MINUS u=unsignedArithmeticConstant{$unsignedArithmeticConstant.isZero}? (a=.) ) -> ^(NEG $a)
  |^(T_MINUS (a=.) u=unsignedArithmeticConstant{$unsignedArithmeticConstant.isZero}?) -> $a
;

unsignedArithmeticConstant returns[boolean isZero]
: 
  i=ICON {if(Integer.parseInt($i.text) == 0) $unsignedArithmeticConstant.isZero = true;}
  | r=RCON {if(Float.parseFloat($r.text) == 0.) $unsignedArithmeticConstant.isZero = true;}
  | d=DCON {if(Double.parseDouble($d.text) == 0.) $unsignedArithmeticConstant.isZero = true;}
  ;