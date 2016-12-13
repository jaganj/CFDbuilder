//===------------- SimplifySubscript.g - simplify subscripts --------------===//
//
//  Simplify constant subscript expressions
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

tree grammar SimplifySubscript;

options {
    tokenVocab=tokenFile;      // use tokens from VecMath.g
    ASTLabelType=FTree; // we're using CommonTree nodes
    output=AST;              // build ASTs from input AST
    filter=true;             // tree pattern matching mode
    backtrack=true;          // allow backtracking if necessary
}

@header{
package pipeliner;
import lexer.*;
import symbol.*;
import translator.*;
import IntegerEvaluator.*;
}

bottomup: xZero
        | xOne
        | pureICONop     
        | splPlusMinusICON           
        | redundantICONop
        ;

//  xZero: (0 * a ) = (a * 0 ) = (0 / a) = 0
xZero
@after{
 // System.out.println("Did a xZero "+retval.tree.toStringTree());
}:     ^(T_STAR a=. b=ICON {$b.getText().contentEquals("0")}?) -> ICON["0"]
     | ^(T_STAR b=ICON {$b.getText().contentEquals("0")}? a=.) -> ICON["0"] 
     | ^(T_DIV b=ICON {$b.getText().contentEquals("0")}? a=.) -> ICON["0"] 
     | ^(T_PLUS a=. b=ICON {$b.getText().contentEquals("0")}?) -> $a 
     | ^(T_PLUS b=ICON {$b.getText().contentEquals("0")}? a=.) -> $a
     | ^(T_MINUS a=. b=ICON {$b.getText().contentEquals("0")}?) -> $a 
     | ^(T_MINUS b=ICON {$b.getText().contentEquals("0")}? a=.) -> $a    
     ;

// xOne: (a * 1) = (1 * a) = (a / 1) = a                     
xOne
@after{
//  System.out.println("Did a xOne "+retval.tree.toStringTree());
}:     ^(T_STAR a=. b=ICON {$b.getText().contentEquals("1")}?) -> $a 
     | ^(T_DIV  a=. b=ICON {$b.getText().contentEquals("1")}?) -> $a 
     | ^(T_STAR b=ICON {$b.getText().contentEquals("1")}? a=.) -> $a 
     ;

// pureICONop: a + b, a - b, a * b, a / b
pureICONop
@init{int val=0;}
@after{
   retval.tree = (FTree)adaptor.create(ICON,Integer.toString(val));
//   System.out.println("Did a pureICONop "+retval.tree.toStringTree());
}:
       ^(T_PLUS a=icon b=icon) {val = $a.val + $b.val;}
     | ^(T_MINUS a=icon b=icon) {val = $a.val - $b.val;}
     | ^(T_STAR a=icon b=icon) {val = $a.val * $b.val;}
     //| ^(T_DIV a=icon b=icon) {val = $a.val / $b.val;}
     ;

/* I don't want to deal with the (ICON - varref) case.
   ICON is either added to or subtracted from the varref, 
   but a varref is never subtracted from an ICON. 
 */
// redundantICONop: (var +/- a  + b) = (a + var +/- b) = (var + c) where c = a+/-b
//                  (var +/- a - b) = var + c where c = +/- a - b
redundantICONop
@init {int i=0;}
@after{
  // Ignore the quotes.  It is just to shut ANTLR up.
  //  -> { i > 0}? ^(T_PLUS["+"] "$"x ICON[Integer.toString(i)]) 
  //  -> {i == 0}? "$"x
  //  ->           ^(T_MINUS["-"] "$"x ICON[Integer.toString(i)])     
     if(i==0)
        retval.tree = (FTree)$x.tree;
     else {
        if(i>0){
           retval.tree = (FTree)adaptor.create(T_PLUS,"+");
           retval.tree.addChild((FTree)$x.tree);
           retval.tree.addChild((FTree)adaptor.create(ICON,Integer.toString(i)));           
        } else { 
        /* i<0 */
           retval.tree = (FTree)adaptor.create(T_MINUS,"-");
           retval.tree.addChild((FTree)$x.tree);
           retval.tree.addChild((FTree)adaptor.create(ICON,Integer.toString(-i)));
        }
     }
//     System.out.println("Did a redundantICONop "+retval.tree.toStringTree());
}
     : ^(T_PLUS x=plusMinusIcon b=icon) {/*System.out.println("redundantICONop T_PLUS "+$x.tree.toStringTree()+"  "+$b.val);*/ i = $b.val + $x.val;}
     | ^(T_PLUS b=icon x=plusMinusIcon)  {/*System.out.println("redundantICONop T_PLUS "+$b.val+"  "+$x.tree.toStringTree());*/i = $b.val + $x.val;}
     | ^(T_MINUS x=plusMinusIcon b=icon) {/*System.out.println("redundantICONop T_MINUS "+$b.val+"  "+$x.tree.toStringTree());*/i = $x.val - $b.val;}
     ;    


// plusMinusIcon:   returns val
//    (var + b) or (b + var) -> val = b
//    (var - b)              -> val = -b
plusMinusIcon returns [int val]
@after{
  retval.tree = $a.tree;
//  System.out.println("Did a plusMinusIcon "+retval.tree.toStringTree()+". Its value is "+$val);
}:
       ^(T_PLUS a=varRef b=icon) {/*System.out.println("plusMinusIcon T_PLUS "+$a.tree.toStringTree()+"  "+$b.val);*/ $val = $b.val;} // -> $a 
     | ^(T_PLUS b=icon a=varRef) {/*System.out.println("plusMinusIcon T_PLUS "+$b.val+"  "+$a.tree.toStringTree());*/ $val = $b.val;} // -> $a 
     | ^(T_MINUS a=varRef b=icon) {/*System.out.println("plusMinusIcon T_MINUS "+$a.tree.toStringTree()+"  "+$b.val);*/ $val = -$b.val;} // -> $a 
     ;               

// splPlusMinusICON: (var + -b) = var - b
//                   (-b + var) = var - b
//                   (var - -b) = var + b
splPlusMinusICON
@after{
//  System.out.println("Did a splPlusMinusICON "+retval.tree.toStringTree());
}:
       ^(T_PLUS a=varRef b=negICON)  -> ^(T_MINUS["-"] $a $b) 
     | ^(T_PLUS b=negICON a=varRef)  -> ^(T_MINUS["-"] $a $b) 
     | ^(T_MINUS a=varRef b=negICON) -> ^(T_PLUS["+"] $a $b)
     ; 

varRef:
       ^(ARRAYREF n=NAME .+)
     |   n=NAME
     ;

icon returns [int val]:
       ^(NEG i=ICON) {$val = -Integer.parseInt($i.text);} 
     |       i=ICON  {$val =  Integer.parseInt($i.text);}
     ;

negICON:
       ^(NEG i=ICON) -> $i
     ;
     