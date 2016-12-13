//===----------- TokenizePPMdir.g - tokenize the PPM directives -----------===//
//
//  Tokenize the PPM directives
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

tree grammar TokenizePPMdir;
options {
    tokenVocab=tokenFile;    
    ASTLabelType=FTree;      // we're using FTree nodes
    output=AST;              // build ASTs from input AST
    filter=true;             // tree pattern matching mode
    backtrack=true;          // allow backtracking if necessary
    rewrite=true;
}

@header{
package translator;
import lexer.*;
import symbol.*;
import IntegerEvaluator.*;
}

@members{
    Scope currentScope;  
    SymbolTable symtab;
    public TokenizePPMdir(TreeNodeStream input, SymbolTable symtab) {
        this(input);
        this.symtab = symtab;
        currentScope = symtab.globals;
    }
    IntegerEvaluator intEval = new IntegerEvaluator();
}

// START: strategy
topdown : resolvedirective ; // tell ANTLR when to attempt which rule
// END: strategy

resolvedirective 
@init{
  String tmp = "";
}:
  ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("TRANSFORM")}? n2=. {$n2.getText().contentEquals("BEGIN")}?)
                        -> ^($PD TRANSFORMBEGIN["TRANSFORMBEGIN"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("TRANSFORM")}? n2=. {$n2.getText().contentEquals("END")}?)                        
                        -> ^($PD TRANSFORMEND["TRANSFORMEND"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("PIPELINE")}?)
                        -> ^($PD PIPELINE["PIPELINE"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("ELIMINATE")}? n2=. {$n2.getText().contentEquals("REDUNDANT")}?
                                                           n3=. {$n3.getText().contentEquals("ITERATIONS")}?)
                        -> ^($PD DELREDITER["DELREDITER"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("ELIMINATE")}? n2=. {$n2.getText().contentEquals("REDUNDANT")}?
                                                           n3=. {$n3.getText().contentEquals("SUBSCRIPTEXP")}? n4=.)                                                           
                        -> ^($PD DELREDSUBEXP["DELREDSUBEXP"] NAME[$n4.getText().toLowerCase()])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("PREFETCH")}? n2=. {$n2.getText().contentEquals("BEGIN")}?)
                        -> ^($PD PREFETCHBEGIN["PREFETCHBEGIN"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("PREFETCH")}? n2=. {$n2.getText().contentEquals("END")}?)                        
                        -> ^($PD PREFETCHEND["PREFETCHEND"])
//| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("DONT")}? n2=. {$n2.getText().contentEquals("ALTER")}? n3=. {$n3.getText().contentEquals("BEGIN")}?)
//                        -> ^($PD DONTALTERBEGIN["DONTALTERBEGIN"])
//| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("DONT")}? n2=. {$n2.getText().contentEquals("ALTER")}? n3=. {$n3.getText().contentEquals("END")}?)
//                        -> ^($PD DONTALTERBEGIN["DONTALTEREND"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("LONGITUDINAL")}? n2=. {$n2.getText().contentEquals("LOOP")}?)                            
                        -> ^($PD LONGIT["LONGIT"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("REPACK")}? n2=. {$n2.getText().contentEquals("LOOP")}?)                            
                        -> ^($PD REPACK["REPACK"])                         
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("DOUBLEBUFFER")}? n2=. )                        
                        -> ^($PD DOUBLEBUFFER["DOUBLEBUFFER"] NAME[$n2.getText().toLowerCase()])  
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("DEDOUBLEBUFFER")}? n2=. )                        
                        -> ^($PD DEDOUBLEBUFFER["DEDOUBLEBUFFER"] NAME[$n2.getText().toLowerCase()])                            
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("INLINE")}?)                        
                        -> ^($PD INLINE["INLINE"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("RETAINIF")}?)                                                   
                        -> ^($PD RETAINIF["RETAINIF"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("UNROLL")}? n2=. )
                        -> ^($PD UNROLL["UNROLL"] ICON[$n2.getText()])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("UNROLL")}?)                                                
                        -> ^($PD UNROLL["UNROLL"] ICON["0"])                        
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("SHRINK")}? n2=. {$n2.getText().contentEquals("MEMORY")}?)                                                    
                        -> ^($PD SHRINKMEM["SHRINKMEM"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("DMA")}?)                          
                        -> ^($PD DMA["DMA"])
/* The list addition (nlist+=.)* or (nlist+=.)+ doesn't work.  It is clearly an ANTLR bug. 
 * The Java code constructs a list_nlist which appends all the tokens matched by nlist. 
 * However the retval.tree is constructed using nlist.  Only the last token is returned, and
 * all the previous matches are lost.  I am going to rewrite this pattern matching pass purely
 * in Java.
 * - JJ, 02/20/2014
 */
//| ^(PD=PPMDIRECTIVE n1=. {$n1.getText().contentEquals("PENUPPERBOUND")}? (nlist+=.)*)
//                        -> ^($PD PENUPPERBOUND["PENUPPERBOUND"] $nlist)
//| ^(PD=PPMDIRECTIVE n1=. {$n1.getText().contentEquals("PENLOWERBOUND")}? (nlist+=.)*)
//                        -> ^($PD PENLOWERBOUND["PENLOWERBOUND"] $nlist)
| ^(PD=PPMDIRECTIVE {currentScope = $PD.symbol.scope;} n1=. {$n1.getText().contentEquals("UPPERBOUND")}? (n=. {tmp = tmp+$n.getText();})+)
                            {
                             tmp = ((BaseScope)currentScope).replaceParameter(tmp);
                             tmp = Integer.toString(intEval.Evaluate(tmp));
                             /*System.out.println("tmp ="+tmp);*/
                             }                      
                        -> ^($PD UPPERBOUND["UPPERBOUND"] ICON[tmp])
| ^(PD=PPMDIRECTIVE {currentScope = $PD.symbol.scope;} n1=. {$n1.getText().contentEquals("LOWERBOUND")}? (n=. {tmp = tmp+$n.getText();})+)
                            {
                             tmp = ((BaseScope)currentScope).replaceParameter(tmp);
                             tmp = Integer.toString(intEval.Evaluate(tmp));
                             /*System.out.println("tmp ="+tmp);*/
                             }                      
                        -> ^($PD LOWERBOUND["LOWERBOUND"] ICON[tmp])
| ^(PD=PPMDIRECTIVE {currentScope = $PD.symbol.scope;} n1=. {$n1.getText().contentEquals("COPYBUFFER")}? (n=. {tmp = tmp+$n.getText();})+ n2=. {$n2.getText().contentEquals("TIMES")}?) //(nt=~TIMES {tmp = tmp+$nt.getText();})+)
                            {
                             tmp = ((BaseScope)currentScope).replaceParameter(tmp);
                             tmp = Integer.toString(intEval.Evaluate(tmp));
                             //System.out.println("tmp ="+tmp);
                             }                         
                        -> ^($PD COPYBUFFER["COPYBUFFER"] ICON[tmp] TIMES["TIMES"])
| ^(PD=PPMDECLDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/  n1=. {$n1.getText().contentEquals("TEMPS")}? n2=. {$n2.getText().contentEquals("BEGIN")}?)
                        -> ^($PD TEMPBEGIN["TEMPBEGIN"])
| ^(PD=PPMDECLDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/  n1=. {$n1.getText().contentEquals("TEMPS")}? n2=. {$n2.getText().contentEquals("END")}?)                        
                        ->  ^($PD TEMPEND["TEMPEND"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("REMOVE")}? n2=. {$n2.getText().contentEquals("BEGIN")}?)
                        -> ^($PD CANREMOVEBEGIN["CANREMOVEBEGIN"])
| ^(PD=PPMDIRECTIVE /*{currentScope = $PD.symbol.scope;}*/ n1=. {$n1.getText().contentEquals("REMOVE")}? n2=. {$n2.getText().contentEquals("END")}?)                        
                        -> ^($PD CANREMOVEEND["CANREMOVEEND"])
                       ;                       
