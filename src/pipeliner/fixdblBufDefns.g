//===--------- fixdblBufDefns.g - fix double buffer definitions -----------===//
//
//  Fix the definition statements for the double buffered temporaries
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

tree grammar fixdblBufDefns;
options {
    tokenVocab=tokenFile;
    output = AST;
    ASTLabelType=FTree; 
    filter=true; 
    backtrack=true;
}

@header{
package pipeliner;
import lexer.*;
import symbol.*;
import translator.*;
import java.util.Hashtable;
import java.util.ListIterator;
}

@members{
  HashMap dblBuffer = null;  
  HashMap dedblBuffer = null;
  
  public fixdblBufDefns(TreeNodeStream input, HashMap dblBuffer, HashMap dedblBuffer) 
  {
      this(input);
      this.dblBuffer = dblBuffer;   
      this.dedblBuffer = dedblBuffer;    
  }
}

topdown : 
      arrayDeclarator
    ;               
/* 16 */
arrayDeclarator 
@after
{     
  int ub, lb, ndim, nchildren;
  String sub, slb, reindexstrbase;
  Bound bound;
  ArraySymbol asym;    
  FTree lastDimTree;
  Scope currentScope; 
  
  currentScope = $ID.symbol.scope;  
  //System.out.println("id "+$ID.text+"  symbol "+$ID.symbol+"   "+$ID.symbol.isArraySymbol());
  if (dblBuffer.containsKey($ID.text) && currentScope == dblBuffer.get($ID.text)){ 
      FTree arrDecExtTree = null;
      
//    System.out.println("Double buffereing the declaration "+dbgname);
      arrDecExtTree = (FTree)adaptor.create(ADEXT,"ADEXT");
      arrDecExtTree.addChild((FTree)adaptor.create(ICON,"0"));
      arrDecExtTree.addChild((FTree)adaptor.create(ICON,"1"));
      adaptor.addChild(retval.tree.getChild(1),arrDecExtTree);   
  } 
  if (dedblBuffer.containsKey($ID.text) && currentScope == dedblBuffer.get($ID.text)){
      FTree adextsTree = (FTree)retval.tree.getChild(1);
      nchildren = adextsTree.getChildCount();
      
      if (nchildren > 1){
          adextsTree.deleteChild(nchildren-1);
      } else {
        /* Dont delete the last dimension if it is the only dimension.
         * I don't want to deal with converting an array symbol into
         * a non-array variable.
         */
         FTree arrDecExtTree = null;
         
         arrDecExtTree = (FTree)adaptor.create(ADEXT,"ADEXT");        
         arrDecExtTree.addChild((FTree)adaptor.create(ICON,"1"));
         
         adextsTree.replaceChildren(0,0,arrDecExtTree);          
      } /* Ends the nchildren if clause */
  } /* Ends the de-double buffer if clause */   
}
:
  ^(ARRAY ID=NAME .+)
  ;
   
          