//===---------------------- fixLabels.g - fix labels ----------------------===//
//
//  This code renumbers the labels and their references after inlining
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
 
tree grammar fixLabels;
options {
    tokenVocab=tokenFile;
    output = AST;
    ASTLabelType=FTree; 
    filter=true; 
    backtrack=true;
}

@header{
package inliner;
import translator.*;
import symbol.*;
}

@members{
  SymbolTable symtab;
  LabelTable lbltab;
  Scope currentScope;
  Scope callScope;
  String name;
  int ibaseidx;
   
  public fixLabels(TreeNodeStream input, SymbolTable symtab, LabelTable lbltab, Scope callScope) {
      this(input);
      this.symtab = symtab;
      this.lbltab = lbltab;
      this.currentScope = symtab.globals;
      this.callScope = callScope;
      this.ibaseidx = lbltab.getSize();
  }
  
  private FTree replaceTree(FTree oldTree, FTree newTree){
    int myidx;                                   
    
    myidx = adaptor.getChildIndex(oldTree);                        
    FTree parent = (FTree)oldTree.getParent();                        
    adaptor.replaceChildren(parent,myidx,myidx,newTree);
    newTree = (FTree)parent.getChild(myidx);
    return newTree;
  }
  
  private FTree newLblNode(FTree lbl, boolean bref){
    Symbol mysymbol = null;  
    FTree tmp = null;
    String newlbl = null;
    int idx = 0;
  
    idx = Integer.parseInt(lbl.toString());    
    newlbl = Integer.toString(ibaseidx+idx);
    //System.out.println("ibaseidx, idx, newlbl = "+ibaseidx+" "+idx+" "+newlbl);
  
    if (bref)
      tmp = (FTree)adaptor.create(ICON,newlbl);
    else
      tmp = (FTree)adaptor.create(LABEL,newlbl);    
      
    mysymbol = lbl.symbol;
  
    if(mysymbol != null){
       try{
           tmp.symbol = (Symbol)mysymbol.clone();
       } catch (CloneNotSupportedException e) {
           e.printStackTrace();
       }
       tmp.symbol.name = newlbl;
       tmp.symbol.scope = callScope;
       if(callScope.resolve(newlbl) == null){
          callScope.define(tmp.symbol);
          lbltab.addSymbol((LabelSymbol)tmp.symbol);
       }         
     } else {
       System.out.println("Symbol not found when generating newLblNode in inliner\n");
     }
     return tmp;
  }
}

topdown
    :   getlabel
    |   getlabelref
    ;               

getlabel
@after{
     FTree tmp = newLblNode(retval.tree, false);
     retval.tree = replaceTree(retval.tree, tmp);
}
:  LABEL ;
  
getlabelref
@after{  
  FTree lb = (FTree)retval.tree.getChild(0);
  FTree tmp = newLblNode(lb, true);
  adaptor.replaceChildren(retval.tree,0,0,tmp);
}
: ^(LABELREF ICON);
