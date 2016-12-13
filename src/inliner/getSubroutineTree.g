//===--------------- getSubroutineTree.g - get a subroutine ---------------===//
//
//  This code returns the AST for a given subroutine name
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
 
tree grammar getSubroutineTree;
options {
    tokenVocab=tokenFile;
    output = AST;
    ASTLabelType=FTree; 
    filter=true; 
    backtrack=true;
}

@header{
package inliner;
import lexer.*;
import symbol.*;
import translator.*;
}

@members{
  SymbolTable symtab;
  Scope currentScope;
  String name;
  FTree newtree;
  public getSubroutineTree(TreeNodeStream input, SymbolTable symtab,String name) {
      this(input);
      this.symtab = symtab;
      this.name = name;
      currentScope = symtab.globals;
  }
}

bottomup
    :   searchsubroutine
    ;               

searchsubroutine
@after{
  String subroutinename = retval.tree.getChild(0).getChild(0).toString();
  if(subroutinename.contentEquals(name)){
/*
PHLIN: whenever I use pattern match to find the subroutine, I store it into this new tree for the caller class to use.
We should guarantee there is only one search result, otherwise, the newtree will store wrong information.
*/  
    newtree = (FTree)((FTreeAdaptor)adaptor).cloneTree(retval.tree);
  }
}
:
  SUBROUTINE
  ;
  
