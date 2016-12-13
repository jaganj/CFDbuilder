//===------------------- printVarName.g - debug utility -------------------===//
//
//  This code prints all the symbol names (variable names, function names, and
//  such) for debugging
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

tree grammar printVarName;
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
  public printVarName(TreeNodeStream input, SymbolTable symtab) {
      this(input);
      this.symtab = symtab;
      this.currentScope = symtab.globals;
  }
}

topdown
    : searchname
    ;               

searchname
@after{
  Symbol tmp = $ID.symbol;
  if(tmp != null){
    System.out.println($ID.text+"\t\t has symbol \t"+tmp.toString());
  }else{
    System.out.println($ID.text+" has null symbol stored in it.");
  }
}
:
  ID=NAME
  ;
  
