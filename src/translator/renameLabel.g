//===------------------- renameLabel.g - rename labels --------------------===//
//
//  Generate unique labels within a program unit
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

// labels need to be unique only within a program unit. A program unit in 
// Fortran is either the main program or a subprogram (ie) function, 
// subroutine or block data statement.

tree grammar renameLabel;
options {
    tokenVocab=tokenFile;
    output = AST;
    ASTLabelType=FTree; 
    filter=true; 
    backtrack=true;
}

@header{
package translator;
import symbol.*;
}

@members{
  SymbolTable symtab;
  LabelTable lbltab;
  Scope currentScope;
  String name;
  public renameLabel(TreeNodeStream input, SymbolTable symtab, LabelTable lbltab) {
      this(input);
      this.symtab = symtab;
      this.lbltab = lbltab;
      currentScope = symtab.globals;
  }
}

topdown
    :   getlabel
    |   getlabelref
    ;               

getlabel
@after{
  //System.out.println("find LABEL:"+retval.tree.toStringTree());
  int idx = lbltab.getIndex((LabelSymbol) retval.tree.symbol);
  $l.symbol.name = Integer.toString(idx+1);
  retval.tree.getToken().setText(Integer.toString(idx+1));
  //System.out.println("Replacing LABEL "+lbltab.getSymbol(idx).name+" to "+retval.tree.toStringTree());
}
:  l=LABEL ;
  
getlabelref
@after{
  //System.out.println("find LABELREF:"+retval.tree.toStringTree());
  FTree lb = (FTree)retval.tree.getChild(0);
//  if(lb.symbol != null) System.out.println(lb.symbol.name);
  int idx = lbltab.getIndex((LabelSymbol) lb.symbol);
  lb.symbol.name = Integer.toString(idx+1);
  lb.getToken().setText(Integer.toString(idx+1));
// System.out.println(idx);
//  if(lbltab.getSymbol(idx) != null)
  //System.out.println("Replacing LABELREF "+lbltab.getSymbol(idx).name+" to "+retval.tree.toStringTree());
}
: ^(LABELREF l=ICON);
