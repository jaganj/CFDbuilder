//===------------- fixReturnStmts.g - remove return statements ------------===//
//
//  This code replaces the return statements in the inlined body with gotos
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
 
tree grammar fixReturnStmts;
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
import java.util.Hashtable;
}

@members{
  FTree label;

  public fixReturnStmts(TreeNodeStream input, FTree label) 
  {
      this(input);      
      this.label = label;      
  }
    
  private FTree createGoto(){
    FTree gotoStmt = null;
    FTree lblTree = null;
    
    gotoStmt = (FTree)adaptor.create(GOTO,"GOTO");
    
    lblTree = (FTree)adaptor.create(LABELREF,"LABELREF");
    lblTree.addChild((FTree)adaptor.dupTree(label));   
    
    gotoStmt.addChild(lblTree);           
    
    return gotoStmt;
  }
  
}

topdown
    : returnStatement
    ;               

returnStatement
@after{
  retval.tree = createGoto();
}
    : 
      ^(T_RETURN .*)
    ;

