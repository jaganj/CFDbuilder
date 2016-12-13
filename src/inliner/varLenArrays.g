//===------------------------- varLenArrays.g - inliner ------------------------===//
//
//  This code returns the dimensions of a variable length array
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
 
tree grammar varLenArrays;
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
import IntegerEvaluator.*;
import java.util.Hashtable;
import java.util.ListIterator;
}

@members{
  String calleename;
  String callername;  
  List arglist;
  List callingArglist;
  Hashtable<String,List> varLenArrayDims;
  public varLenArrays(TreeNodeStream input, String calleename, String callername, List arglist, List callingArglist) 
  {
      this(input);
      this.calleename = calleename;
      this.callername = callername; 
      this.arglist = arglist;
      this.callingArglist = callingArglist;
      this.varLenArrayDims = new Hashtable<String,List>();
  }
}

topdown : arrayDeclarator ;               

   
/* 16 */
arrayDeclarator
@after{  
  if($n.symbol.isArgument()){
     ArraySymbol asym = (ArraySymbol)$n.symbol;
     String argLBDim, argUBDim;
    
     // System.out.println("Array: "+$n.text);
     for(int i=0; i<asym.nDim; i++){
        argLBDim = asym.LBDim[i];
        argUBDim = asym.UBDim[i];
        if(!IntegerEvaluator.isIntConvertable(argLBDim) ||
           !IntegerEvaluator.isIntConvertable(argUBDim)
          ){
            // System.out.println("Adding to varLenArray: "+$n.text);
            varLenArrayDims.put($n.text,$ae.dims);      
        }
     }
  }
  
}:
  ^(ARRAY n=NAME ae=arrayDeclaratorExtents)
  ;
  
arrayDeclaratorExtents returns [List dims]
@init{
   $dims = new ArrayList();
} :
  ^(ADEXTS a=arrayDeclaratorExtent {$dims.add(a.bounds); } (b=arrayDeclaratorExtent {if (null != b){$dims.add(b.bounds);} })*)
  ;

arrayDeclaratorExtent returns [List bounds]
@after{
  FTree lbTree = null, ubTree = null;
  
  /* The expression ^(ADEXT (lb=expression)? ub=expression)) fails when lb does not occur.
   *  Therefore, I moved the questionable occurrence '?' to the end.
   */
  if (null == $b.tree){
     lbTree = (FTree)adaptor.create(ICON,"1");
     ubTree = (FTree)$a.tree;
  } else {
     lbTree = (FTree)$a.tree;
     ubTree = (FTree)$b.tree;
  }
  $bounds = new ArrayList<FTree>();
  $bounds.add(lbTree);
  $bounds.add(ubTree);
}: 
  ^(ADEXT a=expression (b=expression)?)
 ;

/* 74 */
expression :  
    ICON
  | v=varRef 
  | ^(T_PLUS e1=expression e2=expression)
  | ^(T_MINUS e1=expression e2=expression)
  | ^(T_STAR e1=expression e2=expression)
  | ^(T_DIV e1=expression e2=expression)
  | ^(NEG expression)    
  | ^(T_POWER expression expression)
  | ^(MADD expression expression expression)
  | ^(MSUB expression expression expression)  
  ;

varRef
@after{
  if ($n.symbol.isArgument()){     
     for(int i=0; i<arglist.size(); i++){
         if ($n.text.contentEquals(arglist.get(i).toString())){
             retval.tree = (FTree)adaptor.dupTree(callingArglist.get(i));
         }
     }
  }
}:
  n=NAME
  ;

          