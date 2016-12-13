//===------------- fixDefinitions.g - fix variable definitions ------------===//
//
//  This code modifies the variable definitions in the caller for the newly
//  inlined variables
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

tree grammar fixDefinitions;
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
import java.util.ListIterator;
}

@members{
  FTree newDeclBlk;
  FTree newEquivBlk;
  String calleename;
  String callername;  
  Hashtable redundantDefsArg;
  Hashtable redundantDefsNonArg;
  List newEquivs;
  public fixDefinitions(TreeNodeStream input, String calleename, String callername, Hashtable redundantDefsArg, 
                                                                      Hashtable redundantDefsNonArg, List newEquivs) 
  {
      this(input);
      this.calleename = calleename;
      this.callername = callername;
      this.redundantDefsArg = redundantDefsArg;
      this.redundantDefsNonArg = redundantDefsNonArg;
      this.newEquivs  = newEquivs; 
  }
  
  private boolean removeRedundantDef(String name){
    boolean isRemoveNonArg = false, isRemoveDef;    
  
    varSubKey key = new varSubKey(name,calleename,callername);
    uniqueDefValue val;

    if (redundantDefsNonArg.containsKey(key)){
        val = (uniqueDefValue)redundantDefsNonArg.get(key);        
        if (val.isDefIncluded == true){
         /* remove flag */            
            isRemoveNonArg = true;
        } else {
            isRemoveNonArg = false;
            val.isDefIncluded = true;
            redundantDefsNonArg.put(key,val);  
        }
    }
  
    isRemoveDef = true;
    if(!redundantDefsArg.containsKey(key) && !isRemoveNonArg){
       isRemoveDef = false;
    }
    
    return isRemoveDef;  
  }  
  
  private void equivTree(FTree arraydecl){
    FTree equivStmt = null, equivGrp = null;
    String arrayname = (arraydecl.getChild(0)).toString();
    boolean isEquiv = false;
    ListIterator iterator;
    equivVars entry;
    FTree equivVar, var;
    
    iterator = newEquivs.listIterator();
    for(int i=0; i<newEquivs.size(); i++){
        entry = (equivVars)iterator.next();
        equivVar = entry.var1;
        var      = entry.var2;
        if(arrayname.contentEquals(equivVar.toString())){        
        /* Create the EQUIVALENCE statement
           equivalenceStatement : ^(T_EQUIVALENCE equivEntityGroup+)
                equivEntityGroup :  ^(EQUIVGRP varRef+) 
         */
           equivStmt = (FTree)adaptor.create(T_EQUIVALENCE,"equivalence");
         
           equivGrp = (FTree)adaptor.create(EQUIVGRP,"EQUIVGRP");
           equivGrp.addChild((FTree)adaptor.dupTree(var));
           equivGrp.addChild((FTree)adaptor.dupTree(equivVar));
           equivStmt.addChild(equivGrp);           
          
           if(newEquivBlk == null)
              newEquivBlk = (FTree)adaptor.nil();
           newEquivBlk.addChild(equivStmt);
        
           iterator.remove();
           break;
        }   
    }   
  }
  
}

topdown
@init{
  if (newDeclBlk == null)
      newDeclBlk = (FTree)adaptor.create(DECLARATIONBLOCK,"DECLARATIONBLOCK");
}
    : dimensionStatement
    | typeStatement
 //   | ppmdirectives  /* Not including the !dec$ directives */ 
    | commonStatement
    | volatileStatement
    ;               

/* 15 */
dimensionStatement
@init{
  FTree newtree = null;
  newEquivBlk = null;
} 
@after{
  if (newtree != null)
    newDeclBlk.addChild(newtree);  
  if (newEquivBlk != null)
    newDeclBlk.addChild(newEquivBlk);  
}: 
  ^(T_DIMENSION (a=arrayDeclarator
    {
      if ($a.newtree != null){
          if (newtree == null){
            newtree = (FTree)adaptor.create(T_DIMENSION,"dimension");
          }
          newtree.addChild($a.newtree);
      }
    }
   )+)
  ;
    
/* 16 */
arrayDeclarator returns [FTree newtree]
@init {
  $newtree = null;
}
@after{  
  if(!removeRedundantDef($n.text)){
     equivTree(retval.tree);
     $newtree = retval.tree;
  }
}:
  ^(ARRAY n=NAME .+)
  ;
  
volatileStatement
@init{
  FTree newtree = null;
} 
@after{
  if (newtree != null)
    newDeclBlk.addChild(newtree);  
}: 
  ^(T_VOLATILE (a=varName
    {
      if ($a.newtree != null){
          if (newtree == null){
            newtree = (FTree)adaptor.create(T_VOLATILE,"volatile");
          }
          newtree.addChild($a.newtree);
      }
    }
   )+)
  ;

varName returns [FTree newtree]
@init {
  $newtree = null;
}
@after{  
  if(!removeRedundantDef($n.text)){     
     $newtree = retval.tree;
  }
}:
  n=NAME
  ;
  
typeStatement
@init{
  newEquivBlk = null;
}
@after{
  if ($tnl.newtree != null){
      FTree typeStmt = (FTree)adaptor.create(TYPESTMT,"TYPESTMT");
      typeStmt.addChild($t.newtree);
      typeStmt.addChild($tnl.newtree);
      newDeclBlk.addChild(typeStmt);
  }
  if (newEquivBlk != null)
      newDeclBlk.addChild(newEquivBlk);
}:
  ^(TYPESTMT t=type tnl=typeStatementNameList)
  ;

typeStatementNameList returns [FTree newtree]
@init{
  $newtree = null;
}
:
  (n=typeStatementName
    {
      /* The ridiculous print statement is necessary to circumvent a Java bug which causes
         only the first variable in a typeStatement to be recognized.  All the subsequent
         variables in the type statement were ignored.  ANTLR generates a do-while loop
         which is true as long as the next token is a NAME or an ARRAY.  For some bizzare 
         reason, the while loop terminates after the first variable is recognized.  The 
         program behaves correctly when the lookahead tokens (input.LA) are printed.  This
         leads me to conclude that it is a Java bug rather than ANTLRs.  The problem
         came to light when generating code for BGQ where the difference between 32-bit 
         and 64-bit variable definitions matter.  Some variables were not being defined
         as real*8 after inlining, and the double dmin and dmax1 intrinsics didn't like it.
         The Java on the BGQ system produced the same results as the Java on my system and 
         Compton.  It leads me to conclude that all the JVMs have the same bug or it is 
         after all an ANTLR bug.  I am not sure what it is, but I don't care at this point.
             I tried expressing the logic in several different ways, but the problem never
         went away.  I tried dynamic scoping in ANTLR, bypassing typeStatementNameList by
         using typeStatementName direclty in the typeStatement, and replacing even the
         typeStatementName with (n=NAME|ad=arrayDeclarator) directly in the typeStatement.
         In the end, I have decided to use time tested print statement to break compiler
         optimizations.  We know that the token can never be less than -1, but the compiler
         does not.  We exploit this fact to successfully suppress the print statement, and
         it works!
         06/14/2014 - JJ
       */
      if (input.LA(1) < -100) {
         System.out.println("input.LAs = "+input.LA(1)+"  "+input.LA(2));
      }   
      if ($n.newtree != null){
        if ($newtree == null)
          $newtree = (FTree)adaptor.nil();          
        $newtree.addChild($n.newtree);
      }
    }
   )+
  ;
  
typeStatementName  returns [FTree newtree]
@init{
 $newtree = null;
}
@after{
  if (n!=null){
      if(!removeRedundantDef($n.text))
         $newtree = retval.tree;
  } else {
   /* Array Declarator */
      if ($t.newtree != null){
         $newtree = $t.newtree;
      }
  }
}:
  n=NAME
  |  t=arrayDeclarator
  ;
  
typename returns [FTree newtree]
@after{
  $newtree = retval.tree;
}:
  (t=T_REAL
   |t=T_PRECISION
   |t=T_INTEGER
   |t=T_LOGICAL
   |t=T_CHARACTER
   )
  ;
  
type returns [FTree newtree] 
@init{
 $newtree = null; 
}
@after{
 /* retval.tree does not contain the full list here.
    typenameLen goes missing.  Therefore, I decided
    to construct everything from scratch. 
  */   
 $newtree = (FTree)adaptor.nil();
 $newtree.addChild($tn.newtree);
 if (tl!=null)
    $newtree.addChild($tl.newtree); 
}: 
  (tn=typename tl=typenameLen?)
  ;

typenameLen returns [FTree newtree]
@after{
  $newtree = retval.tree;
}:
  ^(s=T_STAR .) 
  ;
  
/*ppmdirectives 
  @after{  
   newDeclBlk.addChild(retval.tree);
  }:   
    ^(PPMDECLDIRECTIVE (~EOS)+)
  | ^(PPMDIRECTIVE (~EOS)+)  
  ;
 */
   
commonStatement
@after{
   System.out.println("Inlined subroutines must not contain COMMON statements. "+calleename+" needs to be fixed.");
   System.exit(-1); 
} :
  ^(T_COMMON .+) ;
          
