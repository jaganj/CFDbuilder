//===------- pass6_loopOverPlanes.g - common outer pipeline loop ----------===//
//
//  Last pipeline pass: generate the common outer pipeline loop; 
//                      ties all the loose ends
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
//  Derived from the ANTLR 2 grammar for Fortran 77 by Olivier Dragon      
//  Copyright (C) 2007  Olivier Dragon  (v2 ANTLR grammar) 
//      In turn derived from Terence Parr's PCCTS grammar.
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

tree grammar pass6_loopOverPlanes;

/* 02/22/2011, 07/08/2011, 09/28/2012:
   Pipeline Phase 6:
   *  Create an 'iplaneFE' loop over the planes of the briquette 'icube'
   *  Insert one to two guard statements after the last LONGIT loop in a pipeline
        This IF statement(s) protects the tail pipeline statements like the write-back of
      computed grid briquettes.  The need for this guard statement arises from pipelining. 
      For every outer (icube) loop of the simple Fortran, we have updated a grid briquette 
      and is ready to be written back.  However, it involves redundant reads, writes, and 
      computation.  We eliminate the redundant iterations with our automated pipelining. 
      We place appropriate IF statements for all the LONGIT loops.  However, the tail 
      statements are left unguarded.  The values we write back then are incorrect for 
      every single iteration of the outer loop.  
         We rectify the situation by identifying the last LONGIT loop in a pipeline.  In 
      general, all we have to do is to place the guard statement after this last LONGIT
      loop.  However, if the last LONGIT loop occurs inside an IF or a nested IF block 
      we have guard both the statements inside this IF statement as well the statements
      immediately after the IF block.  The guard statement inside the IF statement will
      be "if (icube .ge. ICON) then <the remaining stmts in the IF block> endif".  The
      guard statement which occurs outside of any IF statement is 
            "if (icube .lt. ICON) goto <pipeline lblref>"            
   -JJ
 */          

/* 01/02/2011:
   Pattern matching ANTLRs are very elegant, but seems to be very error-prone.  Adding statements
   (nodes/children) seems to work, but deletion doesn't.  TreeVisitor visit method gets all 
   confused.  I performed a whole bunch of experiments from deleting statements in the reverse
   order to getting child index at the time of deletion.  The conclusion is deletion doesn't
   work with pattern matching grammars.  It forces me to use the more clumsy tree grammars, but,
   as demonstrated below, they atleast work.
   
    - JJ     
 */
 
options {
    language = Java;
    backtrack=true;          // allow backtracking if necessary
    output=AST;              // build ASTs from input AST
    tokenVocab=tokenFile;     
    ASTLabelType=FTree; // we're using FTree nodes
}


@header{
package pipeliner;
import lexer.*;
import symbol.*;
import translator.*;
import IntegerEvaluator.*;
import java.util.Hashtable;
}
@members{   
   boolean bpipelineouter = false; // True if it is the outer pipeline loop
   boolean biskipit = false; // True if iskipit has to be inserted
   boolean brepack = false;  //True if it is a repack loop
   boolean bprepack = false; //Previous repack - True if a repack loop has
                             //  already been seen.
                                                                
   String idoutn = null; // Identifier name for the outer pipeline loop
   Symbol idoutsymbol = null; // Symbol for the outer pipeline loop     
   Scope currentScope = null; /* Assign the scope of the outer pipeline loop to currentScope */
   FTree begintransformregion = null; /* Beginning of the current transform region */
   FTree lastlongitStmt = null; /* Last LONGIT statement we encountered so far in a pipelined region */
   FTree blkenclosingLongit = null; /* The immediate IF block if any containing the last LONGIT statement */  
   FTree longitb4repackStmt = null; /* The LONGIT statement of the loop before the repack pipeline stage */  
   FTree ppldoblock = null;   /* DOBLOCK of the pipelined loop */
   FTree planeloopdoblock = null;  /* DOBLOCK of the loop over planes */                                  
   LabelTable lbltab = null;
   FTree outLblStmt = null;    /* Label Reference rule for the outer pipeline loop */
   String outLblRef = "0";     /* Label Reference for the outer pipeline loop */
   Symbol outLblSym = null;    /* Symbol for the outer pipeline loop label */
   int guardStmtIndex = -1;  /* Location to insert the newly constructed GUARD statement */
   int guardStmtInsideIfIndex = -1;  /* Location to insert the newly constructed GUARD statement 
                                      * inside the IF stament if any
                                      */ 
         
   public pass6_loopOverPlanes(TreeNodeStream input, LabelTable lbltab){
      this(input);
      this.lbltab = lbltab;
      //this.lblfregion = lbltab.getSize()+1; 
   }

   public Symbol retrieveSymbol(String name) {
        Symbol vs;
        
        /* I primarily use this subroutine to retrieve the symbols for predefined
           constants and variables like nbdy, nx, ii, ibegin.  Therefore, I am 
           explicitly setting isVectorLoop to false.  
         */  
        if(currentScope.resolve(name)==null){
           vs = new VariableSymbol(name,Symbol.getDataType(name),false);
           currentScope.define(vs);
        }else{
            vs = currentScope.resolve(name);
            if(vs.scope != currentScope){
               vs.setArgument();
            }
            vs.setVectortype((BaseScope)currentScope,false);
        }
        return vs;
   }  

   private void insertGuardInsideIF(){
      FTree parent = null, nextStmt = null;
      FTree ifTree, condTree, thenTree, gotoTree;   
      int nextStmtIndex = -1, lastChildIndex = -1;                      
      
      parent = (FTree)lastlongitStmt.getParent();
      //System.out.println("parent = "+parent+"  "+parent.toStringTree());
            
      if (parent == ppldoblock)
          /* We are not inside any IFs */
          return;
          
      /* Find the next statment inside the IF block after this LONGIT
         loop.  The input is the LONGIT directive.  It is followed by the
         longit loop.  We have to find the statement past this longit loop.
       */                  
      /* Skip over the longit loop which is now an IF statement. */
      nextStmtIndex = lastlongitStmt.getChildIndex()+1;
      nextStmt = (FTree)parent.getChild(nextStmtIndex);                  
      while (!(nextStmt.getType() == T_IF &&
              nextStmt.getChild(0).getType() == T_GE &&
              nextStmt.getChild(0).getChild(0).getText().contains("iplaneFE"))){
              nextStmtIndex++;                
              nextStmt = (FTree)parent.getChild(nextStmtIndex);                
      }
      nextStmtIndex++;  

               
     /******************************************************************
                    The Guard statement
      ******************************************************************/
      ifTree = (FTree)adaptor.create(T_IF,"if");  
   /* Conditional Tree */
      condTree = (FTree)adaptor.create(T_GE,".ge.");
      condTree.addChild((FTree)adaptor.create(NAME,idoutn));         
      ((FTree)condTree.getChild(0)).symbol = currentScope.resolve(idoutn);  /* idoutn must be 'icube' */
      condTree.addChild((FTree)adaptor.create(ICON,Integer.toString(1+ivalueOf("nghostcubes"))));
      ifTree.addChild(condTree);
       
      thenTree = (FTree)adaptor.create(THENBLOCK,"then");                    

      lastChildIndex = parent.getChildCount() - 1; 
      for(int i = nextStmtIndex; i <= lastChildIndex; i++){
          thenTree.addChild((FTree)adaptor.dupTree(parent.getChild(i)));    
      }      
     
      ifTree.addChild(thenTree);
      parent.replaceChildren(nextStmtIndex,lastChildIndex,ifTree);
   }
              
   private void insertGuard(){
      FTree newpplblock = null;
                    
      guardStmtIndex = findstmtafterLongit(lastlongitStmt,ppldoblock);
                  
      newpplblock = (FTree)adaptor.nil();
      newpplblock.addChild(newpplblockPreamble());   
      newpplblock.addChild(doStmt());      
      newpplblock.addChild(guardStmt());
      newpplblock.addChild((FTree)adaptor.dupTree(ppldoblock.getChild(guardStmtIndex)));
                        
      adaptor.replaceChildren(ppldoblock,guardStmtIndex,guardStmtIndex,newpplblock);
      //System.out.println("ppldoblock = "+ppldoblock.toStringTree());
   }        
          
   private int findstmtafterLongit(FTree longitStmt, FTree enclosingdoblock){
      /* Find the next statment in the enclosing doBody after this LONGIT
         loop.  The input is the LONGIT directive.  It is followed by the
         longit loop.  We have to find the statement past this longit loop.
         It is meant to walk up the ancestral tree if need be.  If the 
         current longit loop is inside an IF nest this subroutine will find
         the enclosing parent and then identify the next statement in the 
         pipeline doBody
       */  
      FTree parent = null, prevparent = null, nextStmt = null;
      int nextStmtIndex = -1;                      
      
      parent = (FTree)longitStmt.getParent();
      //System.out.println("parent = "+parent+"  "+parent.toStringTree());
      
      if(parent == enclosingdoblock){
      /* Skip over the longit loop which is now an IF statement. */
         nextStmtIndex = longitStmt.getChildIndex()+1;
         nextStmt = (FTree)parent.getChild(nextStmtIndex);                  
         while (!(nextStmt.getType() == T_IF &&
                nextStmt.getChild(0).getType() == T_GE &&
                nextStmt.getChild(0).getChild(0).getText().contains("iplaneFE"))){
                nextStmtIndex++;                
                nextStmt = (FTree)parent.getChild(nextStmtIndex);                
         }
         nextStmtIndex++;
      } else {
      /* Find the enclosing parent */         
         while (parent != enclosingdoblock) {                                
                prevparent = parent;
                parent = (FTree)parent.getParent();
                //System.out.println("parent = "+parent+"   "+parent.toStringTree());
                //System.out.println("enclosingdoblock = "+enclosingdoblock.toStringTree());
         }
         nextStmtIndex = prevparent.getChildIndex()+1;
      }   
      return  nextStmtIndex;
   }
   

   private int ivalueOf(String parameter){
      Symbol vs;      
      
      vs = currentScope.resolve(parameter);        
      if(vs == null){
         System.out.println("pass6_loopOverPlanes.g (pipelineblock): "+parameter+" is undefined");
         System.exit(-1);
      }     
                 
      return ((ParamSymbol)vs).ivalue;
   }
   
   private FTree newpplblockPreamble(){
      FTree preamble, assignTree, varRefTree;
      FTree ifTree = null, thenTree = null;     
      FTree addTree = null, exprTree = null;
      Symbol vs; 
      int nghostcubes = -1, mynx = -1, nbdy = -1, iexpr = -1;        
                   
     /******************************************************************
                          Preamble statements
      ******************************************************************/
    /*******************************************************************                           
      iinskip = nghostcubes * mynx  -  nbdy
      iinbegin = 1
      if (icube .eq. 1-nghostcubes)   iinbegin = 1 + iinskip
      iinend = mynx
      iskipend = 0
      if (icube .eq. nbqx+nghostcubes)   iskipend = 1
     ******************************************************************/
      preamble = (FTree)adaptor.nil();
      
   /* iinskip = nghostcubes * mynx  -  nbdy */
      assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
      varRefTree = (FTree)adaptor.create(NAME,"iinskip");
      varRefTree.symbol = retrieveSymbol("iinskip");
      assignTree.addChild(varRefTree);
        
      nghostcubes = ivalueOf("nghostcubes");         
      mynx        = ivalueOf("mynx");
      nbdy        = ivalueOf("nbdy");               
                
      iexpr = nghostcubes * mynx - nbdy;
      assignTree.addChild((FTree)adaptor.create(ICON,Integer.toString(iexpr)));
      preamble.addChild(assignTree);
                
   /* iinbegin = 1 */
      assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
      varRefTree = (FTree)adaptor.create(NAME,"iinbegin");
      varRefTree.symbol = retrieveSymbol("iinbegin");
      assignTree.addChild(varRefTree);
      assignTree.addChild((FTree)adaptor.create(ICON,"1"));
      preamble.addChild(assignTree);        
                        
   /* if (icube .eq. 1-nghostcubes)   iinbegin = 1 + iinskip */
   /* ^(T_IF expression ^(THENBLOCK wholeStatement*))     */
      ifTree = (FTree)adaptor.create(T_IF,"if");
      exprTree = (FTree)adaptor.create(T_EQ,".eq.");
      varRefTree = (FTree)adaptor.create(NAME,"icube");
      varRefTree.symbol = retrieveSymbol("icube");
      exprTree.addChild(varRefTree);
      exprTree.addChild((FTree)adaptor.create(ICON,Integer.toString(1-nghostcubes)));        
      ifTree.addChild(exprTree);
                
      assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
      varRefTree = (FTree)adaptor.create(NAME,"iinbegin");
      varRefTree.symbol = retrieveSymbol("iinbegin");
      assignTree.addChild(varRefTree);
      addTree = (FTree)adaptor.create(T_PLUS,"+");
      addTree.addChild((FTree)adaptor.create(ICON,"1"));                
      varRefTree = (FTree)adaptor.create(NAME,"iinskip");
      varRefTree.symbol = retrieveSymbol("iinskip");
      addTree.addChild(varRefTree);
      assignTree.addChild(addTree);
        
      thenTree = (FTree)adaptor.create(THENBLOCK,"THENBLOCK");
      thenTree.addChild(assignTree);
      ifTree.addChild(thenTree);
      preamble.addChild(ifTree);
                        
      assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
      varRefTree = (FTree)adaptor.create(NAME,"iinend");
      varRefTree.symbol = retrieveSymbol("iinend");
      assignTree.addChild(varRefTree);
      assignTree.addChild((FTree)adaptor.create(ICON,Integer.toString(mynx)));
      preamble.addChild(assignTree);
        
   /* iskipend = 0 */
      assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
      varRefTree = (FTree)adaptor.create(NAME,"iskipend");
      varRefTree.symbol = retrieveSymbol("iskipend");
      assignTree.addChild(varRefTree);
      assignTree.addChild((FTree)adaptor.create(ICON,"0"));
      preamble.addChild(assignTree);
        
   /* if (icube .eq. nbqx+nghostcubes)   iskipend = 1 */
   /* ^(T_IF expression ^(THENBLOCK wholeStatement*))     */
      ifTree = (FTree)adaptor.create(T_IF,"if");
      exprTree = (FTree)adaptor.create(T_EQ,".eq.");
      varRefTree = (FTree)adaptor.create(NAME,"icube");
      varRefTree.symbol = retrieveSymbol("icube");
      exprTree.addChild(varRefTree);
      vs = currentScope.resolve("nbqx");        
      if(vs == null){
         System.out.println("pass6_loopOverPlanes.g (pipelineblock): nbqx is undefined");
         System.exit(-1);
      }                
      addTree = (FTree)adaptor.create(T_PLUS,"+");
      addTree.addChild((FTree)adaptor.create(NAME,"nbqx"));
      addTree.addChild((FTree)adaptor.create(ICON,Integer.toString(nghostcubes)));
      ((FTree)addTree.getChild(0)).symbol = vs;
      exprTree.addChild(addTree);        
      ifTree.addChild(exprTree);
                
      assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
      varRefTree = (FTree)adaptor.create(NAME,"iskipend");
      varRefTree.symbol = retrieveSymbol("iskipend");
      assignTree.addChild(varRefTree);
      assignTree.addChild((FTree)adaptor.create(ICON,"1"));
        
      thenTree = (FTree)adaptor.create(THENBLOCK,"THENBLOCK");
      thenTree.addChild(assignTree);
      ifTree.addChild(thenTree);
      preamble.addChild(ifTree);
        
      return preamble;        
   }  
      
   private FTree doBlockPreamble(){    
      FTree doblkpreamble, assignTree, varRefTree;
      FTree ifTree = null, thenTree = null;     
      FTree mulTree = null, addTree = null, subTree = null, exprTree = null;
      Symbol vs; 
      int mynx = ivalueOf("mynx");
      
     /******************************************************************
                       The DO Preamble statements
      ******************************************************************/
     /******************************************************************      
      iplaneFE = iincube  +  (icube - 1) * mynx
      iskipit = 0
      if (iincube .gt. mynx-iinskip)   iskipit = iskipend    
      *******************************************************************/ 
      doblkpreamble = (FTree)adaptor.nil();
               
   /* iplaneFE = iincube  +  (icube - 1) * mynx */
   /* ^(T_ASSIGN varRef expression) */
      assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
      varRefTree = (FTree)adaptor.create(NAME,"iplaneFE");
      varRefTree.symbol = retrieveSymbol("iplaneFE");
      assignTree.addChild(varRefTree);
                
      subTree = (FTree)adaptor.create(T_MINUS,"-");
      varRefTree = (FTree)adaptor.create(NAME,"icube");
      varRefTree.symbol = retrieveSymbol("icube");
      subTree.addChild(varRefTree);
      subTree.addChild((FTree)adaptor.create(ICON,"1"));
         
      mulTree = (FTree)adaptor.create(T_STAR,"*");
      mulTree.addChild(subTree);
      mulTree.addChild((FTree)adaptor.create(ICON,Integer.toString(mynx)));
        
      addTree = (FTree)adaptor.create(T_PLUS,"+");
      varRefTree = (FTree)adaptor.create(NAME,"iincube");
      varRefTree.symbol = retrieveSymbol("iincube");
      addTree.addChild(varRefTree);
      addTree.addChild(mulTree);
                
      assignTree.addChild(addTree);
      doblkpreamble.addChild(assignTree);
      
      if(biskipit){ 
        /* iskipit = 0 */
        assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
        varRefTree = (FTree)adaptor.create(NAME,"iskipit");
        varRefTree.symbol = retrieveSymbol("iskipit");
        assignTree.addChild(varRefTree);
        assignTree.addChild((FTree)adaptor.create(ICON,"0"));
        doblkpreamble.addChild(assignTree);
     
        /* if (iincube .gt. mynx-iinskip)   iskipit = iskipend */
        /* ^(T_IF expression ^(THENBLOCK wholeStatement*))     */
        ifTree = (FTree)adaptor.create(T_IF,"if");
        exprTree = (FTree)adaptor.create(T_GT,".gt.");
        varRefTree = (FTree)adaptor.create(NAME,"iincube");
        varRefTree.symbol = retrieveSymbol("iincube");
        exprTree.addChild(varRefTree);
        
        subTree = (FTree)adaptor.create(T_MINUS,"-");      
        subTree.addChild((FTree)adaptor.create(ICON,Integer.toString(mynx)));        
        varRefTree = (FTree)adaptor.create(NAME,"iinskip");
        varRefTree.symbol = retrieveSymbol("iinskip");
        subTree.addChild(varRefTree);              
        
        exprTree.addChild(subTree);
        ifTree.addChild(exprTree);
                
        assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
        varRefTree = (FTree)adaptor.create(NAME,"iskipit");
        varRefTree.symbol = retrieveSymbol("iskipit");
        assignTree.addChild(varRefTree);
        varRefTree = (FTree)adaptor.create(NAME,"iskipend");
        varRefTree.symbol = retrieveSymbol("iskipend");
        assignTree.addChild(varRefTree);
        
        thenTree = (FTree)adaptor.create(THENBLOCK,"THENBLOCK");
        thenTree.addChild(assignTree);
        ifTree.addChild(thenTree);
        doblkpreamble.addChild(ifTree);
      }
        
      return doblkpreamble;
   }       
    
    private FTree doStmt(){       
       FTree doTree = null, doBodyTree = null, lblrefTree = null, doVarArgTree = null, varRefTree = null;
       int nbdy = -1, mynx = -1, nghostcubes = -1, ioffset = 0;       
       String lblref = "-1";                      
   
      /*********************************************************************
                              The DO statement
       *********************************************************************/
      /*******************************************************************                           
       do 9900   iincube = iinbegin,iinend
       *******************************************************************/ 
      
   /*  do <new label>   iincube = iinbegin,iinend  */
   /*  ^(T_DO (lblRef)? doVarArgs doBody)          */
       doTree = (FTree)adaptor.create(T_DO,"do");
       lblref = Integer.toString(lbltab.getSize()+1);
                
       if(null == lbltab.getSymbol(lblref,currentScope)){        
          LabelSymbol ls = new LabelSymbol(lblref);
          ls.scope = currentScope;
          lbltab.addSymbol(ls);
           
          lblrefTree = (FTree)adaptor.create(LABELREF,"LABELREF");
          lblrefTree.addChild((FTree)adaptor.create(ICON,lblref));
          ((FTree)lblrefTree.getChild(0)).symbol = ls;
           
          doTree.addChild(lblrefTree);
       } else {
          System.out.println("Internal error (pass6_loopOverPlanes.g): Having difficulty generating a new label reference.  The Label table is somehow corrupted.");
          System.exit(-1);
       }
    /* ^(DOVARARG id=NAME e1=expression e2=expression (e3=expression )?) */
       doVarArgTree = (FTree)adaptor.create(DOVARARG,"DOVARARG");
       varRefTree = (FTree)adaptor.create(NAME,"iincube");
       varRefTree.symbol = retrieveSymbol("iincube");
       doVarArgTree.addChild(varRefTree);
        
       varRefTree = (FTree)adaptor.create(NAME,"iinbegin");
       varRefTree.symbol = retrieveSymbol("iinbegin");
       doVarArgTree.addChild(varRefTree);
       
       varRefTree = (FTree)adaptor.create(NAME,"iinend");
       varRefTree.symbol = retrieveSymbol("iinend");
       doVarArgTree.addChild(varRefTree);
        
       doTree.addChild(doVarArgTree);
       
       /* Is iskiptit enabled? */
       nghostcubes = ivalueOf("nghostcubes");
       nbdy        = ivalueOf("nbdy");
       mynx        = ivalueOf("mynx");
     
       /* ioffset = iinskip = nghostcubes * mynx - nbdy
          ioffset is 0 when nbdy is an integer multiple of mynx.
        */
       ioffset = nghostcubes * mynx - nbdy;                    
       if(ioffset > 0) 
         biskipit = true;
        
       /* ^(DOBLOCK wholeStatement+) */      
       doBodyTree = (FTree)adaptor.create(DOBLOCK,"DOBLOCK");
       planeloopdoblock = doBodyTree;
  
       doBodyTree.addChild(doBlockPreamble());
       moveOldpplBlktoDoBody(doBodyTree);     
       if(biskipit)  
          iskipit(doBodyTree);       
       doTree.addChild(doBodyTree);
       
       return doTree;
   }   
   
   private void moveOldpplBlktoDoBody(FTree doBodyTree){      
      int beginTransStmtIndex = -1; /* Location of the TRANSFORM BEGIN ppm directive */      
        /******************************************************************
                    The old pipeline block statements
      ******************************************************************/
     /* Move the statements between TRANSFORMBEGIN and guard statement
        to the doBody.  The guard statement is not yet constructed, but
        I am referring to its location here.              
      */          
        beginTransStmtIndex = begintransformregion.getChildIndex();      
        for(int i = (guardStmtIndex - 1) - beginTransStmtIndex; i > 0;i--){
            FTree tmp = (FTree)ppldoblock.getChild(beginTransStmtIndex+1);   
            doBodyTree.addChild(tmp);
            ppldoblock.deleteChild(beginTransStmtIndex+1);              
            guardStmtIndex--;
        }        
   }
        
   private void iskipit(FTree doBodyTree){
      int numChildren, i, ibegin = -1, iend = -1;
      FTree stmt;
      FTree ifTree, thenTree, exprTree, varRefTree;
      
     /* Enclose the region from ISKIPITBEGIN directive to
        the longitb4repackStmt with the "if (iskipit .eq. 0)" 
        condition.   
      */
      numChildren = doBodyTree.getChildCount();     
      for (i=0;i<numChildren;i++){
           stmt = (FTree)doBodyTree.getChild(i);
           if (stmt.getType() == PPMDIRECTIVE && stmt.getChild(0).getType() == ISKIPITBEGIN){
               ibegin = i+1;
               break;            
           }    
      }
      
      /* Remember now all the longit loops have been moved to the loop over planes */      
      iend = findstmtafterLongit(longitb4repackStmt,planeloopdoblock)-1;      
      
      if(ibegin > -1 && iend > -1){
      /* if (iskipit .eq. 0)
            <iskipitregion>
         endif      
       */
      /* ^(T_IF expression ^(THENBLOCK wholeStatement*))     */
         ifTree = (FTree)adaptor.create(T_IF,"if");
         exprTree = (FTree)adaptor.create(T_EQ,".eq.");
         varRefTree = (FTree)adaptor.create(NAME,"iskipit");
         varRefTree.symbol = retrieveSymbol("iskipit");
         exprTree.addChild(varRefTree);
         exprTree.addChild((FTree)adaptor.create(ICON,"0"));        
         ifTree.addChild(exprTree);
        
         thenTree = (FTree)adaptor.create(THENBLOCK,"THENBLOCK");
         for(i=ibegin;i<=iend;i++)
             thenTree.addChild((FTree)adaptor.dupTree(doBodyTree.getChild(i)));
         
         ifTree.addChild(thenTree);         
         
         adaptor.replaceChildren(doBodyTree,ibegin,iend,ifTree);
      }
   }
   
   private FTree guardStmt(){  
      FTree ifTree, condTree, thenTree, gotoTree;
            
     /******************************************************************
                    The Guard statement
      ******************************************************************/
      ifTree = (FTree)adaptor.create(T_IF,"if");  
   /* Conditional Tree */
      condTree = (FTree)adaptor.create(T_LT,".lt.");
      condTree.addChild((FTree)adaptor.create(NAME,idoutn));         
      ((FTree)condTree.getChild(0)).symbol = currentScope.resolve(idoutn);  /* idoutn must be 'icube' */
      condTree.addChild((FTree)adaptor.create(ICON,Integer.toString(1+ivalueOf("nghostcubes"))));
      ifTree.addChild(condTree);
       
      thenTree = (FTree)adaptor.create(THENBLOCK,"then");
      gotoTree = (FTree)adaptor.create(GOTO,"GOTO");
        
      gotoTree.addChild(outLblStmt);
      thenTree.addChild(gotoTree);
      ifTree.addChild(thenTree);
      
      return ifTree;
   }     

       
}

program :
  ^(CODEROOT (ppmdirectives | directives | executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit :
     functionSubprogram
  |  mainProgram
  |  subroutineSubprogram
  |  blockdataSubprogram
  ;

/* 2 */
mainProgram : /* Dont understand this */
  ^(MAINPROG programStatement subprogramBody)
  ;

/* 3 */
functionSubprogram :
  ^(FUNCTION functionStatement subprogramBody)
  ;

/* 4 */
subroutineSubprogram :
  ^(SUBROUTINE subroutineStatement subprogramBody)
  ;

/* 5 - blockDataSubprogram */
blockdataSubprogram :
  ^(BLOCKDATA blockdataStatement subprogramBody)
  ;

/* 6 */
otherSpecificationStatement :
      dimensionStatement
    | allocatableStatement
    | equivalenceStatement
    | intrinsicStatement
    | saveStatement
    | volatileStatement
  ;

/* 7 */
executableStatement:
    assignmentStatement
  | gotoStatement
  | ifStatement
  | doStatement
  | continueStatement
  | stopStatement
  | pauseStatement
  | readStatement
  | writeStatement
  | printStatement
  | rewindStatement
  | backspaceStatement
  | openStatement
  | closeStatement
  | endfileStatement
  | inquireStatement
  | callStatement
  | returnStatement
  ;

/* 8 */
programStatement :
  ^(T_PROGRAM NAME)
  ;

seos : NEWLINE ;

/* 9, 11, 13 */
/* 10 */
functionStatement :
  ^(T_FUNCTION (type)? functionName namelist?)
  ;

blockdataStatement :
  ^(T_BLOCK NAME)
  ;

/* 12 */
subroutineStatement :
  ^(T_SUBROUTINE subroutineName namelist?)
  ;
  
namelist :
  ^(SUBARG identifier+)
  ;

subprogramBody:
  ^(SUBPROGRAMBLOCK declarationBlock executionBlock)
  ;

declarationBlock :
  ^(DECLARATIONBLOCK declarationStatement*)
  ;

executionBlock :
  ^(EXECUTIONBLOCK wholeStatement*)
  ;

declarationStatement:
    ppmdecldirectives  
/*     directives   ;   c$OMP MASTER which belongs to the execution block 
 *                      gets added to the declaration region if it is the
 *                      very first statement in the execution region.  
 *                      The bigArray gets defined after this OMP directive
 *                      which is incorrect.  Technically we don't have any
 *                      OMP directives in the declarationStatement yet.
 *                      If we have to add them in the future (after 09/24/12)
 *                      we have to draw a distinction between the declaration
 *                      block OMP statements and execution region OMP statements
 *                      All the passes need to be modified.
 */ 
  | includeStatement
  | implicitStatement
  | useStatement
  | parameterStatement
  | typeStatement
  | commonStatement
  | dataStatement
  | externalStatement
  | otherSpecificationStatement
  ;

macroStatement :
  MACRO (~EOS)+
  ;
  
wholeStatement 
// s c o p e{
  //boolean bremoveend;
//}
//@init{
  //$wholeStatement::bremoveend = false;
//}
//@after{
  //if($wholeStatement::bremoveend){
    // int ibeginIndex = -1, iendIndex = -1;
     //FTree parent;
     
     //ibeginIndex = removebegin.getChildIndex();
     //iendIndex = removeend.getChildIndex();
     //parent = (FTree)removebegin.getParent();
     
     //for (int i=iendIndex; i>=ibeginIndex; i--){
       //  parent.deleteChild(i);
     //}
     //$wholeStatement::bremoveend = false;
  //}
//}
  :
    ppmdirectives
  | directives
//  | statement
//  |^(LABEL statement) 
  |formatStatement
  |allocateStatement
//  |macroStatement!
  |statementFunctionStatement  
  |executableStatement
  ;

endStatement:
   ^(T_END (l=LABEL)?)
   ;

/* 15 */
dimensionStatement : 
  ^(T_DIMENSION arrayDeclarator+)
  ;
  
allocatableStatement :  
  ^(T_ALLOCATABLE entityList+)
  ;
  
entityList :  
  arrayName deferredShapeSpecList?
  ; 
  
deferredShapeSpecList : 
  T_COLON+
  ; 
  
allocateStatement : 
  ^(T_ALLOCATE arrayDeclarator+)
  ;

/* 16 */
arrayDeclarator :
  ^(ARRAY arrayName arrayDeclaratorExtents)
  ;

arrayDeclaratorExtents :
  ^(ADEXTS arrayDeclaratorExtent arrayDeclaratorExtent*)
  ;

arrayDeclaratorExtent: 
  ^(ADEXT expression+)
  ;

/* 17 */
equivalenceStatement : 
  ^(T_EQUIVALENCE equivEntityGroup+)
  ;

/* 18 */
equivEntityGroup : 
  ^(EQUIVGRP varRef+)
  ;

/* 19 */
commonStatement :
  ^(T_COMMON (commonBlock+ | commonItems))
  ;

commonName : 
  T_DIV (NAME T_DIV | T_DIV)
  ;

commonItem :
    NAME
  | arrayDeclarator
  ;

commonItems :
  commonItem+ 
  ;

commonBlock :
  commonName commonItems
  ;

dataStatement:
  ^(T_DATA dataBlock+)
  ;

dataBlock:
  NAME T_DIV (DCON | RCON | ICON) T_DIV
  ;
  
/* 20 */
// need to expand the typename rule to produce a better AST
// May need to work on it.
typeStatement :
  ^(TYPESTMT type typeStatementNameList)
  ;

typeStatementNameList :
  typeStatementName (typeStatementName)*
  ;
  
typeStatementName :
    NAME
  | arrayDeclarator
  ;
  
typename :
     T_REAL
   | T_PRECISION
   | T_INTEGER
   | T_LOGICAL
   | T_CHARACTER
  ;
  
type : 
  (typename typenameLen?)
  ;

typenameLen :
  ^(T_STAR lenSpecification) 
  ;

includeStatement :  
  ^(T_INCLUDE SCON)
  ;
  
/* 21 */
implicitStatement : 
  ^(T_IMPLICIT (implicitNone | implicitSpecs) )
  ;

implicitSpec : 
  ^(type implicitLetters+)
  ;

implicitSpecs : 
  implicitSpec+
  ;

implicitNone : 
  T_NONE
  ;

implicitLetter : 
  NAME
  ; 
  
implicitRange : 
  ^(IMPLICITRANGE implicitLetter implicitLetter?)
  ;

implicitLetters : 
  implicitRange+
  ;

/* 22 */
lenSpecification :  
    ICON
  | T_LPAREN expression T_RPAREN
  ;

useStatement:
  ^(T_USE NAME)
  ;

/* 23 */
parameterStatement : 
  ^(T_PARAMETER paramlist)
  ;

paramlist : paramassign ( paramassign )*
  ;

paramassign : 
  ^(T_ASSIGN NAME expression)
  ;

/* 24 */
externalStatement :
  ^(T_EXTERNAL namelist)
  ;

/* 25 */
intrinsicStatement : 
  T_INTRINSIC namelist
  ;

/* 26 */
saveStatement : 
  ^(T_SAVE (saveEntity+)?)
  ;

saveEntity :  
    NAME 
  | ^(T_DIV NAME T_DIV)
  ;
  
volatileStatement : 
  ^(T_VOLATILE NAME+)
  ;  

/* 29 */
assignmentStatement : 
  ^(T_ASSIGN varRef expression)
  ;

/* 30 */
gotoStatement : 
  ^(GOTO unconditionalGoto )
  |^(GOTO assignedGoto)
  ;

/* 31 */
unconditionalGoto : 
  lblRef ;
  
lblRef
@after{
  if (bpipelineouter) {      
      outLblRef = $l.text;
      outLblSym = $l.symbol;
      outLblStmt = (FTree)adaptor.dupTree(retval.tree); 
  }    
}
:
  ^(LABELREF l=ICON)
  ;  
  
labelList : 
  lblRef (lblRef)*
  ;

/* 33 */
assignedGoto : 
  NAME (labelList)?
  ;

/* 34 */
ifStatement :
  ^(T_IF expression blockIfStatement)
  | ^(T_IF expression logicalIfStatement)
  ;
  
/* 35 */
logicalIfStatement : 
  ^(THENBLOCK executableStatement)
  ;

/* 36 */
blockIfStatement : 
   firstIfBlock
  (elseIfStatement)*
  (elseStatement)?
  ;

firstIfBlock :
  ^(THENBLOCK wholeStatement*)
  ;

/* 37 */
elseIfStatement : 
  ^(ELSEIF expression ^(THENBLOCK wholeStatement*))
  
  // this action is used to ensure that in the AST there is a clear difference
  // between
  // ELSE IF ...
  // and
  // ELSE
  //    IF ... END IF
  // END IF
  // this is done by replacing the 'else if' by one 'elseif' inside the AST
  ;

/* 38 */
elseStatement :
  ^(ELSEBLOCK wholeStatement*)
  ;

/* 39 */
//endIfStatement : 
//  (T_ENDIF! | T_END! T_IF!) ;

/* 40 */
doStatement
@init{
   boolean btmp1 = false;
}
@after {     
     bpipelineouter = btmp1;     
     
     if (bpipelineouter){
         ppldoblock = (FTree)$d.tree;
         insertGuardInsideIF();
         insertGuard();
          
         idoutn = null;    
     }
     bpipelineouter = false;
}      
: 
  ^(T_DO (lblRef)? doVarArgs {btmp1 = bpipelineouter;  bpipelineouter = false;}
                   d=doBody);

doVarArgs 
@after
{
  if (bpipelineouter){
     idoutn = $id.toString();
     idoutsymbol = $id.symbol;      
     currentScope = idoutsymbol.scope;     /* We only match the pipeline loop, and hence the outer pipeline loop is seen first.
                                              currentScope can never be null inside this pattern. */
     //System.out.println("CurrentScope = "+currentScope);                                           
  }
} :
  ^(DOVARARG id=NAME e1=expression 
  e2=expression (e3=expression )?)
  ;

doWithLabel:
  (lblRef
   doVarArgs 
   doBody)
  ;

doBody :
  ^(DOBLOCK wholeStatement*)
  ;

doWithEndDo :
  doVarArgs
  doBody
  ;
  
enddoStatement : 
  (T_ENDDO | T_END T_DO| continueStatement)
  ;

/* 41 */
continueStatement: 
  ^(T_CONTINUE (LABEL)?)
  ;

/* 42 */
stopStatement : 
  ^(T_STOP ICON?)
  ;

/* 43 */
pauseStatement : 
  ^(T_PAUSE ICON?)
  ;

iocontrolArgs:
  T_LPAREN
  (ICON | varRef | T_STAR)
  (T_COMMA
  (lblRef | T_STAR))?
  T_RPAREN
;

/* 44 */
writeStatement : 
  ^(T_WRITE iocontrolArgs (~EOS)*)
  ;

/* 45 */
readStatement : 
  ^(T_READ iocontrolArgs (~EOS)*)
  ;

/* 46 */
printStatement : 
  ^(T_PRINT (~EOS)+)
  ;

/* 50 */
openStatement : 
  ^(T_OPEN (~EOS)+)
  ;

/* 51 */
closeStatement : 
  ^(T_CLOSE (~EOS)+)
  ;

/* 52 */
inquireStatement : 
  ^(T_INQUIRE (~EOS)+)
  ;

/* 53 */
backspaceStatement : 
  ^(T_BACKSPACE (~EOS)+)
  ;

/* 54 */
endfileStatement : 
  ^(T_ENDFILE (~EOS)+)
  ;

/* 55 */
rewindStatement : 
  ^(T_REWIND (~EOS)+)
  ;

/* 58-59 */
formatStatement: 
  ^(T_FORMAT (LABEL)? (~EOS)+)
  ;

/* 70 */
statementFunctionStatement : 
  ^(T_LET (~EOS)+)
  ;

/*Function_reference*/
functionReference :
  ^(FUNCREF functionName functionArgumentList)
  ;
  
functionArgumentList: 
  ^(FUNCARG functionArgument*)
  ;

functionArgument :
  expression
  ;
  
/* 71 */
callStatement : 
  ^(T_CALL subroutineCall)
  ;

subroutineCall :
  (subroutineName callArgumentList?)
  ;

callArgumentList : 
  ^(CALLARG callArgument+)
  ;
  
callArgument : 
  expression
  ;

/* 72 */
returnStatement : 
  ^(T_RETURN expression?)
  ;
  
/* 74 */
expression :  
    unsignedArithmeticConstant
  | SCON  
  | v=varRef 
  | functionReference
  | ^(T_LOR expression expression)
  | ^(T_LAND expression expression)
  | ^(T_LNOT expression expression)
  | ^(T_LT expression expression)
  | ^(T_LE expression expression)
  | ^(T_EQ expression expression)
  | ^(T_NE expression expression)
  | ^(T_GT expression expression)
  | ^(T_GE expression expression)
  | ^(T_PLUS e1=expression e2=expression)
  | ^(T_MINUS e1=expression e2=expression)
  | ^(T_STAR e1=expression e2=expression)
  | ^(T_DIV e1=expression e2=expression)
  | ^(NEG expression)    
  | ^(T_POWER expression expression)
  | ^(MADD expression expression expression)
  | ^(MSUB expression expression expression)  
  ;

concatOp :
  T_DIV T_DIV 
  ;

/* 88 */
arrayElementName : 
  ^(NAME expression+)
  ;

subscripts :
  ^(SUBSCRIPT (e=expression)+);
 
varRef :      
      ^(ARRAYREF ID=NAME s=subscripts)
    |   ID=NAME
  ;  

/* 92 */
arrayName :  
  NAME
  ;

/* 97 */
subroutineName : 
  NAME
  ;

implicitfunctionName: 
   T_ACHAR
  |T_ACOS
  |T_ABS
  |T_ALOG
  |T_ATAN
  |T_ATAN2
  |T_COS
  |T_DBLE
  |T_EXP
  |T_FLOAT
  |T_ICHAR
  |T_INT
  |T_LOG
  |T_MAX 
  |T_MIN
  |T_MOD
  |T_SQRT
  |T_SIN
  |T_TAN
  |T_TANH
  |T_DABS
  |T_DACOS
  |T_DATAN
  |T_DATAN2
  |T_DCOS
  |T_DEXP
  |T_DLOG
  |T_DMAX 
  |T_DMIN
  |T_DMOD
  |T_DSIN
  |T_DSQRT
  |T_DTAN
  |T_DTANH
  ;
/* 98 */
functionName :
    implicitfunctionName
  | NAME
  ;

/* 101 */
unsignedArithmeticConstant: 
    i=ICON
  | RCON
  | DCON
  ;

/* 108 */
logicalConstant : 
  (T_TRUE | T_FALSE)
  ;

identifier:
  NAME
  ;

directives:
    ^(DIRECTIVE (T_AND)? (~EOS)+)
  ; 
  
ppmdirectives
@init{boolean blongit = false, begintrans = false;}
@after{
   if(blongit)
      lastlongitStmt  = (FTree)retval.tree;
   if(begintrans)
      begintransformregion = (FTree)retval.tree;
}
 :  ^(PPMDIRECTIVE LONGIT) {blongit = true;}
  | ^(PPMDIRECTIVE REPACK) {
                    bprepack = brepack; 
                    brepack = true;
                    /* It is very important that the below lines are part
                       part of this action and not in the "after" section
                       because the "after" section gets called by all the
                       matching directives.  Since REPACK is followed by a
                       LONGIT in the code, we hit the after section twice
                       for the same loop, and corrupt the 
                       longitb4repackStmt.
                    */            
                    if(true == brepack && false == bprepack) {
                       /* First Repack Stmt */   
                       longitb4repackStmt = lastlongitStmt;                       
                    }
                  }
  | ^(PPMDIRECTIVE TRANSFORMBEGIN) {begintrans = true;}
  | ^(PPMDIRECTIVE PIPELINE) {bpipelineouter = true;}
  | ^(PPMDIRECTIVE (~(LONGIT|REPACK|TRANSFORMBEGIN|PIPELINE))+)       
   ; 

ppmdecldirectives :
    ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;
