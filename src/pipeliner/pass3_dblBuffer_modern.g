//===------------ pass3_dblBuffer_modern.g - double buffering --------------===//
//
//  Double buffer and prefetch deeper briquette layout using mm_prefetch; refer 
//  to the COPYBUFFER directive.
//  Remove double buffering (of big temporaries) in the input which are no 
//  longer needed after pipelining
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

tree grammar pass3_dblBuffer_modern;

/* 03/18/2015:
   The prefetch feature in the translator appears to work. The visualization output
   has the same characteristics as the other two, FI from 08/08/2014 and FW. However,
   even the viz files are ever so slightly different which is evident when you do 
   a diff. The FI viz files for FV-lores are different from FW viz files even for 
   dump 0. The FI_prefetch FV-lores matches FI for the first two dumps, and then 
   begins to become different. Although the FI_prefetch may be acceptably correct, 
   its performance is ever so slightly worse than FI. So I am abandoning the prefetch 
   project very gladly.

   08/12/2014:
   Pattern matching grammar has a few problems:
   (a) More often than not, the rewrites must be performed only in a certain context.  
       The context, in our case, is the enclosing PPM directives which are guiding 
       the transformations.  The directives are at the same level as the statement
       of interest.  Essentially we are matching a list, but we are interested to
       transform only a small number of statements inside this list.  ANLTR doesn't
       allow you to replace this list with another list.  You can only replace this 
       entire list with a single tree.  All the context information is now lost.
   (b) The second option is to not use the rewrite facility but instead perform the 
       rewrite explicitly in the actions.  However, ANTLR stores only the first 
       statement from the list matched into retval.tree.  It's not possible to 
       manipulate the statement(s) of interest because they most likely are not
       inside retval.tree
   (c) We could also just match the statement of interest (so that its tree is in
       retval.tree, and try to figure out the context by parsing the token stream 
       explicitly in Java.  However this is rather difficult, and defeats the purpose 
       of using ANTLR and its features.
   Yet again, pattern matching proves inadequate for most of our needs.  Therefore, 
   we are left to match the whole tree and use global variables to remember the context. 
   The code bloat must be tolerated to avoid writing laborious code, and instead make 
   ANTLR write the code for us.
                

   09/29/2012:
   In this pass, we double buffer and de-double buffer arrays based on directives.
   We need double buffering to support prefetching data from the global arrays in
   the main memory to the stack variables in cache.  The double buffering is 
   invoked by cPPM$ DOUBLEBUFFER <arrayname>.
   
   We also need de-double buffering to support the "grid-block-update" pattern.  
   We bring in a block of the problem state into the L3 cache and performa a 3-D
   update with the help of many threads before we move on to the next block.  The
   ddtemp array is a circular buffer by itself.  We don't any further double buffering.
   However, if we operate on the ddtemp briquettes in FW style, we will overwrite and
   corrupt it.  <do icget = icube-nghostcubes, icube+nghostcubes>  We didn't have 
   this problem with the "grid-briquette-update" pattern because we have a ddold and 
   ddnu.  Because ddtemp acts as both ddold and ddnu, we will corrupt the data if
   we don't pipeline the computation.  To facilitate writing FW, we work around this
   problem by double buffering ddtemp in FW.  We create two new temporaries called
   ddtempold and ddtempnu, and equivalence them to the double buffered ddtemp.  
   The translator then eliminates the double-buffering on ddtemp when the code has 
   been fully pipelined.  We need the programmer to identify ddtemp with the help
   of cPPM$ DE-DBL-BUF <ddtemp> directive.
   An example usage is:
       cPPM$ DE-DBL-BUF DDTEMP
            dimension   ddtemp(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                           ntempbqupdate+4,2)
           
            dimension   ddtempold(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                             ntempbqupdate+4)
            dimension    ddtempnu(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                             ntempbqupdate+4)
            equivalence (ddtemp(1,0,0,1,1),ddtempold)
            equivalence (ddtemp(1,0,0,1,2),ddtempnu)
            
            call ppmmf (    ....
           &                ddtempold(1,1,jbqmod,kbqtemp),
           &                ddtempnu(1,1,jbqmod,kbqtemp), ....)
                                                   
     It will be transformed into,       
             dimension   ddtemp(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                           ntempbqupdate+4)
           
            dimension   ddtempold(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                             ntempbqupdate+4)
            dimension    ddtempnu(nintempcube,0:ntempbq+1,0:ntempbq+1,
           &                                             ntempbqupdate+4)
            equivalence (ddtemp(1,0,0,1),ddtempold)
            equivalence (ddtemp(1,0,0,1),ddtempnu)
            
            call ppmmf (    ....
           &                ddtempold(1,1,jbqmod,kbqtemp),
           &                ddtempnu(1,1,jbqmod,kbqtemp), ....)  
           
       ddtempold, ddtempnu are now equivalent.            
     
   
   01/25/2011:
   Update: I am rewriting it to convert only the DO loops preceeded by cPPM$ LONGIT directive to 
   DDOLOOP in the transform region.  It still has issues with the scalar statements.  I am 
   allowing them momentarily, but will fix it soon.  Refer to fuse.g for a more detailed 
   explanation.
   12/14/2010:
   In the second phase of pipelining, we double buffer and re-index temporaries.  We re-index
   temporaries occuring in a select section of the code we call the transform region.  See the
   comment below from 10/28 for more on re-indexing temporaries.  The second phase is further 
   split into two parts, pass3_dblBuffer.g and pipeline3.g.  Re-indexing is done exclusively in 
   pipeline3.g.  However, we check if the transform region is conformal here.  We only allow 
   DO loops and enclosing IF statements in the transform region.  The DO loops are over the 
   1-D sweep direction, and will be pipelined.  Any non-loop construct (scalar statements) between
   such DO loops may break the pipeline semantics, and, therefore, are not allowed in the
   transform region.  The only exemption are the IF statements enclosing the pipelineable DO
   loops.  
   eg: <transform region begin>
          do ..
            do ..
            enddo
          enddo
          if (ypass .gt. 0)
            do ..  
            enddo
          else
            do ..
            enddo
          enddif
          do ..
          enddo     
       <transform region end>      
   The initial thought was to have two parts for convenience.  The rules for conformity checks were
   very simple.  Once you tag the pipelineable DO loops as DDOLOOP, it is very straight-forward to 
   look for just these patterns, and transform them.  However, the transformation involves adding 
   new statements.  The tree visitor gets all confused when the number of children change dynamically.
   It loops for the initial number of children.  When we add more, the new number of children doesn't
   become the upper bound for the tree visitor loop.  It works on the next few children till it hits 
   the original upper bound and stops.  It doesn't explore a whole bunch of valid children.  The only
   solution is to perform the transformations after we identified all DDOLOOPs in a transform region.       
   This means the rules (not the actions) for conformity check and transformation are almost the same.
   It makes sense to fold them into a single file.  Perform the conformity check topdown, and the 
   transformation bottomup, as we always did.  However, it affects other transformations like double-
   buffering.  Variables outside the transform region may not get double-buffered, as we add more 
   statements to do pipelining.  The same issue as before.  The current solution is to have all
   re-writes which don't add new nodes in one pass, and transformations which add new nodes in separate
   passes.  By current design, we can't deal with more than one transform region belonging to one parent
   ie. a subroutine.  I don't expect such a situation.   
   
   10/28/2010: 
   Re-indexing the temporaries saves space, but is very complex to code and maintain.  A very simple
   alternative is to use a map between the original subscripts (indexes) and the re-indexed quantities.
   To begin with, we use a single array of size nx+nghostcells as a map, and hence all the temporaries
   have the same size.  They could be shrinked later, but through the analysis.  In the case of GPUs,
   the memory reduction from this further analysis will not buy us anything because GPUs are terrible 
   with multitude of indexes, and all the temporaries could never fit in their on-chip memory anyways.
   Just to standardize, the upper limit of the map array is made 0, and therefore the array can only 
   be indexed negative subscripts.  The original subscript expressions are now passed through this map.
   For them to remain unchanged, the loop bounds of the original index are adjusted.  We use 'ii' as the
   map, and 'ibegin' as the lower bound for outer index.  If these names are already in use by the 
   subroutine, I can create new names in the future.
   
   A sample translation would look like,  
c      do 1100   i = 2-nbdy,n+nbdy
      ibegin = 1-nx
      ibegin = max(ibegin,2-nx*(icube+nghostcubes))
      do 1100   i = ibegin,0
!DEC$ VECTOR ALWAYS
c!DEC$ VECTOR ALIGNED
      do jk = 1,nssq
      cl(jk,ii(i)) = .5 * (c(jk,ii(i)) + c(jk,ii(i-1)))
      ...
      
      i is being replaced by ii(i) and i-1 by ii(i-1).  I am set out to do just this and ibegin
      computations in this pass.   
   -JJ
   

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
import java.util.HashMap;
import java.util.Random;
import java.util.LinkedList;
import java.util.Stack;
}

@members{   
   boolean bprefetch = false;  // True in the prefetch region
   boolean btransregion = false; // True in the transform region
   boolean bldoloop = false;  // Longitudinal DO Loop
   boolean bcopybuf = false;  // Loop for copying a buffer into near level memory
   boolean bcopybufbody = false; // Body of the copybuf loop
   boolean bdblBufinSubprogBody = false; // Found a double buffer statement in the Subprogram Body 
   boolean bdedblBufinSubprogBody = false; // Found a de-double buffer statement in the Subprogram Body    
   
   int copytimes = 0;            // Number of times this copybuf loop is executed for each iteration of the prefetch region
   
   FTree currentDeclBlk = null;  // Current declaration block.  
                                 // genSymbol will add the declaration statements for the newly 
                                 // generated names to the end of the current declaration block.
   FTree pipelineDir = null;     // Beginning of the pipeline directive (or pipeline loop)                              
   List pfetchRegions = new ArrayList<PfetchRegion>();       // List of prefetch regions in the current execution block
   List transformRegions = new ArrayList<TransformRegion>(); // List of transform regions in the current execution block
   LinkedList copyBufLoops = new LinkedList(); // List to track the copybuf loops in the current executableUnit
   LinkedList indxCounters = new LinkedList(); // List of counters to track the copybuf subscripts (indexes).
   LinkedList copyBufCondVars = new LinkedList(); // List of IF condition variables for copybuf loops.  
                     
   HashMap dblBuffer = new HashMap();
   HashMap dedblBuffer = new HashMap();   
   
   Scope currentScope = null;  
   SymbolTable symtab = null;  
   LabelTable lbltab = null;                    
   
   class PfetchRegion {
      FTree begin;  // PREFETCHBEGIN directive
      FTree end;    // PREFETCHEND directive
      
      public PfetchRegion() {
        begin = null;
        end = null;
      }
   }
   
   class TransformRegion {
      FTree begin;  // TRANSFORMBEGIN directive
      FTree end;    // TRANSFORMEND directive
      
      public TransformRegion() {
        begin = null;
        end = null;
      }
   }
     
   class DoLoop {
      Integer lb,ub,step;  // loop bounds and step size.  int can't be null, but Integer can be null.
                           //  Use null for bounds which are not constants
      Integer niter = null; // number of iterations                     
      FTree id;        // loop index
      FTree lbtree,ubtree,steptree; // Tree forms for the lowerbound, upperbound, and stepsize
      FTree doblk;     // do body
   }
     
   class OuterLoop {
      FTree begin;  // Beginning of the outer loop
      int nStmts;   // Number of statements in this outer loop;  a rough approximation for the time taken to execute this loop.
   }
   
   class Group {
      FTree begin;  // Beginning (OuterLoop) of the group
      FTree end;    // End (OuterLoop) of the group
      int nStmts;   // Number of statements in this group.
   }
   
   class CopyBufLoop {
      FTree tree;  // inner loop
      FTree pfetchSub; // The index save operation which substitutes the copybuf loop in the prefetch region
      LinkedList arrayRefs; // Track the arrayRefs that need to be prefetched
      Symbol guardSym; // Symbol for the guarding boolean variable
      int times=1;  // The number of times the inner loop needs to be executed
      int size=0; // The number of bytes to be copied per assignment statement 
      int typelen=0; // Size of the copied variable's data type.  It's usually 1,4, or 8 bytes.
      public CopyBufLoop()
      {  // Initialize the list of arrayReferences
         this.arrayRefs = new LinkedList();
      } 
   }
   
   // For copybuf loops
   class CopySize {
      int totSize; // Total size of the transfer in bytes 
      int typelen; // Size of each element in bytes (typically 1,4, or 8 B)
   }
   
   public pass3_dblBuffer_modern(TreeNodeStream input, SymbolTable symtab, LabelTable lbltab){
      this(input);
      this.symtab = symtab;
      this.lbltab = lbltab;      
      //this.lblfregion = lbltab.getSize()+1; 
   }
   
   // Move the below Utils routine to FTree someday
   // If we create a separate grammar to hold the utility routines, then we must
   //    declare these methods as static.  However, create and dupTree methods
   //    are not static.  They are methods derived from non-static methods of ANTLR's
   //    CommonTree.  Therefore we are forced to either replicate the methods below
   //    in each grammar file, or try to add them to FTree.  However, FTree is a pure
   //    Java file, and can't recognize TOKENS.  We must convert it into a .g file.
   //    Consider moving the retrieveSymbol and genSymbol routines into the 'symbol' package.
   public FTree increment(Symbol sym) {                                                     
     // name = name + 1
     // ^(T_ASSIGN NAME[text] ^(T_PLUS[+] NAME[text] ICON["1"]))
     LinkedList nodes = new LinkedList<FNode>();
     nodes.add(new FNode(T_PLUS,"+"));
     nodes.add(new FNode(NAME,sym.name));
     nodes.add(new FNode(ICON,"1"));
     FTree plus = createTree(nodes);
 
     nodes.add(new FNode(T_ASSIGN,"="));
     nodes.add(new FNode(NAME,sym.name));
     nodes.add(new FNode(FTREE,plus));
     return createTree(nodes);
   }   
      
   public FTree set(Symbol sym,int val) {                                                     
     // name = val
     // ^(T_ASSIGN NAME[text] ICON[val])
     LinkedList nodes = new LinkedList<FNode>();
     nodes.add(new FNode(T_ASSIGN,"=")); 
     nodes.add(new FNode(NAME,sym.name));
     nodes.add(new FNode(ICON,Integer.toString(val)));     
     return createTree(nodes);
   }
      
   public FTree set(FTree tree,int val) {                                                     
     // name = val
     // ^(T_ASSIGN NAME[text] ICON[val])
     LinkedList nodes = new LinkedList<FNode>();
     nodes.add(new FNode(T_ASSIGN,"=")); 
     nodes.add(new FNode(FTREE,tree));
     nodes.add(new FNode(ICON,Integer.toString(val)));     
     return createTree(nodes);
   }
      
   public FTree gt(Symbol sym,int val) {
      // name > val         
      // ^(T_GT NAME ICON[val])
      LinkedList nodes = new LinkedList<FNode>();
      nodes.add(new FNode(T_GT,".gt."));
      nodes.add(new FNode(NAME,sym.name));
      nodes.add(new FNode(ICON,Integer.toString(val)));
      return createTree(nodes);
   }
      
   public FTree gt(FTree tree,int val) {
      // name > val         
      // ^(T_GT NAME ICON[val])
      LinkedList nodes = new LinkedList<FNode>();
      nodes.add(new FNode(T_GT,".gt."));
      nodes.add(new FNode(FTREE,tree));
      nodes.add(new FNode(ICON,Integer.toString(val)));
      return createTree(nodes);
   }
   
   public FTree arrayRef(String name,FTree subscripts) {
      LinkedList nodes = new LinkedList<FNode>();                   
      nodes.add(new FNode(ARRAYREF,"ARRAYREF"));
      nodes.add(new FNode(NAME,name));
      nodes.add(new FNode(FTREE,subscripts));
      return createTree(nodes);
   }   
   
   public class FNode {
      int token;
      String name;
      FTree tree;
      
      public FNode(int token, FTree tree) {
         this.token = token;
         this.name = null;
         this.tree = tree;     
      }                 
      public FNode(int token, String name) {
         this.token = token;
         this.name = name;
         this.tree = null;
      }
   }               
                   
   // Create a tree from a linkedlist of nodes 
   private FTree createTree(LinkedList nodes) {
      assert (nodes.size() > 1);
      FTree root = null, child = null;
                     
      FNode node = (FNode) nodes.removeFirst();
      switch (node.token) {
        case FTREE: 
          root = node.tree;
          break;
        case NAME:
          root = (FTree) adaptor.create(NAME,node.name);
          root.symbol = retrieveSymbol(node.name);
          break;
        default:
          root = (FTree) adaptor.create(node.token,node.name);
          break;
      } 
                     
      int nChild = nodes.size();
      for (int i=0; i<nChild; i++) {
          node = (FNode) nodes.removeFirst();
          switch (node.token) {
            case FTREE: 
              child = node.tree;
              break;
            case NAME:
              child = (FTree) adaptor.create(NAME,node.name);
              child.symbol = retrieveSymbol(node.name);
              break;
            default:
              child = (FTree) adaptor.create(node.token,node.name);
              break;
          }
          root.addChild(child);  
      }
      return root;
   }
   // End of the Utils routines
  
   private int ivalueOf(String parameter){
      Symbol vs;      
      
      vs = currentScope.resolve(parameter);        
      if(vs == null){
         System.out.println("pass3_dblBuffer.g (pipelineblock): "+parameter+" is undefined");
         System.exit(-1);
      }     
                 
      return ((ParamSymbol)vs).ivalue;
   }
  
   private Symbol retrieveSymbol(String name) {
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
   
   // Find a unique name in the currentScope and generate a symbol for that name
   private Symbol genSymbol(String basename, boolean barray, FTree arrayExtents){
        Symbol sym;
        String name = basename;
        int i = 0;
        // Try the given name first
        while (null != currentScope.resolve(name)){
            // Try another name
            i++;
            name = basename + Integer.toString(i);
        }
        
        FTree nameTree = null;
        // Found a unique name; now create a symbol
        if (barray) {
            sym = new ArraySymbol(name,arrayExtents,(BaseScope)currentScope);
            nameTree = (FTree) adaptor.create(ARRAY,"ARRAY");           // ^(ARRAY NAME arrayDeclaratorExtents) 
            FTree id = (FTree) adaptor.create(NAME,name);
            id.symbol = sym;
            nameTree.addChild(id);
            nameTree.addChild((FTree) Translator.fTreeAdaptor.dupTree(arrayExtents));
        } else { 
            sym = new VariableSymbol(name,Symbol.getDataType(name),false);
            nameTree = (FTree)adaptor.create(NAME,name); // NAME
            nameTree.symbol = sym;
        }
        currentScope.define(sym);
        
        assert currentDeclBlk != null;
        // ^(TYPESTMT type Name)
        FTree newdecl = (FTree)adaptor.create(TYPESTMT,"TYPESTMT");
        newdecl.addChild((FTree)adaptor.create(T_INTEGER,"integer"));
        newdecl.addChild(nameTree);
        
        currentDeclBlk.addChild(newdecl);
        
        return sym;
   }
   
   private Symbol genScalarSymbol(String basename) {
      return genSymbol(basename, false, null);
   }
            
   private Symbol genArraySymbol(String basename, int[] extentsLB, int[] extentsUB, int ndim) {           
      // create its array declarator extents
      //  arrayDeclaratorExtents : ^(ADEXTS arrayDeclaratorExtent arrayDeclaratorExtent*);
      //  arrayDeclaratorExtent  : ^(ADEXT expression expression?)
      FTree adextents = (FTree) adaptor.create(ADEXTS,"ADEXTS");
      FTree adextent = null;

      assert (ndim < 8);  // Fortran-77 allows only upto 7 dimensions 
            
      for (int i=0; i<ndim; i++) {
          adextent = (FTree) adaptor.create(ADEXT,"ADEXT");
          if (extentsLB[i] != 1)  {
             // Add the lowerbound
             adextent.addChild((FTree) adaptor.create(ICON,Integer.toString(extentsLB[i])));
          }   
          adextent.addChild((FTree) adaptor.create(ICON,Integer.toString(extentsUB[i])));  // Add the upper bound
          adextents.addChild(adextent);
      }
                      
      return genSymbol(basename,true,adextents);
   }
   
      
   /* The Function interface */
   public interface Function {
      void call(FTree root); 
   }
   
   public class ReplaceFunc implements Function {
      String oldname, newname;
      Symbol newSymbol;
 
      public ReplaceFunc(String oldname, String newname, Symbol newsym) {
         this.oldname = oldname;
         this.newname = newname;
         this.newSymbol = newsym;
      }
 
      public void call(FTree node) {
         if (node.toString().contentEquals(oldname)) {
            FTree newtree = (FTree) adaptor.create(NAME,newname);
            newtree.symbol = newSymbol;
            FTree parent = (FTree) node.getParent();
            int indx = node.getChildIndex();
            parent.setChild(indx, newtree);
         }
      }
   }
   
   public class PrintFunc implements Function {
      public void call(FTree node) {
         System.out.println("printStmt: "+node.toStringTree());
      }
   }
   
   public class CountFunc implements Function {
      int count;
      public CountFunc() {
        this.count = 0;
      }
      
      public void call(FTree node) {
         count++;
      }
   }
   
   // Replace the oldname with newname in the given tree 'root'
   public void replaceName(FTree root, String oldname, String newname) {
      Symbol newsym = retrieveSymbol(newname);
      ReplaceFunc replace = new ReplaceFunc(oldname, newname, newsym);
      DFS(root, NAME, replace);
   }
        
   // Debug routine to print all statements rooted at type beginType
   // We perform a DFS search for beginType in root
   private void printStmt(FTree root, int beginType) {
      PrintFunc print = new PrintFunc();
      DFS(root, beginType, print);
   }
   
   // Count the number of assignment statements under root
   private int countAssign(FTree root) {
      CountFunc count = new CountFunc();
      DFS(root, T_ASSIGN, count);
      return count.count;
   }
   
   // When you find a token of 'type' perform 'func'
   public void DFS(FTree root, int type, Function func) {
      // Perform a DFS (right to left)
      Stack nodesvisited = new Stack();
      nodesvisited.push(root);
      while (!nodesvisited.isEmpty()) {
         FTree node = (FTree) nodesvisited.pop();
         if (type == node.getType()) {
            func.call(node);
         } else {
           // Add the children of node to the stack
           for (int i=0; i<node.getChildCount(); i++) {
               nodesvisited.push(node.getChild(i));
           }
         }
      }     
   }
   
   /* Find the enclosing IF for this copy buffer loop inside the prefetch region.
      Note: it's the caller responsibility to make sure that currNode is inside
         the prefetch region.
     
      Populate currEnclosingIF or currEnclosingElse depending on whether we are 
      in the IF block or the Else.  If we are not inside any IF statement, leave 
      them both as nulls.
    */   
   private boolean findEnclosingIF(FTree currNode, FTree[] currEnclosing) {
      // currEnclosingIF   --> currEnclosing[0]
      // currEnclosingElse --> currEnclosing[1]
      // Java doesn't support pass by reference.  The primitive are passed by value, and objects are passed as pointers.  
      //   However, even if we pass the pointer to an immutable object such as a string, it can't be modified.
      //   Since Java doesn't support references, we resort to manipulating array objects to pass values around conveniently.
      FTree parent = null, prevparent = null; 
      int parentType = 0;
      boolean bfound;
 
      // Reset
      currEnclosing[0] = null;
      currEnclosing[1] = null;
      
      assert (currNode != null);
      parent = (FTree)currNode.getParent();
      parentType = parent.getType();
      
      while (! (THENBLOCK == parentType ||
                ELSEBLOCK == parentType ||
                EXECUTIONBLOCK == parentType) // || Reached the beginning of the procedure.  We can stop.
                // We have no other cases.  PPM directive can never be the parent for a loop. It's difficult to check
                //   if we reached the beginning of a prefetch region.
                ) {
                parent = (FTree)parent.getParent();
                parentType = parent.getType();
      }
      
      // Note: the loop of redundant iterations gets converted into an IF statement.  Even if the copybuf loop doesn't
      //  originally have any enclosing IFs, the loop will now come under the IF statement which eliminates the 
      //  redundant iterations.  Although the guard statement for such a coypbuf may seem unnecessary, it avoids 
      //  prefetching the very last briquette twice.  Therefore, it's only appropriate for the copybuf loop to have
      //  the enclosingIF.
      // Implication: if Fortran-W template is followed, a copybuf loop will always have an enclosing IF statement, 
      //   either in the original code, or inserted by CFDbuilder.      
      
      bfound = true;   // Initialize return value to success 
      switch (parentType) {
        case THENBLOCK: 
          currEnclosing[0] = parent;
          break;
        case ELSEBLOCK:
          currEnclosing[1] = parent;
          break;
        default: // For all other cases
          bfound = false;  // Must never happen for Fortran-W because of the reasons laid out above.
          break;    
      } 
      
      return bfound;
   }
   
   private void moveCopyBufBlk(FTree copyBufBlk){
      // Place the copybuf block right after the TRANSFORMBEGIN directive
      //   We assume that there is only one transform region in an executable unit.
      
      // We simulate lists by creating a tree rooted at NIL.  This is because
      //   the children of NIL can be added to NIL's parent by simply invoking addChild.
      //   Internally ANTLR stores NIL as null.
     
      assert (transformRegions.size()>0);
      TransformRegion transformregion = (TransformRegion) transformRegions.get(0);
      FTree transformbegin = transformregion.begin;
      FTree parent = (FTree) transformbegin.getParent();
      int index = transformbegin.getChildIndex();
      
      FTree list = (FTree)adaptor.nil();
      list.addChild(copyBufBlk);
      list.addChild(transformbegin);
      
      parent.replaceChildren(index,index,list);  // setChild doesn't work for lists
   }   
   
   private FTree copybufIfArrayRef(Symbol sym, String sub) {
      // name[sub] > 0
      LinkedList nodes = new LinkedList<FNode>();
      nodes.add(new FNode(SUBSCRIPT,"SUBSCRIPT"));
      nodes.add(new FNode(NAME,sub));
      FTree subscripts = createTree(nodes);
      FTree arrayref = arrayRef(sym.name,subscripts);
      return arrayref;   
   }
   
   private FTree createCopyBufIf(CopyBufLoop copyBufLoop) {
      // Create a skeletal IF statement     
      // ^(T_IF expression ^(THENBLOCK wholeStatement*))
      FTree thenblk = (FTree)adaptor.create(THENBLOCK,"then");
            
      // Generate new symbols to hold the AND of all surrounding IF statements for this copybuffer loop.
      // We need to guard the moved loop with the AND of all IFs.
      // ifcopybuf<suffix> is double-buffered.  create array declarator extents.
      int[] extentsLB = {0};
      int[] extentsUB = {1};   // 2 for double buffering the indexes
      Symbol sym = genArraySymbol("ifcopybuf", extentsLB, extentsUB, 1);
      copyBufLoop.guardSym = sym;
      
      FTree arrayref = copybufIfArrayRef(sym, "ic0");
      FTree exp = gt(arrayref,0);
           
      FTree ifStmt = (FTree)adaptor.create(T_IF,"if");
      ifStmt.addChild(exp);
      
      // Reset the arrayref to 0
      exp = set((FTree) adaptor.dupTree(arrayref),0);
      thenblk.addChild(exp);
      ifStmt.addChild(thenblk);
      
      // Set name to 1 and add it to pfetchSub
      arrayref = copybufIfArrayRef(sym, "ic1");
      FTree assign = set(arrayref,1);      
      copyBufLoop.pfetchSub.addChild(assign);   // add to pfetchSub;
      copyBufCondVars.add(sym); // We need to initialize name.  So track it.
      
      return ifStmt;
   }
   
   private FTree insertDo(CopyBufLoop copyBufLoop) {
     // Copy the incoming loop
     FTree loopcopy = (FTree) adaptor.dupTree(copyBufLoop.tree);
     FTree finalLoop = loopcopy;  // For initialization, assume that the inner copybuf loop needs to be executed only once.
     
     if (copyBufLoop.times > 1) {
        // The copybuf inner loop needs to be executed more than once.  Therefore insert a DO loop around it.
        
        // doStatement: ^(T_DO doVarArgs doBody)
        // doVarArgs  : ^(DOVARARG identifier expression expression (expression )?)
        // doBody     : ^(DOBLOCK wholeStatement*) 
        
        Symbol sym = genScalarSymbol("itimes");
        FTree id = (FTree) adaptor.create(NAME, sym.name);
        id.symbol = sym;
        
        // Replace the token TIMES in the loopcopy with itimes<suffix>.  replaceSubscripts creates this place holder token for the subscripts to the indexarray
        // Perform a DFS (right to left)
        ReplaceFunc replace = new ReplaceFunc("TIMES", sym.name, sym); // Token TIMES has the string "TIMES".  We replace TIMES with NAME, but we can still reuse the ReplaceFunc.
        DFS(loopcopy, TIMES, replace);
  
        FTree dovarargs = (FTree) adaptor.create(DOVARARG, "DOVARARG");
        dovarargs.addChild(id);
        dovarargs.addChild((FTree)adaptor.create(ICON,"1"));
        dovarargs.addChild((FTree)adaptor.create(ICON,Integer.toString(copyBufLoop.times)));
        
        FTree doBody = (FTree) adaptor.create(DOBLOCK, "DOBLOCK");
        doBody.addChild(loopcopy);
        
        FTree doStmt = (FTree) adaptor.create(T_DO, "do");
        doStmt.addChild(dovarargs);
        doStmt.addChild(doBody);
        
        finalLoop = doStmt;
     }
      
     return finalLoop;
   }
      
   private FTree replaceSubscripts(CopyBufLoop copyBufLoop) {
     /* Some of the subscripts for the copyBuf statement must change.  The more obvious cases are
        the subscripts which are computed inside a loop.  We need to store such scalars in an
        array, and use the array element as a subscript.  Even if the subscript is a scalar
        which is not computed inside a loop, at the very least, we need two copies of the scalar
        (old and new) for double buffering.  We can store them in a 2-element array as well.
        
        Eg: do jjbq=1,nbqyperpencil
            ....
            iicube = iindxq(jjbq)
            // copyBuf loop
            do i=1,ntoread
            // copy statement
            ..... = dd(i,iicube)
            enddo
            enddo
            
        We need to store iicube into a 2-dimensional array such as indx_rval(nbqyperpencil,2).
        We will rewrite the copy statement to,
            ..... = dd(i,indx_rval(itimes,ic0))  
            where ic0 refers to the double buffer index when not in the prefetch region and itimes
        refers to the iteration number of the copybuf loop
      */
    
     // Make sure that only assignment statements are inside the copyBuf loop
     FTree doblk, id;
     FTree list = (FTree) adaptor.nil();  // List of statements replacing the copybuf in the prefetch region
     
     Symbol idsym;
     // Parse the loop
     DoLoop doloop = parseDoLoop(copyBufLoop.tree);
     id = doloop.id;
     doblk = doloop.doblk;
     idsym = id.symbol;   
          
     for (int i=0; i<doblk.getChildCount(); i++) {
         FTree stmt = (FTree) doblk.getChild(i);   
               
         if (T_ASSIGN != stmt.getType() ||
             ARRAYREF != stmt.getChild(0).getType() ||
             ARRAYREF != stmt.getChild(1).getType() ) {
            System.out.println("Copy Buffer loop error: Copy Buffer loop must contain only simple assignment statements between two arrays such as:     array1(...) = array2(...)");
            System.out.println("Refer to the derived file output_orig at line:"+stmt.getLine());
            System.exit(-1); 
         }
                  
         // stmt passed the test
         for (int j=0; j<stmt.getChildCount(); j++) {
             FTree arrayref = (FTree) stmt.getChild(j);
             FTree subscripts = (FTree) arrayref.getChild(1);
             for (int k=0; k<subscripts.getChildCount(); k++) {
                 FTree subscript = (FTree) subscripts.getChild(k);
                 int[] extentsLB = new int[7];
                 int[] extentsUB = new int[7];
                 int ndim = 0;
                 
                 FNode node = null;
                 LinkedList nodes = new LinkedList<FNode>();   
                 
                 // don't replace the copybuf loop index
                 // replace all other subscripts
                 if (subscript.symbol != idsym){ 
                    // create a new array index ( indxarr<suffix> ) to hold the subscript       
                         
                    // ^(T_ASSIGN ^(ARRAYREF ID=NAME s=subscripts) expression)
                    // subscripts: ^(SUBSCRIPT expression+)
                    nodes.add(new FNode(SUBSCRIPT,"SUBSCRIPT")); 
                    
                    if (copyBufLoop.times > 1) {
                      // create a counter to track the subscript (index) across iterations     
                      Symbol counterSym = genScalarSymbol("indxcounter");
                      indxCounters.add(counterSym);  // Keep track of the counters for the indexes.  
                                                     //   Initialize them to zero just outside of the prefetch region
                                                     
                      // indxCounter = indxCounter + 1;  Increment the counter
                      FTree assign = increment(counterSym);                              
                      list.addChild(assign);                      
                      
                      // Now save the index in an index array
                      
                      // indxarr<suffix>(indxcounter<suffix>,ic1) = <original index>
                      // subscripts: ^(SUBSCRIPT expression+);  The subscripts for indxarr<suffix> above.
                      nodes.add(new FNode(FTREE,(FTree) adaptor.dupTree(assign.getChild(0))));
                       
                      // Set the extents for the array    
                      extentsLB[ndim] = 1;
                      extentsUB[ndim] = copyBufLoop.times;
                      ndim++; 
                   }  // else { // copyBufLoop.times == 1
                        // No need to create any counters for indexes.
                        // Just create a 2-element array of indexes.
                        
                   // The below lines are common for both copyBufLoop.times >= 1
                   
                   // create array declarator extents for indxarr<suffix>;  We need it to create a symbol for indxarr<suffix>   
                   extentsLB[ndim] = 0;   // ic0 and ic1 take only values 0 and 1
                   extentsUB[ndim] = 1;   // 2 for double buffering the indexes
                   ndim++;
                   // We need it to create a symbol for indxarr<suffix>
                   Symbol indxSym = genArraySymbol("indxarr", extentsLB, extentsUB, ndim);
                   
                   // The subscripts for indxarr<suffix> in "indxarr<suffix>(indxcounter<suffix>,ic1) = <original index>"                           
                   // Prefetch region: add ic1 as the last dimension.
                   nodes.add(new FNode(NAME,"ic1"));
                   FTree indxSubscripts = createTree(nodes);
                   
                   // indxarr<suffix>(indxcounter<suffix>,ic1) = <original index>
                   // ^(T_ASSIGN ^(ARRAYREF ID=NAME s=subscripts) expression)
       
                   FTree indxArrayref = arrayRef(indxSym.name,indxSubscripts);  // indxarr<suffix>
                   
                   nodes.add(new FNode(T_ASSIGN,"="));
                   nodes.add(new FNode(FTREE,indxArrayref));
                   nodes.add(new FNode(FTREE,subscript));  // The right value is the original subscript as is.
                   FTree assign = createTree(nodes);                 
                  
                   //copyBufLoop.pfetchSub = assign; // Store the pointer to the substitute statement.  We need to know where the copybuf loop was originally.
                   
                   list.addChild(assign); // A list of index save operations replace the copybuf in the prefetch region
                   
                   // Now we need to replace the index in the copybuf statement with this newly created index array
                   // Create the subscripts for this newly created index array
                   nodes.add(new FNode(SUBSCRIPT,"SUBSCRIPT"));
                   if (copyBufLoop.times > 1)  nodes.add(new FNode(TIMES,"TIMES"));   // The TIMES token is a place holder for itimes<suffix> variable that will be generated in the InsertDo function
                   nodes.add(new FNode(NAME,"ic0"));
                   indxSubscripts = createTree(nodes);         

                   // create the array ref for this index array
                   indxArrayref = arrayRef(indxSym.name,indxSubscripts);   // indxarr<suffix>
                   
                   // replace the original index with this index array reference
                   subscripts.setChild(k, indxArrayref);    // The original subscript in the current copybuf loop statement is replaced with the newly created index array reference
                 } 
             }
         }
         // Add the rvalue of the assignment statement to the list of arrayRefs to be prefetched.
         FTree arrayref = (FTree) adaptor.dupTree(stmt.getChild(1));
         replaceName(arrayref,"ic0","ic1");
         System.out.println("arrayref in copyloop"+arrayref.toStringTree());
         copyBufLoop.arrayRefs.add(arrayref);
     }
     
     return list;
   
   }
   
   
   
   // Initialize the condition variables in the copybuf IF statements to zero
   private void initializeCopyBufCondVars() {      
      FTree list = (FTree) adaptor.nil();         
      int numCounters = copyBufCondVars.size();
      
      for (int i=0; i<numCounters; i++) {
          Symbol counterSym = (Symbol) copyBufCondVars.removeFirst();          
          FTree assign = null;
          
          if (counterSym.isArraySymbol()) {
             // We have an array symbol here.  We need to set the whole array to 0.
             // The easiest way to do that is to create a loop for the entire size of array (itotalsize)
             //  and insert the assignment statement inside.
             //     let LB1 be the lowerbound for the first dimension
             //     do i=LB1,LB1+(itotalsize-1)
             //       name(i,<lower bounds for all other dims>) = 0 
             //     enddo
             LinkedList nodes = new LinkedList<FNode>();
             // Create the upper bound for the DO loop first
             nodes.add(new FNode(FTREE,(FTree) adaptor.create(T_PLUS,"+")));
             nodes.add(new FNode(FTREE,(FTree) adaptor.dupTree(((ArraySymbol)counterSym).LBDimTree[0])));             
             nodes.add(new FNode(ICON,Integer.toString(((ArraySymbol)counterSym).itotalsize-1)));
             FTree ub = createTree(nodes);
             
             Symbol loopid = genScalarSymbol("i");
             nodes.add(new FNode(DOVARARG, "DOVARARG"));
             nodes.add(new FNode(NAME, loopid.name));
             nodes.add(new FNode(FTREE,(FTree) adaptor.dupTree(((ArraySymbol)counterSym).LBDimTree[0])));
             nodes.add(new FNode(FTREE, ub));
             FTree dovarargs = createTree(nodes);

             // create the array subscripts
             nodes.add(new FNode(SUBSCRIPT,"SUBSCRIPT"));
             nodes.add(new FNode(NAME,loopid.name));  // The loop index
             for (int j=1; j<((ArraySymbol)counterSym).nDim; j++) {
                 nodes.add(new FNode(FTREE,(FTree) adaptor.dupTree(((ArraySymbol)counterSym).LBDimTree[j])));  // The lower bounds for all other dimensions
             }
             FTree subscript = createTree(nodes);
             FTree arrayref = arrayRef(counterSym.name,subscript);
             assign = set(arrayref,0);
                     
             FTree doBody = (FTree) adaptor.create(DOBLOCK, "DOBLOCK");
             doBody.addChild(assign);
        
             FTree doStmt = (FTree) adaptor.create(T_DO, "do");
             doStmt.addChild(dovarargs);
             doStmt.addChild(doBody);
             assign = doStmt;
          } else { 
             assign = set(counterSym,0);
          }         
          list.addChild(assign);
      }
      
      // Find the parent of the pipeline loop, and insert the list in front of the pipeline loop    
      assert (pipelineDir != null);
      
      FTree parent = (FTree) pipelineDir.getParent(); // Store the original parent before we modify pfetchBegin's parent
      int indx = pipelineDir.getChildIndex();
        
      list.addChild(pipelineDir);
      parent.replaceChildren(indx,indx,list);
   }


   
   private void initializeCounters() {
      assert (pfetchRegions.size()>0);
      
      FTree list = (FTree) adaptor.nil();         
      int numCounters = indxCounters.size();
      
      for (int i=0; i<numCounters; i++) {
          // counter = 0          
          Symbol counterSym = (Symbol) indxCounters.removeFirst();
          assert (!counterSym.isArraySymbol());
          
          FTree assign = null;
          assign = set(counterSym,0);            
          list.addChild(assign);
      }
      PfetchRegion pfetchregion = (PfetchRegion) pfetchRegions.get(0); // Get the first prefetch region
      FTree pfetchBegin = pfetchregion.begin;
      FTree parent = (FTree) pfetchBegin.getParent(); // Store the original parent before we modify pfetchBegin's parent
      int indx = pfetchBegin.getChildIndex();
        
      list.addChild(pfetchBegin);
      parent.replaceChildren(indx,indx,list);
   }
   
   private void exitExecutableUnit(FTree execUnit, LinkedList<CopyBufLoop> copyBufLoops) {
      //CopyBufLoop copyBufLoop = null;
      FTree copyBufBlk = (FTree) adaptor.nil();  // List of all the copy buffer loops in this unit.  
      FTree copyBufIf = null;       // copybuffer statement currently under construction
      FTree copyBufThen = null;     // THENBLOCK of the copyBufIf under construction
      FTree prevEnclosingIF = null;   // The THEN part of an IF stmt enclosing the previously seen copybuffer
      FTree prevEnclosingElse = null; // The ELSE part of an IF stmt enclosing the previously seen copybuffer          
                                      // Multiple copybuffer loops might be inside one IF block.  
                                      //   Remembering what was the last seen enclosing IF helps us to not
                                      //   generate one IF statement for every loop.  
      Symbol guardSym = null;          // The symbol for the boolean variable guarding the current copybufloop                                 
      if (copyBufLoops.isEmpty()) return;  // No prefetch loops in this executable unit      
                                      
      for (CopyBufLoop copyBufLoop : copyBufLoops) {
         //copyBufLoop = (CopyBufLoop) copyBufLoops.removeFirst();
         FTree currEnclosingIF = null;   // The THEN part of an IF stmt enclosing the currently seen copybuffer
         FTree currEnclosingElse = null; // The ELSE part of an IF stmt enclosing the currently seen copybuffer
         FTree[] currEnclosing = new FTree[2];   // An array for return values
         //findEnclosingIF(copyBufLoop.pfetchSub, currEnclosing); // See if the copybuf was inside an enclosing block in the prefetch region.  
                                                                // Since copyBuf loop is currently in a no-man's land, we use the substitute statement for copybuf to find the enclosing parent. 
         findEnclosingIF(copyBufLoop.tree, currEnclosing); // See if the copybuf is inside an enclosing block in the prefetch region.                                                       
         currEnclosingIF = currEnclosing[0];
         currEnclosingElse = currEnclosing[1];
      
         // Copybuf loops can occur:
         //   (i)  inside IF
         //  (ii)  inside ELSE
         // (iii)  outside of IF or ELSE
      
         // We can group copybuf loops belonging to the same parent block (IF or ELSE) under one 
         //   IF statement.  For simplicity, we will generate a separate IF statement for each one
         //   of these blocks.  Both the IF and ELSE block map to an IF statement.  copyBufIf stores 
         //   this IF statement under construction.
      
         // We need to close and append this copyBufIf to copybufblk if the current copybuf loop 
         //   is not inside the same block as the previous copybuf loop.
         if (copyBufIf != null) {
            if (currEnclosingIF != prevEnclosingIF || currEnclosingElse != prevEnclosingElse) {
               copyBufBlk.addChild(copyBufIf);
               copyBufIf = null;
               copyBufThen = null;
               guardSym = null;
            }
         }   

         if (null != currEnclosingIF || null != currEnclosingElse) {
            prevEnclosingIF = currEnclosingIF;
            prevEnclosingElse = currEnclosingElse;
            if (null == copyBufIf) {
               // Create a skeletal IF statement along with the initialization of the lval in the IF boolean expression
               copyBufIf = createCopyBufIf(copyBufLoop);
               guardSym = copyBufLoop.guardSym;
               copyBufThen = (FTree) copyBufIf.getChild(1);               
            }
            
            // Place the copybuf loop inside a DO block if necessary
            // Set the guard symbol for the copyBufLoop
            copyBufLoop.guardSym = guardSym;
            // Add the copybuf loop to the open copyBufIf
            copyBufThen.addChild(insertDo(copyBufLoop));
         } else {  // null == currEnclosingIF && null == currEnclosingElse
            // We don't need any IF statements guarding the copy buffer loop
            //   copyBufBlk is a list ie a tree rooted at NIL                        
            // Set the guard symbol for the copyBufLoop
            copyBufLoop.guardSym = null;  // There is no guard statement necessary
            // Place the copybuf loop inside a DO block if necessary
            copyBufBlk.addChild(insertDo(copyBufLoop));
         }   
         
         // replace copybuf loop with pfetchSub list
         FTree parent = (FTree) copyBufLoop.tree.getParent();      
         int indx = copyBufLoop.tree.getChildIndex();
         parent.replaceChildren(indx, indx, copyBufLoop.pfetchSub);  
      }
      
      // We need to close and append any open copyBufIf to copyBufBlk.
      if (copyBufIf != null) {
         copyBufBlk.addChild(copyBufIf);
      }
      copyBufIf = null;
      copyBufThen = null;
      guardSym = null;
      
      List<Group> loopgroups = splitRegions();
      FTree offInits = insertPrefetches(loopgroups, copyBufLoops); // Returns initialization statements for the index offsets to the split copybuf loops
      copyBufBlk.addChild(offInits); // Add the initialization statements to the end of the copybuf block
      
      // Move the copybuf statements from prefetch region to transform region
      moveCopyBufBlk(copyBufBlk);
      initializeCopyBufCondVars(); // initialize the condition variables for the copybuf IF statements
      initializeCounters();  // initialize the counters for the indexarrays used in the copybuf loops
      //printStmt(execUnit,PPMDIRECTIVE);
      copyBufLoops.clear();
      loopgroups.clear();
      pipelineDir = null;
   }
   
   private List<Group> splitRegions() {
      // find outer loops
      assert (transformRegions.size()>0);
      TransformRegion transformregion = (TransformRegion) transformRegions.get(0);
      FTree transformbegin = transformregion.begin;
      FTree transformend = transformregion.end;
      List<OuterLoop> outerloops = findOuterLoops(transformbegin,transformend);
      
      // find the maximum number of assignment statements in any given outerloop
      int maxStmts = -1;
      for (OuterLoop outerloop : outerloops) {
          if (maxStmts < outerloop.nStmts) maxStmts = outerloop.nStmts;  
      }
      
      // Group the outer loops to have approximately equal number of statements
      int grpStmts = 0;  // Number of statements in the current group
      List<Group> loopgroups = new ArrayList<Group>();
      Group group = new Group();
      group.begin = ((OuterLoop)outerloops.get(0)).begin;
      FTree prevLoop = group.begin;
      for (OuterLoop outerloop : outerloops) {
          if (grpStmts + outerloop.nStmts > maxStmts) {
             // close the current group
             group.end = prevLoop;
             group.nStmts = grpStmts;
             loopgroups.add(group);
                          
             grpStmts = 0;
             group = new Group();
             group.begin = outerloop.begin;
          }
          grpStmts = grpStmts + outerloop.nStmts;          
          prevLoop = outerloop.begin;
      }
      // close the last group
      group.end = prevLoop;
      group.nStmts = grpStmts;
      loopgroups.add(group);
      
      outerloops.clear();
      
      return loopgroups;
   }
   
   public FTree createDo(DoLoop doloop) {
      LinkedList<FNode> nodes = new LinkedList<FNode>();
      //^(DOVARARG id=NAME e1=expression e2=expression (e3=expression )?)      
      FTree dovararg = (FTree) adaptor.create(DOVARARG,"DOVARARG");
      dovararg.addChild(doloop.id);
      dovararg.addChild(doloop.lbtree);
      dovararg.addChild(doloop.ubtree);
      dovararg.addChild(doloop.steptree);
      
      // createTree resets nodes
      FTree dotree = (FTree) adaptor.create(T_DO,"do");
      dotree.addChild(dovararg);
      dotree.addChild(doloop.doblk);      
      return dotree;
   }
         
   private FTree genPfetchCalls(FTree dobody) {
      FTree newdobody = (FTree) adaptor.create(DOBLOCK,"DOBLOCK");
      
      for (int i=0;i<dobody.getChildCount();i++) {
          FTree stmt = (FTree) dobody.getChild(i);
          // The dobody can only contain assignment statements.  We have already verified the doblock for conformance.
          FTree rval = (FTree) stmt.getChild(1); // righthand side value (array reference)
          
          // call mm_prefetch(rval of the assignment stmt)
          //
          // callStatement : ^(T_CALL subroutineCall)   
          // subroutineCall :  (subroutineName callArgumentList?)  ! We don't appear to have any root here.
          // callArgumentList : ^(CALLARG callArgument+)
          // callArgument : expression
          FTree callarg = (FTree) adaptor.create(CALLARG,"CALLARG");
          callarg.addChild(rval);  
          
          LinkedList<FNode> nodes = new LinkedList<FNode>();
          nodes.add(new FNode(T_CALL,"call"));
          nodes.add(new FNode(NAME,"mm_prefetch")); //  If it's not already defined, mm_prefetch will be defined as an integer variable by retrieveSymbol.
          nodes.add(new FNode(FTREE,callarg));
          FTree call = createTree(nodes);
          
          newdobody.addChild(call);                                          
      }      
      return newdobody;
   }   
      /* TODO: Turn these high-level sketches into comments:
   moveCopyBufBlk - It uses the tracked 'transformBegin' to know where the transform region begins.
   findEnclosingIf - 
   InitializeCounters - It uses the tracked 'pfetchBegin' 


Analysis:
========
  Structural:
  -----------
  Find the ends of do loops without any surrounding IF statements
  Find the ends of the highest level IF statements

  Measures:
  ---------
  Count the number of statements in each of the identified blocks

Action:
------
 Group the blocks as regions
 Make sure the regions are balanced in the number of statements in each region
 You prefetch briquettes, but you have the opportunity to prefetch across multiple iterations of the loop over planes in the previous briquette. 
Possible useful routines:
------------------------
parseDoLoop - return the loop bounds, loop index, and the doblk

times=1,{2,4}  ! can even be 1
do j=1,<constant> or <variable>
Let's call the upper bound as ub

itimes = 1: 
-----------
jb = -ub/mynx // initialization just outside the transformbegin

if (jb < ub) then
jb = jb + ub/(mynx*nsplits)
do j=jb,jb + ub/(mynx*nsplits),(cachelinesize/typelen)
  call mm_prefetch(dd(j,indxarr(ic1))  
enddo
endif
 
itimes > 1:
----------
if (jb < ub) then
jb = jb + ub/(mynx*nsplits)
do itimes=1,{2,4}
do j=jb,jb + ub/(mynx*nsplits),(cachelinesize/typelen)
  call mm_prefetch(dd(j,indxarr(itimes,ic1))  
enddo
enddo ! itimes
endif

Computing the splits:
--------------------
(i)  Find the max of # assignment statements from all the outer loops.  Let's call this max as max_assign.
(ii) Group loops into blocks such that each block contains approximately max_assign statements.  (nblocks)
(iii) Count the number of copybuf loops (ncopybuf)
(iv) nsplits = nblocks / ncopybuf 
  */
  
   private FTree insertPrefetches(List<Group> loopgroups, List<CopyBufLoop> copyBufLoops) {
      // Each copybuf loop is split nsplits ways for prefetching
     /* 
      jb = lb - ub/(mynx*splits)   // initialization just before the transformbegin

      if (jb < ub) then
      jb = jb + ub/(mynx*nsplits)
      do j=jb,jb + ub/(mynx*nsplits),(cachelinesize/typelen)
         call mm_prefetch(dd(j,indxarr(ic1))  
      enddo
      endif
 
      itimes > 1:
      ----------
      if (jb < ub) then
      jb = jb + ub/(mynx*nsplits)
      do itimes=1,{2,4}
      do j=jb,jb + ub/(mynx*nsplits),(cachelinesize/typelen)
         call mm_prefetch(dd(j,indxarr(itimes,ic1))  
      enddo
      enddo ! itimes
      endif
     */
      int ncopyloops = copyBufLoops.size();
      int nsplits = loopgroups.size() / ncopyloops;
      
      FTree jbInits = (FTree) adaptor.nil();  // Initialization statements for the offsets to the loop splits.  Need to be placed before the transform region.
      int mynx = ivalueOf("mynx");
      //mynx * nsplits
      int denom = mynx * nsplits;

      int linesize = 64; // Cache line size in bytes
      int groupIdx = 0; // Index of the current loop group.   We will insert the copybuf at the end of the current loop group
      
      for (CopyBufLoop copybufloop : copyBufLoops) {
         DoLoop origLoop = parseDoLoop(copybufloop.tree);
         Symbol jb = genScalarSymbol("jb");
      
         int typelen = copybufloop.typelen;
         int step = linesize/typelen;
         FTree steptree = (FTree) adaptor.create(ICON,Integer.toString(step));
                          
         FTree uboff = null; 
         if (null == origLoop.ub) {
            // use the ubtree instead
            uboff = (FTree) adaptor.create(T_DIV,"/");
            uboff.addChild((FTree) adaptor.dupTree(origLoop.ubtree));
            uboff.addChild((FTree) adaptor.create(ICON,Integer.toString(denom)));
         } else {
            // compute the upperbound offset directly
            int iuboff = origLoop.ub / denom; 
            uboff = (FTree) adaptor.create(ICON,Integer.toString(iuboff));
         }
         
         DoLoop doloop = new DoLoop();
         doloop.steptree = (FTree) adaptor.dupTree(steptree);
             
         FTree jbtree = (FTree) adaptor.create(NAME,jb.name); //jb
         jbtree.symbol = jb; 
         doloop.lbtree =  jbtree; // jb 
 
         LinkedList<FNode> nodes = new LinkedList<FNode>();
             
         FTree plus = (FTree) adaptor.create(T_PLUS,"+");       
         plus.addChild((FTree) adaptor.dupTree(jbtree)); // jb
         plus.addChild(uboff);                           // uboff
         doloop.ubtree = plus;                           // jb + uboff
             
         Symbol jsym = genScalarSymbol("j");
         doloop.id = (FTree) adaptor.create(NAME,jsym.name);
         doloop.id.symbol = jsym;
         FTree origDoBlk = (FTree) adaptor.dupTree(origLoop.doblk);
         FTree modDoBlk = genPfetchCalls(origDoBlk); // Convert the assignment statements into mm_prefetch calls
         replaceName(modDoBlk,origLoop.id.symbol.name,jsym.name);  // replace the original inner loop id with the this newly generated loop id.
                                                                   // Since we are replicating the copybuf loop, it's imperative that we use a
                                                                   //   new name. We can't be sure that using the original loop index is still safe.
         replaceName(modDoBlk,"ic0","ic1");  // We are prefetching.  Use ic1.  
             
         doloop.doblk = modDoBlk;
         FTree doTree = createDo(doloop);  
             
         CopyBufLoop modloop = new CopyBufLoop();
         modloop.tree = doTree;
         modloop.times = copybufloop.times;
         FTree ddoTree = insertDo(modloop);  // The DO loop is now possibly embedded in another DO loop for itimes
             
         // jb = jb+uboff
         nodes.add(new FNode(T_ASSIGN,"="));
         nodes.add(new FNode(NAME,jb.name));
         nodes.add(new FNode(FTREE,(FTree) adaptor.dupTree(doloop.ubtree))); // duplicate jb+uboff i.e. jb+ub/(mynx*nsplits)
         FTree assign = createTree(nodes);
             
         FTree thenblk = (FTree) adaptor.create(THENBLOCK,"then");
         thenblk.addChild(assign);
         thenblk.addChild(ddoTree);
             
         // jb < ub .and. copybufloop guard condition (if any)
         FTree ifTree = (FTree) adaptor.create(T_IF,"if");
         FTree ltTree = (FTree) adaptor.create(T_LT,"<");
         ltTree.addChild((FTree) adaptor.dupTree(jbtree));
         ltTree.addChild((FTree) adaptor.dupTree(origLoop.ubtree));
             
         if (null != copybufloop.guardSym) { // not null
            FTree andTree = (FTree) adaptor.create(T_LAND, ".and.");
            FTree arrayref = copybufIfArrayRef(copybufloop.guardSym, "ic1"); // Use ic1 as we are prefetching.                
            andTree.addChild(gt(arrayref,0));
            andTree.addChild(ltTree);
            ifTree.addChild(andTree);
         } else {
            // Just,  if (jb < ub) 
            ifTree.addChild(ltTree);
         }
         ifTree.addChild(thenblk); // add the THEN block  
                      
         for (int i=0;i<nsplits;i++) {            
             Group currgroup = loopgroups.get(groupIdx);
             if (groupIdx < loopgroups.size()-1) groupIdx++; // Don't increment groupIdx past the end of loopgroups
             FTree insertAt = currgroup.end;
             assert (null != insertAt);
             FTree parent = (FTree) insertAt.getParent();
             int idx = insertAt.getChildIndex();
             
             FTree list = (FTree) adaptor.nil();
             list.addChild(insertAt);
             list.addChild((FTree) adaptor.dupTree(ifTree));  // Add the constructed IF statement to the list
             adaptor.replaceChildren(parent,idx,idx,list);             
         }
         FTree minus = (FTree) adaptor.create(T_MINUS,"-");
         minus.addChild((FTree) adaptor.dupTree(origLoop.lbtree));
         minus.addChild((FTree) adaptor.dupTree(uboff));
         
         assign = (FTree) adaptor.create(T_ASSIGN,"=");
         assign.addChild((FTree) adaptor.dupTree(jbtree));
         assign.addChild(minus);
         
         jbInits.addChild(assign);
      }
      return jbInits;
   }
   
   // Find the outer loops in the transform region
   private List findOuterLoops(FTree begin, FTree end) {
      int beginIdx, endIdx;
      List outerloops = new ArrayList<FTree>();
      FTree parent = (FTree) begin.getParent();
      
      beginIdx = begin.getChildIndex();
      endIdx = end.getChildIndex();
      
      for(int i=beginIdx+1;i<endIdx;i++){
         FTree stmt = (FTree) parent.getChild(i);
         switch(stmt.getType()) {
           case T_IF:
             // We assume that the IF statements are enclosing some DO loops.
             // Therefore, we add IF statements as outer loops as well
           case DDOLOOP:
             OuterLoop outerloop = new OuterLoop();
             outerloop.begin = stmt;
             outerloop.nStmts = countAssign(stmt); // Count the number of assignment statements under stmt.  
                                                   //  It's a rough approximation for the time taken to execute this loop
             outerloops.add(outerloop);
             break;
           default:
             // Ignore all other statements such as incrementing counters
             break;     
         }
      }
      //for (int i=0;i<outerloops.size();i++){
      //  System.out.println(((OuterLoop) outerloops.get(i)).nStmts);
      //}
      return outerloops;   
   }
   
   // Calculate the size of data copy inside a copybuf loop.  We consider only the
   //   the first statement in the loop.  This approximate measure will help us 
   //   chunk the loop into possible many prefetch loops
   private CopySize calcCopySize(FTree copyloop,int copytimes) {
      DoLoop doloop = parseDoLoop(copyloop);
      CopySize copysize = new CopySize();
      
      FTree stmt = (FTree) doloop.doblk.getChild(0); // get the first statement in the loop body
                                             // It must be an assignment statement
                                             
      if (T_ASSIGN != stmt.getType()){
         System.out.println("Only assignment statements are permitted inside copy loops.  Offending statement:"+stmt.toStringTree());
         System.exit(-1);
      }                                       
      // assignStmt: ^(T_ASSIGN varref expression)
      // varref: ^(ARRAYREF NAME subscripts) 
      FTree varRef = (FTree) stmt.getChild(0).getChild(0);
      int typelen = varRef.symbol.typelen;  // size of the data type
      
      int copylen;
      if (null != doloop.niter) {
        copylen = typelen*doloop.niter*copytimes; // size of the data type * num of iterations * # outer loop iterations
      } else {
        copylen = -1; // Can't determine the size
      }  
      
      copysize.totSize = copylen;
      copysize.typelen = typelen;
      return copysize;
   }
     
   public Integer getICON(FTree node) {
      if (ICON == node.getType()) {
         return Integer.parseInt(node.toString());
       }
       return null;  // not an integer constant
   }
     
   public DoLoop parseDoLoop(FTree loop) {
      DoLoop doloop = new DoLoop();
      FTree dovararg;
        
      if (DOVARARG == loop.getChild(0).getType()) {
         // ^(DOVARARG ide lb ub step?)
         dovararg = (FTree) loop.getChild(0);   
         doloop.doblk = (FTree) loop.getChild(1);
      } else {
         // This is a doWithLabel.  The next child must be the DOBLOCK
         dovararg = (FTree) loop.getChild(1);
         doloop.doblk = (FTree) loop.getChild(2);
         assert (DOBLOCK == doloop.doblk.getType());
      }
      // ^(DOVARARG id lb ub step?)
      doloop.id = (FTree) dovararg.getChild(0);
      FTree lb = (FTree) dovararg.getChild(1);           
      FTree ub = (FTree) dovararg.getChild(2);
      doloop.lb = getICON(lb);
      doloop.ub = getICON(ub);
      doloop.lbtree = lb;
      doloop.ubtree = ub;
      if (dovararg.getChildCount()>3) {
         // step size exists
         doloop.step = getICON((FTree) dovararg.getChild(3));
         doloop.steptree = (FTree) dovararg.getChild(3);
      } else {
         // default step size is 1
         doloop.step = 1;
         doloop.steptree = (FTree) adaptor.create(ICON,"1");
      }
      doloop.niter = null; // Initialize it to null
      if (null != doloop.lb && null != doloop.ub && null != doloop.step) {
         doloop.niter = (doloop.ub - doloop.lb + 1) / doloop.step; // number of iterations
      }   
      return doloop;
   }
} 

program :
  ^(CODEROOT (ppmdirectives | directives | executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit 
@init {
   assert (!copyBufLoops.isEmpty());
   pfetchRegions.clear();
   transformRegions.clear(); 
   pipelineDir = null;
}
@after {
  exitExecutableUnit(retval.tree, copyBufLoops);
} :
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
executableStatement :
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
subroutineStatement:
  ^(T_SUBROUTINE ID=NAME namelist?)  {currentScope = $ID.symbol.scope;}
  ;
  
namelist :
  ^(SUBARG identifier+)
  ;

subprogramBody
@after{
  if(bdblBufinSubprogBody || bdedblBufinSubprogBody){
    CommonTreeNodeStream donode;
    FTree declBlk;
           
    declBlk = (FTree)retval.tree.getChild(0);    
    donode = new CommonTreeNodeStream(Translator.fTreeAdaptor, declBlk);    
    fixdblBufDefns fixdblBufDefs = new fixdblBufDefns(donode, dblBuffer, dedblBuffer);
    fixdblBufDefs.setTreeAdaptor(Translator.fTreeAdaptor);
    fixdblBufDefs.downup(declBlk, false);
    
    if (bdblBufinSubprogBody)
        bdblBufinSubprogBody = false;   
    if (bdedblBufinSubprogBody)
        bdedblBufinSubprogBody = false;      
  }
}:
  ^(SUBPROGRAMBLOCK declarationBlock executionBlock)
  ;

declarationBlock 
@after{
  currentDeclBlk = retval.tree;
}:
  ^(DECLARATIONBLOCK declarationStatement*)
  ;

executionBlock:
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
  
wholeStatement  :
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

// Move COPYBUFFER loops from the prefetch region to the beginning of the transformregion
/* wholeStatementReplacement : 
@init{
  boolean btmp = false, bpfetchprev = false;
  int counter = -1, index = -1;
} 
@after{
  if (index > -1){
  /* Delete the statements moved out of the earstwhile redundant iterations block * /
      int numChildren = retval.tree.getChildCount();
      for (int i=numChildren-1;i>index;i--){
         //System.out.println("deleting child "+((FTree)retval.tree.getChild(i)).toStringTree());
         adaptor.deleteChild(retval.tree,i);
      }
  }
}:
  ^(DOBLOCK (
             {btmp = bdelrediterbody; bdelrediterbody = false; bpfetchprev = bpfetch;}
              w=wholeStatement 
             { bdelrediterbody = btmp;
               counter++; 
               if (bdelrediterbody && !bpfetch){ 
                 /*  Move the statements between the prefetch region end and the 
                     redundant do loop end, to outside of the loop.  The assumption
                     is that the prefetch region occurs inside the redudant do loop.
                  * /
                   if(bpfetchprev) /* Just got past the prefetch region * /
                      index = counter;
                      //System.out.println("index = "+index+"   counter = "+counter);    
                   else
                      adaptor.addChild(nonpfetchTree,(FTree)adaptor.dupTree($w.tree));
               }                   
             }
            )+
   )
  ;*/
  
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
  NAME T_DIV (RCON | DCON | ICON) T_DIV
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
unconditionalGoto: 
  lblRef
  ;

lblRef:
  ^(LABELREF ICON)
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
@init {
  boolean bcopybufouter = false; // Outer loop in this copybuf nest
}
@after {
   if (bcopybufbody && !bcopybufouter) {
      // copybuf loop must be an inner loop
      // We are not allowing loop nests because we want to know the size of the transfer.  
      //  Loop nests make the distribution of prefetching in chunks more difficult.  
      System.out.println("Copy Buffer loop error: Copy Buffer loop must not be a loop nest.  Convert it into a single loop");
      System.out.println("Refer to the derived file output_orig at line:"+$d.getLine());
      System.exit(-1); 
   } 
  // The copybuffer loop must be inside the prefetch region.
  if (bcopybufouter && bprefetch) {
      bcopybufouter = false; // Not necessary;  we won't be visiting this node again, but it's good programming practice.
      bcopybufbody = false;  // Necessary, bcopybufbody is global
      // Add the copybuf loop to copyBufLoops list.
      CopyBufLoop copyBufLoop = new CopyBufLoop();
      copyBufLoop.tree = retval.tree;  
      copyBufLoop.times = copytimes;
      CopySize copysize = calcCopySize(retval.tree,copytimes); // calculate the copy size
      copyBufLoop.size = copysize.totSize;
      copyBufLoop.typelen = copysize.typelen;
      // replaceSubscripts will return the index save operations.  In addition, it modifies the copybuf loop ie retval.tree
      copyBufLoop.pfetchSub = replaceSubscripts(copyBufLoop); 
      //FTree newStmts = replaceSubscripts(copyBufLoop);
      //retval.tree = newStmts; // Replacing the copybuf loop with a list of newStmts (index save operations)
      
      copytimes = 0;  // Reset
      copyBufLoops.add(copyBufLoop);
   }
   bcopybuf = false; // reset bcopybuf after any DO loop
}:
   /* If I don't reset bldoloop right away here after T_DO, the innermost do loop gets 
      converted to DDOLOOP.  
    */ 
   {bldoloop && btransregion}? ^(d=T_DO  {bldoloop = false;} (doWithEndDo|doWithLabel) ) -> ^(DDOLOOP doWithEndDo? doWithLabel?)
 | {!(bldoloop && btransregion)}? ^(d=T_DO {
                                            if (bcopybuf && bprefetch) { 
                                               bcopybufouter = true;  // local variable
                                               bcopybufbody = true;   // global variable
                                               bcopybuf = false;      // global variable
                                            }    
                                           } (doWithEndDo|doWithLabel) )                                     
  ;

doVarArgs:
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
  (NAME callArgumentList?)
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
expression:  
    unsignedArithmeticConstant
  | SCON  
  | varRef 
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
  | ^(T_PLUS expression expression)
  | ^(T_MINUS expression expression)
  | ^(T_STAR expression expression)
  | ^(T_DIV expression expression)
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

subscripts [boolean bdblBuff, boolean bdedblBuff]
@after{
     if ($bdblBuff){        
        FTree lastdim = null;
        //System.out.println("prefetch = "+bprefetch);
        if (bprefetch)
        {
          /* In the prefetch region.  Add ic1 as the last dimension. */
          lastdim = (FTree)adaptor.create(NAME,"ic1");
          lastdim.symbol = retrieveSymbol("ic1"); /* Note: Even if ic1 doesn't exist yet, it is created here */ 
          retval.tree.addChild(lastdim);            
        } else {
          /* Not in the prefetch region.  Add ic0 as the last dimension. */
          lastdim = (FTree)adaptor.create(NAME,"ic0");
          lastdim.symbol = retrieveSymbol("ic0");    /* Note: Even if ic0 doesn't exist yet, it is created here */
          retval.tree.addChild(lastdim);                    
        } 
     }
     if ($bdedblBuff){
      /* equivalence (ddtempold,ddtemp(....,1))
         equivalence (ddtempnu,ddtemp(.....,2))
         Eliminate the last dimension from ddtemp
       */
       int nchildren = retval.tree.getChildCount();
       if (nchildren > 1){
           retval.tree.deleteChild(nchildren-1);
       } else {
          /* Don't delete if the number of subscripts is just one.
           * I don't want to deal with converting an array symbol
           * into a non-array symbol.
           */
           FTree one = (FTree)adaptor.create(ICON,"1");
           retval.tree.replaceChildren(0,0,one);
       }  
     }
}:
  ^(SUBSCRIPT expression+);
 
varRef:      
      ^(ARRAYREF ID=NAME
                 {if ($ID.symbol.hasEquivalence() && dblBuffer.containsKey($ID.symbol.equivName) /* If it's equivalence is double-buffered */ 
                      && currentScope == dblBuffer.get($ID.symbol.equivName)      /*    in the right scope. */
                      && !dblBuffer.containsKey($ID.text))                                       /* It is not present in the double-buffer table. */
                  {
                      /* Double buffer the equivalence too */
                      dblBuffer.put($ID.text,currentScope);
                  }
                 }
                 s=subscripts[dblBuffer.containsKey($ID.text) && currentScope == dblBuffer.get($ID.text), 
                              dedblBuffer.containsKey($ID.text) && currentScope == dedblBuffer.get($ID.text)
                             ])
    |   ID=NAME
  ;  

/* 92 */
arrayName :  
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
@init {
  boolean btransbegin = false;
  boolean btransend = false;
  boolean bpfetchbegin = false;
  boolean bpfetchend = false;
  boolean bpipelinebegin = false;
} 
@after{
  if (btransbegin)  { // local variable
     TransformRegion transformregion = new TransformRegion();
     transformregion.begin = retval.tree; // TRANSFORMBEGIN directive
     transformRegions.add(transformregion);
  }
  if (btransend) { // local variable     
     int listsize = transformRegions.size();
     assert (listsize>0);  // Make sure the transform region list is not empty
     TransformRegion transformregion = (TransformRegion) transformRegions.get(listsize-1);
     transformregion.end = retval.tree; // TRANSFORMEND directive
  }   
  if (bpfetchbegin) { // local variable
     PfetchRegion pfetchregion = new PfetchRegion();
     pfetchregion.begin = retval.tree; // PREFETCHBEGIN directive
     pfetchRegions.add(pfetchregion);
  }
  if (bpfetchend) { // local variable
     int listsize = pfetchRegions.size();
     assert (listsize>0);  // Make sure the prefetch region list is not empty
     PfetchRegion pfetchregion = (PfetchRegion) pfetchRegions.get(listsize-1);
     pfetchregion.end = retval.tree; // PREFETCHEND directive
  }  
  if (bpipelinebegin) { // local variable
     pipelineDir = retval.tree;
  }
}:
//
//       ^(PPMDIRECTIVE INLINE)
//  |^(PPMDIRECTIVE INLINED)  
//  |^(PPMDIRECTIVE UNROLL ICON)
//   ^(PPMDIRECTIVE UPPERBOUND ic=ICON)
//  |^(PPMDIRECTIVE LOWERBOUND ic=ICON) 
   ^(PPMDIRECTIVE TRANSFORMBEGIN) {btransregion = true; btransbegin = true;}
  |^(PPMDIRECTIVE TRANSFORMEND) {btransregion = false; btransend = true;}  
  |^(PPMDIRECTIVE PREFETCHBEGIN) {bprefetch = true; bpfetchbegin = true;}
  |^(PPMDIRECTIVE PREFETCHEND) {bprefetch = false; bpfetchend = true;}
  |^(PPMDIRECTIVE COPYBUFFER i=ICON TIMES) {bcopybuf = true; copytimes = Integer.parseInt($i.text); }
  |^(PPMDIRECTIVE n=LONGIT) {bldoloop = true;}   
  |^(PPMDIRECTIVE PIPELINE) {bpipelinebegin = true;}
  |^(PD=PPMDIRECTIVE DOUBLEBUFFER ID=NAME)
    {
      /* Add an extra dimension at the end */
      ArraySymbol as = null;
      int nDim;
            
      currentScope = $PD.symbol.scope;      
      as = (ArraySymbol)currentScope.resolve($ID.text);
      if (as == null){
          //currentScope.printSymbols();
          System.out.println("Invalid arrayname "+$ID.text+" specified with the DOUBLEBUFFER directive!");
          System.exit(-1);
      }      
      dblBuffer.put($ID.text,currentScope);
      as.nDim++;
      nDim = as.nDim;            
      as.UBDim[nDim-1]="1";      
      as.UBDimTree[nDim-1] = (FTree)adaptor.create(ICON,"1");
      /* Calling setArraySize is a waste of time. I can perfectly handle it here. */
      as.LBDim[nDim-1] = "0";
      as.LBDimTree[nDim-1] = (FTree)adaptor.create(ICON,"0");
      as.sizeDim[nDim-1] = "2";
      as.itotalsize =  as.itotalsize * 2;
      as.totalsize = Integer.toString(as.itotalsize);
      /* Update the Symbol Table with the updated symbol */
      currentScope.define(as);
      /* The AST for the definition itself is modified only in pipeline4pbp.g.
         Things are too spread out in the pipeliner.  Clean it up!
       */  
      bdblBufinSubprogBody = true; 
    } 
  |^(PPMDIRECTIVE (n=~(PREFETCHBEGIN | PREFETCHEND | PIPELINE | TRANSFORMBEGIN | TRANSFORMEND | LONGIT | DOUBLEBUFFER | DEDOUBLEBUFFER | COPYBUFFER | EOS))+)  
  ;

ppmdecldirectives :
     ^(PD=PPMDIRECTIVE DEDOUBLEBUFFER ID=NAME)
    {
      /* Eliminate the extra dimension at the end */
      ArraySymbol as = null;
      int nDim;
            
      currentScope = $PD.symbol.scope;      
      as = (ArraySymbol)currentScope.resolve($ID.text);
      if (as == null){
          System.out.println("Invalid arrayname "+$ID.text+" specified with the DEDOUBLEBUFFER directive!");
          System.exit(-1);
      }      
      dedblBuffer.put($ID.text,currentScope);

      if (as.nDim > 1){
          as.nDim--;
      } else {
         /* Don't delete the last dimension if there is only one dimension.
          * I don't want to deal with converting an ArraySymbol into a 
          * non-array variable symbol.
          */
         as.UBDim[0]="1";      
         as.UBDimTree[0] = (FTree)adaptor.create(ICON,"1");

         as.LBDim[0] = "1";
         as.LBDimTree[0] = null;
         as.sizeDim[0] = "1";          
      }      
      /* Calling setArraySize is a waste of time. I can perfectly handle it here. */             
      as.itotalsize =  as.itotalsize / 2;
      as.totalsize = Integer.toString(as.itotalsize);
      /* Update the Symbol Table with the updated symbol */
      currentScope.define(as);
      bdedblBufinSubprogBody = true; 
    }
  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

  