//===------------------ pass4_ldLoop2if.g - remove loops ------------------===//
//
//  Replace the inner loop constructs with guard IF statements. The common outer 
//  pipelined loop covers this iteration space.
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


//
// Pass 4: 	Longitudinal loop to IF
//   Here, we merge the pipelined THENBLOCKs with a common IF conditional
//   expression inside the transform region.  The pipelined IF statements
//   are identified by the unique string 'ibegin' we generated in the 
//   conditional.       
//
//   Some accompanying transformations:
//   - Fixbounds for equivalences
//   - Replace icget with icube  
//   - Replace icube with icube-1
//   - Track the bounds of (offsets used with the) pipelined temporaries.
//     This is the liveness analysis. 
//
//   Restrictions:  
//   - The bounds have to be integers whose values are known statically
//     at compile time.
//   - For an outer index say "i" in the last dimension, the subscript
//     expression for the last dimension ought to be of the form ID, 
//     ^( T_MINUS ID ICON ), or ^( T_PLUS ID ICON ), where ID is i.
//   Assumption: 
//     The outer index i does not occur at more than one subscript.
//     I do not see a reason why it could occur at more than one subscript
//     in an array reference, but if it does then the current solution 
//     will not work. 
// 
// 01/02/2011:
//   Pattern matching ANTLRs are very elegant, but seems to be very error-prone.
//   Adding statements (nodes/children) seems to work, but deletion does not.
//   TreeVisitor visit method gets all confused. I performed a whole bunch of
//   experiments from deleting statements in the reverse order to getting child
//   index at the time of deletion.  The conclusion is deletion does not work
//   with pattern matching grammars.  It forces me to use the more clumsy tree
//   grammars, but, as demonstrated below, they atleast work.
//
//   Here, we merge the pipelined THENBLOCKs with a common IF conditional
//   expression inside the transform region.  The pipelined IF statements are
//   identified by the unique string 'ibegin' we generated in the conditional.
//      -JJ
//        
// 07/15/2011: --> Changed.  Don't delete it yet.  Alter it when you are sure.
//  Pipelined temporaries which also occur in the prefetch region are special.  
//  They are nothing but the temporaries we unpack the grid briquettes and other
//  input arrays like xl into.  We compute their live range in the TRANSFORM
//  region, but it is not sufficient for the plane-by-plane implementation.  
//  The live range of the TRANSFORM region represents the range for just one
//  iteration of the iplaneFE loop.  However, we do multiple iplaneFE iterations
//  (nbdy or nsugar) for every grid briquette we fetch.  We have to provision 
//  space for these extra iterations.  We allocate (nsugar + <live range in the
//  TRANSFORM region>) planes for these temporaries. It is slightly wasteful 
//  but simplifies the logic.
//  - JJ


tree grammar pass4_ldLoop2if;
 
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
   boolean bldoloop = false; // True if a longitudinal (target) doloop statement for pipelining
   boolean bldobody = false; // True if a longitudinal (target) doloop body
   boolean btransregion = false; // True in a TRANSFORM region
   boolean btransend = false; // True if there is a transform region in the SUBRPOGRAMBLK    
   boolean bmovfromrediter = false; // True in the region moved from the redundant iterations
      
   String execUnitName = null; // Name of the current executable unit   
   String idn = null; // Identifier name for the current ldoLoop   
   Symbol idsymbol = null; // Symbol for the current ldoLoop identifier      
   Scope currentScope = null; /* The newly created labels need to inserted into the label table too.
                                 Labels need scope. We assign the ldoLoop identifier's scope to the
                                 label encountered. Any other NAME would have worked, but I am
                                 using this identifier */                                  
   SymbolTable symtab = null;                                  
   LabelTable lbltab = null;
   
   
   int upperbound = 0;
   int lowerbound = 0;
   int currLB = 0; /* Lower bound of the current outer do loop */
   int currUB = 0; /* Upper bound of the current outer do loop */   
   int lagiter  = 0; /* Number of planes by which the current 
                    * computational phase lags the plane being unpacked.
                    * Since the loops are right-aligned for merging,
                    * it is equal to 'upperbound' - the upperbound for
                    * the current LONGIT/REPACK loop.
                    *   Remember that we convert the REPACK loops into 
                    * LONGIT loops.  Therefore, we need to track only
                    * the LONGIT loops. 
                    */                 
   int mynx = 0; /* Size of a briquette. */
   int cubelag = 0;  /* The difference between the cube that is unpacked and
                      * the cube corresponding to the planes entering the 
                      * current computational phase.  The phase is approximated 
                      * to the last seen LONGIT loop.  In particular, any print 
                      * statements for icube in the scalar statements between 
                      * the loop nests print the cube number computed with 
                      * respect to the last seen LONGIT/REPACK ie. icube-cubelag. 
                      */
       
   public pass4_ldLoop2if(TreeNodeStream input, SymbolTable symtab, LabelTable lbltab){
      this(input);
      this.symtab = symtab;
      this.lbltab = lbltab;      
      //this.lblfregion = lbltab.getSize()+1; 
   }
   
   IntegerEvaluator IndexEval = new IntegerEvaluator();  
   public class iterBounds {
      public int leftbound;
      public int rightbound;
      public int adjustOffset;
      public iterBounds(int lb, int rb, int off){
          leftbound = lb+off;
          rightbound = rb+off;
          adjustOffset = off;
      }
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
   
   private int ivalueOf(String parameter){
      Symbol vs;      
      
      vs = currentScope.resolve(parameter);        
      if(vs == null){
         System.out.println("pass4_ldLoop2if.g : "+parameter+" is undefined");
         System.exit(-1);
      }     
                 
      return ((ParamSymbol)vs).ivalue;
   }
   
   private boolean isArg(String id){
   /* Subroutine Scope encloses the subroutinebody Scope.  Subroutine contains
      only the argument symbols.  If the resolve can't find the symbol in the 
      subroutinebody scope, it looks up one level at subroutine.  By this 
      process, we can always resolve a symbol that appears anywhere in the
      subroutine.
    */          
      Scope subroutine;  
      subroutine = symtab.globals.enclosedFunctions.get(execUnitName);          
      if(subroutine != null && subroutine.resolve(id) != null)  
         return true;
      else
         return false;                        
   }            
   
} 

program:
  ^(CODEROOT (ppmdirectives | directives | executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit:
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
programStatement 
@after {
   execUnitName = $n.text;
}:
  ^(T_PROGRAM n=NAME)
  ;

seos : NEWLINE ;

/* 9, 11, 13 */
/* 10 */
functionStatement :
  ^(T_FUNCTION (type)? functionName[true] namelist?)
  ;

blockdataStatement 
@after {
   execUnitName = $n.text;
} :
  ^(T_BLOCK n=NAME)
  ;

/* 12 */
subroutineStatement:
  ^(T_SUBROUTINE n=NAME namelist?) {execUnitName = $n.text;}
  ;
  
namelist :
  ^(SUBARG identifier+)
  ;

subprogramBody
@init{
  btransend = false;
}
@after{
  if (btransend) {
      btransend = false;      
      /* pass1_delRedIter.g inserts the transform directives once for every PIPELINE directive, although
         I expect to not see more than one PIPELINE directive per subroutine, or even a module.
       */
      FTree dimTree = null, arrTree = null, arrName = null;
      FTree adexts = null, adext = null, stmt = null, stmtlist = null;
      FTree doTree = null, doArgsTree = null, doBodyTree = null, id = null;
      FTree assignTree = null, plusTree = null, minusTree = null, subscriptTree = null;
      FTree lhsTree = null, rhsTree = null;  /* Left-hand side and right-hand side of an assignment statement*/
      FTree decBlk = null, execBlk = null, piplTree = null, piplBody = null;     
      int numChildren = 0, i = 0, index = 0;      
                
      decBlk = (FTree)retval.tree.getChild(0);  /* Declaration block */
      execBlk = (FTree)retval.tree.getChild(1); /* Execution block */      
                
       /* Find the pipeline directive */       
       numChildren = execBlk.getChildCount();
       //System.out.println("numChildren = "+numChildren);
       for (i=0;i<numChildren;i++){            
            stmt = (FTree)execBlk.getChild(i);
            //System.out.println(stmt.toStringTree());
            //System.out.println("type, childtype = "+stmt.getType()+",  "+stmt.getChild(0).getType()+"\n");
            if (stmt.getType() == PPMDIRECTIVE && stmt.getChild(0).getType() == PIPELINE){
                index = i;
         //       System.out.println("index "+index);
                break;
            }
       }
       if (i>=numChildren){
           System.out.println("Internal error (pass4_ldoLoop2if.g): Pipeline directive not found");                     
           System.exit(-1); 
       }          
     
       stmtlist = (FTree)adaptor.nil();
      /* Initialize the pipeline index variables 
            ic0 = 0
            ic1 = 1
       */
       id = (FTree)adaptor.create(NAME,"ic0");
       id.symbol = retrieveSymbol("ic0");
       assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
       adaptor.addChild(assignTree,id);
       adaptor.addChild(assignTree,(FTree)adaptor.create(ICON,"0"));
       /* Add the assignment statement to the list */
       adaptor.addChild(stmtlist,assignTree);          
                    
       id = (FTree)adaptor.create(NAME,"ic1");
       id.symbol = retrieveSymbol("ic1");
       assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
       adaptor.addChild(assignTree,id);
       adaptor.addChild(assignTree,(FTree)adaptor.create(ICON,"1"));
       /* Add the assignment statement to the list */                    
       adaptor.addChild(stmtlist,assignTree);
          
       /* Add the pipeline directive to the list */
       adaptor.addChild(stmtlist,adaptor.dupTree(stmt)); /* Pipeline directive */
       /* Replace the pipeline directive in the execution block with the constructed list */
       //System.out.println("stmtlist "+stmtlist.toStringTree());
       adaptor.replaceChildren(execBlk,index,index,stmtlist);          
          
       /* Revolve the index variables at the beginning of the pipeline       
             ic0 = 1 - ic0
             ic1 = 1 - ic1
        */
       /* Find the pipelined do loop body, and insert the pipeline index variable 
          swizzle statements at the beginning of the do body.
        */
       stmtlist = (FTree)adaptor.nil(); 
       /* Find the PIPELINE directive again.  We modified the tree */
       numChildren = execBlk.getChildCount();
       for (i=index;i<numChildren;i++){
            stmt = (FTree)execBlk.getChild(i);
            if (stmt.getType() == PPMDIRECTIVE && stmt.getChild(0).getType() == PIPELINE){
                index = i;
                //System.out.println("index "+index);
                break;
            } 
       }
       //System.out.println("pipeline directive "+stmt.toStringTree());
       index = index+1;  // pipelined do loop
       piplTree = (FTree)execBlk.getChild(index);
       //System.out.println("pipl Loop "+piplTree.toStringTree());
       numChildren = piplTree.getChildCount();
       piplBody = (FTree)piplTree.getChild(numChildren-1);
       //System.out.println("piplBody "+piplBody.toString());
       
       /* 1-ic0 */          
       id = (FTree)adaptor.create(NAME,"ic0");
       id.symbol = retrieveSymbol("ic0");
       minusTree = (FTree)adaptor.create(T_MINUS,"-");
       adaptor.addChild(minusTree,(FTree)adaptor.create(ICON,"1"));
       adaptor.addChild(minusTree,id);
       
       /* ic0 = 1-ic0 */
       id = (FTree)adaptor.create(NAME,"ic0");
       id.symbol = retrieveSymbol("ic0");       
       assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
       adaptor.addChild(assignTree,id);
       adaptor.addChild(assignTree,minusTree);
       /* Add the assignment statement to the list */       
       adaptor.addChild(stmtlist,assignTree);          
                    
       /* 1-ic1 */          
       id = (FTree)adaptor.create(NAME,"ic1");
       id.symbol = retrieveSymbol("ic1");
       minusTree = (FTree)adaptor.create(T_MINUS,"-");
       adaptor.addChild(minusTree,(FTree)adaptor.create(ICON,"1"));
       adaptor.addChild(minusTree,id);
       
       /* ic1 = 1-ic1 */
       id = (FTree)adaptor.create(NAME,"ic1");
       id.symbol = retrieveSymbol("ic1");       
       assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
       adaptor.addChild(assignTree,id);
       adaptor.addChild(assignTree,minusTree);
       /* Add the assignment statement to the list */
       adaptor.addChild(stmtlist,assignTree);

       /* Add icget = icube + 1 */
       /* icube + 1 */          
       id = (FTree)adaptor.create(NAME,"icube");
       id.symbol = currentScope.resolve("icube");
       plusTree = (FTree)adaptor.create(T_PLUS,"+");
       adaptor.addChild(plusTree,id);
       adaptor.addChild(plusTree,(FTree)adaptor.create(ICON,"1"));
       
       /* icget = icube+1 */
       id = (FTree)adaptor.create(NAME,"icget");
       id.symbol = currentScope.resolve("icget");       
       assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
       adaptor.addChild(assignTree,id);
       adaptor.addChild(assignTree,plusTree);
       /* Add the assignment statement to the list */
       adaptor.addChild(stmtlist,assignTree);       
       
       adaptor.addChild(stmtlist,(FTree)adaptor.dupTree(piplBody.getChild(0)));
       //System.out.println("pipl stmtlist "+stmtlist.toStringTree());
       adaptor.replaceChildren(piplBody,0,0,stmtlist);
  }     
}:
  ^(SUBPROGRAMBLOCK declarationBlock executionBlock)
  ;

declarationBlock:
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
  |ldoLoop
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

ldoLoop 
 @after {
/* Some ldoLoops are over the entire extent, 1-nbdy to nx+nbdy. Eg: ldoLoops in the non-prefetch
   region of the redundant iterations.  For such ldoloops over the entire extent, we simply
   eliminate the do statement, and return the wholeStatements inside of the doBody.
 */
  FTree doblock;

  doblock = (FTree)retval.tree.getChild(1);
  if(doblock.getType() != DOBLOCK) 
  /* The DO loop has lblRef.  doblock must be the second child of the DDOLOOP. */
     doblock = (FTree)retval.tree.getChild(2);  
   
  if(upperbound == currUB && lowerbound == currLB) {
     FTree stmts = (FTree)adaptor.nil();
     int i, itmp;
     
     itmp = doblock.getChildCount();
     for(i=0;i<itmp;i++){
      /* Add all the DOBLOCK children to the stmts list  */
         stmts.addChild((FTree)doblock.getChild(i));
     }         
     for(i=itmp-1;i>-1;i--){  
      /* Delete the DOBLOCK children now.  I didn't want to confuse ANTLR tree
         walker by lookup and deleting children at the same time.  The semantics is
         much more cleaner now.
       */    
         adaptor.deleteChild(doblock,i);
     }
     retval.tree = stmts;
  } else {
  /* Normal */
     FTree ifTree, iplaneTree, thenTree;
     boolean isVectorLoop = false;
     int i, itmp = 0;
     
     Symbol vs = null;         
        
  /* Replace the DO loop with  'if (iplaneFE .ge. 1-nbdy+<alignment offset>) THENBLOCK <wholeStatements>' */
     ifTree = (FTree)adaptor.create(T_IF,"if");

     iplaneTree = (FTree)adaptor.create(T_GE,".ge.");
     iplaneTree.addChild((FTree)adaptor.create(NAME,"iplaneFE"));
          
     ((FTree)iplaneTree.getChild(0)).symbol = currentScope.resolve("iplaneFE");
                      
     vs = currentScope.resolve("nbdy");
     if(vs == null){        
        System.out.println("nbdy is undefined in "+execUnitName);        
        System.exit(-1);
     }
     
     iplaneTree.addChild((FTree)adaptor.create(ICON,Integer.toString(1-((ParamSymbol)vs).ivalue+(upperbound-$va.ub)+($va.lb-lowerbound))));         
                  
     ifTree.addChild(iplaneTree);
         
  /* Construct the THENBLOCK in the following convoluted fashion. */        
     itmp = doblock.getChildCount();
     thenTree = ((FTree)adaptor.create(THENBLOCK,"then"));
     for(i=0;i<itmp;i++)
      /* Add all the DOBLOCK children to the doblockstmts  */
         thenTree.addChild((FTree)doblock.getChild(i));     
     for(i=itmp-1;i>-1;i--){  
      /* Delete the DOBLOCK children now.  I didn't want to confuse ANTLR tree
         walker by lookup and deleting children at the same time.  The semantics is
         much more cleaner now.
       */    
         adaptor.deleteChild(doblock,i);
     }
     ifTree.addChild(thenTree);     
     retval.tree = ifTree;
   }
  }  
 : ^(DDOLOOP {bldoloop = true; /*System.out.println("pipeliner: pass4_ldoLoop2if.g: I see DDOLOOP");*/} l=lblRef? va=doVarArgs 
                           { 
                             bldoloop = false; 
                             bldobody = true;
                             lagiter = upperbound - currUB;
                             cubelag = lagiter/mynx;                            
                           }
             d=doBody {bldobody = false;})
 ;     


/* 40 */
doStatement : 
  ^(T_DO (doWithEndDo|doWithLabel) )
  ;

doVarArgs returns [int lb, int ub, int newub]
@after
{
  if ( bldoloop ){
     idn = $id.toString();
     idsymbol = $id.symbol;
     currentScope = idsymbol.scope;
//     System.out.println("currentScope set for "+idn);
    
//  System.out.println($id.toString());
//  System.out.println($id.tree.getChild(1).getType()); ??
//  System.out.println($e1.tree);
//  System.out.println($e2.tree);
     String lbs = IndexEval.printInfix($e1.tree);
     String rbs = IndexEval.printInfix($e2.tree);
     lbs = ((BaseScope)currentScope).replaceParameter(lbs);
     rbs = ((BaseScope)currentScope).replaceParameter(rbs);
     //System.out.println(lbs+"   "+rbs);
  
  //System.out.println($e3.tree);
     if (!IndexEval.isIntConvertable(lbs) || !IndexEval.isIntConvertable(rbs)){
         System.out.println("Align error: A Longitudinal Loop bound can't be converted to an integer statically!");
         System.exit(-1); 
     } 
     int lb = IndexEval.Evaluate(lbs);
     int rb = IndexEval.Evaluate(rbs);  
     $lb = lb;
     $ub = rb; /* Right bound is the upper bound if e3 is positive or doesn't exist */
     if(bmovfromrediter){
       currLB = lowerbound;
       currUB = upperbound;
     } else {
       currLB = $lb;
       currUB = $ub;     
     }
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
  ^(FUNCREF functionName[false] functionArgumentList)
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
expression returns [boolean bidnexpr]
@init{
 /* The bidnexpr value is propagated to the subscript level through the expression rule.  
    I don't expect to see arithmetic other than '+'/'-'/'*'/'/' to be done with the 
    outer index subscript.
  */
  $bidnexpr = false;
}:  
    unsignedArithmeticConstant
  | SCON  
  | v=varRef {$bidnexpr = $v.bidnexpr;} 
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
  | ^(T_PLUS e1=expression e2=expression) {$bidnexpr = $e1.bidnexpr || $e2.bidnexpr;}
  | ^(T_MINUS e1=expression e2=expression) {$bidnexpr = $e1.bidnexpr || $e2.bidnexpr;}
  | ^(T_STAR e1=expression e2=expression) {$bidnexpr = $e1.bidnexpr || $e2.bidnexpr;}
  | ^(T_DIV e1=expression e2=expression) {$bidnexpr = $e1.bidnexpr || $e2.bidnexpr;}
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

subscripts[boolean bisArg]  returns [boolean bidnexpr, int offset] 
@init{
  int childIndex = -1;
  int counter = -1;
}
@after{
  /* Assumption: The index i doesn't occur at more than one subscript.  I don't see 
     a reason why it could occur at more than one subscript in an array reference, 
     but if it does then the current solution won't work. 
   */
     FTree childTree;
     
     $offset = -1;
     if (!bisArg && childIndex == counter){ 
      /* i occurs in the last dimension.  
         Replace i with i - (upperbound-currUB) without exception. Buffers such as
         the copy buffers which depend on the index of planes in a briquette are 
         double buffered, and so i won't occur in their dimension. It is safe to 
         pipeline any i in the last dimension. */
         Symbol vs = null;
         childTree = (FTree)retval.tree.getChild(childIndex);
         FTree offTree = null, idTree = null;
            
         $offset = upperbound-currUB; 

         if ($offset >= 0){
          /* The subscript expression for the last dimension ought to be of the form
             ID, ^( T_MINUS ID ICON ), or ^( T_PLUS ID ICON ), where ID is iincube. 
           */
             String indexname = "iincube";
             String lastdimstr = childTree.toString();

             boolean binvalidexpr = true;
             int ioff = 0;
             if (lastdimstr.contentEquals("-") || lastdimstr.contentEquals("+")) {
                 String child0 = childTree.getChild(0).toString();
                 String child1 = childTree.getChild(1).toString();
                 if(child0.contentEquals(indexname)){
                    ioff = Integer.parseInt(child1);                      
                    binvalidexpr = false;
                 } else if(child1.contentEquals(indexname)){
                      ioff = Integer.parseInt(child0);
                      binvalidexpr = false;
                   }
                     
                 if(lastdimstr.contentEquals("-"))
                    $offset = $offset + ioff; 
                 else 
                    $offset = $offset - ioff; /* The offset will still be greater than zero. */                     
             }
             if(lastdimstr.contentEquals(indexname)) {
                binvalidexpr = false;                  
             }
             if(binvalidexpr){
                System.out.println(retval.tree.toStringTree());
                System.out.println("Error: Invalid expression type in the last dimension of a pipeline temporary.  We see the operator or variable "+lastdimstr+
                                   ".  We allow only the outer (Longitudinal) loop index, the + or - operators, and an integer constant, in the last dimension of a "+ 
                                   "pipeline temporary.  (Internal:  The reporting module is pass4_ldLoop2if.g)");
                System.exit(-1);                    
             }                       
             offTree =  (FTree)adaptor.create(T_PLUS,"-");
             idTree = (FTree)adaptor.create(NAME,"iplaneFE");
             idTree.symbol = currentScope.resolve("iplaneFE");
                   
             offTree.addChild(idTree);
             offTree.addChild((FTree)adaptor.create(ICON,Integer.toString($offset)));
             adaptor.replaceChildren(retval.tree,childIndex,childIndex,offTree);           
         } else {
             System.out.println("offset is negative "+$offset);
             System.out.println("upperbound = "+upperbound+";  currUB = "+currUB);
             System.out.println(retval.tree.toStringTree());
             System.exit(-1);
         }
         $bidnexpr = true;
     }           
}:
/* counter keeps count of the expression being processed.  If bidnexpr is true then this is the subscript expression
   we have to modify.  I expect to see the outer index appear in only one subscript.  If it occurs in more than one
   subscript, I have to find another solution. 
 */  
  ^(SUBSCRIPT {$bidnexpr = false;} (e=expression {counter++; /* System.out.println("expr = "+$e.tree.toStringTree()+"  bidnexpr = "+$e.bidnexpr); */
                                                  if ($e.bidnexpr) { childIndex = counter; }
                                                 })+);
 
varRef returns [boolean bidnexpr]
/* Identify the outer index 'i' in subscripts.  Adjusting 'i' happens at the 'subscript' rule level.  
   Return a boolean 'bidnexpr' higher up to aid the process.  
   An old comment still valid: For index variables, I am guaranteed to not see any subscripts.
 */
@after {
    if (idn != null && idn.equals($ID.text) && bldobody){
    /* Replace i with iincube. If i occurs in the last dimension of a
       subscript then subscript will replace it with iplaneFE.
     */
       retval.tree = (FTree)adaptor.create(NAME,"iincube");
       retval.tree.symbol = currentScope.resolve("iincube");
       
       $bidnexpr = true; 
    }   
    else
       $bidnexpr = false;
     //System.out.println($ID.text+"  "+$ID.symbol);
     
 /* Replace 'icube' with  'icube - cubelag' */              
    if (btransregion && $ID.text.contentEquals("icube")){        
       FTree id, minusTree;  
       int nghostcubes;
               
       id = (FTree)adaptor.create(NAME,"icube");
       id.symbol = currentScope.resolve("icube");
       minusTree = (FTree)adaptor.create(T_MINUS,"-");       
       adaptor.addChild(minusTree,id);
       adaptor.addChild(minusTree,(FTree)adaptor.create(ICON,Integer.toString(cubelag)));
       //nghostcubes = ivalueOf("nghostcubes");
       //adaptor.addChild(minusTree,(FTree)adaptor.create(ICON,Integer.toString(nghostcubes)));
       
       retval.tree = minusTree;
    }
       
 /* Replace 'icget' with  'icube' (i.e. icget - 1) */              
    if (btransregion && $ID.text.contentEquals("icget")){        
       FTree id, minusTree;  
       int nghostcubes;
               
       id = (FTree)adaptor.create(NAME,"icube");
       id.symbol = currentScope.resolve("icube");
       
       retval.tree = id;
    }   
          
}:      
      ^(ARRAYREF ID=NAME s=subscripts[isArg($ID.text)]
          {            
            if($ID.symbol.isArgument() == false) {
               currentScope = $ID.symbol.scope;
            }
          })
    |   ID=NAME     {   if($ID.symbol.isArgument() == false) {
                           currentScope = $ID.symbol.scope;
                        } 
                    }
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
functionName[boolean bfuncStmt] :
    implicitfunctionName
  | n=NAME { if(bfuncStmt) execUnitName = $n.text; }
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
  
ppmdirectives:
//
//       ^(PPMDIRECTIVE INLINE)
//  |^(PPMDIRECTIVE INLINED)  
//  |^(PPMDIRECTIVE UNROLL ICON)
   ^(PPMDIRECTIVE UPPERBOUND ic=ICON) 
              {
                upperbound = Integer.parseInt($ic.toString());  
                System.out.println("upperbound = "+upperbound);
                mynx = ivalueOf("mynx");
               }
  |^(PPMDIRECTIVE LOWERBOUND ic=ICON) 
              { 
                lowerbound = Integer.parseInt($ic.toString());  
                System.out.println("lowerbound = "+lowerbound);
               }                 
  |^(PPMDIRECTIVE TRANSFORMBEGIN) {btransregion = true;}  
  |^(PPMDIRECTIVE TRANSFORMEND) {btransregion = false; btransend = true;}
  |^(PPMDIRECTIVE MOVEDFROMREDITERBEGIN) {bmovfromrediter = true;}
  |^(PPMDIRECTIVE MOVEDFROMREDITEREND) {bmovfromrediter = false;}    
  |^(PPMDIRECTIVE PIPELINE)
    {
       Symbol vs = null;

       vs = currentScope.resolve("iincube");
       if(vs!= null){
          System.out.println("pass4_ldLoop2if.g: iincube is a reserved word.  It should not be used as a variable");
          System.exit(-1);        
       }
       if(vs==null){
          vs = new VariableSymbol("iincube",Symbol.getDataType("iincube"),false);
          currentScope.define(vs);
       }

       vs = currentScope.resolve("iplaneFE");
       if(vs!= null){
          System.out.println("pass4_ldLoop2if.g: iplaneFE is a reserved word.  It should not be used as a variable");
          System.exit(-1);        
       }
       if(vs==null){
          vs = new VariableSymbol("iplaneFE",Symbol.getDataType("iplaneFE"),false);
          currentScope.define(vs);
       }
     }
  |^(PPMDIRECTIVE (n=~(UPPERBOUND | LOWERBOUND | TRANSFORMBEGIN | TRANSFORMEND | MOVEDFROMREDITERBEGIN | MOVEDFROMREDITEREND | PIPELINE | EOS))+)
  ;
   
ppmdecldirectives :
   ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

  
