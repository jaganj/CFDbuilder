//===--------------- reduceMem2.g - reduce memory pass 2 ------------------===//
//
//  This code reduces the size of already efficient pipelined variables to
//  their minimum
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
//  Grammar derived from the ANTLR 2 grammar for Fortran 77 by Olivier Dragon      
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

tree grammar reduceMem2;

/* 1/18/2010: 
   Re-size the declarations of temporaries modified by the earlier pipelining stages.  
   This includes the pipelined temporaries and the double buffered arrays.   

   7/12/2011:
   * Find the TRANSFORMBEGIN directive, and insert the pipeline index variable 
     swizzle statements after it.
   * Initialize the pipeline index variables at the beginning of their execution block.
   * Reindex the pipelined temporaries in their last dimension i.e. convert iplaneFE - <something> 
     to an index variable (eg: i86m5).   
   -JJ
 */    
 
options {
    language = Java;
    backtrack=true;          // allow backtracking if necessary
    output=AST;              // build ASTs from input AST
    tokenVocab=tokenFile;     
    ASTLabelType=FTree; // we're using FTree nodes    
}

@header {
package reduceMem;
import lexer.*;
import symbol.*;
import translator.*;
import pipeliner.*;
import java.util.Set;    
import java.util.Iterator;
import java.util.Map;
import java.util.Hashtable;
import java.util.HashMap;
import java.util.List;
import IntegerEvaluator.*;
}

@members{    
    //SymbolTable symtab;
    PipelinedTmps pipelinedTmps;    
    Scope currentScope;    
    boolean bFI = false;    /* True when called for a manually written FI */
    
    String dbgname = null;
    FTree begintransformregion = null; /* Beginning of the current transform region */
    FTree beginremoveregion = null; /* Beginning of the current remove region */
    FTree endremoveregion = null;   /* End of the current remove region */                                    
    
    public reduceMem2(TreeNodeStream input, PipelinedTmps pipelinedTmps, boolean bFI) {
        this(input);
        this.pipelinedTmps = pipelinedTmps;
        this.bFI = bFI;
        //currentScope = symtab.globals;
    }
    
    Hashtable reindextab = new Hashtable(); /* Table for re-index variables in the current fuse region / subroutine. 
                                           We assume that there is only one fuse region per subroutine. */
                                           
    private void insertSwizzleStmts(){
    /* Initialize the re-index temporaries for this subroutine here */
       int ub, lb, i, ibegin = -1, iend = -1, index = -1;
       Bound bound;
       String reindexstr, reindexstrbase;
       String reindexstrSrc, reindexstrDest;        
       FTree reindexNode;
       FTree reindexNodeSrc, reindexNodeDest;        
       FTree assignTree;
       FTree newTree = null, parent = null;
       VariableSymbol vs = null;
       Iterator iterator;
        
    /* Insert the pipeline index variable swizzle statements after the TRANSFORMBEGIN directive.  */     
        if (!reindextab.isEmpty()){
            if(null == begintransformregion){
               System.out.println("Internal error (reduceMem2.g): I must not have seen a TRANSFORMBEGIN directive when the reindextab is not empty.");
               System.exit(-1);
            }
          if (bFI){
            ibegin = beginremoveregion.getChildIndex();
            iend   = endremoveregion.getChildIndex();
            parent = (FTree)beginremoveregion.getParent();
          } else {
            index = begintransformregion.getChildIndex();
            parent = (FTree)begintransformregion.getParent();
          }
            
         /* Initialize the re-index temporaries for this subroutine here */
            newTree = (FTree)adaptor.nil(); /* Nil tree just to hold the list */
            iterator = reindextab.entrySet().iterator();
            
            while(iterator.hasNext()){      
                Map.Entry tmp = (Map.Entry)iterator.next();
                bound = (Bound)tmp.getKey();     
                reindexstrbase = (String)tmp.getValue();       
                ub = bound.ub;
                lb = bound.lb;            
                                
                /* Prologue */
                assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
                reindexstrDest = reindexstrbase+"temp";                
                reindexNodeDest = (FTree)adaptor.create(NAME,reindexstrDest);
                vs = (VariableSymbol)currentScope.resolve(reindexstrDest);                 
                if(vs == null){
                    vs = new VariableSymbol(reindexstrDest,Symbol.getDataType(reindexstrDest));
                    reindexNodeDest.symbol = vs;
                    currentScope.define(vs);  
                }else{
                  if (bFI == false){
                     System.out.println("Error (reduceMem2.g): "+reindexstrDest+" is a reserved temporary");
                     System.exit(-1);
                  }
                }                    
                adaptor.addChild(assignTree,reindexNodeDest);                
                
                reindexstrSrc = reindexstrbase+Integer.toString(-lb);
                reindexNodeSrc = (FTree)adaptor.create(NAME,reindexstrSrc);                               
                /* Symbol must already exist */
                vs = (VariableSymbol)currentScope.resolve(reindexstrSrc);   
                if(vs==null){
                   vs = new VariableSymbol(reindexstrSrc,Symbol.getDataType(reindexstrSrc));                   
                   currentScope.define(vs);  
                }                
                reindexNodeSrc.symbol = (Symbol)vs;
                adaptor.addChild(assignTree,reindexNodeSrc);
                newTree.addChild(assignTree);                  
                             
                for(i=lb+1;i<=ub;i++){
                    reindexstrDest = reindexstrSrc;
                    assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
                    reindexNodeDest = (FTree)adaptor.create(NAME,reindexstrDest);
                 /* Symbol must already exist */   
                    vs = (VariableSymbol)currentScope.resolve(reindexstrDest);   
                    if(vs==null){
                       vs = new VariableSymbol(reindexstrDest,Symbol.getDataType(reindexstrDest));                   
                       currentScope.define(vs);  
                    }                
                    reindexNodeDest.symbol = (Symbol)vs;
                    
                    adaptor.addChild(assignTree,reindexNodeDest);        
                    reindexstrSrc = reindexstrbase+Integer.toString(-i);
                    reindexNodeSrc = (FTree)adaptor.create(NAME,reindexstrSrc);
                    vs = (VariableSymbol)currentScope.resolve(reindexstrSrc);   
                    if(vs==null){
                       vs = new VariableSymbol(reindexstrSrc,Symbol.getDataType(reindexstrSrc));                    
                       currentScope.define(vs);  
                    }                  
                    reindexNodeSrc.symbol = (Symbol)vs;
                    adaptor.addChild(assignTree,reindexNodeSrc);
                    newTree.addChild(assignTree);  
                }
                /* Epilogue */
                reindexstrDest = reindexstrSrc;
                assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
                reindexNodeDest = (FTree)adaptor.create(NAME,reindexstrDest);
                /* Symbol must already exist */   
                vs = (VariableSymbol)currentScope.resolve(reindexstrDest);
                if(vs==null){
                   vs = new VariableSymbol(reindexstrDest,Symbol.getDataType(reindexstrDest));                   
                   currentScope.define(vs);  
                }                  
                reindexNodeDest.symbol = (Symbol)vs;                
                adaptor.addChild(assignTree,reindexNodeDest);
                        
                reindexstrSrc = reindexstrbase+"temp";
                reindexNodeSrc = (FTree)adaptor.create(NAME,reindexstrSrc);                
                vs = (VariableSymbol)currentScope.resolve(reindexstrSrc);
                if(vs==null){
                   vs = new VariableSymbol(reindexstrSrc,Symbol.getDataType(reindexstrSrc));                   
                   currentScope.define(vs);  
                }                 
                reindexNodeSrc.symbol = (Symbol)vs;
                adaptor.addChild(assignTree,reindexNodeSrc);
                newTree.addChild(assignTree);                
             }

          if (bFI){
          /* ibegin points to the CANREMOVEBEGIN directive.  Insert the newTree immediately after it. */  
             //newTree.addChild((FTree)adaptor.dupTree(parent.getChild(index+1)));
             adaptor.replaceChildren(parent,ibegin,iend,newTree);
          } else {          
          /* index points to the TRANSFORMBEGIN directive.  Insert the newTree immediately after it. */  
             newTree.addChild((FTree)adaptor.dupTree(parent.getChild(index+1)));
             adaptor.replaceChildren(parent,index+1,index+1,newTree);
          }
        }
    }

/*    private void removeStmts(){
    / * Remove the statements inside REMOVE region * /
       int i, ibegin, iend;
       FTree newTree = null, parent = null;        
         
       if(null != beginremoveregion){       
          ibegin = beginremoveregion.getChildIndex();
          iend   = endremoveregion.getChildIndex();
          parent = (FTree)beginremoveregion.getParent();
           
          for(i=iend; i>=ibegin; i--)
             adaptor.deleteChild(parent,i);
       }
    }
*/        
    private void initIndices(FTree execBlk){
      /* Initialize the re-index temporaries for this subroutine here */
        int ub, lb, i, index;
        Bound bound;
        String reindexstr, reindexstrbase;
        FTree reindexNode;        
        FTree assignTree;
        FTree newTree = null, parent = null;
        VariableSymbol vs = null;
        Iterator iterator;
    
      /* Get the children list for the execution block.  BaseTree doesn't support
        inserting child at a given index.  Although the protected list children can
        be accessed from a subclass (FTree),  PHL and JJ decided not to extend the
        FTree walker because we might not know what is an extension and what was a 
        base feature.  Here, we will copy the children list, manipulate it, and add
        it back to the tree.
      */                
        iterator = reindextab.entrySet().iterator();
        if (!reindextab.isEmpty()){
          newTree = (FTree)adaptor.nil();           
          while (iterator.hasNext()){      
             Map.Entry tmp = (Map.Entry)iterator.next();
             bound = (Bound)tmp.getKey();     
             reindexstrbase = (String)tmp.getValue();       
             ub = bound.ub;
             lb = bound.lb;
             
          /* The bounds are always negative, but ub > lb. 
             Convert to them to positives to generate the re-index variable names. 
           */   
             for (i=ub;i>=lb;i--){
                assignTree = (FTree)adaptor.create(T_ASSIGN,"=");
                reindexstr = reindexstrbase+Integer.toString(-i);                             
                reindexNode = (FTree)adaptor.create(NAME,reindexstr);
                adaptor.addChild(assignTree,reindexNode);
             /* I revolve the pipeline indexes as soon as I enter the 'iplaneFE' loop.  To compensate,
                I initialize the pipeline indexes a plane off.  It doesn't matter for most cases as 
                the indexes are all relative.  However for the special pipelined temporaries, it 
                matters where we unpack the data in the prefetch region.  Unfortunately, the Fortran-W
                expression for unpacking involves a loop over the planes in a briquette.  It is impossible
                to convert a loop index to the revolving indexes without unrolling.  After much thought on
                implementation, I decided to retain the loop but be able to access the planes of the special
                pipelined temporaries with a modulo arithmetic equation.  I do the below initialization to 
                make such a formula easy or even possible.
                   eg: real plane numbers:    -3     -2     -1      0
                       initialization:      i3m0   i3m1   i3m2   i3m3
                       first rotation:      i3m1   i3m2   i3m3   i3m0   <-- plane i3m0 active i.e. contain valid values.  indexes i3m1 to i3m3 are not used in this iteration
                       second iteration:    i3m2   i3m3   i3m0   i3m1   <-- planes i3m0 and i3m1 are active.  i3m2 and i3m3 are not used.
                       third iteration:     i3m3   i3m0   i3m1   i3m2   <-- planes i3m0 to i3m2 are active.  plane i3m3 is inactive.
                - JJ (07/15/2011)
                adaptor.addChild(assignTree,(FTree)adaptor.create(ICON,Integer.toString(lb+i-ub)));    I am commenting this for the moment.
                I will revive it, if there are problems.  Maybe, the situation is not so dire as the above comments thinks. (09/09/11)             
              */
                adaptor.addChild(assignTree,(FTree)adaptor.create(ICON,Integer.toString(i)));          
                     
                vs = (VariableSymbol)currentScope.resolve(reindexstr);
                if(vs==null){
                    vs = new VariableSymbol(reindexstr,Symbol.getDataType(reindexstr));                    
                    currentScope.define(vs);  
                }                
                reindexNode.symbol = (Symbol)vs;           
                
                newTree.addChild(assignTree);
             }            
          }         
          
          newTree.addChild((FTree)adaptor.dupTree((FTree)execBlk.getChild(0)));
          //System.out.println("The 1st execblk stmt: "+retval.tree.getChild(0).toStringTree());
          adaptor.replaceChildren(execBlk,0,0,newTree);
       }
    }        
                                           
}


program:
  ^(CODEROOT (ppmdirectives | directives | executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit
@after{
/* Clear the reindextab after every subroutine */
    reindextab.clear();
}:
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
  ^(T_SUBROUTINE subroutineName namelist?)
  ;
  
namelist :
  ^(SUBARG identifier+)
  ;

subprogramBody:
  ^(SUBPROGRAMBLOCK declarationBlock executionBlock)
  ;

declarationBlock:
  ^(DECLARATIONBLOCK declarationStatement*)
  ;

executionBlock
@after {
    insertSwizzleStmts();
    //removeStmts();
    initIndices(retval.tree);
    
    begintransformregion = null;
    beginremoveregion = null;
    endremoveregion = null;     
}:
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
  
wholeStatement:
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
arrayDeclarator 
@after
{     
  int ub, lb, ndim, nchildren;
  String sub, slb, subwm, slbwm, reindexstrbase;
  Bound bound;
  ArraySymbol asym;    
  FTree lastDimTree; 
  
  currentScope = $ID.symbol.scope;  
  //System.out.println("id "+$ID.text+"  symbol "+$ID.symbol+"   "+$ID.symbol.isArraySymbol());
  if($ID.symbol.isArraySymbol() && pipelinedTmps.containsKey((ArraySymbol)$ID.symbol)) {   
         asym = (ArraySymbol)$ID.symbol;             
         bound = pipelinedTmps.getBound(asym);
         ub = bound.ub;
         lb = bound.lb;
                 
         if (ub == lb){
             /* Delete the last dimension of a single planed temporary */    
             nchildren = $ad.tree.getChildCount();
             if(nchildren > 1) {
                $ad.tree.deleteChild(nchildren-1);
                asym.nDim--;  
             }   
             else {   
             /* Don't delete the ADEXT if we have only one dimension.
              * I don't want to deal with converting an array symbol 
              * into an ordinary symbol.
              */
               FTree one = (FTree)adaptor.create(ICON,"1"); 
               FTree adext = (FTree)adaptor.create(ADEXT,"ADEXT");
               adext.addChild(one);
               $ad.tree.replaceChildren(0,0,adext);
             }     
         } else {                
         
         /* lastDimTree now points to the ADEXT node of the last dimension */
         lastDimTree = $ad.lt; 
         sub = Integer.toString(ub);
         slb = Integer.toString(lb);
    
         nchildren = lastDimTree.getChildCount();        
         //System.out.println("String tree for lastDimTree "+lastDimTree.toStringTree()+"  "+Integer.toString(nchildren)+"  ub = "+ub+"  lb = "+lb);
         /* Delete children 0 to nchildren-1 of the lastDimTree or the ADEXT of the last dimension */
         FTree newBound = (FTree)adaptor.nil();
         adaptor.addChild(newBound,(FTree)adaptor.create(ICON,slb));
         adaptor.addChild(newBound,(FTree)adaptor.create(ICON,sub));
         adaptor.replaceChildren(lastDimTree,0,nchildren-1,newBound);
         //System.out.println("String tree after replace "+lastDimTree.toStringTree()+"  "+Integer.toString(nchildren)+"  ub = "+ub+"  lb = "+lb);         

         subwm = Integer.toString(-ub);
         slbwm = Integer.toString(-lb);      
      /* Add to re-index table */
         if(ub == 0)
            reindexstrbase = "i"+slbwm+"m";
         else   
            reindexstrbase = "i"+slbwm+subwm+"m";
         //if(null==reindextab.containsKey(bound)){   
            reindextab.put(bound,reindexstrbase);
           // System.out.println("Just put in bound = "+bound.ub+"  "+bound.lb+". currentscope = "+currentScope);
         //}   
         
      /* Change the dimension information in the symbol */   
         ndim = asym.nDim;
                    
      /* There is no way to free memory in Java.  JVM handles the garbage collection.
         Even to think about deleting children of LB and UB trees is a waste of time.
       */   
         asym.LBDimTree[ndim-1] = (FTree)adaptor.create(ICON,slb);
         asym.UBDimTree[ndim-1] = (FTree)adaptor.create(ICON,sub);
           
         asym.LBDim[ndim-1] = slb;
         asym.UBDim[ndim-1] = sub;
         } /* Ends the if for lb == ub */
                      
         asym.setArraySize((BaseScope)currentScope);         
  }
}
:
  ^(ARRAY ID=NAME ad=arrayDeclaratorExtents)
  ;

arrayDeclaratorExtents returns [FTree lt]:
  ^(ADEXTS a=arrayDeclaratorExtent+) {$lt = $a.tree;}
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
doStatement : 
  ^(T_DO (doWithEndDo|doWithLabel) )
  ;

doVarArgs :
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
expression:  
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

subscripts:
  ^(SUBSCRIPT expression+);
 
varRef
@after {
  int ndim, nchildren;  
  String lastdimstr, reindexstr;
  Bound bound;
  FTree subscriptTree, lastdim, reindexNode;
  Symbol vs; /* variable symbol */
    
  //System.out.println("reduceMem2 "+$ID.text+"  "+$ID.symbol);  
  currentScope = $ID.symbol.scope;  
  if(/*blIfStmt &&*/ $ID.symbol.isArraySymbol() && pipelinedTmps.containsKey((ArraySymbol)$ID.symbol) && $s.tree != null) {                
         bound = pipelinedTmps.getBound((ArraySymbol)$ID.symbol);                  
         subscriptTree = $s.tree;         
         nchildren = subscriptTree.getChildCount();   
         
         if (bound.lb == bound.ub) {
             /* Delete the last dimension of a single planed temporary */    
             if(nchildren > 1) {
               subscriptTree.deleteChild(nchildren-1);
             }   
             else {   
             /* Don't delete the subscript if we have only one dimension.
              * I don't want to deal with converting an array symbol 
              * into an ordinary symbol.
              */
               FTree one = (FTree)adaptor.create(ICON,"1"); 
               subscriptTree.replaceChildren(0,0,one);
             }   

         } else {
         
      /* The subscript expression for the last dimension ought to be of the form
         ID or ^( T_MINUS ID ICON ) where ID is the outer loop index of the current
         TRANSFORM region 
       */ 
         lastdim = (FTree)subscriptTree.getChild(nchildren-1);  
         lastdimstr = lastdim.toString();  
         
         reindexstr = (String)reindextab.get(bound);
         if ( lastdimstr.contentEquals("iplaneFE")) {
            reindexstr = reindexstr + "0";
         } else if (lastdimstr.contentEquals("-") == true) {
              int icon = Integer.parseInt(lastdim.getChild(1).toString());
              reindexstr = reindexstr+Integer.toString(icon);          
           } else {
                   System.out.println(retval.tree.toStringTree());
                   System.out.println("Error: Invalid expression type in the last dimension of a pipeline temporary.  We see the operator or variable "+lastdimstr+
                                      ".  We allow only the outer (Longitudinal) loop index, the + or - operators, and an integer constant, in the last dimension of a "+ 
                                      "pipeline temporary.  (Internal:  The reporting module is reduceMem2.g)");
                   System.exit(-1);           
             }    
         reindexNode = (FTree)adaptor.create(NAME,reindexstr);   
         adaptor.replaceChildren(subscriptTree,nchildren-1,nchildren-1,reindexNode);
         //currentScope = $ID.symbol.scope;
         
         vs = currentScope.resolve(reindexstr);
         if(vs==null){
            vs = new VariableSymbol(reindexstr,Symbol.getDataType(reindexstr),false);
            reindexNode.symbol = vs;
            currentScope.define(vs);  
         }else{            
            if(vs.scope != currentScope)
               vs.setArgument();
            reindexNode.symbol = vs;
         }
         } /* Ends the if for bound.lb == bound.ub */

   }
}:
  ID=NAME
  |^(ARRAYREF ID=NAME s=subscripts);
  
/* 92 */
arrayName:  
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
@init{
  boolean begintrans = false, beginremov = false, endremov = false;
}
@after{
  if(begintrans)
     begintransformregion = (FTree)retval.tree;
  if(beginremov)
     beginremoveregion = (FTree)retval.tree;   
  if(endremov)
     endremoveregion = (FTree)retval.tree;   
} :
//
    ^(PPMDIRECTIVE TRANSFORMBEGIN) {begintrans = true;}
  | ^(PPMDIRECTIVE CANREMOVEBEGIN) {beginremov = true;}
  | ^(PPMDIRECTIVE CANREMOVEEND) {endremov = true;}    
  | ^(PPMDIRECTIVE (~(EOS | TRANSFORMBEGIN | CANREMOVEBEGIN | CANREMOVEEND))+)  
  ;
    
ppmdecldirectives :
    ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

    
    
