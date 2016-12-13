//===----------- pass1_delRedIter.g - delete redundant iterations ----------===//
//
//  Eliminate the redundant iterations between the adjacent briquettes
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

tree grammar pass1_delRedIter;

/* 01/02/2011:
   Pattern matching ANTLRs are very elegant, but seems to be very error-prone.  Adding statements
   (nodes/children) seems to work, but deletion doesn't.  TreeVisitor visit method gets all 
   confused.  I performed a whole bunch of experiments from deleting statements in the reverse
   order to getting child index at the time of deletion.  The conclusion is deletion doesn't
   work with pattern matching grammars.  It forces me to use the more clumsy tree grammars, but,
   as demonstrated below, they atleast work.
      -JJ
      
  08/12/2014:
   Pattern matching grammar has few more problems:
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
      
   12/06/2010:
   Pipeline Phase 1:
   *  Adjusts the outer pipeline loop iteration space.  eg: (1,nbqx) -> (1-nghostcubes,nbqx+nghostcubes)
   *  Eliminates redundant iterations. eg: do loop -> if (icget .le. nbqx+nghostcubes), add if (icube .lt. 1-nghostcubes) goto <outer loop label>
   *  Move the statements between the prefetch region end and the redundant do loop end, to outside of the loop. 
      The assumption is that the prefetch region occurs inside the redudant do loop
   *  Insert the TRANSFORMBEGIN directive right after the redundant do loop region and the TRANSFORMEND directive right before the end of the
      outer pipeline loop.  The implicit assumption here is that the redundant do loop occurs inside the outer pipeline loop.        
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
}
@members{  
   boolean bpipelineouter = false; // True if it is the outer pipeline loop
   boolean bdelrediter = false; // Delete the redundant iterations.  True for such a loop.
   boolean bpfetch = false; // Prefetch region
   boolean bdelrediterbody = false;  // Body of the loop to be eliminated   
   boolean bdelredsubexpr = false; /* The flag to mark the region where the redundant varref in subscripts
                                      have to be eliminated.  It is true for the entire redundant iteration
                                      region.  bdelrediter and bdelrediterbody flags go on and off in the 
                                      region, but this flag is going to stay on the whole time. 
                                      We are eliminating the index offset variables generated for the 
                                      redundant loop iterations.
                                    */    
   boolean bsubscript = false; /* True in the subscript rule */                                    
   String idoutn = null; // Identifier name for the outer pipeline loop
   Symbol idoutsymbol = null; // Symbol for the outer pipeline loop     
   Scope currentScope = null; /* The newly created labels need to inserted into the label table too.
                                 Labels need scope. We assign the ddoloop identifier's scope to the
                                 label encountered. Any other NAME would have worked, but I am
                                 using this identifier */ 
   LabelTable lbltab = null;   
   FTree outLBTree = null, outUBTree = null;
   FTree nonpfetchTree = null;
   String outLblRef = "0";     /* Label Reference for the outer pipeline loop */
   Symbol outLblSym = null;    /* Symbol for the outer pipeline loop label */
   HashMap redsubExpr = null; /* We need to eliminate the index offset variables
                                   generated for the redundant loop iterations. This
                                   table stores such variables.
                                 */
   
   public pass1_delRedIter(TreeNodeStream input, LabelTable lbltab, HashMap redsubExpr){
      this(input);
      this.lbltab = lbltab;
      this.redsubExpr = redsubExpr; 
      //this.lblfregion = lbltab.getSize()+1; 
   }
        
   private int ivalueOf(String parameter){
      Symbol vs;      
      
      vs = currentScope.resolve(parameter);        
      if(vs == null){
         System.out.println("pass1_delRedIter.g : "+parameter+" is undefined");
         System.exit(-1);
      }     
                 
      return ((ParamSymbol)vs).ivalue;
   }
        
    private FTree transformBegin(){
      FTree transformTree;           
            
      transformTree = (FTree)adaptor.create(PPMDIRECTIVE,"!PPM$");
      transformTree.addChild((FTree)adaptor.create(TRANSFORMBEGIN,"TRANSFORMBEGIN"));
      
      return transformTree;
    }  
    
    private FTree transformEnd(){
      FTree transformTree;           
            
      transformTree = (FTree)adaptor.create(PPMDIRECTIVE,"!PPM$");
      transformTree.addChild((FTree)adaptor.create(TRANSFORMEND,"TRANSFORMEND"));
      
      return transformTree;
    }  
    
    private FTree iskipitBegin(){
      FTree iskipitTree;           
            
      iskipitTree = (FTree)adaptor.create(PPMDIRECTIVE,"!PPM$");
      iskipitTree.addChild((FTree)adaptor.create(ISKIPITBEGIN,"ISKIPITBEGIN"));
      
      return iskipitTree;
    }  

    private FTree guardStmt(){
      FTree ifgotoTree, ifexprTree, thenTree, gotoTree;
      FTree subTree, lblrefTree;
      Symbol vs = null;
      
   /* Create a new tree for 'if (icube .lt. <pipeline lower bound>) then' .. 'endif' */
      ifgotoTree = (FTree)adaptor.create(T_IF,"if");
      ifexprTree = (FTree)adaptor.create(T_LT,".lt.");
      ifexprTree.addChild((FTree)adaptor.create(NAME,idoutn));         
      ((FTree)ifexprTree.getChild(0)).symbol = idoutsymbol;
      subTree = (FTree)adaptor.create(T_MINUS,"-");
      subTree.addChild(outLBTree);
      vs = currentScope.resolve("nghostcubes");
      if(vs == null){
         System.out.println("nghostcubes is undefined");
         System.exit(-1);
      }
      subTree.addChild((FTree)adaptor.create(NAME,"nghostcubes"));
      ((FTree)subTree.getChild(1)).symbol = vs;
      ifexprTree.addChild(subTree);
      ifgotoTree.addChild(ifexprTree);
      thenTree = ((FTree)adaptor.create(THENBLOCK,"then"));
      gotoTree = (FTree)adaptor.create(GOTO,"goto");
      lblrefTree = (FTree)adaptor.create(LABELREF,"LABELREF");
      lblrefTree.addChild((FTree)adaptor.create(ICON,outLblRef));
      ((FTree)lblrefTree.getChild(0)).symbol = outLblSym;
      gotoTree.addChild(lblrefTree);
      thenTree.addChild(gotoTree);
      ifgotoTree.addChild(thenTree);
      
      return ifgotoTree;
    }  
         
    private FTree redIterDo2if(String idn, Symbol idsymbol, FTree doBody){
      FTree plusTree, ifTree, ifexprTree, thenTree, ifgotoTree, 
            gotoTree, lblrefTree;
      Symbol vs = null;     
     
    /* Create a new tree for 'if (icube .le. <pipeline upper bound>) then' .. 'endif' */
      ifTree = (FTree)adaptor.create(T_IF,"if");
      ifexprTree = (FTree)adaptor.create(T_LE,".le.");
      ifexprTree.addChild((FTree)adaptor.create(NAME,idn));         
      ((FTree)ifexprTree.getChild(0)).symbol = idsymbol;
      plusTree = (FTree)adaptor.create(T_PLUS,"+");
      plusTree.addChild(outUBTree);
      vs = currentScope.resolve("nghostcubes");
      plusTree.addChild((FTree)adaptor.create(NAME,"nghostcubes"));
      ((FTree)plusTree.getChild(1)).symbol = vs;
      ifexprTree.addChild(plusTree);
      ifTree.addChild(ifexprTree);         

    /* Copy the children of the DOBLOCK (doBody) to the THENBLOCK.
      The most efficient solution is to convert the DOBLOCK token to the
      THENBLOCK token. It works!  
     */
      thenTree = (FTree)adaptor.dupTree(doBody);    
      thenTree.getToken().setText("then");
      thenTree.getToken().setType(THENBLOCK);
      thenTree.addChild(guardStmt());
      ifTree.addChild(thenTree);      
         
      return ifTree;
   }         
       
   private FTree movedStatements(){
   /* Contains:
      i) MOVEDFROMREDITERBEGIN directive,
      ii) the non-prefetch statements moved out of the redundant DO loop, and
      iii) the MOVEDFROMREDITEREND directive.
    */   
      FTree movStmts = (FTree)adaptor.nil();
      FTree movTree;

   /* Add child "MOVEDFROMREDITERBEGIN" next */         
      movTree = (FTree)adaptor.create(PPMDIRECTIVE,"!PPM$");
      movTree.addChild((FTree)adaptor.create(MOVEDFROMREDITERBEGIN,"MOVEDFROMREDITERBEGIN"));
      movStmts.addChild(movTree);
         
   /* Add the non-prefetch statements */
      movStmts.addChild(nonpfetchTree);
         
   /* Add child "MOVEDFROMREDITEREND" last */         
      movTree = (FTree)adaptor.create(PPMDIRECTIVE,"!PPM$");
      movTree.addChild((FTree)adaptor.create(MOVEDFROMREDITEREND,"MOVEDFROMREDITEREND"));
      movStmts.addChild(movTree);
         
      return movStmts;
   } 
} 

program :
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
  ^(T_SUBROUTINE s=subroutineName namelist?)
  ;
  
namelist :
  ^(SUBARG identifier+)
  ;

subprogramBody
:
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
  
wholeStatement :
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

lblRef

@after{
  if (bpipelineouter) {      
      outLblRef = $l.text;
      outLblSym = $l.symbol; 
  }    
}:
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
scope {
   String idn; // Identifier name for the current doloop
   Symbol idsymbol; // Symbol for the current doloop identifier   
}  
@init{
   boolean btmp1 = false, btmp2 = false;
   $doStatement::idn = null;
   $doStatement::idsymbol = null;
}
@after {
     FTree plusTree, subTree, 
           ifTree, ifexprTree, thenTree, ifgotoTree, 
           gotoTree, lblrefTree, transformTree, movTree;
     FTree newTree;
     Symbol vs = null;     
     boolean isVectorLoop = false;     
     int init = 0;
     
     bpipelineouter = btmp1;
     bdelrediter = btmp2;
     if(bdelrediter){
     /* I am re-writing this redundant DO loop to the an IF
        statement, TRANSFORMBEGIN directive, ISKIPITBEGIN directive,
        MOVEDFROMREDITERBEGIN directive, the non-prefetch statements 
        moved out of this redundant DO loop, and the MOVEDFROMREDITEREND 
        directive.
      */
        newTree = (FTree)adaptor.nil();
         
        newTree.addChild(redIterDo2if($doStatement::idn, $doStatement::idsymbol, $d.tree));
        newTree.addChild(transformBegin());
        newTree.addChild(iskipitBegin());
        newTree.addChild(movedStatements());
         
        retval.tree = newTree;
         
        bdelredsubexpr = false;              
        bdelrediter = false; 
     }
     if (bpipelineouter){
         /* Insert the TRANSFORMEND directive right before the end of the outer pipeline loop. */                  
         $d.tree.addChild(transformEnd()); 
         bpipelineouter = false;
     }
}      
 : 
    ^(T_DO (lblRef)? doVarArgs {btmp1 = bpipelineouter;  bpipelineouter = false;}
                               {btmp2 = bdelrediter;
                                if (bdelrediter)
                                    bdelrediterbody = true;                                
                                bdelrediter = false;
                                } 
                   d=doBody);

doVarArgs
@after
{
  $doStatement::idn = $id.toString();
  $doStatement::idsymbol = $id.symbol;
  if (bpipelineouter){
     Symbol vs = null;
     FTree plusTree = null, minusTree = null;
     int itmp = -1;

     idoutn = $id.toString();
     idoutsymbol = $id.symbol;      
     currentScope = idoutsymbol.scope;     /* We only match the pipeline loop, and hence the outer pipeline loop is seen first.
                                           currentScope can never be null inside this pattern. */
     //System.out.println("CurrentScope = "+currentScope);

     itmp = ivalueOf("nghostcubes");
                 
     /* Replace expression e1 with e1 - (nghostcubes+1) */     
     minusTree = (FTree)adaptor.create(T_MINUS,"-");
     minusTree.addChild((FTree)adaptor.dupTree($e1.tree));
     minusTree.addChild((FTree)adaptor.create(ICON,Integer.toString(itmp+1)));             
     retval.tree.replaceChildren(1,1,minusTree);
     outLBTree = (FTree)adaptor.dupTree($e1.tree);
     
     /* Replace expression e2 with e2+nghostcubes */
     plusTree = (FTree)adaptor.create(T_PLUS,"+");
     plusTree.addChild((FTree)adaptor.dupTree($e2.tree));
     plusTree.addChild((FTree)adaptor.create(ICON,Integer.toString(itmp)));
     retval.tree.replaceChildren(2,2,plusTree);
     outUBTree = (FTree)adaptor.dupTree($e2.tree);
  }
}:
  ^(DOVARARG id=NAME e1=expression 
  e2=expression (e3=expression )?)
  ;

doWithLabel:
  (lblRef
   doVarArgs 
   doBody)
  ;

doBody 
@init{
  boolean btmp = false, bpfetchprev = false;
  int counter = -1, index = -1;
} 
@after{
  if (index > -1){
  /* Delete the statements moved out of the earstwhile redundant iterations block */
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
                  */
                   if(bpfetchprev) /* Just got past the prefetch region */
                      index = counter;
                      //System.out.println("index = "+index+"   counter = "+counter);    
                   else
                      adaptor.addChild(nonpfetchTree,(FTree)adaptor.dupTree($w.tree));
               }                   
             }
            )+
   )
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

subscripts
@init {boolean btmp = false;}:
  ^(SUBSCRIPT {btmp = bsubscript; bsubscript = true;} expression+ {bsubscript = btmp;});
   /* By using another temporary btmp, we account for subscripts occuring inside subscript 
      expression, and make sure that bsubscript is always on in the subscript region.
    */ 
 
varRef
@init{
  boolean bscalar = false;
}
@after{
  if (bscalar){
      if (/*bdelredsubexpr &&*/ bsubscript && redsubExpr.containsKey($ID.getText())){
          retval.tree = (FTree)adaptor.create(ICON,"0");
      }
  }
}:      
      ^(ARRAYREF ID=NAME s=subscripts)
    |   ID=NAME {bscalar = true;}
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
  
ppmdirectives:
   ^(PPMDIRECTIVE PREFETCHBEGIN) {bpfetch = true;  nonpfetchTree = (FTree)adaptor.nil();}
  |^(PPMDIRECTIVE PREFETCHEND)   {bpfetch = false;}    
  |^(PD=PPMDIRECTIVE DELREDITER) {bdelrediter = true; }  
  |^(PD=PPMDIRECTIVE DELREDSUBEXP ID=NAME)
    {
      bdelredsubexpr = true;
      currentScope = $PD.symbol.scope;
      redsubExpr.put($ID.text,currentScope);
    }
  |^(PPMDIRECTIVE PIPELINE) {bpipelineouter = true;}
  |^(PPMDIRECTIVE ~(PREFETCHBEGIN | PREFETCHEND | DELREDITER | DELREDSUBEXP | PIPELINE | DEDOUBLEBUFFER | EOS)+)  
  ;

ppmdecldirectives :
   ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

  
