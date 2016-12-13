//===------------------------- inliner.g - inliner ------------------------===//
//
//  This code performs the bulk of inlining
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
 
tree grammar inliner;

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
package inliner;
import lexer.*;
import symbol.*;
import translator.*;
import java.util.Hashtable;
}

@members{   
  boolean bpipelineouter = false; // True if it is the outer pipeline loop
  boolean binline = false;  // True when we see an inline directive
   
  SymbolTable symtab;
  LabelTable lbltab;
  Scope currentScope;
  CommonTreeNodeStream backup;
  public boolean ismodified;
  List newEquivs = new ArrayList();
  Hashtable redundantDefsArg = new Hashtable();
  Hashtable redundantDefsNonArg = new Hashtable();  
  FTree cleanedDeclBlk = null;  /* Hold the declaration block for the inlined subroutine */
  
  public void setsymtable(SymbolTable sym){
    this.symtab = sym;
  }
  
  public inliner(TreeNodeStream input, SymbolTable symtab, LabelTable lbltab) {
      this(input);
      this.symtab = symtab;
      this.lbltab = lbltab;
      currentScope = symtab.globals;
      this.backup = new CommonTreeNodeStream(Translator.fTreeAdaptor,input.getTreeSource());
      this.ismodified = false;
  }
  
  public FTree inliner(FTree callstmt){
    currentScope = ((FTree)callstmt.getChild(0)).symbol.scope;
    //System.out.println("Inlining "+callstmt.getChild(0).toString()+" in "+currentScope.getScopeName());
    List callingArglist;
    FTree inlinedExecBlk = null;
    
    if(callstmt.getChild(1) != null)
      callingArglist = ((FTree)callstmt.getChild(1)).getChildren();
    else
      callingArglist = null;
    
    //System.out.println(callstmt.toStringTree());
    CommonTreeNodeStream wholenode = backup;
    getSubroutineTree getsubroutine = new getSubroutineTree(wholenode,symtab,callstmt.getChild(0).toString());
    getsubroutine.setTreeAdaptor(Translator.fTreeAdaptor);
    getsubroutine.downup(((FTree)wholenode.getTreeSource()),true);
    FTree subtree = getsubroutine.newtree;
    inlinedExecBlk = updateInlinedSubroutine(callstmt, currentScope, callingArglist, subtree);
    //System.out.println("subtree:"+subtree.toStringTree());
    ismodified = true;
    return inlinedExecBlk;
  }

  private List getArgList(FTree subroutine, List callingArglist){
    List arglist;
    
 /* subroutineSubprogram: ^(SUBROUTINE subroutineStatement subprogramBody) */     
    /* subroutineStatement:  ^(T_SUBROUTINE subroutineName namelist?) */ 
    if(subroutine.getChild(0).getChild(1) != null)
       arglist = ((FTree)subroutine.getChild(0).getChild(1)).getChildren();
    else
       arglist = null;    
    
    if((arglist!= null && callingArglist != null)&&(arglist.size() != callingArglist.size()))  {
       System.err.println("Argument number mismatch!!! Stop translation!!");
       System.exit(-1);
    }
    return arglist;
  }
  
  private FTree createLabel(String calleename, Scope currentScope){
    FTree label;
    LabelSymbol ls = null;
    String lblname = Integer.toString(lbltab.getSize()+1);
                
    if(null == lbltab.getSymbol(lblname,currentScope)){     
       ls = new LabelSymbol(lblname);
       ls.scope = currentScope;
       ls.name = calleename;
       lbltab.addSymbol(ls);
    } else {
       System.out.println("Internal error (inliner.g): Having difficulty generating a new label reference.  The Label table is somehow corrupted.");
       System.exit(-1);
    }
    
    label = (FTree)adaptor.create(ICON,lblname);    
    label.symbol = ls;
    
    return label;
  }
    
  private FTree createContinue(FTree label){
    FTree contStmt = null;
    FTree lblNode = null;    
    FTree duplbl = null;
    
    contStmt = (FTree)adaptor.create(T_CONTINUE,"continue"); 
       
 /* Unlike a GOTO statement, label here is a token of type LABEL. */        
    lblNode = (FTree)adaptor.create(LABEL,label.toString());    
    lblNode.symbol = label.symbol;
       
    contStmt.addChild(lblNode);
     
    return contStmt;
  }
  
  private FTree cleanExecBlk(FTree execBlk, String calleename, Scope callScope){
    FTree cleanedExecBlk = (FTree)adaptor.nil();
    FTree contStmt, label;
    CommonTreeNodeStream donode;    

    donode = new CommonTreeNodeStream(Translator.fTreeAdaptor, execBlk);

    fixLabels fixlbl = new fixLabels(donode, symtab, lbltab, callScope);
    fixlbl.setTreeAdaptor(Translator.fTreeAdaptor);
    fixlbl.downup(execBlk, false);
    
    /*  Rename the labels in the inline copy */
    donode.reset();    
    renameLabel rlbl = new renameLabel(donode,symtab,lbltab);
    rlbl.setTreeAdaptor(Translator.fTreeAdaptor);        
    rlbl.downup(execBlk,false);

 /* Look for RETURN statements nested inside the children themselves.  
    Replace them with GOTO statements.  You then need to create a
    corresponding CONTINUE statement at the end of this inlined subroutine.
  */
    label = createLabel(calleename, callScope);
    
    donode.reset();
    fixReturnStmts fixReturns = new fixReturnStmts(donode, label);    
    fixReturns.setTreeAdaptor(Translator.fTreeAdaptor);
    fixReturns.downup(execBlk, false);
    
 /* Copy the execution block statements to a new tree.
    We do this to remove the EXECUTIONBLOCK token. 
  */
    for(int i=0;i<execBlk.getChildCount();i++){
        cleanedExecBlk.addChild(execBlk.getChild(i)); 
    }     
 /* Add a CONTINUE statement in place of the END statement */
    contStmt = createContinue(label);
    cleanedExecBlk.addChild(contStmt);
       
    return cleanedExecBlk;  
  }

  private FTree cleanDeclBlk(FTree declBlk, String calleename, String callername){    
    FTree cleanedDeclBlk = (FTree)adaptor.nil();
    CommonTreeNodeStream donode;

    donode = new CommonTreeNodeStream(Translator.fTreeAdaptor, declBlk);    
    fixDefinitions fixDefs = new fixDefinitions(donode, calleename, callername, redundantDefsArg, 
                                                                  redundantDefsNonArg, newEquivs);
    fixDefs.setTreeAdaptor(Translator.fTreeAdaptor);
    fixDefs.downup(declBlk, false);
    declBlk = fixDefs.newDeclBlk;
        
/* Copy the execution block statements to a new tree.
    We do this to remove the EXECUTIONBLOCK token. 
  */
    for(int i=0;i<declBlk.getChildCount();i++){
        cleanedDeclBlk.addChild(declBlk.getChild(i)); 
    }
    return cleanedDeclBlk;
  }

  private Hashtable findVarLenArrays(CommonTreeNodeStream donode, FTree declBlk, String calleename,  
                                           String callername, List arglist, List callingArglist){
    varLenArrays changeDims = new varLenArrays(donode, calleename, callername, arglist, callingArglist);
    changeDims.setTreeAdaptor(Translator.fTreeAdaptor);
    changeDims.downup(declBlk,false);
    return changeDims.varLenArrayDims;
  }
  
  private FTree updateInlinedSubroutine(FTree callStmt, Scope callScope, List callingArglist, FTree subroutine ){
    FTree newsubPrgmBody = (FTree)adaptor.nil();
    FTree tmp, parent, declBlk, execBlk, cleanedExecBlk;
    int parentType;
    List arglist;    
    String calleename = ((FTree)subroutine.getChild(0).getChild(0)).toString();
    String callername = null;
    Hashtable varLenArrayDims;
    CommonTreeNodeStream donode;
   
    callername = callScope.getScopeName();
    arglist = getArgList(subroutine, callingArglist);

 /* subroutineSubprogram: ^(SUBROUTINE subroutineStatement subprogramBody) */    
    newsubPrgmBody = (FTree)subroutine.getChild(1);
    declBlk = (FTree)newsubPrgmBody.getChild(0);
    /*if(calleename.contentEquals("difuze")) {
       System.out.println(calleename+" declBlk:"+declBlk.toStringTree());
       System.exit(0);
    }*/   
    
    donode = new CommonTreeNodeStream(Translator.fTreeAdaptor, declBlk);    
    varLenArrayDims = findVarLenArrays(donode, declBlk, calleename, callername, arglist, callingArglist);
    
    donode = new CommonTreeNodeStream(Translator.fTreeAdaptor, newsubPrgmBody);
    fixVarRefs changename = new fixVarRefs(donode, symtab, calleename, callername, arglist, 
                                            callingArglist, callScope, redundantDefsArg, 
                                            redundantDefsNonArg, newEquivs, varLenArrayDims);
    changename.setTreeAdaptor(Translator.fTreeAdaptor);
    changename.downup(newsubPrgmBody,false);
 
    execBlk = (FTree)newsubPrgmBody.getChild(1);
    cleanedExecBlk = cleanExecBlk(execBlk, calleename, callScope);
    
    cleanedDeclBlk = cleanDeclBlk(declBlk, calleename, callername); /* cleanedDeclBlk is a global variable */
        
    return cleanedExecBlk;    
  }

  private void updateDefinitions(FTree computeSubroutine){     
//    System.out.println("inliner: updateDefinitions: ");
//    System.out.println("computeSubroutine = "+computeSubroutine.toStringTree()); 
//    System.out.println("computeSubroutine.numChild = "+computeSubroutine.getChildCount());      
    FTree subprogblock = (FTree)computeSubroutine.getChild(1); 
//    System.out.println("subprogramblock = "+subprogblock.toStringTree());
//    System.out.println("cleanedDeclBlk = "+cleanedDeclBlk.toStringTree());
    adaptor.addChild(subprogblock.getChild(0),cleanedDeclBlk);
  }
    
}


program :
  ^(CODEROOT (ppmdirectives | directives | executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit 
@after{
  if (bpipelineouter) {
     /* This is the subroutine which contains the pipeline region */
     /* The execution block of one of the subroutines inside it have been inlined.  However, we need to update the declaration block. */
     updateDefinitions(retval.tree);
     bpipelineouter = false;
  }
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
  
lblRef :
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
doStatement : 
  ^(T_DO (lblRef)? doVarArgs d=doBody);

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
callStatement 
@after{ 
    if (!ismodified){
       /* We don't inline more than one subroutine at a time.  Therefore, we check to see if ismodified is false. */
       if (true == binline){ 
         /* We found the subroutine call associated with the INLINE directive */
         retval.tree = inliner((FTree)retval.tree);
         //redundantDefsArg.clear();
         //redundantDefsNonArg.clear();
         binline = false;
       }
    }
}: 
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
@init{
  boolean blocalinline = false;  /* Track the directive seen */
}
@after{
  if (binline){
    if(false == blocalinline){
      System.out.println("Error: compute-CFDBDR.f (Intermediate file), line "+retval.tree.getLine()+": No PPM directives are allowed between the INLINE directive and the subroutine call.");
      System.out.println("Statement seen: "+retval.tree.toStringTree());
      System.exit(-1);
    }
//    System.out.println("compute-CFDBDR.f (Intermediate file), line "+retval.tree.getLine()+": "+retval.tree.toStringTree());
    blocalinline = false;
    retval.tree = (FTree)adaptor.create(PPMDIRECTIVE,"cPPM$");
    retval.tree.addChild((FTree)adaptor.create(INLINED,"INLINED"));
  }  
}: 
    ^(PPMDIRECTIVE PIPELINE) {bpipelineouter = true;}
  | ^(PPMDIRECTIVE INLINE)   {if (!ismodified && bpipelineouter) { binline = true; blocalinline = true; }}
  | ^(PPMDIRECTIVE (~(INLINE|PIPELINE))+)       
   ;   

ppmdecldirectives :
    ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

