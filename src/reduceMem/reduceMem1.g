//===--------------- reduceMem1.g - reduce memory pass 1 ------------------===//
//
//  This code converts the indexes of pipelined variables to an intermediate 
//  form for further analysis and optimization (memory reduction)
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

tree grammar reduceMem1;

/* 01/02/2011:
   Pattern matching ANTLRs are very elegant, but seems to be very error-prone.  Adding statements
   (nodes/children) seems to work, but deletion doesn't.  TreeVisitor visit method gets all 
   confused.  I performed a whole bunch of experiments from deleting statements in the reverse
   order to getting child index at the time of deletion.  The conclusion is deletion doesn't
   work with pattern matching grammars.  It forces me to use the more clumsy tree grammars, but,
   as demonstrated below, they atleast work.

   Convert i[a][b]m[c] - > iplaneFE - [c] in the subscripts
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
package reduceMem;
import lexer.*;
import symbol.*;
import translator.*;
import pipeliner.*;
import IntegerEvaluator.*;
import java.util.Hashtable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
}
@members{   
   boolean btransregion = false; // True if there is a transform region in the SUBRPOGRAMBLK      
   Scope currentScope = null; /* The newly created labels need to inserted into the label table too.
                                 Labels need scope. We assign the ldoLoop identifier's scope to the
                                 label encountered. Any other NAME would have worked, but I am
                                 using this identifier */ 
   LabelTable lbltab = null;            
   PipelinedTmps pipelinedTmps = null;
   boolean bFI = false;    /* True when called for a manually written FI */
   boolean bseenPT = false; /* True when a pipelined temporary has been seen in an execution unit */   
      
   public reduceMem1(TreeNodeStream input, LabelTable lbltab, PipelinedTmps pipelinedTmps, boolean bFI){
      this(input);
      this.lbltab = lbltab;
      this.pipelinedTmps = pipelinedTmps;
      this.bFI = bFI;
      //this.lblfregion = lbltab.getSize()+1; 
   }
  
   IntegerEvaluator IndexEval = new IntegerEvaluator();

   
} 

program:
  ^(CODEROOT (ppmdirectives | directives | executableUnit)+)
  ;

/* one unit of a fortran program */
executableUnit
@init{
  bseenPT = false;
}
@after{
/* Call fixBoundsforEquivalences for the current executable unit
 * if we found any pipelined temporaries.
 */
    if (bseenPT){
        pipelinedTmps.fixBoundsForEquivalences(currentScope);
    }    
    bseenPT = false;    
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

/* 40 */
doStatement : 
  ^(T_DO (doWithEndDo|doWithLabel) )
  ;

doVarArgs
 :
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

subscripts  returns [boolean bidnexpr, int offset] 
@init{
  int childIndex = -1;
  int counter = -1;
}
@after{
  /* Assumption: The index i[a][b]m[c] doesn't occur at more than one subscript.  I don't see 
     a reason why it could occur at more than one subscript in an array reference, 
     but if it does then the current solution won't work. 
   */
     FTree childTree;     
     
     $offset = -1;
     if (childIndex == counter){
        /* We have a valid outer index i occurring in the subscript expression */
        $bidnexpr = true;
        childTree = (FTree)retval.tree.getChild(childIndex);
        
        if (bFI){
            Symbol vs = null;
            FTree offTree = null, idTree = null;
        
            /* indirect index of i occurs in the last dimension.  Replace i with iplaneFE - offset. */        
            String indexStr = childTree.toString();
            String[] temp; 
            int length = 0;
        
            temp = indexStr.split("m");        
            length = temp.length;
            $offset = Integer.parseInt(temp[length-1]);        
               
            offTree =  (FTree)adaptor.create(T_MINUS,"-");
            idTree = (FTree)adaptor.create(NAME,"iplaneFE");
  
            vs = currentScope.resolve("iplaneFE");    
            if(vs==null){
               vs = new VariableSymbol("iplaneFE",Symbol.getDataType("iplaneFE"),false);
               currentScope.define(vs);
            }
            idTree.symbol = vs;       
            offTree.addChild(idTree);
            offTree.addChild((FTree)adaptor.create(ICON,Integer.toString($offset)));
            adaptor.replaceChildren(retval.tree,childIndex,childIndex,offTree);
        }   else {
            /* Find the offset from the expression iplaneFE (- <offset>)? */
            if (childTree.toString().contentEquals("iplaneFE")){
                /* The subscript expression has no offset */
                $offset = 0;
            } else {
                /* The offset is the right child of childTree */
                $offset = Integer.parseInt(childTree.getChild(1).toString());
            }
        }    
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
@init{
   boolean bname = false;     
}
/* Identify the outer indirect index i[a][b]m[c] in subscripts.
   We convert it to iplaneFE - [c] at the 'subscript' rule level.  
   Return a boolean 'bidnexpr' higher up to aid the process.  
   An old comment still valid: For index variables, I am guaranteed to not see any subscripts.
 */
@after {
   $bidnexpr = false;
   if (bname && btransregion){
       if (bFI && Pattern.matches("i\\d+m\\d", $ID.text))        
          $bidnexpr = true;
       if (!bFI && $ID.text.contentEquals("iplaneFE"))
          $bidnexpr = true;
   }        
 //System.out.println($ID.text+"  "+$ID.symbol);     
}:      
      ^(ARRAYREF ID=NAME s=subscripts
          {
            currentScope = $ID.symbol.scope;
            /* Re-dimension the pipelined temporaries */
            if($s.bidnexpr){
               if(pipelinedTmps.getSymbol($ID.text, currentScope) == null) {                           
                  pipelinedTmps.addEntry((ArraySymbol)$ID.symbol);
                  bseenPT = true;
               }
               pipelinedTmps.updateBound((ArraySymbol)$ID.symbol,-$s.offset);   
            }
          })
    |   ID=NAME     {currentScope = $ID.symbol.scope; bname = true;}
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
   ^(PPMDIRECTIVE TRANSFORMBEGIN) {btransregion = true;} 
  |^(PPMDIRECTIVE TRANSFORMEND) {btransregion = false;}
  |^(PPMDIRECTIVE (n=~(TRANSFORMBEGIN | TRANSFORMEND | EOS))+)
  ;
   
ppmdecldirectives :
    ^(PPMDIRECTIVE (~EOS)+)
//    ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

  
