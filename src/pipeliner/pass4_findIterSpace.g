//===----------- pass4_findIterSpace.g - find iteration spaces ------------===//
//
//  Find the bounds (iteration spaces) of the loops to be pipelined
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

tree grammar pass4_findIterSpace;

/* 01/02/2011:
   Pattern matching ANTLRs are very elegant, but seems to be very error-prone.  Adding statements
   (nodes/children) seems to work, but deletion doesn't.  TreeVisitor visit method gets all 
   confused.  I performed a whole bunch of experiments from deleting statements in the reverse
   order to getting child index at the time of deletion.  The conclusion is deletion doesn't
   work with pattern matching grammars.  It forces me to use the more clumsy tree grammars, but,
   as demonstrated below, they atleast work.

   Here, we merge the pipelined THENBLOCKs with a common IF conditional expression inside the
   transform region.  The pipelined IF statements are identified by the unique string 'ibegin'
   we generated in the conditional.       
      -JJ
        
 * 07/15/2011: --> Changed.  Don't delete it yet.  Alter it when you are sure.
 * Pipelined temporaries which also occur in the prefetch region are special.  
 * They are nothing but the temporaries we unpack the grid briquettes and other
 * input arrays like xl into.  We compute their live range in the TRANSFORM region,
 * but it is not sufficient for the plane-by-plane implementation.  The live range
 * of the TRANSFORM region represents the range for just one iteration of the iplaneFE
 * loop.  However, we do multiple iplaneFE iterations (nbdy or nsugar) for every grid
 * briquette we fetch.  We have to provision space for these extra iterations.  We 
 * allocate "nsugar + <live range in the TRANSFORM region>" planes for these temporaries.
 * It is slightly wasteful but simplifies the logic.
 * - JJ     
 *  */
 
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
      
   public pass4_findIterSpace(TreeNodeStream input, SymbolTable symtab, LabelTable lbltab){
      this(input);
      this.symtab = symtab;
      this.lbltab = lbltab;      
      //this.lblfregion = lbltab.getSize()+1; 
   }
   
   IntegerEvaluator IndexEval = new IntegerEvaluator();  

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
programStatement :
  ^(T_PROGRAM n=NAME)
  ;

seos : NEWLINE ;

/* 9, 11, 13 */
/* 10 */
functionStatement :
  ^(T_FUNCTION (type)? functionName namelist?)
  ;

blockdataStatement :
  ^(T_BLOCK n=NAME)
  ;

/* 12 */
subroutineStatement:
  ^(T_SUBROUTINE n=NAME namelist?)
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
      /* Insert the LOWERBOUND and UPPERBOUND directives before the PIPELINE directive */
      FTree stmt = null, stmtlist = null;

      FTree decBlk = null, execBlk = null;     
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
           System.out.println("Internal error (pass4_findIterSpace.g): Pipeline directive not found");                     
           System.exit(-1); 
       }          
     
       stmtlist = (FTree)adaptor.nil();
       
       /* Add the LOWERBOUND */             
       stmt = (FTree)adaptor.create(PPMDIRECTIVE,"cPPM$");  
       stmt.addChild((FTree)adaptor.create(LOWERBOUND,"LOWERBOUND"));
       stmt.addChild((FTree)adaptor.create(ICON,Integer.toString(lowerbound)));
       stmtlist.addChild(stmt);                  
                     
       /* Add the UPPERBOUND */              
       stmt = (FTree)adaptor.create(PPMDIRECTIVE,"cPPM$");  
       stmt.addChild((FTree)adaptor.create(UPPERBOUND,"UPPERBOUND"));
       stmt.addChild((FTree)adaptor.create(ICON,Integer.toString(upperbound)));
       stmtlist.addChild(stmt);     
                  
       /* Add the pipeline directive to the list */
       stmt = (FTree)execBlk.getChild(index);
       adaptor.addChild(stmtlist,adaptor.dupTree(stmt)); /* Pipeline directive */
       /* Replace the pipeline directive in the execution block with the constructed list */
       //System.out.println("stmtlist "+stmtlist.toStringTree());
       adaptor.replaceChildren(execBlk,index,index,stmtlist);    
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
 : ^(DDOLOOP {bldoloop = true;} l=lblRef? va=doVarArgs {bldoloop = false; bldobody = true;}
                                 d=doBody {bldobody = false;})
 ;     


/* 40 */
doStatement : 
  ^(T_DO (doWithEndDo|doWithLabel) )
  ;

doVarArgs
@after
{
  if ( bldoloop ){
     idn = $id.toString();
     idsymbol = $id.symbol;
     currentScope = idsymbol.scope;
     //System.out.println("currentScope set for "+idn);
    
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
     /* Right bound is the upper bound if e3 is positive or doesn't exist */
     if (lowerbound > lb)
         lowerbound = lb;
     if (upperbound < rb)
         upperbound = rb;    
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
/* counter keeps count of the expression being processed.  If bidnexpr is true then this is the subscript expression
   we have to modify.  I expect to see the outer index appear in only one subscript.  If it occurs in more than one
   subscript, I have to find another solution. 
 */  
  ^(SUBSCRIPT (e=expression)+);
 
varRef :      
      ^(ARRAYREF ID=NAME s=subscripts)
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
  | n=NAME
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
  |^(PPMDIRECTIVE TRANSFORMEND) {btransregion = false; btransend = true;}
  |^(PPMDIRECTIVE (n=~(TRANSFORMBEGIN | TRANSFORMEND | EOS))+)
  ;
   
ppmdecldirectives :
   ^(PPMDIRECTIVE (~EOS)+)
//   ^(PPMDIRECTIVE DEDOUBLEBUFFER (~EOS)+)
//  | ^(PPMDIRECTIVE PENUPPERBOUND (~EOS)+)  
//  | ^(PPMDIRECTIVE PENLOWERBOUND (~EOS)+)
  ;

  
