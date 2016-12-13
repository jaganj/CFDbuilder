//===-------------- symtableCreator.g - populate symbol table -------------===//
//
//  Create the symbol table
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

tree grammar symtableCreator;

options {
  language = Java;
  backtrack=true;
  output = AST;
  tokenVocab = tokenFile;
  ASTLabelType = FTree;
}

@header{
package symbol;
import translator.FTree;
}

@members{
    boolean isVectorLoop = false;
    SymbolTable symtab;
    LabelTable lbltab;
    Scope currentScope;
    public symtableCreator(TreeNodeStream input, SymbolTable symtab, LabelTable lbltab) {
        this(input);
        this.symtab = symtab;
        this.lbltab = lbltab;
        currentScope = symtab.globals;
    }
}

program :
  ^(CODEROOT (ppmdirectives|directives|executableUnit)+)  
  ;

/* one unit of a fortran program */
executableUnit :
  functionSubprogram 
  | mainProgram 
  | subroutineSubprogram 
  | blockdataSubprogram
  ;

/* 2 */
mainProgram : 
  ^(MAINPROG programStatement subprogramBody)
  ;

/* 3 */
/* For FunctionSubprogram, subroutineSubprogram, the function scope
are started at functionStatement and subroutineStatement. Here, we
only need to close the scope after parsing functionSubprogram and 
subroutineSubprogram.*/
functionSubprogram 
@after
{
  ((GlobalScope)currentScope.getEnclosingScope()).enclosedFunctions.put(currentScope.getScopeName(),(FunctionSymbol)currentScope);
  currentScope = currentScope.getEnclosingScope();
}
:
  ^(FUNCTION f=functionStatement 
  {currentScope = new LocalScope(currentScope,$f.name);}
  subprogramBody
  {
    ((FunctionSymbol)currentScope.getEnclosingScope()).enclosedScope = currentScope;
    currentScope = currentScope.getEnclosingScope();
  }
  )
  ;

/* 4 */
subroutineSubprogram 
@after
{
  ((GlobalScope)currentScope.getEnclosingScope()).enclosedFunctions.put(currentScope.getScopeName(),(FunctionSymbol)currentScope);
  currentScope = currentScope.getEnclosingScope();
}
:
  ^(SUBROUTINE  s=subroutineStatement 
  {currentScope = new LocalScope(currentScope,$s.name);}
  subprogramBody
  {
    ((FunctionSymbol)currentScope.getEnclosingScope()).enclosedScope = currentScope;
    currentScope = currentScope.getEnclosingScope();
  }
  )
  ;

/* 5 - blockDataSubprogram */
blockdataSubprogram 
@after
{
  ((GlobalScope)currentScope.getEnclosingScope()).enclosedFunctions.put(currentScope.getScopeName(),(FunctionSymbol)currentScope);
  currentScope = currentScope.getEnclosingScope();
}
:
  ^(BLOCKDATA b=blockdataStatement 
  {currentScope = new LocalScope(currentScope,$b.name);}
  subprogramBody
  {currentScope = currentScope.getEnclosingScope();}
  )
  ;

/* 6 */
otherSpecificationStatement :
     dimensionStatement 
    |allocatableStatement 
 //   |allocateStatement 
    |equivalenceStatement 
    |intrinsicStatement 
    |saveStatement
    |volatileStatement
//    |macroStatement
  ;

/* 7 */
executableStatement :
   assignmentStatement 
  |gotoStatement 
  |ifStatement 
  |doStatement 
  |continueStatement 
  |stopStatement 
  |pauseStatement
  |readStatement
  |writeStatement
  |printStatement
  |rewindStatement
  |backspaceStatement
  |openStatement
  |closeStatement
  |endfileStatement
  |inquireStatement
  |callStatement
  |returnStatement
  ;

/* 8 */
programStatement :
  ^(T_PROGRAM NAME);

seos : NEWLINE ;

/* 9, 11, 13 */
/* 10 */
functionStatement returns [String name]
@after{$name = $ID.text;}
:
  ^(T_FUNCTION (type)? ID=NAME
  {
    FunctionSymbol fs = new FunctionSymbol($ID.text,currentScope);
    $ID.symbol = fs;
    currentScope.define(fs); // def method in globals
    currentScope = fs;       // set current scope to method scope
    //System.out.println("Scope:"+currentScope.getScopeName());
  } 
  namelist?)
  ;

blockdataStatement returns [String name]
@after{$name = $ID.text;}
:
  ^(T_BLOCK ID=NAME)
  {
    FunctionSymbol fs = new FunctionSymbol($ID.text,currentScope);
    $ID.symbol = fs;
    currentScope.define(fs); // def method in globals
    currentScope = fs;       // set current scope to method scope
    //System.out.println("Scope:"+currentScope.getScopeName());
  } 
  ;

/* 12 */
subroutineStatement returns [String name]
@after{$name = $ID.text;}
:
  ^(T_SUBROUTINE ID=NAME 
  {
    FunctionSymbol fs = new FunctionSymbol($ID.text,currentScope);
    $ID.symbol = fs;
    currentScope.define(fs); // def method in globals
    currentScope = fs;       // set current scope to method scope
    //System.out.println("Scope:"+fs.getScopeName());
  } 
  namelist?)
  ;  

namelist :
  ^(SUBARG identifier+) ;

//statement :
//   formatStatement 
//  |includeStatement 
//  |implicitStatement 
//  |useStatement
//  |parameterStatement 
//  |typeStatement 
//  |commonStatement 
//  |externalStatement 
//  |otherSpecificationStatement  
//  |statementFunctionStatement 
//  |executableStatement
//  ;

subprogramBody :
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
  |includeStatement
  |implicitStatement
  |useStatement
  |parameterStatement 
  |typeStatement
  |commonStatement 
  |dataStatement
  |externalStatement
  |otherSpecificationStatement
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

endStatement 
@after{
  if($l != null){
    LabelSymbol tmp = lbltab.getSymbol($l.text,currentScope);
    if((tmp != null) && (tmp.scope == currentScope)){
      $l.symbol = tmp;
      //System.out.println("label "+$l.text+" is in LabelTable"+" in scope "+tmp.scope.getScopeName());
    }else{
      //System.out.println("label "+$l.text+" isn't in LabelTable and label will be added into table in "+currentScope.getScopeName());
      LabelSymbol ls = new LabelSymbol($l.text);
      ls.scope = currentScope;
      $l.symbol = ls;
      lbltab.addSymbol(ls);
    }
  }
}
:
   ^(T_END (l=LABEL)?)
   ;

/* 15 */
dimensionStatement : 
  ^(T_DIMENSION arrayDeclarator+);
  
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
  ^(T_ALLOCATE arrayDeclarator+);

/* 16 */
arrayDeclarator 
@after
{
  if(!$commonBlock.isEmpty() && $commonBlock::bcommonblock){
     System.out.println("Array definitions inside common statement are disallowed.  "+$ID.text+" cannot be declared inside a commonstatement.");
     System.exit(-1);
  }
  if(currentScope.resolve($ID.text)==null){
    ArraySymbol as = new ArraySymbol($ID.text,$ad.tree,(BaseScope)currentScope);
    if(!$typeStatement.isEmpty())
      as.setType($typeStatement::type, $typeStatement::typelen);
    $ID.symbol = as; 
    currentScope.define(as);
  }else{
    Symbol s = currentScope.resolve($ID.text);
    if (s.bcommonblock){
        System.out.println("Array definitions of members in a common block must appear before the common statement.  "+$ID.text+" cannot be declared after a commonstatement.");
        System.exit(-1);       
    }
    if(s.isArraySymbol()){
      if(!$typeStatement.isEmpty())
        s.setType($typeStatement::type, $typeStatement::typelen);
      $ID.symbol = s;
    }else if(s.scope != currentScope){ 
      ArraySymbol as = new ArraySymbol($ID.text,$ad.tree,(BaseScope)currentScope);
      as.setArgument();
      if(!$typeStatement.isEmpty())
        as.setType($typeStatement::type, $typeStatement::typelen);
      $ID.symbol = as; 
      s.scope.define(as);      
      //System.out.println(s.name+"array is not defined inside:"+s.scope.getScopeName());
    }
  }
}
:
/*
We should avoid using arrayname, instead using NAME. Because symbol should be linked directly to TOKEN, not the tree of any grammar return type.
It will be okay just change in symtableCreator.g. arrayname basically equals to the NAME.
*/
  ^(ARRAY ID=NAME ad=arrayDeclaratorExtents)
  ;

arrayDeclaratorExtents :
  ^(ADEXTS arrayDeclaratorExtent arrayDeclaratorExtent*);

arrayDeclaratorExtent : 
  ^(ADEXT expression expression?)
  ;

/* 17 */
equivalenceStatement : 
  ^(T_EQUIVALENCE equivEntityGroup+);

/* 18 */
equivEntityGroup 
@after
{
/*  if($v1.vs.getType() != $v2.vs.getType()){
    System.err.println("Equivalences between different types are disallowed");
    System.err.println("The problematic equivalence is between "+$v1.vs.name + " and "+$v2.vs.name);    
//    System.exit(-1);
  } 
 */
  //System.out.println($v1.vs.name);
  ((Symbol)$v1.vs).setEquivalence((BaseScope)currentScope,$v1.tree,$v2.tree);
}
: 
  ^(EQUIVGRP v1=varRef v2=varRef);

/* 19 */
commonStatement :
  ^(T_COMMON (commonBlock+ |commonItems)) ;

commonName 
@after{
  if (ID == null)
      $commonBlock::commonname = "unnamedcmnBlk";
  else 
      $commonBlock::commonname = $ID.text;
  $commonBlock::iposition = 0;    
}
: 
  T_DIV ((ID=NAME T_DIV) | T_DIV) ;

commonItem :
  ID=NAME 
  {
    $commonBlock::iposition++;
    Symbol vs = currentScope.resolve($ID.text);
    if(vs == null){
      vs = new VariableSymbol($ID.text,Symbol.getDataType($ID.text));      
      currentScope.define(vs);
    }else{
      vs = currentScope.resolve($ID.text);
    }
    vs.bcommonblock = true;
    vs.commonName = $commonBlock::commonname;
    vs.ipositionInBlk = $commonBlock::iposition;    
    $ID.symbol = vs;
  }
  | arrayDeclarator ;

commonItems :
  commonItem+ ;

commonBlock
scope
{
  boolean bcommonblock;
  String commonname;
  int iposition;
}
@init{$commonBlock::bcommonblock = true;}
@after{$commonBlock::bcommonblock = false;}
:
  commonName commonItems ;

dataStatement:
  ^(T_DATA dataBlock+)
  ;

dataBlock:
  ID=NAME{
    Symbol vs = currentScope.resolve($ID.text);
    if(vs == null){
      vs = new VariableSymbol($ID.text,Symbol.getDataType($ID.text));
      $ID.symbol = vs;
      currentScope.define(vs);
    }else{
      $ID.symbol = currentScope.resolve($ID.text);
    }
  } T_DIV (RCON | DCON | ICON) T_DIV
  ;
  
typeStatement
scope {
  Type.typeenum type;
  int typelen;
}
:
  ^(TYPESTMT type typeStatementNameList)
  ;

typeStatementNameList :
  typeStatementName (typeStatementName)*
  ;
  
typeStatementName :
  ID=NAME{
    Symbol vs = currentScope.resolve($ID.text);
    if(vs == null){
      vs = new VariableSymbol($ID.text,Symbol.getDataType($ID.text));
      vs.setType($typeStatement::type, $typeStatement::typelen);
      $ID.symbol = vs;
      currentScope.define(vs);
    }else{
      if(vs.scope != currentScope)
        vs.setArgument();
      currentScope.resolve($ID.text).setType($typeStatement::type, $typeStatement::typelen);
      $ID.symbol = currentScope.resolve($ID.text);
    }
  } 
  |arrayDeclarator 
  ;
  
typename 
@init{
  if(!$typeStatement.isEmpty()){
    $typeStatement::type = Type.typeenum.INVALID;
  }
}
:
  /* typelen's are meant more so for equivalence computations than anything else. I am not expecting an equivalence 
     on LOGICAL datatype. In the case of characters, it is a rule that lenSpecification be part of the definition.
     Variable definitions which don't come through the type statement have their typelen set to 4 in the Symbol class.
     We are dealing with 32-bit arithmetic here. If the user needs 64-bit, we will deal with it later. If we don't set
     the typelen below and if this statement doesn't contain lenSpecification, we would overwrite the default value 
     created in Symbol class with garbage for integers and floats.
   */   
  (T_REAL  {if(!$typeStatement.isEmpty()) {$typeStatement::type = Type.typeenum.REAL; $typeStatement::typelen = 4;}}
   |T_PRECISION {if(!$typeStatement.isEmpty()) {$typeStatement::type = Type.typeenum.DOUBLE; $typeStatement::typelen = 8;}}
   |T_INTEGER {if(!$typeStatement.isEmpty()) {$typeStatement::type = Type.typeenum.INTEGER; $typeStatement::typelen = 4;}}  
   |T_LOGICAL {if(!$typeStatement.isEmpty()) $typeStatement::type = Type.typeenum.LOGICAL;}
   |T_CHARACTER {if(!$typeStatement.isEmpty()) {$typeStatement::type = Type.typeenum.CHARACTER; $typeStatement::typelen = 1;}}
   )
  ;
  
type : 
  typename typenameLen?;

typenameLen 
@after{
  if(!$typeStatement.isEmpty()){
    $typeStatement::typelen = $ls.typelen;
  }
}
:
  ^(T_STAR ls=lenSpecification);

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
  implicitSpec+;

implicitNone : 
  T_NONE ;

implicitLetter : 
  n=NAME ; 
  
implicitRange : 
  ^(IMPLICITRANGE implicitLetter implicitLetter?);

implicitLetters : 
  implicitRange+ ;

/* 22 */
lenSpecification returns [int typelen]
@init{
  $typelen = 4;  // Default to 4.
}
:  
  i=ICON {$typelen = Integer.parseInt($i.text);} 
  |T_LPAREN expression T_RPAREN
  ;

useStatement:
  ^(T_USE NAME)
;

/* 23 */
parameterStatement : 
  ^(T_PARAMETER paramlist);

paramlist : paramassign ( paramassign )* ;

paramassign 
@after
{
  if(currentScope.resolve($ID.text)==null){
    ParamSymbol ps = new ParamSymbol($ID.text);
    $ID.symbol = ps;
    currentScope.define(ps);
    ps.updateValue((BaseScope)currentScope,$ID.text,$e.text);
    //System.out.println("Parameter:"+ps.name+" at "+currentScope.getScopeName()+" with "+ps.value);  
  }
}
: 
  ^(T_ASSIGN ID=NAME e=expression);

/* 24 */
externalStatement :
  ^(T_EXTERNAL namelist);

/* 25 */
intrinsicStatement : 
  T_INTRINSIC namelist 
  ;

/* 26 */
saveStatement : 
  ^(T_SAVE (s+=saveEntity+)?);

saveEntity :  
    NAME 
  | ^(T_DIV NAME T_DIV) ;

volatileStatement : 
  ^(T_VOLATILE varRef+)
  ;
  
/* 29 */
assignmentStatement : 
  ^(T_ASSIGN varRef expression)
  ;

/* 30 */
gotoStatement : 
  ^(GOTO unconditionalGoto)
  |^(GOTO assignedGoto)
  ;

/* 31 */
unconditionalGoto : 
  lblRef ;

lblRef
@after{
  LabelSymbol tmp = lbltab.getSymbol($l.text,currentScope);
  if((tmp != null) && (tmp.scope == currentScope)){
    $l.symbol = tmp;
    //System.out.println("label "+$l.text+" is in LabelTable"+" in scope "+tmp.scope.getScopeName());
  }else{
    //System.out.println("label "+$l.text+" isn't in LabelTable and label will be added into table in "+currentScope.getScopeName());
    LabelSymbol ls = new LabelSymbol($l.text);
    ls.scope = currentScope;
    $l.symbol = ls;
    lbltab.addSymbol(ls);
  }
}
:
  ^(LABELREF l=ICON)
  ;

labelList : 
  lblRef (lblRef)* ;

/* 33 */
assignedGoto : 
  NAME (labelList)? ;

/* 34 */
ifStatement :
  ^(T_IF expression blockIfStatement)
  | ^(T_IF expression logicalIfStatement)
//  | ^(T_IF expression arithmeticIfStatement)
  ;
  
//arithmeticIfStatement : 
//  lblRef  lblRef  lblRef ;

/* 35 */
logicalIfStatement : 
  ^(THENBLOCK executableStatement)
  ;

/* 36 */

firstIfBlock :
  ^(THENBLOCK wholeStatement*)
  ;

blockIfStatement : 
  firstIfBlock
  (elseIfStatement)*
  (elseStatement)?
//  endIfStatement
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
  ^(T_DO (doWithEndDo|doWithLabel) );

doVarArgs :
  ^(DOVARARG identifier expression 
  expression (expression )?)
  ;

doWithLabel :
  lblRef 
  doVarArgs 
  doBody{isVectorLoop = false;}
//  enddoStatement
  ;

doBody :
  ^(DOBLOCK wholeStatement*) 
  ;

doWithEndDo :
  doVarArgs
  doBody{isVectorLoop = false;}
//  enddoStatement
  ;
  
enddoStatement : 
  (T_ENDDO! | T_END! T_DO!| continueStatement) ;

/* 41 */
continueStatement 
@after{
  if($l != null){
    LabelSymbol tmp = lbltab.getSymbol($l.text,currentScope);
    if((tmp != null) && (tmp.scope == currentScope)){
      $l.symbol = tmp;
      //System.out.println("label "+$l.text+" is in LabelTable"+" in scope "+tmp.scope.getScopeName());
    }else{
      //System.out.println("label "+$l.text+" isn't in LabelTable and label will be added into table in "+currentScope.getScopeName());
      LabelSymbol ls = new LabelSymbol($l.text);
      ls.scope = currentScope;
      $l.symbol = ls;
      lbltab.addSymbol(ls);
    }
  }
}
: 
  ^(T_CONTINUE (l=LABEL)?) ;

/* 42 */
stopStatement : 
  ^(T_STOP ICON?);

/* 43 */
pauseStatement : 
  ^(T_PAUSE ICON?);


iocontrolArgs
//@after{if($l.tree != null) System.out.println($l.tree.toStringTree());}
:
  T_LPAREN
  (ICON|varRef|T_STAR)
  (T_COMMA
  (l=lblRef|T_STAR))?
  T_RPAREN
;

/* 44 */
writeStatement : 
  ^(T_WRITE iocontrolArgs (~EOS)*);

/* 45 */
readStatement : 
  ^(T_READ iocontrolArgs (~EOS)*);

/* 46 */
printStatement : 
  ^(T_PRINT (~EOS)+);

/* 50 */
openStatement : 
  ^(T_OPEN (~EOS)+);

/* 51 */
closeStatement : 
  ^(T_CLOSE (~EOS)+);

/* 52 */
inquireStatement : 
  ^(T_INQUIRE (~EOS)+);

/* 53 */
backspaceStatement : 
  ^(T_BACKSPACE (~EOS)+);

/* 54 */
endfileStatement : 
  ^(T_ENDFILE (~EOS)+);

/* 55 */
rewindStatement : 
  ^(T_REWIND (~EOS)+);

/* 58-59 */
formatStatement 
@after{
  if($l != null){
    LabelSymbol tmp = lbltab.getSymbol($l.text,currentScope);
    if((tmp != null) && (tmp.scope == currentScope)){
      $l.symbol = tmp;
      //System.out.println("label "+$l.text+" is in LabelTable"+" in scope "+tmp.scope.getScopeName());
    }else{
      //System.out.println("label "+$l.text+" isn't in LabelTable and label will be added into table in "+currentScope.getScopeName());
      LabelSymbol ls = new LabelSymbol($l.text);
      ls.scope = currentScope;
      $l.symbol = ls;
      lbltab.addSymbol(ls);
    }
  }
}
: 
  ^(T_FORMAT (l=LABEL)? (~EOS)+);

/* 70 */
statementFunctionStatement : 
  ^(T_LET (~EOS)+);

/*Function_reference*/
functionReference :
  ^(FUNCREF functionName functionArgumentList);
  
functionArgumentList: 
  ^(FUNCARG functionArgument*);

functionArgument :
  expression;
  
/* 71 */
callStatement : 
  ^(T_CALL subroutineCall);

subroutineCall 
@after{
  Symbol symbol = new Symbol($ID.text);
  symbol.scope = currentScope;
  $ID.symbol = symbol;
}
:
  (ID=NAME callArgumentList?);
 
callArgumentList : 
  ^(CALLARG callArgument+);
  
callArgument : 
  expression;

/* 72 */
returnStatement : 
  ^(T_RETURN expression?);

/* 74 */
expression : 
  
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
  ;


concatOp :
  T_DIV T_DIV 
  ;

/* 88 */
arrayElementName : 
  ^(NAME expression+);

subscripts :
  ^(SUBSCRIPT expression+) ;

varRef returns [Symbol vs]
@after
{
  //Symbol vs;
  if((currentScope.resolve($ID.text)==null) && ($vs == null) ){
    $vs = new VariableSymbol($ID.text,Symbol.getDataType($ID.text),isVectorLoop);
    $ID.symbol = $vs;
    currentScope.define($vs);  
  }else{
    $vs = currentScope.resolve($ID.text);
    if($vs.scope != currentScope){
       $vs.setArgument();
    }
    $vs.setVectortype((BaseScope)currentScope,isVectorLoop);
    $ID.symbol = $vs;
  }
}
:
  ID=NAME
  |^(ARRAYREF ID=NAME s=subscripts);

//substringApp :
//  ^(T_COLON lexpr2? lexpr2?);

/* 92 */
arrayName :  
  NAME;

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
  |NAME 
  ;

/* 101 */
unsignedArithmeticConstant : 
  ICON 
  | RCON
  | DCON
  ;

/* 108 */
logicalConstant : 
  (T_TRUE | T_FALSE)
  ;

identifier returns[Symbol vs]
@after{
  //Symbol vs;
  if((currentScope.resolve($ID.text)==null)&&($vs==null)){
	  $vs = new VariableSymbol($ID.text,Symbol.getDataType($ID.text), isVectorLoop);
	  $ID.symbol = $vs;
	  currentScope.define($vs);
    //System.out.println("identifier:"+$ID.text+" "+currentScope.getScopeName());
  }else{
    $vs = currentScope.resolve($ID.text);
    if($vs.scope != currentScope){
        $vs.setArgument();
    }
    $vs.setVectortype((BaseScope)currentScope,isVectorLoop);
    $ID.symbol = $vs;
  }
}
:
  ID=NAME 
  ;

//to : 
//  n=NAME {$n.getText().compareToIgnoreCase("to") == 0}? {$n.setType(TO);} ;

directives 
@after{
  if(list_n.size() == 2){
    String tmp1 = ((FTree)list_n.get(0)).toString();
    String tmp2 = ((FTree)list_n.get(1)).toString();
    if((tmp1.contentEquals("vector")) && (tmp2.contentEquals("always"))) isVectorLoop = true;
    if((tmp1.contentEquals("vector")) && (tmp2.contentEquals("end"))) isVectorLoop = false;
  }
}
:
    ^(DIRECTIVE (T_AND)? (n+=~EOS)+)
  ;
  
ppmdirectives 
@after {
 Symbol ps = new Symbol("ppm"); 
 ps.scope = currentScope;
 $PD.symbol = ps; 
}:
    ^(PD=PPMDIRECTIVE (~EOS)+)
  ;

ppmdecldirectives  
@after {
 Symbol ps = new Symbol("ppm"); 
 ps.scope = currentScope;
 $PD.symbol = ps; 
}:
     ^(PD=PPMDIRECTIVE (~EOS)+)
  ;

