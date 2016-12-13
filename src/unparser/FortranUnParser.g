//===------------- FortranUnParser.g - backend code generation ------------===//
//
//  This is an ANTLR v3 grammar to translate the Fortran AST to output code
//
//===----------------------------------------------------------------------===//
//
//  Developed by Jagan Jayaraj and Pei-Hung Lin
//  Copyright 2007-2013, Regents of the University of Minnesota
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

tree grammar FortranUnParser;

options {
  language = Java;
  backtrack=true;
  output = template;
  tokenVocab = tokenFile;
  ASTLabelType = FTree;
}

@header{
package unparser;
import lexer.*;
import symbol.*;
import translator.*;
}

@members{
  String prefix="";
  SymbolTable symtab;
  Scope currentScope;
  int ifCount = 0;
  public FortranUnParser(TreeNodeStream input, SymbolTable symtab) {
      this(input);
      this.symtab = symtab;
      currentScope = symtab.globals;
  }
}

program :
  ^(CODEROOT (p+=ppmdirectives|p+=directives|p+=executableUnit)+) -> program(prefix={prefix},values={$p})
  ;

/* one unit of a fortran program */
executableUnit 
@after{prefix="";}
:
  e1=functionSubprogram -> executableUnit(value={$e1.st})
  | e2=mainProgram -> executableUnit(value={$e2.st})
  | e3=subroutineSubprogram -> executableUnit(value={$e3.st})
  | e4=blockdataSubprogram-> executableUnit(value={$e4.st})
  ;

/* 2 */
mainProgram : /* Dont understand this */
  ^(MAINPROG p=programStatement s=subprogramBody) -> mainProgram(p={$p.st},s={$s.st})
  ;

/* 3 */
functionSubprogram :
  ^(FUNCTION f=functionStatement s=subprogramBody) -> functionSubprogram(f={$f.st},s={$s.st})
  ;

/* 4 */
subroutineSubprogram :
  ^(SUBROUTINE s1=subroutineStatement s2=subprogramBody) -> subroutineSubprogram(s1={s1.st},s2={s2.st})
  
  ;

/* 5 - blockDataSubprogram */
blockdataSubprogram :
  ^(BLOCKDATA b=blockdataStatement s=subprogramBody) -> blockdataSubprogram(b={$b.st},s={$s.st})
  ;

/* 6 */
otherSpecificationStatement :
     o1=dimensionStatement{prefix="";} -> otherSpecificationStatement(os={$o1.st})
    |o2=allocatableStatement{prefix="";} -> otherSpecificationStatement(os={$o2.st})
//    |o3=allocateStatement{prefix="";} -> otherSpecificationStatement(os={$o3.st})
    |o4=equivalenceStatement{prefix="      ";} -> otherSpecificationStatement(os={$o4.st})
    |o5=intrinsicStatement{prefix="      ";} -> otherSpecificationStatement(os={$o5.st})
    |o6=saveStatement{prefix="      ";}-> otherSpecificationStatement(os={$o6.st})
    |o7=volatileStatement{prefix="      ";}-> otherSpecificationStatement(os={$o7.st})
//    |o7=macroStatement{prefix="      ";}-> otherSpecificationStatement(os={$o7.st})
  ;

/* 7 */
executableStatement :
   e1=assignmentStatement{prefix="      ";}  -> executableStatement(es={$e1.st})
  |e2=gotoStatement{prefix="      ";}  -> executableStatement(es={$e2.st})
  |e3=ifStatement{prefix="      ";}  -> executableStatement(es={$e3.st})
  |e4=doStatement{prefix="      ";}  -> executableStatement(es={$e4.st})
  |e18=continueStatement{prefix="";} -> executableStatement(es={$e18.st})
  |e5=stopStatement{prefix="      ";}  -> executableStatement(es={$e5.st})
  |e6=pauseStatement{prefix="      ";} -> executableStatement(es={$e6.st})
  |e7=readStatement{prefix="      ";} -> executableStatement(es={$e7.st})
  |e8=writeStatement{prefix="      ";} -> executableStatement(es={$e8.st})
  |e9=printStatement{prefix="      ";} -> executableStatement(es={$e9.st})
  |e10=rewindStatement{prefix="      ";} -> executableStatement(es={$e10.st})
  |e11=backspaceStatement{prefix="      ";} -> executableStatement(es={$e11.st})
  |e12=openStatement{prefix="      ";} -> executableStatement(es={$e12.st})
  |e13=closeStatement{prefix="      ";} -> executableStatement(es={$e13.st})
  |e14=endfileStatement{prefix="      ";} -> executableStatement(es={$e14.st})
  |e15=inquireStatement{prefix="      ";} -> executableStatement(es={$e15.st})
  |e16=callStatement{prefix="      ";} -> executableStatement(es={$e16.st})
  |e17=returnStatement{prefix="      ";} -> executableStatement(es={$e17.st})
  ;

/* 8 */
programStatement :
  ^(T_PROGRAM NAME) -> programStatement(prefix={prefix="      "},n={$NAME.text})
  ;

seos : NEWLINE ;

/* 9, 11, 13 */
/* 10 */
functionStatement :
  ^(T_FUNCTION (type)? f=functionName namelist?) -> functionStatement(prefix={prefix="      "},t={$type.st},n={$f.st},nl={$namelist.st})
  
  ;

blockdataStatement :
  ^(T_BLOCK NAME) -> blockdataStatement(prefix={prefix="      "},n={$NAME.text})
  ;

/* 12 */
subroutineStatement:
  ^(T_SUBROUTINE s=subroutineName namelist?) ->subroutineStatement(prefix={prefix="      "},n={$s.st},nl={$namelist.st})
  ;
  
namelist :
  ^(SUBARG i+=identifier+) -> namelist(values={$i})
  ;

//statement :
//   s1=formatStatement{prefix="";}  -> statement(s={$s1.st})
//  |s2=includeStatement{prefix="      ";}  -> statement(s={$s2.st})
//  |s3=implicitStatement{prefix="      ";}  -> statement(s={$s3.st})
//  |s4=useStatement{prefix="      ";} -> statement(s={$s4.st})
//  |s5=parameterStatement{prefix="      ";}  -> statement(s={$s5.st})
//  |s6=typeStatement{prefix="      ";}  -> statement(s={$s6.st})
//  |s7=commonStatement{prefix="      ";}  -> statement(s={$s7.st})
//  |s8=externalStatement{prefix="      ";}  -> statement(s={$s8.st})
//  |s9=otherSpecificationStatement   -> statement(s={$s9.st})
//  |s10=statementFunctionStatement{prefix="      ";}  -> statement(s={$s10.st})
//  |s11=executableStatement -> statement(s={$s11.st})
//  ;

subprogramBody :
  ^(SUBPROGRAMBLOCK d=declarationBlock e=executionBlock) -> subprogramBody(d={$d.st},e={$e.st})
  ;

declarationBlock :
  ^(DECLARATIONBLOCK d+=declarationStatement*) -> declarationBlock(d={$d})
  ;

executionBlock:
  ^(EXECUTIONBLOCK w+=wholeStatement*) -> executionBlock(w={$w})
  ;

declarationStatement
/*@after{
  System.out.println(retval.st.toString());
  System.out.flush();
}*/
:
    ppmdecldirectives{prefix="";} -> declarationStatement(prefix={prefix},s={$ppmdecldirectives.st})
/*    directives{prefix="";} -> declarationStatement(prefix={prefix},s={$directives.st})
 *                  ;   c$OMP MASTER which belongs to the execution block 
 *                      gets added to the declaration region if it is the
 *                      very first statement in the execution region.  
 *                      This affects any declaration block statements we 
 *                      append to the block.  Technically we don't have any
 *                      OMP directives in the declarationStatement yet.
 *                      If we have to add them in the future (after 09/24/12)
 *                      we have to draw a distinction between the declaration
 *                      block OMP statements and execution region OMP statements
 *                      All the passes need to be modified.
 */
  |includeStatement{prefix="      ";} -> declarationStatement(prefix={prefix},s={$includeStatement.st})
  |implicitStatement{prefix="      ";} -> declarationStatement(prefix={prefix},s={$implicitStatement.st})
  |useStatement{prefix="      ";} -> declarationStatement(prefix={prefix},s={$useStatement.st})
  |parameterStatement{prefix="      ";} -> declarationStatement(prefix={prefix},s={$parameterStatement.st})
  |typeStatement{prefix="      ";} -> declarationStatement(prefix={prefix},s={$typeStatement.st})
  |commonStatement{prefix="      ";} -> declarationStatement(prefix={prefix},s={$commonStatement.st})
  |dataStatement{prefix="      ";} -> declarationStatement(prefix={prefix},s={$dataStatement.st})
  |externalStatement{prefix="      ";} -> declarationStatement(prefix={prefix},s={$externalStatement.st})
  |otherSpecificationStatement -> declarationStatement(prefix={prefix},s={$otherSpecificationStatement.st})
  ;

macroStatement :
  MACRO (~EOS)+
  ;
  
wholeStatement
/*@after{
  System.out.println("Just processed: "+retval.st.toString());
  System.out.flush();
}*/
:
    ppmdirectives{prefix="";} -> wholeStatement(prefix={prefix},s={$ppmdirectives.st})
  | directives{prefix="";} -> wholeStatement(prefix={prefix},s={$directives.st})
//  | statement -> wholeStatement(prefix={prefix},s={$statement.st})
//  |^(LABEL statement) 
  |formatStatement{prefix="";} -> wholeStatement(prefix={prefix},s={$formatStatement.st})
  |allocateStatement{prefix="";} -> wholeStatement(prefix={prefix},s={$allocateStatement.st})
//  |macroStatement!
  |statementFunctionStatement{prefix="      ";} -> wholeStatement(prefix={prefix},s={$statementFunctionStatement.st})
  |executableStatement -> wholeStatement(prefix={prefix},s={$executableStatement.st})
  ;

endStatement
@init{String comment = "";}
//@after{if($l != null) System.out.println(comment);}
:
   ^(T_END (l=LABEL{comment = "c LABEL was "+$l.symbol.name+"\n"; prefix="";})?) -> endStatement(prefix={prefix},l={$l.text},c={comment})
   ;

/* 15 */
dimensionStatement : 
  ^(T_DIMENSION a+=arrayDeclarator+) -> dimensionStatement(av={$a})
  ;
  
allocatableStatement :  
  ^(T_ALLOCATABLE e+=entityList+) -> allocatableStatement(ev={$e})
  ;
  
entityList :  
  a=arrayName d=deferredShapeSpecList? -> entityList(a={$a.st},d={$d.st})
  ; 
  
deferredShapeSpecList : 
  c+=T_COLON+ -> deferredShapeSpecList(cv={$c})
  ; 
  
allocateStatement : 
  ^(T_ALLOCATE a+=arrayDeclarator+) -> allocateStatement(av={$a})
  ;

/* 16 */
arrayDeclarator :
  ^(ARRAY n=arrayName a=arrayDeclaratorExtents) -> arrayDeclarator(n={$n.st},value={$a.st})
  ;

arrayDeclaratorExtents :
  ^(ADEXTS a+=arrayDeclaratorExtent a+=arrayDeclaratorExtent*) -> arrayDeclaratorExtents(values={$a})
  ;

arrayDeclaratorExtent 
@init{String column="";}
: 
  ^(ADEXT i1=expression (i2=expression{column = ":";})?) -> arrayDeclaratorExtent(i1={$i1.st},column={column},i2={$i2.st})
  ;

/* 17 */
equivalenceStatement : 
  ^(T_EQUIVALENCE e+=equivEntityGroup+) -> equivalenceStatement(ev={$e})
  ;

/* 18 */
equivEntityGroup : 
  ^(EQUIVGRP e+=varRef+) -> equivEntityGroup(ev={$e})
  ;

/* 19 */
commonStatement :
  ^(T_COMMON (c+=commonBlock+ |c+=commonItems)) -> commonStatement(cv={$c})
  ;

commonName : 
  T_DIV (c+=NAME T_DIV | T_DIV) -> commonName(cv={$c})
  ;

commonItem :
  n=NAME -> commonItem(c={$n.text})
  | c=arrayDeclarator -> commonItem(c={$c.st})
  ;

commonItems :
  c+=commonItem+ -> commonItems(cv={$c}) 
  ;

commonBlock :
  cn=commonName ci=commonItems -> commonBlock(cn={$cn.st},ci={$ci.st})
  ;

dataStatement:
  ^(T_DATA d+=dataBlock+) -> dataStatement(cv={$d})
  ;

dataBlock:
  n=NAME T_DIV (i=RCON | i=DCON | i=ICON) T_DIV -> dataBlock(cn={$n.text},ci={$i.text})
  ;
  
/* 20 */
// need to expand the typename rule to produce a better AST
// May need to work on it.
typeStatement :
  ^(TYPESTMT t=type tnl=typeStatementNameList) -> typeStatement(t={$t.st},tnl={$tnl.st})
  ;

typeStatementNameList :
  t+=typeStatementName (t+=typeStatementName)* -> typeStatementNameList(tv={$t})
  ;
  
typeStatementName :
  n=NAME -> typeStatementName(t={$n.text})
  |  t=arrayDeclarator -> typeStatementName(t={$t.st})
  ;
  
typename 
@init{
   String txt = "";
}:
  (  T_REAL      {txt = "real";}
   | T_PRECISION {txt = "double precision";}
   | T_INTEGER   {txt = "integer";}
   | T_LOGICAL   {txt = "logical";}
   | T_CHARACTER {txt = "character";}
   ) -> typename(t={txt})
  ;
  
type : 
  (tn=typename tl=typenameLen?) -> type(tn={$tn.st},tl={$tl.st})
  ;

typenameLen :
  ^(s=T_STAR l=lenSpecification) -> typenameLen(s={$s.text},l={$l.st}) 
  ;

includeStatement :  
  ^(T_INCLUDE s=SCON) -> includeStatement(s={$s})
  ;
  
/* 21 */
implicitStatement : 
  ^(T_IMPLICIT (i+=implicitNone | i+=implicitSpecs) ) -> implicitStatement(i={$i})
  ;

implicitSpec : 
  ^(t=type i=implicitLetters+) -> implicitSpec(t={$t.st},i={$i.st})
  ;

implicitSpecs : 
  i+=implicitSpec+ -> implicitSpecs(iv={$i})
  ;

implicitNone : 
  T_NONE -> implicitNone()
  ;

implicitLetter : 
  n=NAME -> implicitLetter(l={$n})
  ; 
  
implicitRange : 
  ^(IMPLICITRANGE i1=implicitLetter i2=implicitLetter?) -> implicitRange(i1={$i1.st}, i2={$i2.st})
  ;

implicitLetters : 
  i+=implicitRange+ -> implicitLetters(iv={$i})
  ;

/* 22 */
lenSpecification :  
  l=ICON -> lenSpecification(l={$l.text})
  |T_LPAREN e=expression T_RPAREN -> lenSpecification(l={$e.st})
  ;

useStatement:
  ^(T_USE n=NAME) -> useStatement(n={$n})
  ;

/* 23 */
parameterStatement : 
  ^(T_PARAMETER p=paramlist) -> parameterStatement(p={$p.st})
  ;

paramlist : p+=paramassign ( p+=paramassign )*-> paramlist(pv={$p})
  ;

paramassign : 
  ^(a=T_ASSIGN n=NAME e=expression) -> paramassign(n={$n},a={$a},e={$e.st})
  ;

/* 24 */
externalStatement :
  ^(T_EXTERNAL n=namelist) -> externalStatement(n={$n.st})
  ;

/* 25 */
intrinsicStatement : 
  T_INTRINSIC n=namelist -> instrinsicStatement(n={$n.st})
  ;

/* 26 */
saveStatement : 
  ^(T_SAVE (s+=saveEntity+)?) -> saveStatement(sv={$s})
  ;

saveEntity :  
    s+=NAME 
  | ^(s+=T_DIV s+=NAME s+=T_DIV) -> saveEntity(sv={$s})
  ;

volatileStatement : 
  ^(T_VOLATILE (n+=NAME)+) -> volatileStatement(sv={$n})
  ;

/* 29 */
assignmentStatement : 
  ^(T_ASSIGN vr=varRef e=expression) -> assignmentStatement(vr={$vr.st},e={$e.st})
  ;

/* 30 */
gotoStatement : 
  ^(GOTO ug=unconditionalGoto ) -> gotoStatement(g={$ug.st})
  |^(GOTO ag=assignedGoto) -> gotoStatement(g={$ag.st})
  ;

/* 31 */
unconditionalGoto
@init{String comment = "";}
//@after{System.out.println(comment);}
: 
  l=lblRef{comment = "c LABEL was "+$l.name;} -> unconditionalGoto(l={$l.st},c={comment})
  ;

lblRef returns [String name;]
@after{
  $lblRef.name = $l.symbol.name;
//  System.out.println("lblref was:"+$l.symbol.name);
}
:
  ^(LABELREF l=ICON) -> lblRef(l={$l})
  ;

labelList : 
  l+=lblRef (l+=lblRef)* -> labelList(lv={$l})
  ;

/* 33 */
assignedGoto : 
  n=NAME (l=labelList)? -> assignedGoto(n={$n},l={$l.st})
  ;

/* 34 */
ifStatement :
  ^(T_IF e=expression b=blockIfStatement) -> ifStatement(e={$e.st},b={$b.st})
  | ^(T_IF e=expression l=logicalIfStatement) -> ifStatement(e={$e.st},b={$l.st})
//  | ^(T_IF expression arithmeticIfStatement)
  ;
  
//arithmeticIfStatement : 
//  lblRef  lblRef  lblRef ;

/* 35 */
logicalIfStatement : 
  ^(THENBLOCK e=executableStatement) -> logicalIfStatement(e={$e.st})
  ;

/* 36 */
blockIfStatement : 
  f=firstIfBlock
  (ei+=elseIfStatement)*
  (es=elseStatement)? -> blockIfStatement(f={$f.st},eiv={$ei},es={$es.st},end={""})
//  endIfStatement
  ;

firstIfBlock :
  ^(THENBLOCK w+=wholeStatement*) -> firstIfBlock(w={$w})
  ;

/* 37 */
elseIfStatement : 
  ^(ELSEIF e=expression ^(THENBLOCK w+=wholeStatement*)) -> elseIfStatement(e={$e.st},wv={$w})
  
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
  ^(ELSEBLOCK wv+=wholeStatement*) -> elseStatement(wv={$wv})
  ;

/* 39 */
//endIfStatement : 
//  (T_ENDIF! | T_END! T_IF!) ;

/* 40 */
doStatement : 
  ^(T_DO (de=doWithEndDo|dl=doWithLabel) ) -> doStatement(dl={$dl.st},de={$de.st})
  ;

doVarArgs 
@init{
  String increment = "1";
}
:
  ^(DOVARARG i=identifier e1=expression 
  e2=expression (e3=expression{increment = $e3.st.toString();})?) -> doVarArgs(i={$i.st},e1={$e1.st},e2={$e2.st},e3={increment})
  ;

doWithLabel
@init{String comment = "";}
//@after{System.out.println(comment);}
:
  (l=lblRef {comment = "c LABEL was "+$l.name+"\n";}
  d=doVarArgs 
  dd=doBody) -> doWithLabel(l={$l.st},d={$d.st},dd={$dd.st},c={comment})
  //enddoStatement
  ;

doBody :
  ^(DOBLOCK w+=wholeStatement*) -> doBody(wv={$w})
  ;

doWithEndDo :
  d=doVarArgs
  db=doBody -> doWithEndDo(d={$d.st},db={$db.st})
//  enddoStatement
  ;
  
enddoStatement : 
  (T_ENDDO | T_END T_DO| continueStatement) ->enddoStatement()
  ;

/* 41 */
continueStatement 
@init{String comment = "";}
//@after{if($l != null) System.out.println(comment);}
: 
  ^(T_CONTINUE (l=LABEL{comment = "c LABEL was "+$l.symbol.name+"\n";})?) -> continueStatement(l={$l},c={comment})
  ;

/* 42 */
stopStatement : 
  ^(T_STOP i=ICON?) -> stopStatement(i={$i})
  ;

/* 43 */
pauseStatement : 
  ^(T_PAUSE i=ICON?) -> pauseStatement(i={$i})
  ;

iocontrolArgs
@init{String a="", b="";}
:
  T_LPAREN
  (i=ICON{a = $i.text; }|v=varRef{a = $v.st.toString();}|t=T_STAR{a = $t.text;})
  (T_COMMA{b = ",";}
  (l=lblRef{b = b.concat($l.st.toString());}|t=T_STAR{b = b.concat($t.text);}))?
  T_RPAREN -> iocontrolArgs(a={a},b={b})
;


/* 44 */
writeStatement : 
  ^(T_WRITE i=iocontrolArgs (n+=~EOS)*) -> writeStatement(i={$i.st},n={$n})
  ;

/* 45 */
readStatement : 
  ^(T_READ i=iocontrolArgs (n+=~EOS)*) -> readStatement(i={$i.st},n={$n})
  ;

/* 46 */
printStatement : 
  ^(T_PRINT (n+=~EOS)+) -> printStatement(n={$n})
  ;

/* 50 */
openStatement : 
  ^(T_OPEN (n+=~EOS)+) -> openStatement(n={$n})
  ;

/* 51 */
closeStatement : 
  ^(T_CLOSE (n+=~EOS)+) -> closeStatement(n={$n})
  ;

/* 52 */
inquireStatement : 
  ^(T_INQUIRE (n+=~EOS)+) -> inquireStatement(n={$n})
  ;

/* 53 */
backspaceStatement : 
  ^(T_BACKSPACE (n+=~EOS)+) -> backspaceStatement(n={$n})
  ;

/* 54 */
endfileStatement : 
  ^(T_ENDFILE (n+=~EOS)+) -> endfileStatement(n={$n})
  ;

/* 55 */
rewindStatement : 
  ^(T_REWIND (n+=~EOS)+) -> rewindStatement(n={$n})
  ;

/* 58-59 */
formatStatement
@init{String comment = "";}
//@after{if($l != null) System.out.println(comment);}
: 
  ^(T_FORMAT (l=LABEL{comment = "c LABEL was "+$l.symbol.name+"\n";})? (n+=~EOS)+) -> formatStatement(n={$n},l={$l},c={comment})
  ;

/* 70 */
statementFunctionStatement : 
  ^(T_LET (n+=~EOS)+) -> statementFunctionStatement(n={$n})
  ;

/*Function_reference*/
functionReference :
  ^(FUNCREF /*{System.out.println(((FTree)input.LT(2)).getType());}*/ f1=functionName f2=functionArgumentList) -> functionReference(f1={$f1.st},f2={$f2.st})
  ;
  
functionArgumentList: 
  ^(FUNCARG f+=functionArgument*) -> functionArgumentList(f={$f})
  ;

functionArgument :
  e=expression  -> functionArgument(e={$e.st})
  ;
  
/* 71 */
callStatement : 
  ^(T_CALL s=subroutineCall)  -> callStatement(s={$s.st})
  ;

subroutineCall :
  (n=subroutineName c=callArgumentList?) -> subroutineCall(n={$n.st},c={$c.st})
  ;

callArgumentList : 
  ^(CALLARG c+=callArgument+) -> callArgumentList(cv={$c})
  ;
  
callArgument : 
  e=expression-> callArgument(e={$e.st})
  ;

/* 72 */
returnStatement : 
  ^(T_RETURN i=expression?) -> returnStatement(i={$i.st})
  ;

/* 74 */
expression
: 
    ^(op=T_LOR e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_LAND e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_LNOT e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_LT e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_LE e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_EQ e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_NE e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_GT e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_GE e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_PLUS e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_MINUS e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_STAR e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=T_DIV e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=NEG e1=expression) -> negvar(e1={$e1.st})
  | ^(op=T_POWER e1=expression e2=expression) -> expression(op={$op}, e1={$e1.st}, e2={$e2.st})
  | ^(op=MADD e1=expression e2=expression e3=expression) -> madd(op={"+"}, e1={$e1.st}, e2={$e2.st}, e3={$e3.st})
  | ^(op=MSUB e1=expression e2=expression e3=expression) -> madd(op={"-"}, e1={$e1.st}, e2={$e2.st}, e3={$e3.st})
  | uac=unsignedArithmeticConstant -> expressionuac(uac={$uac.st})
  | s=SCON  -> stringconstant(s={$s.text})
  | vr=varRef -> expressionvr(vr={$vr.st})
  | f=functionReference -> expressionfunc(f={$f.st})
  ;


concatOp :
  T_DIV T_DIV 
  ;

/* 88 */
arrayElementName : 
  ^(n=NAME e+=expression+) -> arrayElementName(n={$n},iv={$e})
  ;

subscripts :
  ^(SUBSCRIPT e+=expression+) -> subscripts(ev={$e})
  ;

varRef 
@init{String name="";}
:
  n=NAME
  {
    if($n.symbol.isParameter()){
      name = ((ParamSymbol)$n.symbol).value;
    }else{
      name = $n.text;
    }
  }  -> varRef(n={name}, s={""})
  |^(ARRAYREF n=NAME
  {
    if($n.symbol.isParameter()){
      name = ((ParamSymbol)$n.symbol).value;
    }else{
      name = $n.text;
    }
  }
  s=subscripts?) -> varRef(n={name}, s={$s.st})
  ;

//substringApp :
//  ^(T_COLON lexpr2? lexpr2?);

/* 92 */
arrayName :  
  n=NAME-> arrayName(n={$n})
  ;

/* 97 */
subroutineName : 
  n=NAME -> subroutineName(n={$n})
  ;

implicitfunctionName
@after{
//  System.out.println("implicitfunctionname = "+retval.toString());
}: 
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
   i=implicitfunctionName /*{System.out.println("unparser implicit function name = "+$i.text);}*/ -> functionName(n={$i.text})
  |n=NAME -> functionName(n={$n.text})
  ;

/* 101 */
unsignedArithmeticConstant : 
  i=ICON -> unsignedArithmeticConstant(u={$i.text})
  | r=RCON -> unsignedArithmeticConstant(u={$r.text})
  | d=DCON -> unsignedArithmeticConstant(u={$d.text})
  ;

/* 108 */
logicalConstant : 
  (l=T_TRUE | l=T_FALSE) -> logicalConstant(l={$l.text})
  ;

identifier 
@init{String name="";}
:
  n=NAME
  {
    if($n.symbol.isParameter()){
      name = ((ParamSymbol)$n.symbol).value;
    }else{
      name = $n.text;
    }
  } -> identifier(i={name})
  ;

//to : 
//  n=NAME {$n.getText().compareToIgnoreCase("to") == 0}? {$n.setType(TO);} ;

directives 
@init{String t = "";}
:
    ^(d=DIRECTIVE (T_AND{t="&";})? (n+=~EOS)+) -> directives(d={$d.text+t},n={$n})
  ;
  
ppmdirectives :
//@after{
//  System.out.println("At the end of a PPM directive");
//  System.out.println(retval.st.toString());
//  //System.exit(-1);
//}:
    ^(d=PPMDIRECTIVE (n+=~EOS)+) -> directives(d={$d.text},n={$n})
  ;

ppmdecldirectives :
    ^(d=PPMDIRECTIVE (n+=~EOS)+) -> directives(d={$d.text},n={$n})
  ;
