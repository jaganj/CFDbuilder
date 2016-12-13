#===------------------------------- makefile -------------------------------===#
#
#  Usage: make package, make all, make
#
#===------------------------------------------------------------------------===#
#
#  Copyright 2016, Jagan Jayaraj
#
#  This file is part of CFDbuilder.
#
#  CFDbuilder is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  CFDbuilder is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with CFDbuilder.  If not, see <http://www.gnu.org/licenses/>.
#
#  Contacts: {jaganj,phlin}@cs.umn.edu, paul@lcse.umn.edu
#
#===------------------------------------------------------------------------===#

# WARNING:
#   Do not use multi-level makefiles. There are dependences in the build order,
# and you cannot enforce them with multi-level makefiles unless you have all
# the make calls in one line.


# No space between the libs. The line continuation character '\' adds a space which
# must be avoided.
ANTLRLIBS=../lib/jnlp.jar;../lib/appFramework-1.0.jar;../lib/antlr-3.3-complete.jar
ANTLRTOOL=org.antlr.Tool
# The make system itself can work with the linux slash path notation on 
# Windows systems. However, ANTLRGEN is used in make clean where "del" 
# and "rm" expect the right slash notation for the path. Therefore, we
# must keep the OS check below.
ifeq ($(OS), Windows_NT)
ANTLRGEN=..\antlr-generated
BIN=..\bin
PACKAGE_FILE=..\cfdbuilder.jar
else
ANTLRGEN=../antlr-generated
BIN=../bin
PACKAGE_FILE=../cfdbuilder.jar
endif
ANTLR=$(JVM) -cp $(ANTLRLIBS) $(ANTLRTOOL)

# Java options
JVM=java
JCC=javac
JCOPTS=-sourcepath .;$(ANTLRGEN) -classpath ../bin;$(ANTLRLIBS) -d ../bin

FTREE=translator/FTree.java
TOKENFILE=$(ANTLRGEN)/parser/tokenFile.tokens
#
# Creating explicit targets for the antlr generated .java files is useful for development.
# Without such an explicit rule, the intermediate .java files are deleted if they have
# syntax errors.
#
# It is not sufficient to just set a depedency against a grammar file.
# The dependents consume the output of a grammar file, not the grammar file directly.
# If the makesystem were to execute tasks parallely or if a dependent task were to
# fire off before the .tokens/.java files are produced from a grammar then it will
# be consuming the old .tokens/.java files even though the grammar has changed.
# However, it is sufficient to depend on just the .java file. The production of a new
# .java file also implies the production of a new .tokens file from the same grammar.
LEXER_B=$(BIN)/lexer/FortranLexer.class
PARSER_B=$(BIN)/parser/FortranParser.class \
	$(BIN)/parser/FortranTreeParser.class
INTEGEREVAL_B=$(BIN)/IntegerEvaluator/IntegerEvaluator.class \
	$(BIN)/IntegerEvaluator/IntegerEvaluatorLexer.class \
	$(BIN)/IntegerEvaluator/IntegerEvaluatorParser.class
SYMBOL_B=$(BIN)/symbol/ArraySymbol.class \
	$(BIN)/symbol/BaseScope.class \
	$(BIN)/symbol/FunctionSymbol.class \
	$(BIN)/symbol/GlobalScope.class \
	$(BIN)/symbol/LabelSymbol.class \
	$(BIN)/symbol/LabelTable.class \
	$(BIN)/symbol/LocalScope.class \
	$(BIN)/symbol/ParamSymbol.class \
	$(BIN)/symbol/Scope.class \
	$(BIN)/symbol/Symbol.class \
	$(BIN)/symbol/SymbolTable.class \
	$(BIN)/symbol/symtableCreator.class \
	$(BIN)/symbol/Type.class \
	$(BIN)/symbol/VariableSymbol.class
EQUIVALENCER_B=$(BIN)/equivalencer/equivalencer.class \
	$(BIN)/equivalencer/tmpEntry.class \
	$(BIN)/equivalencer/Tmps.class
INLINER_B=$(BIN)/inliner/fixDefinitions.class \
	$(BIN)/inliner/fixLabels.class \
	$(BIN)/inliner/fixReturnStmts.class \
	$(BIN)/inliner/fixVarRefs.class \
	$(BIN)/inliner/getSubroutineTree.class \
	$(BIN)/inliner/inliner.class \
	$(BIN)/inliner/printVarName.class \
	$(BIN)/inliner/varLenArrays.class \
	$(BIN)/inliner/equivVars.class \
	$(BIN)/inliner/uniqueDefValue.class \
	$(BIN)/inliner/varSubKey.class
PIPELINER_B=$(BIN)/pipeliner/pass1_delRedIter.class \
	$(BIN)/pipeliner/pass2_repack.class \
	$(BIN)/pipeliner/pass3_dblBuffer.class \
	$(BIN)/pipeliner/pass3_dblBuffer_modern.class \
	$(BIN)/pipeliner/pass4_findIterSpace.class \
	$(BIN)/pipeliner/pass4_ldLoop2if.class \
	$(BIN)/pipeliner/pass6_loopOverPlanes.class \
	$(BIN)/pipeliner/SimplifySubscript.class \
	$(BIN)/pipeliner/fixdblBufDefns.class \
	$(BIN)/pipeliner/Bound.class \
	$(BIN)/pipeliner/PipelinedTmps.class
REDUCEMEM_B=$(BIN)/reduceMem/reduceMem1.class \
	$(BIN)/reduceMem/reduceMem2.class
TRANSLATOR_B=$(BIN)/translator/renameLabel.class \
	$(BIN)/translator/TokenizePPMdir.class \
	$(BIN)/translator/updateFMA.class \
	$(BIN)/translator/Translator.class \
	$(BIN)/translator/ModAutoIndentWriter.class \
	$(BIN)/translator/FTree.class \
	$(BIN)/translator/FTreeAdaptor.class \
	$(BIN)/translator/FTreeVisitor.class \
	$(BIN)/translator/FTreeErrorNode.class \
	$(BIN)/translator/FBaseTree.class \
	$(BIN)/translator/FBaseTreeAdaptor.class \
	$(BIN)/translator/FCommonTree.class \
	$(BIN)/translator/FCommonTreeAdaptor.class \
	$(BIN)/translator/FCommonTreeNodeStream.class
UNPARSER_B=$(BIN)/unparser/FortranUnParser.class
STRINGTEMPLATE_B=$(BIN)/StringTemplate/AltiVec.stg \
	$(BIN)/StringTemplate/f2c.stg \
	$(BIN)/StringTemplate/f77.stg \
	$(BIN)/StringTemplate/SSE.stg

LEXER_G=$(ANTLRGEN)/lexer/FortranLexer.java
PARSER_G=$(ANTLRGEN)/parser/FortranParser.java \
	$(ANTLRGEN)/parser/FortranTreeParser.java
INTEGEREVAL_G=$(ANTLRGEN)/IntegerEvaluator/IntegerEvaluatorLexer.java \
	$(ANTLRGEN)/IntegerEvaluator/IntegerEvaluatorParser.java
SYMBOL_G=$(ANTLRGEN)/symbol/symtableCreator.java
EQUIVALENCER_G=$(ANTLRGEN)/equivalencer/equivalencer.java
INLINER_G=$(ANTLRGEN)/inliner/fixDefinitions.java \
	$(ANTLRGEN)/inliner/fixLabels.java \
	$(ANTLRGEN)/inliner/fixReturnStmts.java \
	$(ANTLRGEN)/inliner/fixVarRefs.java \
	$(ANTLRGEN)/inliner/getSubroutineTree.java \
	$(ANTLRGEN)/inliner/inliner.java \
	$(ANTLRGEN)/inliner/printVarName.java \
	$(ANTLRGEN)/inliner/varLenArrays.java
PIPELINER_G=$(ANTLRGEN)/pipeliner/fixdblBufDefns.java \
	$(ANTLRGEN)/pipeliner/pass1_delRedIter.java \
	$(ANTLRGEN)/pipeliner/pass2_repack.java \
	$(ANTLRGEN)/pipeliner/pass3_dblBuffer.java \
	$(ANTLRGEN)/pipeliner/pass3_dblBuffer_modern.java \
	$(ANTLRGEN)/pipeliner/pass4_findIterSpace.java \
	$(ANTLRGEN)/pipeliner/pass4_ldLoop2if.java \
	$(ANTLRGEN)/pipeliner/pass6_loopOverPlanes.java \
	$(ANTLRGEN)/pipeliner/SimplifySubscript.java
REDUCEMEM_G=$(ANTLRGEN)/reduceMem/reduceMem1.java \
	$(ANTLRGEN)/reduceMem/reduceMem2.java
TRANSLATOR_G=$(ANTLRGEN)/translator/renameLabel.java \
	$(ANTLRGEN)/translator/TokenizePPMdir.java \
	$(ANTLRGEN)/translator/updateFMA.java
UNPARSER_G=$(ANTLRGEN)/unparser/FortranUnParser.java

ANTLR_EXTRACT=$(BIN)/antlr

all: grammar binaries

package: $(PACKAGE_FILE)

$(PACKAGE_FILE): grammar binaries $(ANTLR_EXTRACT)
	jar cfe $@ translator.Translator -C $(BIN) .

# Extract the ANTLR jar
$(ANTLR_EXTRACT): FORCE
# Check if the jar is already extracted
ifeq ("$(wildcard $(BIN)/antlrjar.marker)", "")
# No marker present; the jar has not been extracted
ifeq ($(OS), Windows_NT)
	cd $(BIN) & jar xf ..\lib\antlr-3.3-complete.jar
	echo "" > $(BIN)\antlrjar.marker
else
	cd $(BIN); jar xf ../lib/antlr-3.3-complete.jar
	touch $(BIN)\antlrjar.marker
endif
endif

# Necessary to execute the recipe for ANTLR_EXTRACT
FORCE:

# BINARIES
binaries: $(LEXER_B) $(PARSER_B) $(INTEGEREVAL_B) $(SYMBOL_B) $(EQUIVALENCER_B) $(INLINER_B) $(PIPELINER_B) $(REDUCEMEM_B) $(TRANSLATOR_B) $(UNPARSER_B) $(STRINGTEMPLATE_B)

$(BIN)/%.class: %.java
	$(JCC) $(JCOPTS) $<
$(BIN)/%.class: $(ANTLRGEN)/%.java
	$(JCC) $(JCOPTS) $<

$(BIN)/StringTemplate/%.stg: StringTemplate/%.stg
ifeq ($(OS), Windows_NT)
	if not exist $(BIN)\StringTemplate (mkdir $(BIN)\StringTemplate)
	copy StringTemplate\$(@F) $(BIN)\StringTemplate\$(@F)
else
	mkdir -p $(BIN)/StringTemplate
	cp $< $@
endif
# END BINARIES


# GRAMMAR
grammar: $(LEXER_G) $(PARSER_G) $(INTEGEREVAL_G) $(SYMBOL_G) $(EQUIVALENCER_G) $(INLINER_G) $(PIPELINER_G) $(REDUCEMEM_G) $(TRANSLATOR_G) $(UNPARSER_G)

$(ANTLRGEN)/lexer/%.java: lexer/%.g
	$(ANTLR) -fo $(ANTLRGEN)/lexer $<
$(TOKENFILE): parser/tokenFile.g $(LEXER_G)
	$(ANTLR) -fo $(ANTLRGEN)/parser -lib $(ANTLRGEN)/lexer $<
$(ANTLRGEN)/parser/Fortran%.java: parser/Fortran%.g $(TOKENFILE)
	$(ANTLR) -fo $(ANTLRGEN)/parser $<

$(ANTLRGEN)/IntegerEvaluator/IntegerEvaluatorLexer.java: IntegerEvaluator/IntegerEvaluator.g
	$(ANTLR) -fo $(ANTLRGEN)/IntegerEvaluator $<
$(ANTLRGEN)/IntegerEvaluator/IntegerEvaluatorParser.java: IntegerEvaluator/IntegerEvaluator.g
	$(ANTLR) -fo $(ANTLRGEN)/IntegerEvaluator $<

$(ANTLRGEN)/symbol/sym%.java: symbol/sym%.g $(TOKENFILE)
	$(ANTLR) -fo $(ANTLRGEN)/symbol -lib $(ANTLRGEN)/parser $<
$(ANTLRGEN)/equivalencer/equivalencer.java: equivalencer/equivalencer.g $(TOKENFILE)
	$(ANTLR) -fo $(ANTLRGEN)/equivalencer -lib $(ANTLRGEN)/parser $<
$(ANTLRGEN)/inliner/%.java: inliner/%.g $(TOKENFILE)
	$(ANTLR) -fo $(ANTLRGEN)/inliner -lib $(ANTLRGEN)/parser $<
$(ANTLRGEN)/pipeliner/%.java: pipeliner/%.g $(TOKENFILE)
	$(ANTLR) -fo $(ANTLRGEN)/pipeliner -lib $(ANTLRGEN)/parser $<
$(ANTLRGEN)/reduceMem/%.java: reduceMem/%.g $(TOKENFILE)
	$(ANTLR) -fo $(ANTLRGEN)/reduceMem -lib $(ANTLRGEN)/parser $<
$(ANTLRGEN)/translator/%.java: translator/%.g $(TOKENFILE)
	$(ANTLR) -fo $(ANTLRGEN)/translator -lib $(ANTLRGEN)/parser $<
$(ANTLRGEN)/unparser/FortranUnParser.java: unparser/FortranUnParser.g $(TOKENFILE)
	$(ANTLR) -fo $(ANTLRGEN)/unparser -lib $(ANTLRGEN)/parser $<
# END GRAMMAR 


clean:
ifeq ($(OS), Windows_NT) 
	del /F /Q /s $(ANTLRGEN)\* $(BIN)\* $(PACKAGE_FILE)
else
	rm -rf $(ANTLRGEN)/* $(BIN)/* $(PACKAGE_FILE)
endif
