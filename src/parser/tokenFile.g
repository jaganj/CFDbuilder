//===--------------- tokenFile.g - Additional tokens grammar --------------===//
//
//  This file adds more tokens which serve as markers in the parsed tree to aid 
//  the transformations. Isolating the tokens to a single file speeds the 
//   compilation.
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

/*
 * An independent token file to speed up the compilation. 
 */
parser grammar tokenFile;
options {
  output=AST;
  language=Java;
  backtrack=true;
  tokenVocab=FortranLexer;
}      

tokens
{
  ADEXT;
  ADEXTS;
  ALIGNOFFSET;
  ARRAY;
  ARRAYREF;
  BLOCKDATA;
  CALLARG;
  CANREMOVEBEGIN;
  CANREMOVEEND;
  CODEROOT;
  COPYBUFFER;
  DDOLOOP;
  DECLARATIONBLOCK; 
  DEDOUBLEBUFFER; 
  DELREDITER;  
  DELREDSUBEXP; // Eliminate redundant subscript expression
  DMA;  
  DOBLOCK;
  //DONTALTERBEGIN;
  //DONTALTEREND;
  DOUBLEBUFFER;
  DOVARARG;
  ELSEIF;
  ELSEBLOCK;
  EQUIVGRP;
  EXECUTIONBLOCK;
  FTREE;  // Represents a node;  Used by createTree
  FUNCARG;
  FUNCREF;
  FUNCTION;
  GOTO;
  IMPLICITRANGE;
  INCLUDE;
  INLINE;
  INLINED;
  ISKIPITBEGIN;
  LABELREF; // reference to a defined LABEL, eg. in a goto statement
  LONGIT; // Longitudinal loop in the sweep (1-D) direction
  LOWERBOUND;
  MAINPROG;
  MADD;  
  MSUB;
  MOVEDFROMREDITERBEGIN;
  MOVEDFROMREDITEREND;
  NEG;
  NEWLINE;
  NONE;
  PENLOWERBOUND;
  PENUPPERBOUND;
  PROGSTAT;
  PIPELINE;  
  PREFETCHBEGIN;
  PREFETCHEND;
  REPACK;
  RETAINIF;
  SHRINKMEM;
  SUBARG;
  SUBPROGRAMBLOCK;
  SUBROUTINE;
  SUBSCRIPT;
  TEMPBEGIN;
  TEMPEND;
  THENBLOCK;
  TIMES;  
  TO;
  TRANSFORMBEGIN;
  TRANSFORMEND;
  TYPESTMT;
  UNROLL;  
  UNROLLED;
  UNROLLOUTER;
  UNROLLEDOUTER;
  UPPERBOUND;
  }

@header {
  package parser;  
}

dummyRule: ICON
         ;
           