//===------------- FTreeErrorNode.java - tree error node ------------------===//
//
//  Error node data type for FTree
//
//===----------------------------------------------------------------------===//
//
//  Developed by Pei-Hung Lin
//  Copyright 2007-2013, Regents of the University of Minnesota
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

package translator;
import org.antlr.runtime.*;

public class FTreeErrorNode extends FTree {
    public FTreeErrorNode(TokenStream input, Token start, Token stop,
                            RecognitionException e)
    {
	    super(start); // no need to record anything for this example
    }
}
