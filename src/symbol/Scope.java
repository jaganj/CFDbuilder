//===-------------------- Scope.java - Scope type -------------------------===//
//
//  This interface defines the Fortran scope
//
//===----------------------------------------------------------------------===//
//
//  Developed by Jagan Jayaraj and Pei-Hung Lin
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

package symbol;
public interface Scope {
    public String getScopeName();

    /** Where to look next for symbols */
    public Scope getEnclosingScope();

    /** Define a symbol in the current scope */
    public void define(Symbol sym);

    public void remove(String name);

    /** Look up name in this scope or in enclosing scope if not here */
    public Symbol resolve(String name);
    
    public boolean isGlobalScope();
    
    public void printSymbols();    
}
