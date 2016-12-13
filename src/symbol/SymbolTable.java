//===------------------ SymbolTable.java - symbol table -------------------===//
//
//  This class implements the two-scope (local and global) Fortran symbol table
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

import java.util.Iterator;
import java.util.Set;

public class SymbolTable {
    public GlobalScope globals = new GlobalScope();

    public SymbolTable() { initTypeSystem(); }
    protected void initTypeSystem() {
        //BuiltInTypeSymbol tInt = new BuiltInTypeSymbol("int");
        //BuiltInTypeSymbol tVoid = new BuiltInTypeSymbol("void");
        //globals.define(tInt);
        //globals.define(tVoid); // pseudo-type
        //globals.define(new FunctionSymbol("printf", tVoid, globals));
    }

    public String toString() { return globals.toString(); }
    
    public void reset(){
    	Set<String> set = globals.enclosedFunctions.keySet();
        Iterator<String> iter=set.iterator();
        while(iter.hasNext()){
        	FunctionSymbol f = globals.enclosedFunctions.get(iter.next());
        	f.orderedArgs.clear();
        	((LocalScope)f.enclosedScope).symbols.clear();
        }
        globals.enclosedFunctions.clear();
    }
}
