//===--------------- FunctionSymbol.java - Function symbol ----------------===//
//
//  This file implements the symbol for Fortran functions
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
import java.util.LinkedHashMap;
import java.util.Map;

public class FunctionSymbol extends Symbol implements Scope {
	public Map<String, Symbol> orderedArgs = new LinkedHashMap<String, Symbol>();
    Scope enclosingScope;
    public Scope enclosedScope;

    public FunctionSymbol(String name, Scope enclosingScope) {
        super(name);
        this.enclosingScope = enclosingScope;
    }

    public Symbol resolve(String name) {
		Symbol s = orderedArgs.get(name);
        if ( s!=null ) return s;
		// if not here, check any enclosing scope
//		if ( getEnclosingScope() != null ) {
//			return getEnclosingScope().resolve(name);
//		}
		return null; // not found
	}

	public void define(Symbol sym) {
		orderedArgs.put(sym.name, sym);
		sym.scope = this; // track the scope in each symbol
	}

	public void remove(String name) {
		orderedArgs.remove(name);
	}
	
    //public boolean isVoid() { return type.getName().equals("void"); }

	public Scope getEnclosingScope() { return enclosingScope; }
	public String getScopeName() { return name; }

    public String toString() { return "method"+super.toString(); }

	public boolean isGlobalScope() {return false;}
	
	public void printSymbols(){
        Iterator iterator = orderedArgs.keySet().iterator();
        
        System.out.println("Args in the scope: ");
        while (iterator.hasNext()) {  
          String key = iterator.next().toString();  
          String value = orderedArgs.get(key).toString();  

          System.out.println(key + " " + value);  
        }  	
	}
}
