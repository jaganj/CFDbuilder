//===--------------------- BaseScope.java - Scope type --------------------===//
//
//  This file implements the scope type for Fortran. Other scopes inherit this
//  base scope
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
import java.util.Set;
import java.util.Stack;

public abstract class BaseScope implements Scope {
	String scopename;
	Scope enclosingScope; // null if global (outermost) scope
	public Map<String, Symbol> symbols = new LinkedHashMap<String, Symbol>();

    public BaseScope(Scope enclosingScope, String name) 
    { 
    	this.scopename = name;
    	this.enclosingScope = enclosingScope;	
    }
    public String getScopeName(){
    	return scopename;
    }
    
    public Symbol resolve(String name) {    	
		Symbol s = symbols.get(name);		
        if ( s!=null ) return s;
/* 
 * If not here, localscope will check the FunctionSymbol to find the arguments.
 * Arguments should not be declared inside subroutine for C.
 * For Fortran, only the arrays should be re-dimensioned in the subroutine.
*/
		if ((enclosingScope != null) && (enclosingScope instanceof FunctionSymbol) ) return enclosingScope.resolve(name);
		return null; // not found
	}

	public void define(Symbol sym) {
		symbols.put(sym.name, sym);
		sym.scope = this; // track the scope in each symbol
	}

	public void remove(String name){
		symbols.remove(name);
	}
	
	public void printSymbols(){
        Iterator iterator = symbols.keySet().iterator();
        
        System.out.println("Symbols in the scope: ");
        while (iterator.hasNext()) {  
          String key = iterator.next().toString();  
          String value = symbols.get(key).toString();  

          System.out.println(key + " " + value);  
        }  	
	}
	
    public Scope getEnclosingScope() { return enclosingScope; }

	public String toString() { return symbols.keySet().toString(); }
		
	public boolean isglobalScope(){return this instanceof GlobalScope;}
	
	public String replaceParameter(String oldvalue){
		Set<String> set = symbols.keySet();
		String tmpname;
		String[] operands;
		Symbol tmp;
		Stack<String> parameters = new Stack<String>();
		Iterator<String> iter=set.iterator();
		operands = oldvalue.split("\\W");		/*regular expression \W will take non-word characters to split the string*/

		while(iter.hasNext()){
			parameters.push(iter.next());
		}
		while(!parameters.isEmpty()){
		  tmpname = parameters.pop();
		  tmp = symbols.get(tmpname);
		  if(tmp.isParameter() && ((ParamSymbol)tmp).value!= null){
			  for(int i=0;i<operands.length;i++){
				  if(operands[i].equals(tmpname) && operands[i].length()==tmpname.length()){
					//System.out.println("replace "+operands[i]+" to "+tmpname);
						oldvalue = oldvalue.replaceFirst(operands[i], ((ParamSymbol)tmp).value);
					//System.out.println("replace "+oldvalue+" to "+tmpname);
				  }
			  }

		  }
	}

		/*while(iter.hasNext()){
			  tmpname = iter.next();
			  tmp = symbols.get(tmpname);
			  if(tmp.isParameter() && ((ParamSymbol)tmp).value!= null){
				  for(int i=0;i<operands.length;i++){
					  if(operands[i].equals(tmpname)){
						  System.out.println("replace "+operands[i]+" to "+tmpname);
						oldvalue = oldvalue.replaceFirst(operands[i], ((ParamSymbol)tmp).value);
					  }
				  }

			  }
		}*/
		return oldvalue;
	}
}
