//===------------- ParamSymbol.java - symbol for parameters ---------------===//
//
//  This class implements the symbol for parameters
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

import IntegerEvaluator.IntegerEvaluator;

public class ParamSymbol extends Symbol {
	public String value;
	public int ivalue;
	public IntegerEvaluator EqEvaluator = new IntegerEvaluator();
	public ParamSymbol(String name){
	  	super(name, Type.typeenum.INTEGER);
	}
	public ParamSymbol(String name, String value){
	  	super(name, Type.typeenum.INTEGER);
		this.ivalue=EqEvaluator.evalInfix(value);
		this.value = Integer.toString(this.ivalue);
	}
	public void updateValue(BaseScope scope, String name, String oldvalue){
		int ivalue;
		IntegerEvaluator EqEvaluator = new IntegerEvaluator();
		oldvalue = scope.replaceParameter(oldvalue);

		ivalue = EqEvaluator.evalInfix(oldvalue);
		  ((ParamSymbol)scope.symbols.get(name)).ivalue= ivalue;
		  ((ParamSymbol)scope.symbols.get(name)).value= ((Integer)ivalue).toString();
		  //System.out.println("after:"+name+" "+oldvalue+"="+EqEvaluator.evalInfix(oldvalue));
		  
	}
}
