//===-------------- ArraySymbol.java - Symbol for Fortran arrays ----------===//
//
//  This file implements the symbol for Fortran arrays, and additional features
//  such as calculating the offset for an array expression from the array's base
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

package symbol;
import IntegerEvaluator.IntegerEvaluator;
import translator.*;

import java.util.Arrays;
import java.util.List;
import java.util.Collection;
import java.lang.reflect.Field;

public class ArraySymbol extends Symbol {
	public int nDim=0, itotalsize=-1;
	public String totalsize = "";
	public String[] sizeDim = new String[7];
	public String[] LBDim = new String[7];
	public String[] UBDim = new String[7];
	public FTree[] LBDimTree = new FTree[7];
	public FTree[] UBDimTree = new FTree[7];
	//public IntegerEvaluator EqEvaluator = new IntegerEvaluator();
	public ArraySymbol(String name, FTree ArrayExtent,BaseScope scope){
	  	super(name);
	  	this.type = Symbol.getDataType(name);
	  	this.nDim = ArrayExtent.getChildCount();
	  	for(int i=0;i<7;i=i+1){
	  		LBDimTree[i] = null;
	  		UBDimTree[i] = null;
	  		LBDim[i]="";
	  		UBDim[i]="";
	  		sizeDim[i] = "0";
		}
	  	for(int i=0;i<nDim;i++){
	  		FTree child = (FTree)ArrayExtent.getChild(i);
	  		if(child.getChildCount()>1){
	  			//LBDimTree[i] = (FTree)child.getChild(0);
	  			//UBDimTree[i] = (FTree)child.getChild(1);
	  			LBDimTree[i] = (FTree) Translator.fTreeAdaptor.dupTree((FTree)child.getChild(0));
	  			UBDimTree[i] = (FTree) Translator.fTreeAdaptor.dupTree((FTree)child.getChild(1));
	  		}else{
	  			//UBDimTree[i] = (FTree)child.getChild(0);
	  			UBDimTree[i] = (FTree) Translator.fTreeAdaptor.dupTree((FTree)child.getChild(0));
	  		}
	  	}
  		if (name.contentEquals("dd")) {
  			System.out.println("dd: ndim = "+nDim);
  			System.out.println("UBDimTree[0] = "+UBDimTree[0].toStringTree());
  		}
	  	this.setArraySize(scope);
	  	/*04/12/10: setup totalsize inside the constructor to avoid bugs*/
	}
	
	public void setArraySize(BaseScope scope){
		IntegerEvaluator EqEvaluator = new IntegerEvaluator();
		String oldvalue="",lb="";
		totalsize = "";
		int ivalue=0;
		//System.out.println(name+":"+nDim);
		for(int i=0;i<nDim;i++){
	  		oldvalue = EqEvaluator.printInfix(UBDimTree[i]);
	  		oldvalue = scope.replaceParameter(oldvalue);
	  		LBDim[i] = "1";
	  		if(IntegerEvaluator.isIntConvertable(oldvalue)){
	  			ivalue = EqEvaluator.evalInfix(oldvalue);
	  			sizeDim[i] = ((Integer)ivalue).toString();
		  		UBDim[i] = ((Integer)ivalue).toString();
	  		}else{
	  			sizeDim[i] = oldvalue;
	  			UBDim[i] = oldvalue;
	  		}
	  		if(LBDimTree[i] != null){	
		  		lb = EqEvaluator.printInfix(LBDimTree[i]);
		  		lb = scope.replaceParameter(lb);
		  		if(IntegerEvaluator.isIntConvertable(lb)){
		  			ivalue = EqEvaluator.evalInfix(lb);
		  			LBDim[i] = ((Integer)ivalue).toString();
		  		}else
		  			LBDim[i] = lb;
		  		
	  			oldvalue = "((".concat(oldvalue).concat("-").concat(lb).concat(")+1)");
		  		oldvalue = scope.replaceParameter(oldvalue);
		  		if(IntegerEvaluator.isIntConvertable(oldvalue)){
		  			ivalue = EqEvaluator.evalInfix(oldvalue);
		  			sizeDim[i] = ((Integer)ivalue).toString();
		  		}else
		  			sizeDim[i] = oldvalue;
	  		}
		  	if(i!=0) totalsize= totalsize.concat("*");
		  	totalsize = totalsize.concat(sizeDim[i]);
		  	if(IntegerEvaluator.isIntConvertable(totalsize)){
		  		itotalsize = EqEvaluator.evalInfix(totalsize);
		  		totalsize = ((Integer)itotalsize).toString();
		  	}
		  //	if(name.contentEquals("rho")){
		  //	    System.out.println("rho: i"+i);
		  //	    if(LBDimTree[i]!=null)
          //         System.out.println("  "+(LBDimTree[i]).toStringTree());
		  //	    System.out.println("  "+(UBDimTree[i]).toStringTree());
		  //	}
	  	}
		//if(name.contentEquals("rho")){
		//	System.out.println("rho totalsize, itotalsize = "+totalsize+" "+itotalsize+"\n");
		//}
	}
	
	public String getOffset(BaseScope scope, FTree t)
	{
		String name;
		
		FTree treechild;
		String childstring="";
		String tmp = "", offset="";
		IntegerEvaluator EqEvaluator = new IntegerEvaluator();
		if(t.getChildCount() == 0)
			name = t.toString();
		else name = t.getChild(0).toString();
		ArraySymbol s1 = (ArraySymbol)scope.resolve(name);
		int index=0;
		
		//System.out.println(name+" "+t.toStringTree()+" "+s1.name+" "+s1.nDim+" "+t.getChildCount());
		if(t.getChildCount()==0){
			index = 0;
			offset = ((Integer)index).toString();
		}
		else
		{
			FTree argtree = (FTree)t.getChild(1);
			treechild = (FTree) argtree.getChild(0);
			childstring = EqEvaluator.printInfix(treechild);
			childstring = scope.replaceParameter(childstring);
			tmp = "(".concat(childstring).concat("-").concat(s1.LBDim[0]).concat(")");
			for(int i=1;i<argtree.getChildCount();i++){
				treechild = (FTree) argtree.getChild(i);
				childstring = EqEvaluator.printInfix(treechild);
				childstring = scope.replaceParameter(childstring);
				tmp = tmp.concat("+").concat(s1.sizeDim[i-1]).concat("*(");
				offset = "(".concat(childstring).concat("-").concat(s1.LBDim[i]).concat(")");
				tmp = tmp.concat(offset);
			}
			for(int i=1;i<argtree.getChildCount();i++){
				tmp = tmp.concat(")");
			}
			if(IntegerEvaluator.isIntConvertable(tmp)){
				index = EqEvaluator.evalInfix(tmp);
				offset = ((Integer)index).toString();
			}
			else {
				offset = tmp;
				System.out.println("Warning in equivalence: unable to retrieve the offset value for "+name);
			}
		}
		return offset;
	}

	public String toString() {
		  StringBuilder result = new StringBuilder();
		  String newLine = System.getProperty("line.separator");
		  String tmpresult = null;

		  result.append( this.getClass().getName() );
		  result.append( " Object {" );
		  result.append(newLine);

		  //determine fields declared in this class only (no fields of superclass)
		  Field[] fields = this.getClass().getDeclaredFields();

		  //print field names paired with their values
		  for ( Field field : fields  ) {
		    result.append("  ");
		    try {
		      result.append( field.getName() );
		      result.append(": ");
		      //requires access to private field:
		      //System.out.println("Type of the object: "+field.getType());
		      if (field.get(this) instanceof Object[]){
		    	  tmpresult = Arrays.toString((Object[])field.get(this));
		    	  //System.out.println("do I ever get inside this collection clause? tmpresult = "+tmpresult);
		    	  result.append(tmpresult);
		      } else {
		         result.append( field.get(this) );
		      }   
		    } catch ( IllegalAccessException ex ) {
		      System.out.println(ex);
		    }
		    result.append(newLine);
		  }
		  result.append("}");

		  return result.toString();
		}	
}
