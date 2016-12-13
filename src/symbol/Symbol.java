//===--------------------- Symbol.java - Fortran symbol -------------------===//
//
//  This file implements the symbol type for Fortran
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
import IntegerEvaluator.*;
import translator.FTree;

public class Symbol implements Cloneable{
	  public String name; // All symbols at least have a name
	  public Type.typeenum type; // Symbols have types
	  public Scope scope;      // All symbols know what scope contains them.
	  public boolean bArgument=false;
	  public boolean bcommonblock;	  
	  public String commonName;  /* Name of the common block it appears in */
	  public int ipositionInBlk; /* Position of the member in the common block. The count
	                                starts from one. Common Block name symbol is assigned
	                                zero.	                                 
	                              */   
	  public boolean isVectortype = false;
	  public int typelen;
    /* All variables have their typelen default to 4 here. We are dealing with 
       32-bit arithmetic here. If the user needs 64-bit, we will deal with it later.
     */  
	  public String equivName;
	  Type.typeenum equivType;
	  String equivOffset; // in bytes
       
      /* We use clone to duplicate a symbol.  Copy constructors are not an 
       * option because we want statements like 
       *    if(callsubroutinebody.resolve(ID.getText()) != $ID.symbol) {
       * to succeed.  It will fail if we try to match a copy instead of the
       * original symbol.
       */	  
	  
	  public Symbol(String name){
	    this.name = name;
	    this.type = Type.typeenum.INVALID;
	    this.bcommonblock=false;
	    this.typelen = 4;
	  }
	  
	  public Symbol(String name, Type.typeenum type){
	    this.name = name;
	    this.type = type;
	    this.bcommonblock = false;
	    this.typelen = 4;
	  }
	  
	  public Symbol(String name, Type.typeenum type, boolean isvectorloop){
		    this.name = name;
		    this.type = type;
		    this.bcommonblock = false;
		    this.typelen = 4;
		    if(type.equals(Type.typeenum.REAL) && isvectorloop) this.isVectortype = true;
		  }
	  
	  public void setArgument(){
		  this.bArgument = true;
	  }
	  
	  public void setVectortype(BaseScope scope,boolean isvector){
		  if((isvector) && (!bArgument) && type.equals(Type.typeenum.REAL)) this.isVectortype = true;
		  if(this.hasEquivalence()){
			  Symbol s1 = (Symbol)scope.resolve(this.equivName);
			  s1.isVectortype = this.isVectortype;
		  }
		  /*PHLIN: still not sure whether we should promote integer array into vector type.
		   *The case of mycube[] and mycubef[] would be translated incorrectly if we promote it to integer 
		  */
		  //if((isvector) && (!bArgument) && type.equals(Type.typeenum.INTEGER) && (this.isArraySymbol())){
		  //  this.isVectortype = true;
		  //}
		  
	  }
	  
	  public void setCommonBlock(){
		  this.bcommonblock = true;
	  }
	  
	  public void setType(Type.typeenum type, int len){
		  this.type = type;
		  this.typelen = len;
	  }
	  /*
	  getDataType: return the data type based on variable name
	  Fortran uses implicit data type that forces all variable name starting with "i,j,k,l,m,n" to be integer
	  */
	  public Type.typeenum getType(){
		  return this.type;
	  }
	  public static Type.typeenum getDataType(String name){
	  	  char c1;
	  	  c1 = name.charAt(0);
	  	  if((c1>='i')&&(c1<='n')) 
	  	 	return Type.typeenum.INTEGER;/*return type integer*/
	  	  else 	return Type.typeenum.REAL;/*return type real*/
	  }
	  public String getName() { return name; }
	  public boolean isGlobal() { return scope instanceof GlobalScope; }
	  public boolean isParameter() { return this instanceof ParamSymbol; }
	  public boolean isFunctionSymbol() { return this instanceof FunctionSymbol; }
	  public boolean isArraySymbol() { return this instanceof ArraySymbol; }
	  public boolean isVariableSymbol() { return this instanceof VariableSymbol; }
	  public boolean isArgument() {return bArgument; }
	  public boolean isCommonblock() {return bcommonblock; }
	  
	  public void setEquivalence(BaseScope scope,FTree t1, FTree t2){
		String name1, name2;
		String offset1="", offset2="";
		int ioffset1=0, ioffset2=0;
		int itotalsize1=0, itotalsize2 = 0;
		IntegerEvaluator EqEvaluator = new IntegerEvaluator();
		if(t1.getChildCount() == 0)
			name1 = t1.toString();
		else name1 = t1.getChild(0).toString();
		if(t2.getChildCount() == 0)
			name2 = t2.toString();
		else name2 = t2.getChild(0).toString();

		Symbol s1 = (Symbol)scope.resolve(name1);
		Symbol s2 = (Symbol)scope.resolve(name2);
	
		//System.out.println("equivalence:"+t1.toStringTree()+" and "+t2.toStringTree());
		if (s1.isArraySymbol()){
			offset1 = ((ArraySymbol)s1).getOffset(scope,t1);
			itotalsize1 = EqEvaluator.evalInfix(((ArraySymbol)s1).totalsize) * s1.typelen;
		}
		else
		{
			offset1 = "0";
			itotalsize1 = s1.typelen;
		}
		if (s2.isArraySymbol()){
			offset2 = ((ArraySymbol)s2).getOffset(scope,t2);
			itotalsize2 = EqEvaluator.evalInfix(((ArraySymbol)s2).totalsize) * s2.typelen;
		}
		else
		{
			offset2 = "0";
			itotalsize2 = s2.typelen;
		}
		/*This part might be wrong!! In the symbol info, the array size and offset value might not be retrieved. In that case, it would be hard
		 * to determine whether the equivalence is valid for our assumptions.*/
		ioffset1 = EqEvaluator.evalInfix(offset1);
		ioffset2 = EqEvaluator.evalInfix(offset2);
		/* Here is a lot of logic to compute the offset in terms of the equivalenced data type.
		 * Uncomment the lines which begin with //u to retrieve them. It would enforce a constraint
		 * which may not be desirable generally. */ 
		//u if (s1.typelen != s2.typelen)
		//u {
			ioffset1 = ioffset1 * s1.typelen;
			ioffset2 = ioffset2 * s2.typelen;			
		//u }

		if(itotalsize1 <= itotalsize2){
			if(ioffset2<ioffset1)
			{
				System.err.println("1:Negative offsets are disallowed in equivalences. ie. a smaller object cannot start from a negative offset of a larger object");
				System.err.println("The problematic equivalence is between "+name1 + " and "+name2+" in "+scope.scopename);
//				System.err.println("s1.typelen: "+s1.typelen+"; s2.typelen: "+s2.typelen);
//				System.err.println("ioffset1: "+ioffset1+"; ioffset2: "+ioffset2+"; itotalsize1: "+itotalsize1+"; itotalsize2: "+itotalsize2);
				System.exit(-1);
			}
			s1.equivName = name2;
			s1.equivType = s2.type;			
			//u if (s1.typelen != s2.typelen)
			//u {
			//u if (ioffset1 - (ioffset1 / s2.typelen)* s2.typelen > 0)
			//u {
			//u 	System.err.println("Equivalence error: "+name1+" is equivalenced to within a "+name2+" element!");
			//u 	System.err.println("Got to be equivalenced at elements boundaries");
			//u 	System.exit(-1);
			//u }
			//u ioffset1 = ioffset1 / s2.typelen;
			//u offset1 = ((Integer)ioffset1).toString();
			//u }
			/* Comment the below two lines if you don't want the offset in bytes */
			offset1 = ((Integer)ioffset1).toString();
			offset2 = ((Integer)ioffset2).toString();
			s1.equivOffset = "(".concat(offset2).concat("-").concat(offset1).concat(")");
		}else{
				if(ioffset1<ioffset2)
				{
					System.err.println("2:Negative offsets are disallowed in equivalences. ie. a smaller object cannot start from a negative offset of a larger object");
					System.err.println("The problematic equivalence is between "+name1 + " and "+name2+" in "+scope.scopename);
//					System.err.println("s1.typelen: "+s1.typelen+"; s2.typelen: "+s2.typelen);
//					System.err.println("ioffset1: "+ioffset1+"; ioffset2: "+ioffset2+"; "+s1.name+" totalsize: "+itotalsize1+"; "+s2.name+" totalsize: "+itotalsize2);
					System.exit(-1);
				}
				s2.equivName = name1;
				s2.equivType = s1.type;				
				//u if (s1.typelen != s2.typelen)
				//u {
				//u if (ioffset2 - (ioffset2 / s1.typelen)* s1.typelen > 0)
				//u {
				//u 	System.err.println("Equivalence error: "+name2+" is equivalenced to within a "+name1+" element!");
				//u 	System.err.println("Got to be equivalenced at elements boundaries");					
				//u 	System.exit(-1);
				//u }				
				//u ioffset2 = ioffset2 / s1.typelen;
				//u offset2 = ((Integer)ioffset2).toString();
				//u }
				/* Comment the below two lines if you don't want the offset in bytes */
				offset1 = ((Integer)ioffset1).toString();
				offset2 = ((Integer)ioffset2).toString();			
				s2.equivOffset = "(".concat(offset1).concat("-").concat(offset2).concat(")");
			}
   	  	 /*   
   	  	    if (name1.contentEquals("testequivr") || name.contentEquals("testequivd") ||
   	  	    	name1.contentEquals("testequivc") || name.contentEquals("testequivcl")	)
	  	    {
   				if(s1.hasEquivalence())
   				{
   					System.out.println(((Symbol)scope.resolve(name1)).name+" has equiv to "+((Symbol)scope.resolve(name1)).equivName +
   							" at "+((Symbol)scope.resolve(name1)).equivOffset);
   					System.out.println("itotalsize1 = "+itotalsize1+ " itotalsize2 = "+itotalsize2);
   					System.out.println("ioffset1 = "+ioffset1+ " ioffset2 = "+ioffset2);
   					System.out.println("s1.typelen = "+s1.typelen+ " s2 = "+s2.typelen);
   				}
   				if(s2.hasEquivalence())
   				{
   					System.out.println(((Symbol)scope.resolve(name2)).name+" has equiv to "+((Symbol)scope.resolve(name2)).equivName +
					" at "+((Symbol)scope.resolve(name2)).equivOffset);
					System.out.println("itotalsize1 = "+itotalsize1+ " itotalsize2 = "+itotalsize2);
					System.out.println("ioffset1 = "+ioffset1+ " ioffset2 = "+ioffset2);
					System.out.println("s1.typelen = "+s1.typelen+ " s2 = "+s2.typelen);
   				}
	  	    } 
	  	  */			
		}
		
		public boolean hasEquivalence(){
			return (this.equivName != null);
			
		}
		
		public Object clone() throws CloneNotSupportedException {
			Symbol clone = (Symbol)super.clone();
			return clone;
		}

}
