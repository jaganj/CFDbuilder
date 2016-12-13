//===------------------ Tmps.java - table for temporaries -----------------===//
//
//  Helper class to remember and rearrange the layout of the temporaries.
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

package equivalencer;

import java.util.*;
import symbol.*;

public class Tmps {
   ArrayList<tmpEntry> tmpstable = new ArrayList<tmpEntry>(400);
   /* A table for all the names encountered.  It includes both 
    * the entries in the tmpstable and their equivalences. 
    */    
   ArrayList<tmpEntry> allnames = new ArrayList<tmpEntry>(400);
   int bigArraySize = 0;
   
   public Tmps(){}
   public void add(ArraySymbol ts){	   
	   int size = ts.itotalsize;
	   tmpEntry tmp = new tmpEntry(ts,size);
	   
	   if (allnames.contains(tmp))
		   return;	   
	   allnames.add(tmp);
	   	   
	   /* Check to see if the temporary has an equivalence.
	    * If the temporary has an equivalence it means that the
	    * the temporary is smaller than the equivalenced array.
	    * The idea here is to follow the equivalence pointers all
	    * the way to the superset array for the encountered  
	    * temporary.  We only include the superset array in the 
	    * tmpstable.  That way all it's subset temporary arrays
	    * are covered.  How neat!
	    */	   
	   while(ts.equivName != null){
    	   Scope       myscope = ts.scope;    	   
           ts = (ArraySymbol)myscope.resolve(ts.equivName);           
       }
	   
	   /* We only add the stack variables to the tmps table. 
	    * Check to see if the incoming symbol is an argument 
	    * or has an argument in it's array declaration, a 
	    * global variable, or a member of a common block.  
	    * If so, don't add.  If the subroutine arguments appear 
	    * in a declaration statement then those arrays won't 
	    * have a valid size in our symbol table 
	    *            i.e. itotalsize < 1.  
	    * We can therefore use itotalsize to weed out such 
	    * variables.
	    */
	   if(ts.isCommonblock() || ts.isArgument() || ts.isGlobal() || ts.itotalsize < 1)
		  return;
	   	   
       tmpEntry tmpnew = new tmpEntry(ts,ts.itotalsize);	   
	   if(tmpstable.contains(tmpnew)==false){
		   tmpstable.add(tmpnew);   
		   bigArraySize = bigArraySize + ((ArraySymbol)tmpnew.sym).itotalsize * tmpnew.sym.typelen;
		  // System.out.println("name,size,bigarraysize = "+((ArraySymbol)tmpnew.sym).name
				//             +"  "+((ArraySymbol)tmpnew.sym).itotalsize+"   "+bigArraySize+"\n");
		   /* Round bigArraySize to a multiple of 16 bytes */
		   if(bigArraySize % 16 > 0)
		      bigArraySize = ((bigArraySize / 16) + 1) * 16;
	   }	   
   }
  
   public void printTable(){
	   System.out.println("tmps table entries follow:");
	   for(int i=0;i<tmpstable.size();i++){
		     tmpEntry tmp = tmpstable.get(i);
		     System.out.println(tmp.sym.name+"  "+((ArraySymbol)tmp.sym).itotalsize/*+"  "+tmp.getKey()*/);
       }
   }   
   public void rearrange(){
	   int j=0, size = -1;
	   for(int i=0;i<tmpstable.size();i++){
		   tmpEntry tmp = tmpstable.get(j);
		   size = ((ArraySymbol)tmp.sym).itotalsize * tmp.sym.typelen;
		   if(size % 16 == 0)
			   j++;
		   else 
		   {
			/* Move the unaligned entry from the current location 
			 * to the end of the list */ 
			   tmpstable.remove(j);
			   tmpstable.add(tmp);
		   }
	   }
   }
   public void reset(){
	   tmpstable.clear();
	   allnames.clear();
	   bigArraySize = 0;
   }
}