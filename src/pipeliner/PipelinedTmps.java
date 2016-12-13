//===-------------- PipelinedTmps.java - Pipelined variables --------------===//
//
//  A data type to aid memory reduction
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

package pipeliner;
import java.util.*;

import symbol.*;

public class PipelinedTmps {
   HashMap boundstable = new HashMap();
   public PipelinedTmps(){}
   public void addEntry(ArraySymbol ts){
       if (!ts.isArraySymbol()){
     	   return;		            
       }
	   Bound bound = new Bound();
	   bound.lb = 1000; // Set extreme values for the bounds.  There will never be a stencil 1000 or 2000 long.
	   bound.ub = -1000;
	   boundstable.put(ts,bound);
   }
   public ArraySymbol getSymbol(String temp, Scope myscope){
	   Iterator iterator = boundstable.entrySet().iterator();
	   while (iterator.hasNext()){	   
		   Map.Entry tmp = (Map.Entry)iterator.next();
		   if(((ArraySymbol)tmp.getKey()).name.contentEquals(temp) && (((ArraySymbol)tmp.getKey()).scope == myscope))
			   return (ArraySymbol)tmp.getKey();
	   }
	   return null;
   }  
   public Bound getBound(ArraySymbol as){
	   Iterator iterator = boundstable.entrySet().iterator();
	   while (iterator.hasNext()){	   
		   Map.Entry tmp = (Map.Entry)iterator.next();
		   if((ArraySymbol)tmp.getKey() == as) 
			   return (Bound)tmp.getValue();		   
	   }
	   return null;
   }   
   public Set getEntries(){
	   return boundstable.entrySet();
   }
   public int getSize(){
	   return boundstable.size();
   }
   public boolean containsKey(ArraySymbol as){
	   return boundstable.containsKey(as);
   }
   public void printTable(){
	   Iterator iterator = boundstable.entrySet().iterator();
	   if (!boundstable.isEmpty()){
	      System.out.println("Bounds table entries follow:");
	      while (iterator.hasNext()){	   
		     Map.Entry tmp = (Map.Entry)iterator.next();
		     System.out.println(((ArraySymbol)tmp.getKey()).name+"  "+((Bound)tmp.getValue()).ub+"  "+((Bound)tmp.getValue()).lb/*+"  "+tmp.getKey()*/);			  
	      }	   
       }
   }
   public void updateBound(ArraySymbol as, int offset){
	   Iterator iterator = boundstable.entrySet().iterator();
	   Bound bound;
	   boolean bmod = false;
	   
	   if (offset > 0)
		   return;  /* offset has to be negative.  However since we call
		             * updateBound from fixBoundsforEquivalences you may
		             * see a positive value (1000) for variables which 
		             * have been equivalenced to some pipelined tmp but 
		             * do not appear in the pipelined region by itself.
		             */  
	   while (iterator.hasNext()){	   
		   Map.Entry tmp = (Map.Entry)iterator.next();
		   if((ArraySymbol)tmp.getKey() == as){
			  bound = (Bound)tmp.getValue();			  
			  if(bound.ub < offset){
				 bound.ub = offset;
				 bmod = true;
			  }	 
			  if(bound.lb > offset){
				 bound.lb = offset;
			     bmod = true;
		      }     
			  if(bmod)
				 tmp.setValue(bound);			  
		   }  
	   }
   }   

   public boolean sameBounds(ArraySymbol as, ArraySymbol equivAs){
	   Iterator iterator = boundstable.entrySet().iterator();
	   Bound boundA = null, boundE = null;
	   boolean bfoundA = false, bfoundE = false;
	   while (iterator.hasNext()){	   
		   Map.Entry tmp = (Map.Entry)iterator.next();
		   if(!bfoundA && (ArraySymbol)tmp.getKey() == as){
			  boundA = (Bound)tmp.getValue();
			  bfoundA = true;
		   }
		   if(!bfoundE && (ArraySymbol)tmp.getKey() == equivAs){
			  boundE = (Bound)tmp.getValue();
			  bfoundE = true;
		   }	   
		   if (bfoundA && bfoundE)
			   break;
	   }
	   if (boundA.lb != boundE.lb || boundA.ub != boundE.ub)
	       return false;
	   else
		   return true;
   } 
   
   public void fixBoundsForEquivalences(Scope currentScope){
	   boolean biterate; /* False when all the bounds in the current scope have
	                     * converged with their equivalences 
	                     */
	   Symbol sym = null, equivSym = null;
	   //ArraySymbol as = null, equivAs = null;
       Bound bound, equivBound;       
       String key = null;
       
       /***********************************************************************
        * 
        *           ____________ equiv1
        *          /                        
        *       mainarray <------ equiv2 <---------- equiv4 
        *          \________________ 
        *                           equiv3  
        *
        *    The equivalence pointers don't form a cycle.  There are no back
        *    pointers from a bigger array to a smaller array.         
        *                           
        *    The lower bounds have to converge between all equivalenced arrays      
        ***********************************************************************/
       
       /* Perform a reduction on the bounds between the equivalenced arrays */
       biterate = true;
       while(biterate){
    	 biterate = false; /* Reset it */
    	     	 
	     Iterator iteratorSym = ((BaseScope)currentScope).symbols.keySet().iterator();           
         while (iteratorSym.hasNext()) {  
	       key = iteratorSym.next().toString();  
	       
	       sym = ((BaseScope)currentScope).symbols.get(key);
	       
	       if(sym.equivName != null){
	    	   equivSym =  currentScope.resolve(sym.equivName);  
	       } else 
	    	   continue;
	       
           if (!boundstable.containsKey(sym) && !boundstable.containsKey(equivSym)){
        	   continue;
           }           
	       if (!sym.isArraySymbol() || !equivSym.isArraySymbol()){
               System.out.println(sym.name+" and / or it's equivalence "+equivSym.name+" is "
               		 +" / are not an array."
   	                 +"  The pipeline temporaries and all its equivalences must be arrays."
   			         +"  The reporting module is PipelinedTmps.java.");
               System.exit(-1);
	       }
	       
	       //as = (ArraySymbol)sym;
	       //equivAs = (ArraySymbol)equivSym;
	       
           if(checkDimsbtnEquivs((ArraySymbol)sym,(ArraySymbol)equivSym)){
        	  /* Create an entry for the equivalence */ 
        	  if (!boundstable.containsKey(sym))
	            this.addEntry((ArraySymbol)sym);
        	  else 
        		if (!boundstable.containsKey(equivSym))
        		   this.addEntry((ArraySymbol)equivSym);        	  
           }
           if(!sameBounds((ArraySymbol)sym, (ArraySymbol)equivSym)){
        	  biterate = true;  /* The bounds have not converged yet */
              //System.out.println("I am reducing the equiv "+equivSym.name+" on behalf of "+sym.name+"\n");

              bound = (Bound)boundstable.get(sym);              
           	  equivBound = (Bound)boundstable.get(equivSym);      
           	  
           	  if(bound.lb>bound.ub){
           		  /* The variable sym has not appeared in the pipelined region.
           		   * We have the initial values for the bounds.
           		   */
               	  /* Update the symbol */
                  this.updateBound((ArraySymbol)sym, equivBound.lb);
                  this.updateBound((ArraySymbol)sym, equivBound.ub);           		  
           	  } else if(equivBound.lb>equivBound.ub){
           		  /* The variable sym has not appeared in the pipelined region.
           		   * We have the initial values for the bounds.
           		   */
               	  /* Update the equivSymbol */
                  this.updateBound((ArraySymbol)equivSym, bound.lb);
                  this.updateBound((ArraySymbol)equivSym, bound.ub);           		  
           	  } else {          	  
                 // System.out.println("fixing equivalence: "+sym.name+"("+bound.lb+":"+bound.ub+") and "+equivSym.name+"("+equivBound.lb+":"+equivBound.ub+")");
            	 /* Update the symbol itself */
                 this.updateBound((ArraySymbol)sym, equivBound.lb);
                 this.updateBound((ArraySymbol)sym, equivBound.ub);
               
                 /* Update the equivalence */
                 this.updateBound((ArraySymbol)equivSym, bound.lb);
                 this.updateBound((ArraySymbol)equivSym, bound.ub);
           	  }
           }
         }   /* End the loop over the symbols in the current scope */         
       }     /* Ends the convergence (biterate) loop */
   }   
   
   private boolean checkDimsbtnEquivs(ArraySymbol as, ArraySymbol equivAs){
       /* For the pipelined temporaries to be memory reduced, we demand
        * that their equivalences be also pipelined temporaries with the
        * exact same dimensions. 
        */	   
       if (as.nDim == equivAs.nDim && as.itotalsize == equivAs.itotalsize) { 
     	  int i = 0;
     	  while(i<as.nDim && as.LBDim[i].contentEquals(equivAs.LBDim[i])
     	    	          && as.UBDim[i].contentEquals(equivAs.UBDim[i])){
     	    	i++;
          }
     	  if (i < as.nDim){
     	     /* The dimensions don't match */
     	     System.out.println("The temporary "+as.name+" and its equivalence "+equivAs.name 
     	    		          +" must have exactly the same dimensions for them to be memory "
     	    		          +"reduced in the Pipeline Region.  The reporting module is PipelinedTmps.java");
     	     System.exit(-1);
     	  }
       }
	   return true;
   }
   
   public void reset(){
	   boundstable.clear();
   }
}