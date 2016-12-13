//===---------------------- Bound.java - Array bounds ---------------------===//
//
//  A data type to store the upper and lower bounds of arrays.
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

package pipeliner;

public class Bound {
   public int ub; /* Upper bound */
   public int lb; /* Lower bound */

   public boolean equals(Object obj) {    	
       if (obj == null) return false;
       //System.out.println("varName1 = "+varName+"  SubName1 = "+calleename+"  varName2"+((varSubKey) obj).varName + "  subName2 = "+((varSubKey) obj).calleename);
       if (!this.getClass().equals(obj.getClass())) return false;
       Bound obj2 = (Bound) obj;
       if (this.ub == obj2.ub && this.lb == obj2.lb) {
           return true;
       }
       else return false;
   }
   
   public int hashCode() {
       int hash = 0;
       // Method 1-Concatenate the strings
       hash = (this.ub+5)*100 + (this.lb) ;
       return hash;
   }   
}