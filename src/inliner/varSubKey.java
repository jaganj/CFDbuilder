//===-------------- varSubKey.java - inliner helper class ------------===//
//
//  This class stores the caller and callee names for a variable
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

package inliner;

class varSubKey {
	String varName;
	String calleename;
	String callername;
	
    private int hash;

    public varSubKey(String varName, String calleename, String callername) {
        this.varName = varName;
        this.calleename = calleename;
        this.callername = callername;
        // Object cannot change.  Calculate hash code at creation time.
        this.hash = calculateHashCode();
    }

    public boolean equals(Object obj) {    	
        if (obj == null) return false;
        //System.out.println("varName1 = "+varName+"  SubName1 = "+calleename+"  varName2"+((varSubKey) obj).varName + "  subName2 = "+((varSubKey) obj).calleename);
        if (!this.getClass().equals(obj.getClass())) return false;
        varSubKey obj2 = (varSubKey) obj;
        if (
            this.varName.equals(obj2.varName) &&
            this.calleename.equals(obj2.calleename) &&
            this.callername.equals(obj2.callername)
        ) {
            return true;
        }
        else return false;
    }
    
    public int hashCode() {
        return hash;
    }
    
    private int calculateHashCode() {
        int tmp = 0;
        // Method 1-Concatenate the strings
        tmp = (varName + calleename + callername).hashCode();
        return tmp;
    }
}
