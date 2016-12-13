//===----------------- tmpEntry.java - automatic variable -----------------===//
//
//  A data type to hold the symbol and size of a temporary.
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

import symbol.*;

class tmpEntry {
	Symbol sym;
	int size;
	   
    public tmpEntry(Symbol sym, int size) {
        this.sym = sym;
        this.size = size;
    }
    
    public boolean equals(Object obj) {    	
        if (obj == null) return false;        
        if (!this.getClass().equals(obj.getClass())) return false;    
        return this.sym.equals(((tmpEntry)obj).sym);        
    }
}
