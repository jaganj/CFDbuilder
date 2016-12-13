//===---------------- LabelTable.java - table for labels ------------------===//
//
//  This class implements a table to hold all the labels seen in a source file
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
import java.util.ArrayList;
public class LabelTable {
   ArrayList<LabelSymbol> labeltable = new ArrayList<LabelSymbol>();
   public LabelTable(){}
   public void addSymbol(LabelSymbol ls){
	   if(labeltable.size() > 8999){
		   System.err.println("Warning!! LABEL number exceeds 9000!!");
		   System.exit(0);
	   }
	   labeltable.add(ls);
   }
   public LabelSymbol getSymbol(String label, Scope myscope){
	   for(int i=0;i<labeltable.size();i++){
		   LabelSymbol tmp = labeltable.get(i);
		   if(tmp.name.contentEquals(label) && (tmp.scope == myscope))
			   return tmp;
	   }
	   return null;
   }
   public LabelSymbol getSymbol(int idx){
	   LabelSymbol tmp = labeltable.get(idx);
	   return tmp;   
   }
   public int getIndex(LabelSymbol ls){
	   int idx;
	   idx = labeltable.indexOf(ls);
	   return idx;
   }
   public int getSize(){
	   return labeltable.size();
   }
   public void reset(){
	   labeltable.clear();
   }
}
