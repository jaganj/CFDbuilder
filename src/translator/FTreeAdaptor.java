//===------------------ FTreeAdaptor.java - Tree adaptor ------------------===//
//
//  Adaptor for FTree
//
//===----------------------------------------------------------------------===//
//
//  Modified by Pei-Hung Lin
//  Copyright 2007-2013, Regents of the University of Minnesota
//
//  ANTLR v3 implementation
//  Copyright (c) 2005-2009 Terence Parr
//
//  This file is part of CFDbuilder.
//
// [The "BSD license"]
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
//     derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//===----------------------------------------------------------------------===//

package translator;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.antlr.runtime.TokenStream;

import symbol.*;

public class FTreeAdaptor extends FCommonTreeAdaptor {
        public Object create(Token token) {
            return new FTree(token);
        }
        public Object dupNode(Object t) {
            if ( t==null ) {
                return null;
            }
            Object newnode = create(((FTree)t).token);
            ((FTree)newnode).symbol = ((FTree)t).symbol;
            
            return newnode;
            //return create(((FTree)t).token);
        }
        
        
        public Object cloneNode(Object t) {
            if ( t==null ) {
                return null;
            }
            Object newnode = create(((FTree)t).token);
            //((FTree)newnode).symbol = ((FTree)t).symbol;
            if(((FTree)t).symbol != null){
              try {
				  ((FTree)newnode).symbol = (Symbol) ((FTree)t).symbol.clone();
			  } catch (CloneNotSupportedException e) {
				  System.err.println("clone error");
				  e.printStackTrace();
			  }
            }
            return newnode;
            //return create(((FTree)t).token);
        }

        public Object cloneTree(Object tree) {
    		return cloneTree(tree, null);
    	}
        public Object cloneTree(Object t, Object parent) {
            if ( t==null ) {
                    return null;
            }
            Object newTree = cloneNode(t);
            // ensure new subtree root has parent/child index set
            setChildIndex(newTree, getChildIndex(t)); // same index in new tree
            setParent(newTree, parent);
            int n = getChildCount(t);
            for (int i = 0; i < n; i++) {
                    Object child = getChild(t, i);
                    Object newSubTree = cloneTree(child, t);
                    addChild(newTree, newSubTree);
            }
            return newTree;
    }
/*
        public Object dupTree(Object t, Object parent) {
                 if ( t==null ) {
                         return null;
                 }
                 Object newTree = dupNode(t);
                 // ensure new subtree root has parent/child index set
                 setChildIndex(newTree, getChildIndex(t)); // same index in new tree
                 setParent(newTree, parent);
                 int n = getChildCount(t);
                 for (int i = 0; i < n; i++) {
                         Object child = getChild(t, i);
                         Object newSubTree = dupTree(child, t);
                         addChild(newTree, newSubTree);
                 }
                 return newTree;
         }
*/
        public Object errorNode(TokenStream input, Token start, Token stop,
                                RecognitionException e)
        {
            FTreeErrorNode t = new FTreeErrorNode(input, start, stop, e);
            //System.out.println("returning error node '"+t+"' @index="+input.index());
            return t;
        }
}