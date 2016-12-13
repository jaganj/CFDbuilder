//===--------------- FTreeVisitor.java - Tree visitor ---------------------===//
//
//  Tree visitor implementation for FBaseTree
//
//===----------------------------------------------------------------------===//
//
//  Modified by Pei-Hung Lin
//  Copyright 2007-2013, Regents of the University of Minnesota
//
//  ANTLR v3 TreeVisitor implementation    
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

import org.antlr.runtime.tree.TreeVisitorAction;

public class FTreeVisitor {
    protected FTreeAdaptor adaptor;
    
    public FTreeVisitor(FTreeAdaptor adaptor) {
        this.adaptor = adaptor;
    }
    public FTreeVisitor() { this(new FTreeAdaptor()); }
    
    /** Visit every node in tree t and trigger an action for each node
     *  before/after having visited all of its children.
     *  Execute both actions even if t has no children.
     *  If a child visit yields a new child, it can update its
     *  parent's child list or just return the new child.  The
     *  child update code works even if the child visit alters its parent
     *  and returns the new tree.
     *
     *  Return result of applying post action to this node.
     */
    public Object visit(Object t, TreeVisitorAction action) {
        // System.out.println("visit "+((Tree)t).toStringTree());
        boolean isNil = adaptor.isNil(t);
        if ( action!=null && !isNil ) {
            t = action.pre(t); // if rewritten, walk children of new t
        }
        for (int i=0; i<adaptor.getChildCount(t); i++) {
            Object child = adaptor.getChild(t, i);
            Object visitResult = visit(child, action);
            Object childAfterVisit = adaptor.getChild(t, i);
            if ( visitResult !=  childAfterVisit ) { // result & child differ?
                adaptor.setChild(t, i, visitResult);
            }
        }
        if ( action!=null && !isNil ) t = action.post(t);
        return t;
    }
}
