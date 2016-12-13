//===------ ModAutoIndentWriter.java - modified AutoIndentWriter ----------===//
//
//  Wrap the Fortran output from StringTemplate automatically when they exceed 
//  72 characters on a line
//
//===----------------------------------------------------------------------===//
//
//  Developed by Jagan Jayaraj
//  Copyright 2007-2013, Regents of the University of Minnesota
//
//  Based on StringTemplate AutoIndentWriter implementation
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

import org.antlr.stringtemplate.AutoIndentWriter;

import java.io.IOException;
import java.io.Writer;

/* This modified AutoIndentWriter handles the hard wrapping of lines at 72 columns. 
 * The linewidth 72 and the wrap string "\n     &" have been hard-coded in places.
 * The semantics of the variable charPosition was hard to decipher. The code claims
 * it starts from 0, but doesn't appear so. With many trials and errors, the 
 * comparison and the assignment of charPosition seem to produce the desired effect
 * of columnWidth < 72. I don't want to deal with this any more, unless a bug dares
 * to surface itself. The hard wrapping is softened as much as possible with the
 * wrap attribute in the f77.stg file. The presence of so many wrap attributes
 * destroys the elegance of the templates, but I don't want to beautify the code
 * at the expense of unnecessary effort. After all, this is just  output, and not
 * optimizations. - JJ, 04/08/2010 */
public class ModAutoIndentWriter extends AutoIndentWriter {
	private int strlength = 0;

	public ModAutoIndentWriter(Writer out, String newline) {
		super(out,newline);		
	}

	public ModAutoIndentWriter(Writer out) {		
		super(out);
	}

	/** Write out a string literal or attribute expression or expression element.*/
	public int write(String str) throws IOException {
		int n = 0;
		
		for (int i=0; i<str.length(); i++) {
			char c = str.charAt(i);
			// found \n or \r\n newline?
			if ( c=='\r' || c=='\n' ) {
				atStartOfLine = true;
				charPosition = -1; // set so the write below sets to 0
				n += newline.length();
				out.write(newline);
				charPosition += n; // wrote n more char				
				// skip an extra char upon \r\n
				if ( (c=='\r' && (i+1)<str.length() && str.charAt(i+1)=='\n') ) {
					i++; // loop iteration i++ takes care of skipping 2nd char
				}
				continue;
			}
			// normal character
			// Exceeds 72
			if (charPosition >= 72) {
				n += newline.length();
				out.write(newline);
				out.write("     &");
				charPosition = 7; // Although we write 6 columns, by trial and error
				                  // we find, this number got to be 7 and not 6. 				
			}
			// check to see if we are at the start of a line; need indent if so
			if ( atStartOfLine ) {
				n+=indent();
				atStartOfLine = false;
			}
			n++;
			out.write(c);
			charPosition++;
		}
		return n;
	}
	
	/** Write out a string literal or attribute expression or expression element.
	 *
	 *  If doing line wrap, then check wrap before emitting this str.  If
	 *  at or beyond desired line width then emit a \n and any indentation
	 *  before spitting out this str.
	 */	

	public int write(String str, String wrap) throws IOException {
		strlength = str.length();
		int n = writeWrapSeparator(wrap);
		return n + write(str);
	}

	public int writeWrapSeparator(String wrap) throws IOException {
		int n = 0;
		// if want wrap and not already at start of line (last char was \n)
		// and we have hit or exceeded the threshold
		if ( wrap!=null && !atStartOfLine &&
			 charPosition + strlength >= 72 )
		{
			// ok to wrap
			// Walk wrap string and look for A\nB.  Spit out A\n
			// then spit indent or anchor, whichever is larger
			// then spit out B.
			for (int i=0; i<wrap.length(); i++) {
				char c = wrap.charAt(i);
				if ( c=='\n' ) {
					n++;
//					out.write("I am wrapping here");
					out.write(c);
					//atStartOfLine = true;
					charPosition = 0;
					int indentWidth = getIndentationWidth();
					int lastAnchor = 0;
					if ( anchors_sp>=0 ) {
						lastAnchor = anchors[anchors_sp];
					}
					if ( lastAnchor > indentWidth ) {
						// use anchor not indentation
						n+=indent(lastAnchor);
					}
					else {
						// indent is farther over than last anchor, ignore anchor
						n+=indent();
					}
					// continue writing any chars out
				}
				else {  // write A or B part
					n++;
					out.write(c);
					charPosition++;
				}
			}
		}
		return n;
	}
}
