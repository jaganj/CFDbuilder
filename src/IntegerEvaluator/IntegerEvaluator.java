//===-------------- IntegerEvaluator.java - Integer Evalautor -------------===//
//
//  This code evaluates static integer expressions to aid other passes
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

package IntegerEvaluator;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Stack;

import org.antlr.runtime.*;

import translator.FTree;

public class IntegerEvaluator {
	
	private static final String operators = "-+/*";
    private static final String operands = "0123456789";
    
    public int evalInfix(String infix){
    	//System.out.println(evaluateMaxMin(infix));
        return Evaluate(infix);
    }
    
    public int Evaluate(String inputstring){
    	InputStream is = new   ByteArrayInputStream(inputstring.getBytes());
    	int output = 0;
        // create a CharStream that reads from standard input
        ANTLRInputStream input = null;
		try {
			input = new ANTLRInputStream(is);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.err.println();
			System.exit(-1);
		}

        // create a lexer that feeds off of input CharStream
        IntegerEvaluatorLexer lexer = new IntegerEvaluatorLexer(input);

        // create a buffer of tokens pulled from the lexer
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // create a parser that feeds off the tokens buffer
        IntegerEvaluatorParser parser = new IntegerEvaluatorParser(tokens);
        // begin parsing at rule r
        try {
			output = parser.start();
		} catch (RecognitionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		//System.out.println("input:"+inputstring+"  and output:"+output);
        return output;
    }
    public String convert2Postfix(String infixExpr) {
        char[] chars = infixExpr.toCharArray();
        Stack<Character> stack = new Stack<Character>();
        StringBuilder out = new StringBuilder(infixExpr.length());
        char prevChar = (char)0;

/* The idea is to separate the operands with ')'. All operands
   would have a trailing ')', but the operators won't. 
  	   if(out.length() > 0 && isOperand(out.charAt(out.length()-1))) condition
  	   guards appending ')' to the in-construction postfix string, and it tests
  	   if the previous character is an operand. 
 */  
        for (char c : chars) {
                if (isOperator(c)) {
               /* For unary operators, appending a '0' to the output simplifies
                  the logic in evaluatePostfix. We don't a special treatment then,
                       since   0-operand = -operand. 
              Note: ***** Unary plus is not supported *******        		                      
              Unary minus, I assume, can occur only in the following 
              circumstances: 
	              i) no characters before an operator
	              ii) the previous character is another operator
	              iii) An operator immediately following an open parenthesis
	      There are no whitespaces. They are eaten by the lexer. 
	      There is a complication with expression like a--b which is handled
	      by introducing special code in the operatorGreaterOrEqual procedure.
	      Detailed comments are available in the procedure for edification.
            */                         
                if(prevChar == (char)0 || isOperator(prevChar) || prevChar == '(') 
                {
//                    System.out.println("prevChar = "+prevChar);
                    out.append("0)");
                } else if(out.length() > 0 && isOperand(out.charAt(out.length()-1)))                        	
                	{
                       	out.append(')');
                }
                        while (!stack.isEmpty() && stack.peek() != '(') {
                        /* Check the operator precedence in the current nest level. */
                                if (operatorGreaterOrEqual(stack.peek(), c)) {
                                        out.append(stack.pop());
                                } else {
                                        break;
                                }
                        }
                        /* Store the operator in the stack */
                        stack.push(c);
                } else if (c == '(') {
                        stack.push(c);
                } else if (c == ')') {
                	if(out.length() > 0 && isOperand(out.charAt(out.length()-1)))                        
          			out.append(')');		
          		/* Transfer all the operators corresponding to the current
          		   sub-expression enclosed in parenthesis from the stack
          		   to the output
          		 */
                        while (!stack.isEmpty() && stack.peek() != '(') {
                                out.append(stack.pop());
                        }
                        /* Remove the '(' */
                        if (!stack.isEmpty()) {
                                stack.pop();
                        }
                } else if (isOperand(c)) {
                        out.append(c);
                }
                prevChar = c;
        }
        if (out.length() > 0 && isOperand(out.charAt(out.length()-1)) ) 
		out.append(')');   
/* popping out any remaining operators in the stack */	 		     
        while (!stack.empty()) {

                out.append(stack.pop());
        }

        //System.out.println(out.toString());
        return out.toString();
    }

    private int getPrecedence(char operator) {
        int ret = 0;
        if (operator == '-' || operator == '+') {
                ret = 1;
        } else if (operator == '*' || operator == '/') {
                ret = 2;
        }
        return ret;
    }
    private boolean operatorGreaterOrEqual(char op1, char op2) {
	if (op1 == op2)
/* If the two operators are the same, the order of the
   operators in the postfix expression would not make a 
   difference. Semantically, I am violating the meaning
   of this procedure name, but it helps to handle a very
   special case with the unary minus. I handle the unary
   minus by inserting an extra 0 in convert2postfix. The
   idea is the integer value of unary minus is generated
   by subtracting 0 with the number in evaluatePostfix. 
   However, for the expression a--b which is equivalent
   to a+b, we see an incorrect postfix expression like
   a0-b-. Since the two negatives '-' are of the same
   precedence, we pop one of the negatives from the 
   operator stack, and stick it at the end of 0. Now,
   the 0 is subtracted from the preceding expression, 
   rather than getting subtracted by the succeeding
   operand. The resulting expression is equivalent to 
   ab- which is wrong. Since the order of operators in
   the postfix don't make a difference between the same
   operators, if we don't maintain the order of occurrence,
   we would get the right postfix expression, a0b-- !		   
 */       	
		return false;
		else
            return getPrecedence(op1) >= getPrecedence(op2);
	}

    private boolean isOperator(char val) {
        return operators.indexOf(val) >= 0;
    }
    private boolean isOperand(char val) {
        return operands.indexOf(val) >= 0;
    }
    
    public String printInfix(FTree tree){
		String tmp="",output="";
		FTree Lchild, Rchild;
		output = output.concat("(");
		//System.out.print("(");
		if(tree.getChildCount()==0){
			output = output.concat(tree.getText());
			//System.out.print(tree.getText());
			output = output.concat(")");
			//System.out.print(")");
			return output;
		}else if(tree.getChildCount()==1){
			output = output.concat("-");
			tmp = printInfix((FTree)tree.getChild(0));
			output = output.concat(tmp);
			output = output.concat(")");
			//System.out.println("got NEG "+tmp);
			return output;
		}else{
			Lchild = (FTree) tree.getChild(0);
			Rchild = (FTree) tree.getChild(1);
			tmp = printInfix(Lchild);
			output = output.concat(tmp);
			output = output.concat(tree.getText());
			//System.out.print(tree.getText());
			tmp = printInfix(Rchild);
			output = output.concat(tmp);
			output = output.concat(")");
			//System.out.print(")");
			return output;
		}
	}
    public static boolean isIntConvertable(String input){
  	  if(input.matches("(\\d|\\+|\\-|\\()(\\d|\\+|\\-|\\*|\\/|\\(|\\))*")) return true;
  	  else return false;

  	}

    
/*    
    public static void main(String[] args) {
        IntegerEvaluator ie = new IntegerEvaluator();
    	String input = "(8*(max(4,5)*1))";
        int result=0;
        try {
			result = ie.Evaluate(input);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("I am done parsing "+input+" result:"+result);
    }
*/
}