//===---------------- fixVarRefs.g - fix variable references --------------===//
//
//  This code fixes the variable references in the inlined body to be human 
//  readable and intuitive
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

/*    
   *********************************************************************
   **             fixVarRefs :  Fix the Variable References           **
   **                                                                 **     
   **                        Jagan Jayaraj                            **
   **                          03/07/2012                             **
   *********************************************************************
   
     This pass fixes the variable references in the body of the 
   procedure being inlined.  The objective is to have a readable and 
   an intuitive code after inlining.  If the variable reference is an 
   argument, this pass converts the callee name and callee subscripts, 
   if any, to the caller name and caller dimensions.  If the variable
   reference is not an argument then it tries to retain the callee name.
   If the callee name is not unique in the caller scope then it creates
   a unique name for this variable in the caller scope.

   Cases:
   (i)  arguments
   (ii) non-arguments
 */
tree grammar fixVarRefs;
options {
    tokenVocab=tokenFile;
    output = AST;
    ASTLabelType=FTree; 
    filter=true; 
    backtrack=true;
}

@header{
package inliner;
import lexer.*;
import symbol.*;
import translator.*;
import IntegerEvaluator.*;
import java.util.Hashtable;
import java.util.Arrays;
import java.util.Set;
}

@members{
  SymbolTable symtab;
  Scope currentScope;
  Scope callScope;
  String calleename;
  String callername;
  boolean[] bargSeen;  /* True if the argument has been seen once */ 
  FTree[] argInlineName;
  int[][] dimensionmap;
  FTree[][] AdjustOff;  /* Need to adjust the offsets of the retained or renamed
                           variable.  It occurs when the prefix dimensions of
                           caller and the callee argument match in size, but
                           not in their lower bounds.  We need to add or 
                           subtract some offset to the necessary subscript at
                           each instance of this inlined variable to successfully
                           convert it into the caller argument variable.
                         */  
  boolean[] bAdjustOff;                       
  List arglist;
  List callingArglist;
  List newEquivs;
  Hashtable redundantDefsArg;
  Hashtable redundantDefsNonArg;
  Hashtable varLenArrayDims;
  Scope subroutine;
  Scope subroutinebody;
  Scope callsubroutine;
  Scope callsubroutinebody;
  /* Optimization notes:
      You can store the values of arglist into an array like argTable for faster access.  
   */
  public fixVarRefs(TreeNodeStream input, SymbolTable symtab,String calleename,String callername, 
                    List arglist, List callingArglist, Scope callScope, Hashtable redundantDefsArg, 
                    Hashtable redundantDefsNonArg, List newEquivs, Hashtable varLenArrayDims) {
      this(input);                           /* Copy of the procedure body to be inlined */ 
      this.symtab = symtab;
      this.calleename = calleename;
      this.callername = callername;
      this.arglist = arglist;
      this.callingArglist = callingArglist;
      this.currentScope = symtab.globals;
      this.callScope = callScope;
      this.redundantDefsArg = redundantDefsArg;
      this.redundantDefsNonArg = redundantDefsNonArg;
      this.newEquivs = newEquivs;
      this.varLenArrayDims = varLenArrayDims;
      this.bargSeen = new boolean[arglist.size()];
      this.argInlineName = new FTree[arglist.size()];
      this.dimensionmap = new int[arglist.size()][7];
      this.AdjustOff   = new FTree[arglist.size()][7];
      this.bAdjustOff   = new boolean[arglist.size()];
      for (int i=0;i<arglist.size();i++){
           this.bargSeen[i] = false;
           this.bAdjustOff[i] = false;
      }
      this.subroutine = symtab.globals.enclosedFunctions.get(this.calleename);
      this.subroutinebody = ((FunctionSymbol)this.subroutine).enclosedScope;
      this.callsubroutine = symtab.globals.enclosedFunctions.get(this.callername);
      if(this.callsubroutine == null)
         /* MAINPROG */
         this.callsubroutinebody = symtab.globals;
      else   
         this.callsubroutinebody = ((FunctionSymbol)this.callsubroutine).enclosedScope;            
  }
  
  private FTree fixNonArg(FTree retvaltree, FTree ID, boolean barray){
    int i, j, ioff, myidx;
    String appendedname = "_";
    String callName = "";
    String newname= "";    
    Symbol mysymbol = null;
    FTree tmp = null;

    appendedname = appendedname.concat(calleename);
     /*                 Not an Argument: Retain or Append
     ============================================================================= 
                                  Names match
                               (Call site scope)            
     =============================================================================
        retain                         N                        
        -----------------------------------------------------------------------
        append                         Y                      
     =============================================================================
               
       Retaining the same name for non-arguments is hard.  Arguments already have a 
       valid definition in the callScope.  Non-arguments come cold the first time.  
       We need to create a new symbol in the callScope.  However, for all the subsequent
       times, we need to retain the name, and not append it.  This is a desirable
       feature, but not necessary.  We can always live with the messy appended names.  
       But, here we chose not to.
         An unavoidable problem though is the issue of redundant definitions.  The 
       subsequent inlining of the same subroutine must not include the redundant  
       definitions of the (appended)name.  We construct a table to remember if this
       variable from the called subroutine has it's definition included once in the
       caller subroutine.  After the first time, we don't include the same definition
       in the caller subroutine.  We could also double check to see if the variable
       is unique in the calling scope.  If so we can retain the name.
     */
       varSubKey key = new varSubKey(ID.getText(),calleename,callername); /* I always need the key */
       uniqueDefValue val;
       
       //System.out.println(ID.getText()+" is not an argument");
       if (callScope.resolve(ID.getText()) == null){
           newname = ID.getText();
           val = new uniqueDefValue();
           val.isUnique = true;
           val.isDefIncluded = false;
           redundantDefsNonArg.put(key,val);           
           //System.out.println("I put in "+key.varName+"  from "+key.calleename); 
       } else {
           if (redundantDefsNonArg.containsKey(key)){
               //System.out.println("I see "+key.varName+"  from "+key.calleename+"  already defined");
               val = (uniqueDefValue)redundantDefsNonArg.get(key);
               if (val.isUnique == true){
                   newname = ID.getText();                   
                   //System.out.println("I see "+key.varName+"  from "+key.calleename+"  already defined is unique");
               } else {
                   newname = ID.getText().concat(appendedname);                           
               }
           } else {
               //System.out.println(key.varName+"  from "+key.calleename+" exists in the callScope. Append it");
               val = new uniqueDefValue();
               val.isUnique = false;
               val.isDefIncluded = false;
               redundantDefsNonArg.put(key,val);
               newname = ID.getText().concat(appendedname);
            /* Enter the newname into the table too.  
               When we delete the redundant iterations, we will be using the newname to 
               construct the key.  It makes things easier if we make another entry into
               the table. 
             */
               key = new varSubKey(newname,calleename,callername); /* I always need the key */               
               val = new uniqueDefValue();
               val.isUnique = false;
               val.isDefIncluded = false;               
               redundantDefsNonArg.put(key,val);                               
           }          
       }         

       
     //System.out.println("newname = "+newname);
       tmp = (FTree)adaptor.create(NAME,newname);
  
  
      /* ID may have a symbol, but still not defined in the subroutinebody. 
         It happens with the subroutine names in the callStatement.  We handle
         them with the if statement below.
       */    
      mysymbol = subroutinebody.resolve(ID.getText());
  
      if(mysymbol != null){
         try{
             tmp.symbol = (Symbol)mysymbol.clone();
         } catch (CloneNotSupportedException e) {
             e.printStackTrace();
         }
         tmp.symbol.name = newname;
         tmp.symbol.scope = callScope;
         if(callScope.resolve(newname) == null){
            callScope.define(tmp.symbol);
         }else{
            tmp.symbol = callScope.resolve(newname);
            //System.out.println(callScope.getScopeName()+" has "+newname+" already!!");
         }
         
         if(barray)
            adaptor.replaceChildren(retvaltree,0,0,tmp);
         else
            retvaltree = replaceTree(retvaltree, tmp);
         
      }else{
         /* Subroutine names, part of the callStatement, have symbols, but are not defined in the 
            enclosingScope i.e. they are not in the symboltable.  We still adjust them to reflect
            the right scope they belong to. */
         if(ID.symbol != null){
            ID.symbol.scope = callScope;
            //System.out.println(ID.getText()+" updates its scope to "+callScope);
         }
         System.out.println("warning!! variable "+retvaltree.toStringTree()+" is not in symbol table!");
      }
  
      return retvaltree;

  }
  
  private FTree fixNonArrayArg(FTree retvaltree, FTree callarg){ 
  /* To fix a name occurrence which is an argument we simply replace it by 
     the callarg.  The callarg can be an array variable, scalar, or even a 
     constant at the callsite.
   */
     FTree tmp;
     String callName;
                 
     if(callarg.getType() == NAME){
        callName = callarg.toString();
        varSubKey key = new varSubKey(callName,calleename,callername); /* I always need the key */
        redundantDefsArg.put(key,callName); /* Redundant Definition */
     }
     tmp = (FTree)adaptor.dupTree(callarg);        
     retvaltree = replaceTree(retvaltree, tmp);
     return retvaltree; 
  }      
  
  private int getArgIndex(String argName){  
    int i, argidx = -1;
    String currArgName = null;
    
 /* A subroutine statement's argument list is merely a collection of NAMEs.
    We can extract one NAME at a time to compare against the passed argName.
  */ 
    for(i=0;i<arglist.size();i++){
        currArgName = ((FTree)arglist.get(i)).toString();           
        if(currArgName.contentEquals(argName)){
           argidx = i;
           break; /* Very crucial to get out when you found the argument */
        }
    }
    return argidx;
  }
  
  private FTree replaceTree(FTree oldTree, FTree newTree){
    int myidx;                                   
    
    myidx = adaptor.getChildIndex(oldTree);                        
    FTree parent = (FTree)oldTree.getParent();                        
    adaptor.replaceChildren(parent,myidx,myidx,newTree);
    newTree = (FTree)parent.getChild(myidx);
    return newTree;
  }
     
  /* Report mismatching bounds between the caller and the callee.  
     It is useful for debugging the input code.
   */
  private void checkBounds(FTree callNameNode, Symbol argSymbol, Symbol callargSymbol){
    String callName = null, argName = null;
    String callargLBDim = null, argLBDim = null;
    String callargUBDim = null, argUBDim = null;    
                          
    callName = callargSymbol.name;        
    argName  = argSymbol.name;                
    if(argSymbol.typelen != callargSymbol.typelen) {
       System.out.println("Arguments type lengths must match between the caller and the callee for "+callName);
       System.exit(-1);             
    }    
       
    if (argSymbol.isArraySymbol()){
     /* If argsymbol is an array symbol then callargSymbol has to be an array symbol too. */     
        if(!callargSymbol.isArraySymbol()){
           System.out.println("The callsite array reference "+callName+" is probably incorrect");
           System.exit(-1);
        }   
        if(((ArraySymbol)argSymbol).nDim != ((ArraySymbol)callargSymbol).nDim){
           System.out.println("Warning: "+callName+" in "+callername+" and "+ argName
                             +" in "+calleename+" do not have the same number of dimensions.");
        } else {
          for(int i = 0; i<((ArraySymbol)argSymbol).nDim;i++){
              callargLBDim = ((ArraySymbol)callargSymbol).LBDim[i];
              callargUBDim = ((ArraySymbol)callargSymbol).UBDim[i];
                  argLBDim =     ((ArraySymbol)argSymbol).LBDim[i];              
                  argUBDim =     ((ArraySymbol)argSymbol).UBDim[i];                  
              if(!callargLBDim.contentEquals(argLBDim) ||
                 !callargUBDim.contentEquals(argUBDim)
                ){
                System.out.println("Warning: "+callName+" and "+ argName+" have "
                             +"mismatching bounds.");
                break;                             
              } 
          }
        }
    }
  }

  /* Handle arrays with non-constant dimensions i.e. the sizes of the dimensions themselves
   *  are passed in as arguments.  We allow only variables such as the problem state which
   *  are passed in as arguments to the computational region themselves to have non-constant
   *  dimensions.  All the temporaries declared inside the computational region must have
   *  constant bounds.  The bounds of such temporaries must be constants even inside other
   *  subroutines called within the computational region. 
   */
  private FTree handleNonConstantDims(Symbol argSymbol, Symbol callargSymbol, FTree inlinedName, FTree callarg, FTree retvaltree){
    FTree offTree;
    FTree subscripts, callsubscripts, newsubscript, newsubscripts;
    String callName = null, argName = null;
    String argLBDim = null;
    String argUBDim = null;    
    boolean bConsBounds = true; /* The bounds are constants for the variable.  
                                 *   Reset the flag otherwise. 
                                 */              
                                        
    callName = callargSymbol.name;        
    argName  = argSymbol.name;  

    for(int i = 0; i<((ArraySymbol)argSymbol).nDim;i++){
        argLBDim =  ((ArraySymbol)argSymbol).LBDim[i];              
        argUBDim =  ((ArraySymbol)argSymbol).UBDim[i];                  
        if(!IntegerEvaluator.isIntConvertable(argLBDim) ||
           !IntegerEvaluator.isIntConvertable(argUBDim)
          ){
            bConsBounds = false;    /* A non-constant array bound has been detected */
            break;                             
        } 
    }
    
    if (bConsBounds) return null;    /* Constant bounds are handled by mapping dimensions better */

    /*
    if (!callargSymbol.isArgument()){
     * The variable has non-constant bounds but is not an argument in 
      * the calling routine.  It is not allowed.
      *
        System.out.println("The computational temporary has non-constant bounds "
                   +callName+" in the subroutine calleename."
                   +" Not allowed.  Computational temporaries must have constant bounds"
                   +" everywhere\n"); 
        System.exit(-1);        
    }
    */
              
    if(argSymbol.typelen != callargSymbol.typelen) {
       System.out.println("Arguments type lengths must match between the caller and the callee for "+callName+" in the subroutine "+calleename+"\n");
       System.exit(-1);             
    }    
       
    if (argSymbol.isArraySymbol()){
     /* If argsymbol is an array symbol then callargSymbol has to be an array symbol too. */     
        if(!callargSymbol.isArraySymbol()){
           System.out.println("The callsite array reference "+callName+" is probably incorrect when calling the subroutine "+calleename+"\n");
           System.exit(-1);
        }
    }
    
    /* Construct the offset for the first dimesion to which we will be adding the linearized subscripts */
    offTree = null;
    if(callarg.getChildCount() > 0){
       callsubscripts = (FTree)callarg.getChild(1);
       offTree  = (FTree)callsubscripts.getChild(0);  /* Get the first subscript */                 
    } else {
       offTree  = (FTree)adaptor.dupTree(((ArraySymbol)callargSymbol).LBDimTree[0]);
    }
//    System.out.println("inliner: fixVarRefs.g:");

    subscripts = (FTree)adaptor.dupTree((FTree)retvaltree.getChild(1));
    newsubscript = linearizeNonConsBounds(argSymbol, callargSymbol, subscripts, offTree);    
//    System.out.println("subscripts: "+subscripts.toStringTree());
//    System.out.println("newsubscript: "+newsubscript.toStringTree());
    
    newsubscripts = dummySubscripts(callarg);    
    adaptor.setChild(newsubscripts,0,newsubscript);  /* Replace the first subscript */ 
    
    /* Replace the susbcripts */
    adaptor.replaceChildren(retvaltree,1,1,newsubscripts);
    
    /* Replace the name in the arrayref */        
    /* NOTE: It is very important to replace the name with a new copy of callNameNode each time.
     *  I spent a week debugging in the dark when I tried replacing the name node below with callNameNode directly.
     */
    adaptor.replaceChildren(retvaltree,0,0,inlinedName);
    
    varSubKey key = new varSubKey(callName,calleename,callername); /* I always need the key */
    redundantDefsArg.put(key,callName); /* Redundant Definition. callScope already has this variable defined.
                                                       I am replacing the name even at the definitions.  callName exists
                                                       at the definitions and not argName.  Therefore, we got to remove
                                                       this callName definition from the inlined subroutine.
                                                     */
//    System.out.println("retvaltree: "+retvaltree.toStringTree());
    //System.exit(0);
    return retvaltree;  
  }

  private FTree linearizeNonConsBounds(Symbol argsymbol, Symbol callargSymbol, FTree subscripts, FTree offTree){
     FTree subscript = null, newstartSubscript = null, linTree = null;
     FTree indextree = null;
     FTree argLBTree = null, argprevLBTree = null, argprevUBTree = null;
     FTree callargLBTree = null;
     FTree addtree = null, minustree = null, multree = null; 
     FTree sizeprevDimTree = null;        
     int startDim = 0;
     int endDim = ((ArraySymbol)argsymbol).nDim-1;
     int j;
     String arglb, argub;
     String argDimSize;                        
     List newdims;
     
     newdims = (List)varLenArrayDims.get(argsymbol.name);
     if (null == newdims){
         System.out.println("Internal error: fixVarRefs.g: the argument "+argsymbol.name+" of the subroutine "+calleename+" is not present in the dimension map table for variable length arrays\n");         
         Set<Map.Entry<String, List>> entrySet = varLenArrayDims.entrySet();
         System.out.println("varLenArrayDims:");
         for (Map.Entry entry : entrySet) {
              System.out.print("key,val: ");
              System.out.println(entry.getKey() + "," + entry.getValue());
         } 
         System.exit(-1);
     }
     argLBTree = (FTree)adaptor.dupTree((FTree)((List)(newdims.get(0))).get(0));
     indextree = (FTree)adaptor.create(T_MINUS,"-");                     
     indextree.addChild((FTree)adaptor.dupTree((FTree)subscripts.getChild(startDim)));
     indextree.addChild(argLBTree);  
     
     newstartSubscript = (FTree)adaptor.create(T_PLUS,"+");
     newstartSubscript.addChild(indextree);
     if (null != offTree) {  /* offTree will be null when LBDimTree of callarg's first subscript is null */
        newstartSubscript.addChild((FTree)adaptor.dupTree(offTree));
        //System.out.println("the adjusted subscript : "+newTree.toStringTree());
     } else {
        if (null == ((ArraySymbol)callargSymbol).LBDimTree[0]) {
           /* If the LBDimTree of the first dimension is null then assign 1 to lowerbound */
           callargLBTree = (FTree)adaptor.create(ICON,"1");
        } else {
           callargLBTree = (FTree)adaptor.dupTree(((ArraySymbol)callargSymbol).LBDimTree[0]);
        }
        newstartSubscript.addChild(callargLBTree);  // callarg first dimension LBDimTree or, if null, "1"
     }
                                                               
  /* If there are more than one arg dimension, linearize the remaining dimensions. */
     for(j=endDim;j>startDim;j--){
         // subscripts[j] - lb[j] 
         argLBTree = (FTree)adaptor.dupTree((FTree)((List)(newdims.get(j))).get(0));         
         indextree = (FTree)adaptor.create(T_MINUS,"-");                     
         indextree.addChild((FTree)adaptor.dupTree((FTree)subscripts.getChild(j)));
         indextree.addChild(argLBTree);
               
         if (null != linTree){
          // (subscripts[j] - lb[j]) + <expression computed for the subsequent dimensions>
             addtree = (FTree)adaptor.create(T_PLUS,"+");
             addtree.addChild(indextree);
             addtree.addChild(linTree);
             indextree = addtree;
         }    
                  
         /* If the size of the previous dimension is a compile time integer constant then 
          *  use the constant.  Else, the size of the previous dimension = (prevUB-prevLB+1)
          */  
         if (IntegerEvaluator.isIntConvertable(((ArraySymbol)argsymbol).sizeDim[j-1])){
             sizeprevDimTree = (FTree)adaptor.create(ICON,((ArraySymbol)argsymbol).sizeDim[j-1]);
         } else {
             // (ub-lb+1)
             argprevLBTree = (FTree)adaptor.dupTree((FTree)((List)(newdims.get(j-1))).get(0));
             argprevUBTree = (FTree)adaptor.dupTree((FTree)((List)(newdims.get(j-1))).get(1));
             minustree = (FTree)adaptor.create(T_MINUS,"-");
             minustree.addChild((FTree)adaptor.dupTree(argprevUBTree));
             minustree.addChild((FTree)adaptor.dupTree(argprevLBTree));
             sizeprevDimTree = (FTree)adaptor.create(T_PLUS,"+");             
             sizeprevDimTree.addChild(minustree);
             sizeprevDimTree.addChild((FTree)adaptor.create(ICON,"1"));
         } 
                                                
      // (subscripts[j] - lb[j] +<expression computed for the subsequent dimensions, if any>) * sizeprevdim
         multree = (FTree)adaptor.create(T_STAR,"*");
         multree.addChild(indextree);
         multree.addChild(sizeprevDimTree);
         linTree = multree;                                 
     }

     if(null != linTree) {
        addtree = (FTree)adaptor.create(T_PLUS,"+");
        addtree.addChild(newstartSubscript);
        addtree.addChild(linTree);
        linTree = addtree;
     } else {
        linTree = newstartSubscript;
     }
                                          
     return linTree;
  }    

  private FTree generateEquiv(FTree callNameNode, Symbol argSymbol, Symbol callargSymbol){
    FTree equivArgNameNode = null;           
    String equivArgName = null;
    String callName = null;
    Symbol equivSymbol = null;    
                          
    callName = callargSymbol.name;                       
    if(argSymbol.typelen != callargSymbol.typelen) {
       System.out.println("Arguments type lengths must match between the caller and the callee for "+callName);
       System.exit(-1);             
    }   
 /* Currently support argument type conversion between INTEGER and REAL only */   
    if (argSymbol.type == Type.typeenum.INTEGER)
    /*  implicit typing */
        equivArgName = "i"+callName;
    else if (argSymbol.type == Type.typeenum.REAL)
          /* implicit typing */
             equivArgName = "r"+callName;
         else {
             System.out.println("Arguments types must match between the caller and the callee for "+callName+
                                ".  Currently support argument type conversion between INTEGER and REAL only");
             System.exit(-1);                   
         }
             
    try{
       equivSymbol = (Symbol)callargSymbol.clone(); /* Duplicate the symbol.  Don't just assign 
                                                     * Copy constructors are not an option because we want statements such as 
                                                     *    if(callsubroutinebody.resolve(ID.getText()) != \$ID.symbol) {
                                                     * to succeed.  It will fail if we try to match a copy instead of the
                                                     * original symbol.
                                                     */
    } catch (CloneNotSupportedException e) {
       e.printStackTrace();
    }        
    equivSymbol.name = equivArgName;
    equivSymbol.type = argSymbol.type;
    equivArgNameNode = (FTree)adaptor.create(NAME,equivArgName);
    equivArgNameNode.symbol = equivSymbol;

    equivVars evars = new equivVars();
    evars.var1 = equivArgNameNode;
    evars.var2 = callNameNode;                        
    newEquivs.add(evars);
          
 /* inliner.g creates the declarations and equivalence statements for these newly
    made arrays.  You may however create a callScope symbol table entry here. 
  */
    callScope.define(equivSymbol);          
 /* Who should set equivalence to who? equivarg should be equivalenced to callarg.
    Reasoning is simple, equivalence is a many-to-one map.
    How does it affect code generation, or memory reduction in pipelining?
  */
    equivArgNameNode.symbol.setEquivalence((BaseScope)callScope,equivArgNameNode,callNameNode);
         
    return equivArgNameNode;     
  } 
  
  private void ConstructDimensionMap(int argidx, Symbol mysymbol, Symbol callargSymbol){
     int ilb, i, j;
     int icurriter, jcurriter;
     int callargdimsize, argdimsize;
     int callsumsofar;
     int argsumsofar;
     boolean bManyargtoOnecallarg = false;
      
  /* Creating the dim map table */
     for(i=0;i<7;i++)
         dimensionmap[argidx][i] = -1;
     
     i=0;
     j=0;
     icurriter = i; /* The value of i at the beginning of an iteration. */
     jcurriter = j; /* The value of j at the beginning of an iteration. */
     callsumsofar = 0;
     argsumsofar = 0;
                  
     while(i<((ArraySymbol)callargSymbol).nDim && j<((ArraySymbol)mysymbol).nDim) {
        /* There can never be excess unmapped arg dimensions. */
           ilb = 0;
           callargdimsize = Integer.parseInt(((ArraySymbol)callargSymbol).sizeDim[i]);
           argdimsize = Integer.parseInt(((ArraySymbol)mysymbol).sizeDim[j]);  
              
           icurriter = i;
           jcurriter = j;
           bManyargtoOnecallarg = false; /* Reset */
                               
           if (callargdimsize == argdimsize) {
               dimensionmap[argidx][icurriter] = jcurriter;
               i++;
               j++;
           }
           if (callargdimsize < argdimsize) {
            /* Many callarg dimensions map to one arg dimension */
               if (callsumsofar > argsumsofar) {
                /* The callarg dimension 'i' can't be factored into the current
                   subset of arg dimensions.
                 */   
                   String argName = mysymbol.name;
                   String callName = callargSymbol.name; 
                   System.err.println("Inliner: Inlining failed. Cannot map "+argName+" in "+calleename+
                                      " to "+callName+" in "+callername+". Some subset of dimensions of "+
                                      callName+" in "+callername+" appeared to map onto one dimension of "+
                                      argName+" in "+calleename+", but the sizes mismatch.  The caller "+
                                      "argument dimensions are not exact factors of the called argument dimension.");
                   System.exit(-1);                       
                 //bdimsmap = false;
                 //break; 
               }                       
                   
               dimensionmap[argidx][icurriter] = jcurriter;
             //System.out.println(callargdimsize+"<"+argdimsize+": dim map: "+icurriter+"->"+jcurriter);                  
                    
               if (callsumsofar > 0)     
                   callsumsofar = callsumsofar * callargdimsize;
               else
                   callsumsofar = callargdimsize;
               if (argsumsofar == 0)
                   argsumsofar = argdimsize;
               i++;
             //System.out.println("callsumsofar = "+callsumsofar+"; argsumsofar = "+argsumsofar);            
               if (callsumsofar == argsumsofar){
                   callsumsofar = 0;
                   argsumsofar = 0;                   
                   j++;   
               }                     
           }       
           if (callargdimsize > argdimsize) {
            /* Many arg dimensions map to one callarg dimension */
               if (argsumsofar > callsumsofar){
                /* The arg dimension 'j' can't be factored into the current
                   subset of callarg dimensions.
                 */   
                   String argName = mysymbol.name;
                   String callName = callargSymbol.name;
                   System.err.println("Inliner: Inlining failed. Cannot map "+argName+" in "+calleename+
                                      " to "+callName+" in "+callername+". Some subset of dimensions of "+
                                      argName+" in "+calleename+" appeared to map onto one dimension of "+
                                      callName+" in "+callername+", but the sizes mismatch.  The called "+
                                      "argument dimensions are not exact factors of the caller argument dimension.");
                   System.exit(-1);
                 //bdimsmap = false;                       
                 //break; 
               }
           
               bManyargtoOnecallarg = true;
               if (argsumsofar > 0)
                   argsumsofar = argsumsofar * argdimsize;
               else
                   argsumsofar = argdimsize;             
               if (callsumsofar == 0)
                   callsumsofar = callargdimsize;
               j++;
             //System.out.println("callsumsofar = "+callsumsofar+"; argsumsofar = "+argsumsofar);
               if (callsumsofar == argsumsofar){
                   dimensionmap[argidx][icurriter] = jcurriter; /* (dimensionmap[icurriter-1],dimensionmap[icurriter])
                                                                   represents the subset of arg dimensions callarg 
                                                                   dimension 'icurriter' factors into. */
                 //System.out.println(callargdimsize+">"+argdimsize+": dim map: "+icurriter+"->"+jcurriter);                                
                   callsumsofar = 0;
                   argsumsofar = 0;
                   i++; 
               }
           }
     }
     if(true == bManyargtoOnecallarg && i == icurriter && j>((ArraySymbol)mysymbol).nDim-1){
     /* The last dimension(s) of arg map to one callarg dimension, but the size of the arg 
        dim(s) is less than the size of the callarg dim.  The 'if (callsumsofar == argsumsofar)'
        condition fails in such cases, and we miss assigning the dimensionmap.
      */
        dimensionmap[argidx][icurriter] = ((ArraySymbol)mysymbol).nDim-1;
        i++;
     }
     while(i<((ArraySymbol)callargSymbol).nDim){
        /* Excess unmapped callarg dimensions */
           dimensionmap[argidx][i] = ((ArraySymbol)mysymbol).nDim;  /* Used in the termination condition 
                                                                       of the bAdjustOff section in the retval 
                                                                       construction at the calling routine */
         //System.out.println("Excess dimensions: dim map: "+i+"->"+j);                      
           i++;
     }
  }
  
  private void ConstructAdjustOff(int argidx, Symbol mysymbol, FTree callarg, Symbol callargSymbol){
     String off = "0";  /* Offset at the callsite; ith subscript of callarg */
     int iarglb = 0; 
     int icallarglb = 0; 
     boolean boff = false;
     FTree callsubscripts = null;
     FTree callsubscript = null;
     int i, ioff;
     int prevmapdim, mapdim, nextmapdim;
            
     for(i=0;i<7;i++)
         AdjustOff[argidx][i] = null;
         
     /* Have taken care of excess unmapped callarg dimensions with the dummy list newsubscripts 
        in the final tree construction.  No need to construct AdjustOff for the unmapped
        dimensions. However I should turn bAdjustOff on.  Otherwise, we will not invoke 
        dummySubscripts.  */   
     if(((ArraySymbol)callargSymbol).nDim != ((ArraySymbol)mysymbol).nDim)
          bAdjustOff[argidx] = true;
                
     mapdim = -1;
     nextmapdim = dimensionmap[argidx][0];
     for (i=0;i<((ArraySymbol)callargSymbol).nDim && nextmapdim < ((ArraySymbol)mysymbol).nDim;i++){
          prevmapdim = mapdim;
          mapdim = nextmapdim;             
          if (i == ((ArraySymbol)callargSymbol).nDim - 1) // Last dimension
              nextmapdim = ((ArraySymbol)mysymbol).nDim;  // nextmapdim acts as a limit, and doesn't exist.  
                                                          // It's alright because we never access subscript[nextmapdim] for the last dimension.
          else    
              nextmapdim = dimensionmap[argidx][i+1];  // dimensionmap maps the callarg dimensions to the arg dimensions.  
                                                       // Only callarg matters.  Args become callargs after inlining.
          

          if (prevmapdim < mapdim){
          /* A one to one map or the beginning of a many to one map.
           * Get the lower bound for only the first arg dimension in this map.
           */
             iarglb = Integer.parseInt(((ArraySymbol)mysymbol).LBDim[prevmapdim+1]);
          }                                                         
          icallarglb = Integer.parseInt(((ArraySymbol)callargSymbol).LBDim[i]);                                                                     

                       
          callsubscript = null;
          if(callarg.getChildCount() > 0){
             callsubscripts = (FTree)callarg.getChild(1);
             callsubscript  = (FTree)callsubscripts.getChild(i);                 
          }
          
          if(mapdim > prevmapdim) {
          /* A one to one map or the beginning of the many to one map */
          
             if(mapdim > prevmapdim + 1)
                bAdjustOff[argidx] = true;  /* Many to one map.  
                                               Need to adjust the offset.
                                             */  
          /* If there IS an offset at the call site then the offset to add is 
                (icallarglb - iarglb) + (callsubscript - icallarglb) = (callsubscript - iarglb). 
             If there is NO offset at the call site then the offset to add is 
                (icallarglb - iarglb). 
           */
             if (callsubscript != null) {
                FTree newtree = null;
                if(iarglb<0){
                   newtree = (FTree)adaptor.create(T_PLUS,"+");
                   iarglb = -iarglb;
                } else {
                   newtree = (FTree)adaptor.create(T_MINUS,"-");
                }   
                newtree.addChild((FTree)adaptor.dupTree(callsubscript));
                newtree.addChild((FTree)adaptor.create(ICON,Integer.toString(iarglb)));
                AdjustOff[argidx][i] = newtree;
                bAdjustOff[argidx] = true;
             } else {                
                if (icallarglb - iarglb != 0) {
                    bAdjustOff[argidx] = true;
                    AdjustOff[argidx][i] = (FTree)adaptor.create(ICON,Integer.toString(icallarglb - iarglb));
                }    
             }
          } else {                   
          /* Many callarg map to one arg, but it is not the beginning callarg dimension of the map */                  
          /* Offset is just the offset at the callsite, if any, or it is the lowerbound of the callarg dimension */
             if (callsubscript != null)
                 AdjustOff[argidx][i] = (FTree)adaptor.dupTree(callsubscript);
             else    
                 AdjustOff[argidx][i] = (FTree)adaptor.create(ICON,Integer.toString(icallarglb));
             bAdjustOff[argidx] = true;    
          } 
          /* If many arg map to one callarg, but it is not the beginning arg dimension of the map,
             do nothing.  It is all taken care in the tree construction below.
             I need another table to store the offsets of arg to callarg.  Also, 
             another one to store the size of the previous dimensions.  It seems
             very bloated.  I am not creating these two data structures.  Instead,
             I will move this logic to the final tree construction phase.
           */                     
     }
  }
  
  private FTree dummySubscripts(FTree callarg){
         /* Construct a dummy list */
    FTree newsubscripts = null;
    Symbol callargSymbol;
    String lb=null;
    int i;
         
    if(callarg.getChildCount() > 0){
       newsubscripts = (FTree)adaptor.dupTree((FTree)callarg.getChild(1));
       callargSymbol = (Symbol)((FTree)callarg.getChild(0)).symbol;
    } else {
    /* callArgType == NAME */
       callargSymbol = (Symbol)callarg.symbol;             
       newsubscripts = (FTree)adaptor.create(SUBSCRIPT,"SUBSCRIPT");    
       for(i=0;i<((ArraySymbol)callargSymbol).nDim;i++){
           lb = ((ArraySymbol)callargSymbol).LBDim[i];                    
           if (IntegerEvaluator.isIntConvertable(lb)) {
               newsubscripts.addChild((FTree)adaptor.create(ICON,((ArraySymbol)callargSymbol).LBDim[i]));
           } else {
               newsubscripts.addChild((FTree)adaptor.dupTree(((ArraySymbol)callargSymbol).LBDimTree[i]));          
           }
       }
    }
    return newsubscripts; 
  }           
  
  private FTree linearizeArgDims(Symbol argsymbol, FTree subscripts, int startDim, int endDim, FTree offTree){
     FTree subscript = null, newstartSubscript = null, linTree = null;
     FTree indextree = null;
     FTree addtree = null, multree = null;         
     int j, iarglb, argDimSize;                         

     argDimSize = Integer.parseInt(((ArraySymbol)argsymbol).sizeDim[startDim]);
                  
     if(subscripts != null && argDimSize > 1) 
     /* Don't output the subscript if argDimSize is 1.  It creates problems with the memory
        reductions in pipelining.
      */   
        subscript = (FTree)adaptor.dupTree((FTree)subscripts.getChild(startDim));
     else 
        subscript = (FTree)adaptor.create(ICON,((ArraySymbol)argsymbol).LBDim[startDim]);
        
     newstartSubscript = subscript;       
     if(offTree != null){        
        newstartSubscript = (FTree)adaptor.create(T_PLUS,"+");                                               
        newstartSubscript.addChild(subscript);
        newstartSubscript.addChild((FTree)adaptor.dupTree(offTree));
         //System.out.println("the adjusted subscript : "+newTree.toStringTree());                      
     }  
                                       
  /* If there are more than one arg dimension, linearize the remaining dimensions. */
     for(j=endDim;j>startDim;j--){                        
         iarglb = Integer.parseInt(((ArraySymbol)argsymbol).LBDim[j]);         
         if (subscripts != null){
          // subscripts[j] - lb[j]         
             if (iarglb > 0)
              /* The logic here is right. */
                 indextree = (FTree)adaptor.create(T_MINUS,"-");
             else {    
                 indextree = (FTree)adaptor.create(T_PLUS,"+");
                 iarglb = -iarglb;
             }                              
             indextree.addChild((FTree)adaptor.dupTree((FTree)subscripts.getChild(j)));
             indextree.addChild((FTree)adaptor.create(ICON,Integer.toString(iarglb)));
         } else {
          /* There are no subscripts.  Just assign the lowerbound to the indextree */ 
             indextree = (FTree)adaptor.create(ICON,Integer.toString(iarglb));   
         }
         
         argDimSize = Integer.parseInt(((ArraySymbol)argsymbol).sizeDim[j]);
         if(argDimSize == 1)
         /* Don't output the subscript if argDimSize is 1.  It creates problems with the memory
            reductions in pipelining.
          */
            indextree = (FTree)adaptor.create(ICON,"0");
               
         if (linTree != null){
          // (subscripts[j] - lb[j]) + <expression computed for the subsequent dimensions>
             addtree = (FTree)adaptor.create(T_PLUS,"+");
             addtree.addChild(indextree);
             addtree.addChild(linTree);
             indextree = addtree;
         }     
                                                
      // (subscripts[j] - lb[j] +<expression computed for the subsequent dimensions, if any>) * sizeprevdim
         multree = (FTree)adaptor.create(T_STAR,"*");
         multree.addChild(indextree);
         multree.addChild((FTree)adaptor.create(ICON,((ArraySymbol)argsymbol).sizeDim[j-1]));
         linTree = multree;                                 
     }

     if(linTree != null) {
        addtree = (FTree)adaptor.create(T_PLUS,"+");
        addtree.addChild(newstartSubscript);
        addtree.addChild(linTree);
        linTree = addtree;
     } else
        linTree = newstartSubscript;
                                          
     return linTree;
  }    

  private FTree fixArg(FTree retvaltree, FTree ID, boolean barray){  
  int ndimtoadd = 0;                                      
  int i, j, ioff, callArgType, callsubscriptType;
  int argidx = -1;  
  int argtypelen = -1, callargtypelen = -1;
  boolean bequiv = false;
  
  String callName = "";
  Symbol mysymbol = null;  
  Symbol callargSymbol = null;
  FTree equivArg = null, inlinedName = null;
  FTree callarg = null;
  FTree callNameNode = null;  
  FTree callsubscripts = null;
  FTree callsubscript = null;
  FTree newsubscripts = null;
  FTree newsubscript = null;
  FTree varLenArrayTree = null;
  
  /*                 JJ: 04/17/2014
     We retain the caller side names for an argument.  If the dimensions of an
     array argument are constants then we map the subscripts between the callee 
     and the caller for this array.  Sometimes, the dimensions for an array
     argument are passed in as arguments themselves.  In some cases, the dimensions
     of such an array are not constants even in the caller.  Therefore, it is 
     impossible to find the array bounds even if we perform interprocedural analysis.  
        It would be ideal if we could just create an equivalence between the caller
     and the callee for such arrays whose bounds vary.  However, Fortran doesn't
     allow equivalences when the sizes are not constants.  We will instead map all 
     the callee subscripts to the first subscript in the caller.  

     eg: subroutine caller(len1,len2,len3)
             dimension array(len1*len2,len3)
             call callee(array,len1,len2,len3)
             ....
         end

         subroutine callee(array,len1,len2,len3)
             dimension arrayvar(len1,len2,len3)
             arrayvar(1,5,6) = 5   ! stmt1
             ....
         end

      The stmt1 in callee will be inlined as follows,
          
             array(1+len1*((5-1)+(6-1)*len2),1,1) = 5 
     
     For arguments with constant dimensions, we have a very sophisticated mechanism 
     to retain or replace the variables.  We try to map the subscripts of the callee 
     argument to its corresponding caller side argument dimensions.  We support 
     many-to-one mappings of the dimensions as long as the total sizes of the arrays 
     match between the caller and the callee.
        We also support the case when the last dimension on the callee side not of 
     the same size as the caller side.  A natural side-effect is that any extra 
     dimensions at the end from the call side gets added to the inlined variable.
     If necessary the inliner can be extended to handle other complicated cases with 
     mismatching sizes by creating equivalences or by linearizing the subscripts.
     However, they will create complications with the pipelining and reduceMem 
     phases.  In addition, you must be careful with equivalencing.  The subroutine
     may be called with different sets of arguments.  You must not equivalence these
     different arguments.  One solution is to create unique names for the variables
     to be equivalenced everytime the subroutine is inlined.  Ignoring such complexities,
     this inliner is probably the most powerful for Fortran with regards 
     to retaining the original caller names and generating readable code.   
   */

 /* Argument */
 /* 04/26/2011 - Adding dimensionmap */
    
    argidx = getArgIndex(ID.getText());       
    callarg = (FTree)callingArglist.get(argidx);
              
    if (!barray)
        return fixNonArrayArg(retvaltree, callarg);

 /* ****** We deal with only ARRAYREFs and array declarations here ****** */ 
    mysymbol = subroutinebody.resolve(ID.getText());
    if(callarg.getChildCount() > 0)
       callNameNode = (FTree)callarg.getChild(0);
    else
       callNameNode = callarg;         
    callName = callNameNode.toString();
    callargSymbol = (Symbol)callNameNode.symbol;
            
    if(bargSeen[argidx] == false){
    /* *** Seeing the argument name for the first time *** */
       inlinedName = (FTree)adaptor.dupTree(callNameNode);

    /* Handle arrays whose lengths themselves are passed in as arguments */
      /* NOTE: It is very important to use a new copy of callNameNode each time you do replacement.
       *  I spent a week debugging in the dark when I tried replacing the name node in handleNonConstantDims
       *  procedure with callNameNode directly.  Therefore, pass inlinedName in place of callNameNode.
       */
       varLenArrayTree = handleNonConstantDims(mysymbol, callargSymbol, inlinedName, callarg, retvaltree);       
       if (varLenArrayTree != null) return varLenArrayTree;
       
    /* We handle the special cases for constant dimensions of the inlined variables here.
     * The caller could have passed variable length arrays, but only in the last dimension. 
     */
    
    /* Report mismatching bounds between the caller and the callee.  
       It is useful for debugging the input code.
     */
       checkBounds(callNameNode, mysymbol, callargSymbol);
       
       if(mysymbol.type != callargSymbol.type){
       /* Create an equivalent array of same dimensions as the callarg but of type arg.
          Now imagine that this equivalent arg was passed to the callee instead of the
          original callarg.  We then can successfully map arg to callarg. 
          We can accomplish this by simply changing callName to the generated equivalent
          argument name.  generateEquiv comes up with the appropriate name node.
        */        
          inlinedName = generateEquiv(callNameNode, mysymbol, callargSymbol);
          callName = inlinedName.toString();
          bequiv = true;
       }                
          
    /* dimensionmap:
        Map for (lb1:ub1,lb2:ub2,lb3:ub3) -> (lb0,ub0):  call site (off0)
           (i,j,k) -> ((i+lb0-lb1)+(j-lb2)*(ub1-lb1+1)+(k-lb3)*(ub1-lb1+1)*(ub2-lb2+1)+(off0-lb0))
        Vice-versa, (lb0,ub0) -> (lb1:ub1,lb2:ub2,lb3:ub3):  call site (off1,off2,off3)
           (i) -> (i+(lb1-lb0)+(off1-lb1),off2,off3)      lb is the default offset for any dimension
        Test cases:
            1) callarg:  dimension a(-10:89,5:6,2:3,-5:5,0:10,10,10)       
               callsite: call sub(a(0,7,2,0,5,2,2))
               arg:      dimension b(10,10,4,-5:5,5)
               map:      b(5,5,3,2,3) -> a(((5 + -1) + ((5 - 1) * 10)),(3 + 6),2,(2 + 5),(3 + 4),2,2)
     */   
       ConstructDimensionMap(argidx, mysymbol, callargSymbol);
       ConstructAdjustOff(argidx, mysymbol, callarg, callargSymbol);          
    
       if (bequiv == false){
           varSubKey key = new varSubKey(callName,calleename,callername); /* I always need the key */
           redundantDefsArg.put(key,callName); /* Redundant Definition. callScope already has this variable defined.
                                                       I am replacing the name even at the definitions.  callName exists
                                                       at the definitions and not argName.  Therefore, we got to remove
                                                       this callName definition from the inlined subroutine.
                                                     */
       }                                              
       bargSeen[argidx] = true; 
       argInlineName[argidx] = inlinedName;                                                            
    }
 /* End of the newly seen argument name IF statement */    
 
    inlinedName = (FTree)adaptor.dupTree(argInlineName[argidx]);         
    if (bAdjustOff[argidx]) {
        FTree subscripts = null;
        int prevmapdim, mapdim, nextmapdim;
        
        newsubscripts = dummySubscripts(callarg);
        //System.out.println("newsubscripts from dummySubscripts = "+newsubscripts.toStringTree());
                            
        mapdim = -1;
        nextmapdim = dimensionmap[argidx][0];
        for (i=0;i<((ArraySymbol)callargSymbol).nDim && nextmapdim < ((ArraySymbol)mysymbol).nDim;i++){
             prevmapdim = mapdim;
             mapdim = nextmapdim;
             if (i == ((ArraySymbol)callargSymbol).nDim - 1) // Last dimension
                 nextmapdim = ((ArraySymbol)mysymbol).nDim;  // nextmapdim acts as a limit, and doesn't exist.  
                                                             // It's alright because we never access subscript[nextmapdim] for the last dimension.
             else    
                 nextmapdim = dimensionmap[argidx][i+1];  // dimensionmap maps the callarg dimensions to the arg dimensions.  
                                                          // Only callarg matters.  Args become callargs after inlining.                                                  
          // case 1:
             if(mapdim > prevmapdim){
                subscripts = (FTree)adaptor.dupTree((FTree)retvaltree.getChild(1));
                newsubscript = linearizeArgDims(mysymbol, subscripts, prevmapdim+1, mapdim, AdjustOff[argidx][i]);                      
             } else {  
             // case 2:  mapdim == prevmapdim 
             // More than one callarg dimensions map to one arg dimension.  This is not 
             // the first callarg dimension which map to the arg dimension.
             // We have already computed the right offset, and stored it in AdjustOff.
                newsubscript =(FTree)adaptor.dupTree(AdjustOff[argidx][i]);
             }
             //System.out.println(retvaltree.toStringTree());
             //System.out.println("callName, i = "+callName+"  "+i+"newsubscripts = "+newsubscripts.toStringTree()+" newsubscript = "+newsubscript.toStringTree());
             adaptor.setChild(newsubscripts,i,newsubscript);  
        }
     /* Replace the susbcripts */
        adaptor.replaceChildren(retvaltree,1,1,newsubscripts);
    } 
 /* Replace the name in the arrayref */        
    adaptor.replaceChildren(retvaltree,0,0,inlinedName);

    return retvaltree;  
  }
  
  private FTree fixRef(FTree retvaltree, FTree ID, boolean barray){
  /* Subroutine Scope encloses the subroutinebody Scope.  Subroutine contains
     only the argument symbols.  If the resolve can't find the symbol in the 
     subroutinebody scope, it looks up one level at subroutine.  By this 
     process, we can always resolve a symbol that appears anywhere in the
     subroutine.
   */
     if(subroutine.resolve(ID.getText()) != null)  
        retvaltree = fixArg(retvaltree, ID, barray);
     else
        retvaltree = fixNonArg(retvaltree, ID, barray);
                
     return retvaltree;        
  }
  
}

topdown
    : array
    | name
    ;               

array
@after{    
   if(callsubroutinebody.resolve(ID.getText()) != $ID.symbol) {
   /* The token is not duplicated from the caller. */
      //System.out.println("ID (array) = "+$ID.text);
    //System.out.println("retval.tree (before) = "+retval.tree.toStringTree());
      retval.tree = fixRef((FTree)retval.tree,$ID,true);
    //System.out.println("retval.tree (after) = "+retval.tree.toStringTree());
   } 
}:   
   ^(ARRAY ID=NAME .+)
  |^(ARRAYREF ID=NAME .+) 
 ;
 
name
@after{
   //System.out.println("retval.tree (before) = "+retval.tree.toStringTree());
   if (((FTree)retval.tree.getParent()).getType() != ARRAYREF && ((FTree)retval.tree.getParent()).getType() != ARRAY){
         if(callsubroutinebody.resolve(ID.getText()) != $ID.symbol) {
         /* The token is not duplicated from the caller. */
            //System.out.println("ID (name) = "+$ID.text);
            // System.out.println("parent type = "+((FTree)retval.tree.getParent()).getType());
            retval.tree = fixRef((FTree)retval.tree,$ID,false);
         }
   }      
   //System.out.println("retval.tree (after) = "+retval.tree.toStringTree());
}:
   ID=NAME
 ;
    
 
