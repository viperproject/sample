package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.ToStringUtilities

/** 
 * This class represents a point of the program. It is specific 
 * for a given programming language, so it is defined as an abstract
 * class that should be extended by different parsers
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
abstract class ProgramPoint

/** 
 * This class represents all the sequential statements of 
 * a "standard" OO program
 * Programs written in many different languages (e.g. Scala,
 * Java, C#, F#, and, why not, Java bytecode and MSIL) may
 * be transformed using this representation.
 * These statements are supposed to be the atomic pieces of
 * an OO language
 *
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
abstract class Statement(programpoint : ProgramPoint) extends SingleLineRepresentation {  
      /** 
       * The abstract forward semantics of the statement.
	   *
	   * @param state the initial state
	   * @return the state obtained after the execution of the statement 
	   */
  def forwardSemantics[S <: State[S]](state : S) : S
  
  	  /** 
       * The abstract backward semantics of the statement.
	   *
	   * @param state the exit state
	   * @return the state obtained before the execution of the statement 
	   */
  def backwardSemantics[S <: State[S]](state : S) : S
  
      /** 
       * The program point of the statement.
	   *
	   * @return the program point 
	   */  
  def getPC() : ProgramPoint = programpoint
  
  def normalize() : Statement = this match {
	  case x : ControlFlowGraph =>
	    x.asInstanceOf[ControlFlowGraph].nodes match {
	      case y :: Nil => y match {
	        case w :: Nil => w.normalize()
	        case _ => x
	      }
          case _ => x
       }
       case Assignment(programpoint, left, right) => Assignment(programpoint, left.normalize(), right.normalize())
       case VariableDeclaration(programpoint, variable, typ, right) => VariableDeclaration(programpoint, variable, typ, right.normalize) 
       case FieldAccess(pp, objs, field, typ) => FieldAccess(pp, normalizeList(objs), field, typ)
       case MethodCall(pp, method, parametricTypes, parameters, returnedType) => MethodCall(pp, method.normalize(), parametricTypes, normalizeList(parameters), returnedType)
       case Throw(programpoint, expr) => Throw(programpoint, expr.normalize()) 
	   case z => z
	}
  
  private def normalizeList(list : List[Statement]) : List[Statement] = list match {
    case Nil => Nil
    case x :: xs => x.normalize() :: normalizeList(xs)
  }
  
  override def toString() : String
}

/** 
 * This class represents an assignment of the form
 * <code>left=right</code>
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class Assignment(programpoint : ProgramPoint, left : Statement, right : Statement) extends Statement(programpoint : ProgramPoint) {
	  
      /** 
       * It applies the method <code>assignVariable</code>
       * of class <code>State</code>
	   *
       * @see State.assignVariable(Variable, Expression)
	   * @param state the initial state
	   * @return the state in which <code>left</code> has been 
       * assigned to <code>right</code> 
	   */
      override def forwardSemantics[S <: State[S]](state : S) : S = {
        val stateleft : S = left.forwardSemantics[S](state)
        val exprleft = stateleft.getExpression();
        stateleft.removeExpression();
        val stateright : S = right.forwardSemantics[S](stateleft)
        val exprright = stateright.getExpression();
        stateright.removeExpression();
        stateright.assignVariable(exprleft, exprright)
      }
      
      //the backward semantics of x=E is S[|havoc x|](S[|x==E||](\sigma)) 
      override def backwardSemantics[S <: State[S]](state : S) : S = {
        if(state.equals(state.bottom())) return state;
        var stateleft : S = left.backwardSemantics[S](state)
        val exprleft = stateleft.getExpression();
        stateleft=stateleft.removeExpression();
        var stateright : S = right.backwardSemantics[S](stateleft)
        val exprright = stateright.getExpression();
        stateright=stateright.removeExpression();
        var result=stateright.setVariableToTop(exprleft);
        val condition=result.getExpression().createBinaryExpression(exprleft, exprright, ArithmeticOperator.==, result, exprleft.getType(state).top());//TODO type is wrong
        result=result.setExpression(condition);
        return result.testTrue().backwardAssignVariable(exprleft, exprright);
	  }
      
      override def toString() : String = left + " = " + right;
      
      override def toSingleLineString() : String = left.toSingleLineString + " = " + right.toSingleLineString;
      
}

/** 
 * This class represents a variable declaration of the form
 * <code>typ variable[=right]</code>
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class VariableDeclaration(programpoint : ProgramPoint, val variable : Variable, typ : Type, right : Statement) extends Statement(programpoint : ProgramPoint) {

      /** 
       * It creates the variable relying on the method 
       * <code>createVariable</code> of class <code>State</code> 
       * and it eventually assigns <code>right</code> relying on 
       * the method <code>assignVariable</code> of class 
       * <code>State</code>
	   *
       * @see State.createVariable(Variable, Type)
       * @see State.assignVariable(Variable, Expression)
	   * @param state the initial state
	   * @return the state in which <code>variable</code> has been 
       * created, and eventually it has been assigned to <code>right</code> 
	   */
    override def forwardSemantics[S <: State[S]](state : S) : S = {
      var variableEval : S = variable.forwardSemantics[S](state)
      val varExpr = variableEval.getExpression()
      variableEval = variableEval.removeExpression()
      var state1 = variableEval createVariable(varExpr, typ);
      if(right!=null) {
        var rightEval : S = right.forwardSemantics[S](state1)
        val rightExpr = rightEval.getExpression()
        rightEval = rightEval.removeExpression()
        rightEval assignVariable(varExpr, rightExpr)
      }
      else state1
      }
    
    override def backwardSemantics[S <: State[S]](state : S) : S = {
      var st=state;
      if(right!=null)
        st=new Assignment(programpoint, new Variable(programpoint, new VariableIdentifier(variable.getName(), typ)), right).backwardSemantics[S](st);
      return st.removeVariable(st.getExpression().createVariable(variable, st, typ));
    }

    override def toString() : String = "declare "+ToStringUtilities.toStringIfNotNull(typ)+" "+variable.toString()+ToStringUtilities.assignedIfNotNull(right);
    override def toSingleLineString() : String = "declare "+ToStringUtilities.toStringIfNotNull(typ)+" "+variable.toString()+
      {if(right!=null) "="+right.toSingleLineString() else ""} ;
}

/** 
 * This class represents the access of a variable <code>id</code>
 * It extends MethodIdentifier as it may be used to identify a method
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class Variable(programpoint : ProgramPoint, val id : Identifier) extends Statement(programpoint) {

  
    def getName() : String = id.toString;
  
    /** 
     * It does nothing, since the access of a variable does not modify
     * the state of the computation
	 *
	 * @param state the initial state
	 * @return the initial state 
	 */
    override def forwardSemantics[S <: State[S]](state : S) : S = state.getVariableValue(id)
    
    override def backwardSemantics[S <: State[S]](state : S) : S = state.backwardGetVariableValue(id);      
    override def toString() : String = id.getName;
    
    override def toSingleLineString() : String = toString;
}

/** 
 * This class represents the access of a field of the form
 * <code>variable.field</code> where <code>typ</code> is the
 * type of the accessed field 
 * It extends variable as it is seen as a variable access
 * 
 * obj is null iff the field access is preceeded by a statement that returns a variable
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class FieldAccess(pp : ProgramPoint, val objs : List[Statement], val field : String, val typ : Type) extends Statement(pp) {

    /** 
     * It does nothing, since the access of a field does not modify
     * the state of the computation
	 *
	 * @param state the initial state
	 * @return the initial state 
	 */
    override def forwardSemantics[S <: State[S]](state : S) : S = {
      val (listObjs, state1) = UtilitiesOnStates.forwardExecuteListStatements[S](state, objs)
      val result = state1.getFieldValue(listObjs, field, typ)
      result
    }
    
    override def backwardSemantics[S <: State[S]](state : S) : S = {
      val (listObjs, state1) = UtilitiesOnStates.backwardExecuteListStatements[S](state, objs)
      val result = state1.backwardGetFieldValue(listObjs, field, typ)
      result
    }
      
    override def hashCode() : Int = field.hashCode
    
    override def toString() : String = if(objs!=null) objs.toString()+"."+field; else "."+field;
                                                                                 
    override def toSingleLineString() : String = {
      var result : String ="";
      var first : Boolean = true;
      for(obj <- objs)
        if(first) {
          first=false;
          result= result+obj.toSingleLineString
        }
        else result= result+", "+obj.toSingleLineString;
      result+"."+field;
    }
}

/** 
 * This class represents a method call of the form
 * <code>method<parametricTypes>(parameters)</code>
 * where <code>returnedType</code> is the type of the
 * returned balue
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class MethodCall(pp : ProgramPoint, val method: Statement, val parametricTypes : List[Type], val parameters: List[Statement], val returnedType : Type) extends Statement(pp) {
  
      /** 
       * It analyzes the invocation of <code>method(parameters</code>
       * on <code>variable</code> passing <code>parameters</code>.
       * It relies on the contracts written on the target method, i.e.
       * it asserts the preconditions, and it assumes the postconditions
       * and the class invariants through methods <code>assert</code>
       * and <code>assume</code> of class <code>State</code>
	   *
       * @see State.assert(Expression)
       * @see State.assume(Expression)
	   * @param state the initial state
	   * @return the state in which postconditions and class invariants
       * of the target method are assumed to hold 
	   */
    override def forwardSemantics[S <: State[S]](state : S) : S = {
      val result=SystemParameters.getForwardSemantics[S](state, this);
      result;
    }
    //TODO: Contracts!
    /*{
      val methodInvoked : MethodDeclaration = SystemParameter.system getMethodDeclaration(method, state)
      state assert(methodInvoked.preconditions)
      state.top().assume(methodInvoked.postconditions) assume(methodInvoked.invariants)
      }*/
    
    override def backwardSemantics[S <: State[S]](state : S) : S = SystemParameters.getBackwardSemantics[S](state, this);
      
    override def toString() : String = method.toString()+ToStringUtilities.parametricTypesToString(parametricTypes)+"("+ToStringUtilities.listToCommasRepresentation[Statement](parameters)+")"
    
    override def toSingleLineString() : String = method.toSingleLineString()+ToStringUtilities.parametricTypesToString(parametricTypes)+"("+ToStringUtilities.listStatementToCommasRepresentationSingleLine(parameters)+")"
}

/** 
 * This class represents the creation of a fresh address of the form
 * <code>new typ</code>
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class New(pp : ProgramPoint, typ: Type) extends Statement(pp) {
  
      /** 
       * It creates a new reference pointing to something of type
       * <code>typ</code> through the method 
       * <code>createAddress</code> of class <code>State</code>
	   *
       * @see State.createAddress(Type)
	   * @param state the initial state
	   * @return the state in which a fresh address pointing to something 
       * of type <code>typ</code> has been created 
	   */
    override def forwardSemantics[S <: State[S]](state : S) : S = state createAddress(typ, pp)
    
    override def backwardSemantics[S <: State[S]](state : S) : S = {
      val ex=state.createAddress(typ, pp).getExpression();
      return state.removeExpression().removeVariable(ex);
    }
      
    override def toString() : String = "new "+typ toString;
    
    override def toSingleLineString() : String = toString();
}

/** 
 * This class represents a numerical constant <code>value</code>
 * where <code>type</code> is the type of such value
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class NumericalConstant(pp : ProgramPoint, value : String, typ : Type) extends Statement(pp)  {
    /** 
     * It does nothing, since a numerical constant does not modify
     * the state of the computation
	 *
	 * @param state the initial state
	 * @return the initial state 
	 */
    override def forwardSemantics[S <: State[S]](state : S) : S = state.evalNumericalConstant(value, typ)
    
    override def backwardSemantics[S <: State[S]](state : S) : S = state.evalNumericalConstant(value, typ)
      
    override def toString() : String = value;
    
    override def toSingleLineString() : String = toString();
}

/** 
 * This class represents a return statement of the form
 * <code>throw expr</code>
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class Throw(programpoint : ProgramPoint, expr : Statement) extends Statement(programpoint : ProgramPoint) {

      /** 
       * It applies the method <code>throws</code> of class <code>State</code>
	   *
       * @see State.throws(Expression)
	   * @param state the initial state
	   * @return the state in which <code>expr</code> has been thrown 
	   */
    override def forwardSemantics[S <: State[S]](state : S) : S = {
      var state1 = expr.forwardSemantics[S](state)
      val thrownexpr = state1.getExpression();
      state1 = state1.removeExpression();
      state1 throws(thrownexpr)
    }
    
    override def backwardSemantics[S <: State[S]](state : S) : S = state.top()
      
    override def toString() : String = "throw "+expr toString;
    
    override def toSingleLineString() : String = "throw "+expr.toSingleLineString();
}

/** 
 * This class represents an empty statement 
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
case class EmptyStatement(programpoint : ProgramPoint) extends Statement(programpoint : ProgramPoint) {

      /** 
       * An empty statement does nothing
	   *
       * @param state the initial state
	   * @return the initial state
       * thrown 
	   */
    override def forwardSemantics[S <: State[S]](state : S) : S = state.removeExpression();
    
    override def backwardSemantics[S <: State[S]](state : S) : S = state.removeExpression();
    
    override def toString() : String = "#empty statement#";
    
    override def toSingleLineString() : String = toString();
}

//TODO: delete it!!!
case class Switch(programpoint : ProgramPoint, condition : Statement) extends Statement(programpoint : ProgramPoint) {
	  var cases : List[(Statement, Statement, Statement)] = Nil
   
      def addCase(a : Statement, b: Statement, c: Statement) {
        cases = cases ::: (a, b, c) :: Nil;
      }
      
      override def forwardSemantics[S <: State[S]](state : S) : S = state //TODO: semantics of switch statement
      
      override def backwardSemantics[S <: State[S]](state : S) : S = state //TODO: semantics of switch statement
      
      override def toString() : String = {
        var result : String="switch " + condition.toString()+" { \n";
        for((a, b, c) <- cases) {
          result=result+"case "+a+" if "+b+" => "+c+"\n"
        }
        return result+"}\n";
      }
      
      override def toSingleLineString() : String = {
        var result : String="switch " + condition.toSingleLineString()+" { \n";
        for((a, b, c) <- cases) {
          result=result+"case "+a.toSingleLineString()+" if "+b.toSingleLineString()+" => "+c.toSingleLineString()+"\n"
        }
        return result+"}\n";
      }
}