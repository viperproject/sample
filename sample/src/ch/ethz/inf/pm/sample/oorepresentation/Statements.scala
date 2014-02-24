package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.ToStringUtilities
import ch.ethz.inf.pm.sample.reporting.Reporter

/**
 * This class represents a point of the program. It is specific 
 * for a given programming language, so it is defined as an abstract
 * class that should be extended by different parsers
 */
trait ProgramPoint {

  /**
   * Gets a description of the program point, for example
   * "in file somefile.scala at line 1, column 2"
   */
  def description: String

}

case object DummyProgramPoint extends ProgramPoint {
  override def description = "Dummy"
  override def toString = "Dummy"
}

abstract class LineColumnProgramPoint extends ProgramPoint {
  def getLine: Int

  def getColumn: Int

  override def description: String = {
    "at line " + getLine + ", column " + getColumn
  }
}

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
abstract class Statement(programpoint: ProgramPoint) extends SingleLineRepresentation {
  /**
   * The abstract forward semantics of the statement.
   *
   * @param state the initial state
   * @return the state obtained after the execution of the statement
   */
  def forwardSemantics[S <: State[S]](state: S): S

  /**
   * The abstract (refining) backward semantics of the statement.
   *
   * @param state the post state
   * @param oldPreState the old pre state to be refined
   * @return the state obtained before the execution of the statement
   */
  def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S

  /**
   * The program point of the statement.
   *
   * @return the program point
   */
  def getPC(): ProgramPoint = programpoint

  def normalize(): Statement = this match {
    case x: ControlFlowGraph =>
      x.asInstanceOf[ControlFlowGraph].nodes match {
        case y :: Nil => y match {
          case w :: Nil => w.normalize()
          case _ => x
        }
        case _ => x
      }
    case Assignment(programpoint, left, right) =>
      Assignment(programpoint, left.normalize(), right.normalize())
    case VariableDeclaration(programpoint, variable, typ, right) =>
      VariableDeclaration(programpoint, variable, typ, right.map(_.normalize))
    case FieldAccess(pp, obj, field, typ) =>
      FieldAccess(pp, obj.normalize(), field, typ)
    case MethodCall(pp, method, parametricTypes, parameters, returnedType) =>
      MethodCall(pp, method.normalize(), parametricTypes, normalizeList(parameters), returnedType)
    case Throw(programpoint, expr) => Throw(programpoint, expr.normalize())
    case z => z
  }

  private def normalizeList(list: List[Statement]): List[Statement] = list match {
    case Nil => Nil
    case x :: xs => x.normalize() :: normalizeList(xs)
  }

  override def toString: String

  def getChildren: List[Statement]

}

/**
 * This class represents an assignment of the form
 * <code>left=right</code>
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class Assignment(programpoint: ProgramPoint, left: Statement, right: Statement) extends Statement(programpoint: ProgramPoint) {

  /**
   * It applies the method <code>assignVariable</code>
   * of class <code>State</code>
   *
   * @see State.assignVariable(Variable, Expression)
   * @param state the initial state
   * @return the state in which <code>left</code> has been
   *         assigned to <code>right</code>
   */
  override def forwardSemantics[S <: State[S]](state: S): S = {
    // The value-driven heap analysis can execute the whole LHS statement,
    // because its `getFieldValue` method returns an `AccessPathIdentifier`
    // that can be passed to `assignField` directly.
    val leftToExecute =
      if (SystemParameters.isValueDrivenHeapAnalysis) left
      else left match { case f: FieldAccess => f.obj case _ => left }

    val (leftExpr, leftState) = UtilitiesOnStates.forwardExecuteStatement(state, leftToExecute)
    val (rightExpr, rightState) = UtilitiesOnStates.forwardExecuteStatement(leftState, right)

    left match {
      case fa: FieldAccess =>
        rightState.assignField(leftExpr, fa.field, rightExpr)
      case _ =>
        rightState.assignVariable(leftExpr, rightExpr)
    }
  }

      override def backwardSemantics[S <: State[S]](state : S, oldPreState: S) : S = {
        if(state.equals(state.bottom())) return state
        var stateleft : S = left.backwardSemantics[S](state, oldPreState)
        val exprleft = stateleft.expr
        stateleft=stateleft.removeExpression()
        var stateright : S = right.backwardSemantics[S](stateleft, oldPreState)
        val exprright = stateright.expr
        stateright=stateright.removeExpression()
        var result=stateright.setVariableToTop(exprleft)
        val condition=ExpressionFactory.createBinaryExpression(exprleft, exprright, ArithmeticOperator.==, exprleft.getType().top());//TODO type is wrong
        result=result.setExpression(condition)
        return result.testTrue().backwardAssignVariable(oldPreState, exprleft, exprright)
	  }

  override def toString: String = left + " = " + right

  override def toSingleLineString(): String = left.toSingleLineString + " = " + right.toSingleLineString

  override def getChildren: List[Statement] = List(left, right)

}

/**
 * Represents the declaration of a variable.
 * @param programpoint where the variable is declared
 * @param variable the variable being declared
 * @param typ the type of the variable declaration
 * @param right the optional statement assigned to the variable
 */
case class VariableDeclaration(
                                programpoint: ProgramPoint,
                                variable: Variable,
                                typ: Type,
                                right: Option[Statement] = None)
  extends Statement(programpoint: ProgramPoint) {

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
   *         created, and eventually it has been assigned to <code>right</code>
   */
  override def forwardSemantics[S <: State[S]](state: S): S = {
    var variableEval: S = variable.forwardSemantics[S](state)
    val varExpr = variableEval.expr
    variableEval = variableEval.removeExpression()
    val state1 = variableEval createVariable(varExpr, typ, programpoint)
    if (right.isDefined) {
      var rightEval: S = right.get.forwardSemantics[S](state1)
      val rightExpr = rightEval.expr
      rightEval = rightEval.removeExpression()
      rightEval assignVariable(varExpr, rightExpr)
    }
    else state1
  }

  override def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    var st = state
    if (right.isDefined)
      st = new Assignment(programpoint, variable, right.get).backwardSemantics[S](st, oldPreState)
    st.removeVariable(ExpressionFactory.createVariable(variable, typ, programpoint))
  }

  override def toString: String =
    "declare " + ToStringUtilities.toStringIfNotNull(typ) + " " +
      variable.toString + ToStringUtilities.assignedIfNotNull(right)

  override def toSingleLineString(): String =
    "declare " + ToStringUtilities.toStringIfNotNull(typ) + " " +
      variable.toString + {
      if (right.isDefined) "=" + right.get.toSingleLineString() else ""
    }

  override def getChildren: List[Statement] =
    List(Some(variable), right).flatten
}

/**
 * This class represents the access of a variable <code>id</code>
 * It extends MethodIdentifier as it may be used to identify a method
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class Variable(programpoint: ProgramPoint, id: VariableIdentifier) extends Statement(programpoint) {

  def getName: String = id.toString

  /**
   * It does nothing, since the access of a variable does not modify
   * the state of the computation
   *
   * @param state the initial state
   * @return the initial state
   */
  override def forwardSemantics[S <: State[S]](state: S): S = state.getVariableValue(id)

  override def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = state.backwardGetVariableValue(id)

  override def toString: String = id.getName

  override def toSingleLineString(): String = toString

  override def getChildren: List[Statement] = Nil

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
case class FieldAccess(pp: ProgramPoint, obj: Statement, field: String, typ: Type) extends Statement(pp) {

  /**
   * It does nothing, since the access of a field does not modify
   * the state of the computation
   *
   * @param state the initial state
   * @return the initial state
   */
  override def forwardSemantics[S <: State[S]](state: S): S = {
    if (SystemParameters.isValueDrivenHeapAnalysis) {
      // I need a List of ExpressionSet
      var current: Statement = obj
      var accPath: List[String] = Nil
      while (current.isInstanceOf[FieldAccess]) {
        val fieldAccSt = current.asInstanceOf[FieldAccess]
        accPath = fieldAccSt.field :: accPath
        current = fieldAccSt.obj
      }
      assert(current.isInstanceOf[Variable], "The root of FieldAccess should be a variable.")
      val rootOfFieldAcc = current.asInstanceOf[Variable]
      accPath = rootOfFieldAcc.getName :: accPath
      // TODO: The below fix is a hack and should not be handled this way
      val finalType = if (typ.toString.contains("<none>")) getTypeOfStatement(obj).possibleFields.filter(f => f.getName.equals(field)).head.typ else typ
      val pathExpr = AccessPathIdentifier(accPath :+ field)(finalType, pp)
      val newResult = state.getFieldValue(ExpressionSet(pathExpr), field, finalType)
      newResult
    } else {
      val (expr, objState) = UtilitiesOnStates.forwardExecuteStatement(state, obj)
      val result = objState.getFieldValue(expr, field, typ)
      result
    }
  }

  private def getTypeOfStatement(s: Statement): Type = {
    s match {
      case v: Variable => v.id.typ
      case fa: FieldAccess =>
        assert(!fa.typ.toString.contains("<none>"), "Typ = " + fa.typ + " - The type uf field access should never be Unit")
        fa.typ
      case _ => throw new Exception("Should not happen as we use this only inside access path.")
    }
  }

  override def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    val (expr, newState) = UtilitiesOnStates.backwardExecuteStatement(state, oldPreState, obj)
    newState.backwardGetFieldValue(expr, field, typ)
  }

  override def hashCode(): Int = field.hashCode

  override def toString: String =
    s"${obj.toString}.$field"

  override def toSingleLineString() =
    s"${obj.toSingleLineString()}.$field"

  override def getChildren = List(obj)

}

/**
 * This class represents a method call of the form
 * <code>method<parametricTypes>(parameters)</code>
 * where <code>returnedType</code> is the type of the
 * returned value
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class MethodCall(
                       pp: ProgramPoint,
                       method: Statement,
                       parametricTypes: List[Type],
                       parameters: List[Statement],
                       returnedType: Type) extends Statement(pp) {

  /**
   * It analyzes the invocation of <code>method</code>
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
   *         of the target method are assumed to hold
   */
  override def forwardSemantics[S <: State[S]](state: S): S = {
    val body: Statement = method.normalize()
    var result: S = state.bottom()
    //Method call used to represent a goto statement to a while label
    if (body.isInstanceOf[Variable] && body.asInstanceOf[Variable].getName.startsWith("while"))
      throw new Exception("This should not appear here!"); //return state

    if (!body.isInstanceOf[FieldAccess]) return state
    //TODO: Sometimes it is a variable, check if $this is implicit!
    val castedStatement = body.asInstanceOf[FieldAccess]
    val calledMethod = castedStatement.field
    forwardAnalyzeMethodCallOnObject[S](castedStatement.obj, calledMethod, state, getPC())
  }

  override def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    val body: Statement = method.normalize()
    val castedStatement = body.asInstanceOf[FieldAccess]
    val calledMethod = castedStatement.field
    backwardAnalyzeMethodCallOnObject[S](castedStatement.obj, calledMethod, state, oldPreState, getPC())
  }

  private def forwardAnalyzeMethodCallOnObject[S <: State[S]](obj: Statement, calledMethod: String, initialState: S,
                                                              programpoint: ProgramPoint): S = {
    val (calledExpr, resultingState) = UtilitiesOnStates.forwardExecuteStatement[S](initialState, obj)
    val (parametersExpr, resultingState1) = UtilitiesOnStates.forwardExecuteListStatements[S](resultingState, parameters)

    // TODO: verify that this is indeed correct.
    if (calledExpr.isBottom)
      return initialState.bottom()
    if (calledExpr.isTop)
      return initialState.top()
    applyNativeForwardSemanticsOnObject(calledMethod, calledExpr, parametersExpr, resultingState1, programpoint)
  }

  private def applyNativeForwardSemanticsOnObject[S <: State[S]](invokedMethod: String, thisExpr: ExpressionSet,
                                                                 parametersExpr: List[ExpressionSet], state: S, programpoint: ProgramPoint): S = {
    for (sem <- SystemParameters.nativeMethodsSemantics) {
      val res = sem.applyForwardNativeSemantics[S](thisExpr, invokedMethod, parametersExpr, parametricTypes,
        returnedType, programpoint, state)
      res match {
        case Some(s) =>
          return s
        case None => ()
      }
    }
    Reporter.reportImprecision("Type " + thisExpr.getType() + " with method " + invokedMethod + " not implemented", programpoint)
    state.top()
  }

  private def backwardAnalyzeMethodCallOnObject[S <: State[S]](obj: Statement, calledMethod: String, postState: S, oldPreState: S, programpoint: ProgramPoint): S = {
    // to be included when ExecutionHistoryState is committed
	???
  }

  override def toString: String =
    method.toString + ToStringUtilities.parametricTypesToString(parametricTypes) + "(" +
      ToStringUtilities.listToCommasRepresentation[Statement](parameters) + ")"

  override def toSingleLineString(): String =
    method.toSingleLineString() + ToStringUtilities.parametricTypesToString(parametricTypes) + "(" +
      ToStringUtilities.listStatementToCommasRepresentationSingleLine(parameters) + ")"

  override def getChildren: List[Statement] = List(method) ::: parameters
}

/**
 * This class represents the creation of a fresh address of the form
 * <code>new typ</code>
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class New(pp: ProgramPoint, typ: Type) extends Statement(pp) {

  /**
   * It creates a new reference pointing to something of type
   * <code>typ</code> through the method
   * <code>createAddress</code> of class <code>State</code>
   *
   * @see State.createAddress(Type)
   * @param state the initial state
   * @return the state in which a fresh address pointing to something
   *         of type <code>typ</code> has been created
   */
  override def forwardSemantics[S <: State[S]](state: S): S =
    state.createObject(typ, pp, Some(typ.possibleFields))

  override def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    val ex = state.createObject(typ, pp, Some(typ.possibleFields)).expr
    state.removeExpression().removeVariable(ex)
  }

  override def toString: String = "new " + typ.toString

  override def toSingleLineString(): String = toString

  override def getChildren: List[Statement] = Nil

}

/**
 * This class represents a numerical constant <code>value</code>
 * where <code>type</code> is the type of such value
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class ConstantStatement(pp: ProgramPoint, value: String, typ: Type) extends Statement(pp) {

  /**
   * It does nothing, since a numerical constant does not modify
   * the state of the computation
   *
   * @param state the initial state
   * @return the initial state
   */
  override def forwardSemantics[S <: State[S]](state: S): S = state.evalConstant(value, typ, pp)

  override def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = state.evalConstant(value, typ, pp)

  override def toString: String = value

  override def toSingleLineString(): String = toString

  override def getChildren: List[Statement] = Nil

}

/**
 * This class represents a return statement of the form
 * <code>throw expr</code>
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class Throw(programpoint: ProgramPoint, expr: Statement) extends Statement(programpoint: ProgramPoint) {

  /**
   * It applies the method <code>throws</code> of class <code>State</code>
   *
   * @see State.throws(Expression)
   * @param state the initial state
   * @return the state in which <code>expr</code> has been thrown
   */
  override def forwardSemantics[S <: State[S]](state: S): S = {
    var state1 = expr.forwardSemantics[S](state)
    val thrownExpr = state1.expr
    state1 = state1.removeExpression()
    state1 throws (thrownExpr)
  }

  override def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = state.top()

  override def toString: String = "throw " + expr.toString

  override def toSingleLineString(): String = "throw " + expr.toSingleLineString()

  override def getChildren: List[Statement] = Nil

}

/**
 * This class represents an empty statement 
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class EmptyStatement(programpoint: ProgramPoint) extends Statement(programpoint: ProgramPoint) {

  /**
   * An empty statement does nothing
   *
   * @param state the initial state
   * @return the initial state
   *         thrown
   */
  override def forwardSemantics[S <: State[S]](state: S): S = state.removeExpression()

  override def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = state.removeExpression()

  override def toString: String = "#empty statement#"

  override def toSingleLineString(): String = toString

  override def getChildren: List[Statement] = Nil

}
