/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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
  * A program point that is based on another program point and has a tag added
  * to it. Tagged program points can be used to create new unique program points
  * that do not exist in the original program.
  *
  * @param base The base program point.
  * @param tag  The tag of the program point.
  * @author Jerome Dohrau
  */
case class TaggedProgramPoint(base: ProgramPoint, tag: String)
  extends ProgramPoint {

  override def toString: String = description

  override def description: String = s"$tag@$base"
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
  * @author Pietro Ferrara
 * @version 0.1
 */
abstract class Statement(programpoint: ProgramPoint) extends SingleLineRepresentation {
  /**
   * The abstract forward semantics of the statement.
   *
   * @param state the pre state
   * @return the state obtained after the execution of the statement
   */
  def forwardSemantics[S <: State[S]](state: S): S

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  def backwardSemantics[S <: State[S]](state: S): S

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  def specialBackwardSemantics[S <: State[S]](state: S): S

  /**
    * The abstract (refining) backward semantics of the statement.
    *
    * @param state the post state
    * @param oldPreState the pre state to be refined
    * @return the state obtained before the execution of the statement
    */
  def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S

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

  override def toString: String

  def getChildren: List[Statement]

  private def normalizeList(list: List[Statement]): List[Statement] = list match {
    case Nil => Nil
    case x :: xs => x.normalize() :: normalizeList(xs)
  }

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
    val (leftExpr, leftState) = UtilitiesOnStates.forwardExecuteStatement(state, left)
    val (rightExpr, rightState) = UtilitiesOnStates.forwardExecuteStatement(leftState, right)
    left match {
      case fa: FieldAccess =>
        rightState.assignField(leftExpr, fa.field, rightExpr)
      case _ =>
        rightState.assignVariable(leftExpr, rightExpr)
    }
  }

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = {
    var leftState = left.backwardSemantics(state) // evaluate the left
    val leftExpr = leftState.expr
    leftState = leftState.removeExpression()
    var rightState = right.backwardSemantics(leftState) // evaluate the right
    val rightExpr = rightState.expr
    rightState = rightState.removeExpression()
    left match {
      case f: FieldAccess => rightState.assignField(leftExpr, f.field, rightExpr)
      case _ => rightState.assignVariable(leftExpr, rightExpr)
    } // perform a field or variable assignment accordingly
  }

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = {
    var leftState = left.specialBackwardSemantics(state) // evaluate the left
    val leftExpr = leftState.expr
    leftState = leftState.removeExpression()
    // evaluate the right
    val rightExpr = right.specialBackwardSemantics(leftState).expr
    val result =
      left match {
        case f: FieldAccess => leftState.assignField(leftExpr, f.field, rightExpr)
        case _ => leftState.assignVariable(leftExpr, rightExpr)
      }
    right.specialBackwardSemantics[S](result).removeExpression()
  }

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    if (state.equals(state.bottom())) return state
    var stateleft: S = left.refiningSemantics[S](state, oldPreState)
    val exprleft = stateleft.expr
    stateleft = stateleft.removeExpression()
    var stateright: S = right.refiningSemantics[S](stateleft, oldPreState)
    val exprright = stateright.expr
    stateright = stateright.removeExpression()
    var result = stateright.setVariableToTop(exprleft)
    val condition = ExpressionSetFactory.createBinaryArithmeticExpression(exprleft, exprright, ArithmeticOperator.==)
    result = result.setExpression(condition)
    result.testTrue().refiningAssignVariable(oldPreState, exprleft, exprright)
  }

  override def toString: String = left + " = " + right

  override def toSingleLineString(): String = left.toSingleLineString + " = " + right.toSingleLineString

  override def getChildren: List[Statement] = List(left, right)


}

/**
 * Represents the declaration of a variable.
  *
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

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = {
    right match {
      case Some(right) => // the declaration contains an assignment
        var resState = Assignment(programpoint, variable, right).backwardSemantics(state) // perform the assigment
        resState = variable.backwardSemantics(resState) // evaluate the variable
        resState.removeVariable(resState.expr) // remove the variable
      case None => // the declaration does not contain an assigment
        val resState = variable.backwardSemantics(state) // evaluate the variable
        resState.removeVariable(resState.expr) // remove the variable
    }
  }

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    var st = state
    if (right.isDefined)
      st = Assignment(programpoint, variable, right.get).refiningSemantics[S](st, oldPreState)
    st.removeVariable(ExpressionSetFactory.createVariable(variable, typ, programpoint))
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

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = {
    right match {
      case Some(right) => // the declaration contains an assignment
        var resState = Assignment(programpoint, variable, right).specialBackwardSemantics(state) // perform the assigment
        resState = variable.specialBackwardSemantics(resState) // evaluate the variable
        resState.removeVariable(resState.expr) // remove the variable
      case None => // the declaration does not contain an assigment
        val resState = variable.specialBackwardSemantics(state) // evaluate the variable
        resState.removeVariable(resState.expr) // remove the variable
    }
  }
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

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = state.getVariableValue(id)

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = state.refiningGetVariableValue(id)

  override def toSingleLineString(): String = toString

  override def toString: String = id.getName

  override def getChildren: List[Statement] = Nil

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = state.getVariableValue(id)
}

/**
 * This class represents the access of a field of the form
 * <code>variable.field</code> where <code>typ</code> is the
 * type of the accessed field
 * It extends variable as it is seen as a variable access
 *
 * obj is null iff the field access is preceded by a statement that returns a variable
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
      var accPath: List[VariableIdentifier] = Nil
      while (current.isInstanceOf[FieldAccess]) {
        val fieldAccSt = current.asInstanceOf[FieldAccess]
        accPath = VariableIdentifier(fieldAccSt.field)(fieldAccSt.typ, fieldAccSt.pp) :: accPath
        current = fieldAccSt.obj
      }
      if (SystemParameters.DEBUG) assert(current.isInstanceOf[Variable], "The root of FieldAccess should be a variable.")
      val rootOfFieldAcc = current.asInstanceOf[Variable]
      accPath = rootOfFieldAcc.id :: accPath
      // TODO: The below fix is a hack and should not be handled this way
      val finalType = if (typ.toString.contains("<none>")) getTypeOfStatement(obj).possibleFields.filter(f => f.getName.equals(field)).head.typ else typ
      val fieldId = VariableIdentifier(field)(finalType, pp)
      val pathExpr = AccessPathIdentifier(accPath :+ fieldId)
      val newResult = state.getFieldValue(ExpressionSet(pathExpr), field, finalType)
      newResult
    } else {
      val objState = obj.forwardSemantics(state) // evaluate the receiver
      objState.getFieldValue(objState.expr, field, typ) // get the field value
    }
  }

  private def getTypeOfStatement(s: Statement): Type = {
    s match {
      case v: Variable => v.id.typ
      case fa: FieldAccess =>
        if (SystemParameters.DEBUG) assert(!fa.typ.toString.contains("<none>"), "Typ = " + fa.typ + " - The type uf field access should never be Unit")
        fa.typ
      case _ => throw new Exception("Should not happen as we use this only inside access path.")
    }
  }

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = {
    val objState = obj.backwardSemantics(state) // evaluate the receiver
    objState.getFieldValue(objState.expr, field, typ) // get the field value
  }

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    val (expr, newState) = UtilitiesOnStates.refiningExecuteStatement(state, oldPreState, obj)
    newState.refiningGetFieldValue(expr, field, typ)
  }

  override def hashCode(): Int = field.hashCode

  override def toString: String =
    s"${obj.toString}.$field"

  override def toSingleLineString() =
    s"${obj.toSingleLineString()}.$field"

  override def getChildren = List(obj)

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = {
    val objState = obj.specialBackwardSemantics(state) // evaluate the receiver
    objState.getFieldValue(objState.expr, field, typ) // get the field value
  }

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
    // Method call used to represent a goto statement to a while label
    body match {
      case variable: Variable if variable.getName.startsWith("while") => throw new Exception("This should not appear here!")
      case _ =>
    }; //return state
    body match {
      case body: FieldAccess => forwardAnalyzeMethodCallOnObject[S](body.obj, body.field, state, getPC())
      case body: Variable =>
        var curState = state
        val parameterExpressions = for (parameter <- parameters) yield {
          curState = parameter.forwardSemantics[S](curState)
          curState.expr
        }
        curState.setExpression(ExpressionSetFactory.createFunctionCallExpression(returnedType, body.getName, parameterExpressions, pp))
    }
  }

  private def forwardAnalyzeMethodCallOnObject[S <: State[S]](obj: Statement, calledMethod: String, preState: S,
                                                              programPoint: ProgramPoint): S = {

    // Evaluate object and parameters
    var curState = preState
    curState = if (obj == null) preState else obj.forwardSemantics[S](curState)
    val objectExpression = curState.expr
    val parameterExpressions = for (parameter <- parameters) yield {
      curState = parameter.forwardSemantics[S](curState)
      curState.expr
    }

    if (objectExpression.isBottom) return curState.bottom()
    if (parameterExpressions.exists(_.isBottom)) return curState.bottom()
    if (curState.isBottom) return curState.bottom()

    // Evaluate called method
    applyNativeForwardSemanticsOnObject(calledMethod, objectExpression, parameterExpressions, curState, programPoint)

  }

  private def backwardAnalyzeMethodCallOnObject[S <: State[S]](obj: Statement, calledMethod: String, postState: S,
                                                               programPoint: ProgramPoint): S = {
    var currentState = postState
    currentState = obj.backwardSemantics[S](currentState)
    val objExpr = currentState.expr
    val paramExpr = for (param: Statement <- parameters) yield {
      currentState = param.backwardSemantics[S](currentState)
      currentState.expr
    }
    applyNativeBackwardSemanticsOnObject(calledMethod, objExpr, paramExpr, currentState, programPoint)
  }

  private def specialBackwardAnalyzeMethodCallOnObject[S <: State[S]](obj: Statement, calledMethod: String, postState: S,
                                                               programPoint: ProgramPoint): S = {
    var currentState = obj match {
      case null => postState
      case _ => obj.specialBackwardSemantics[S](postState)
    }
    val objExpr = currentState.expr
    val paramExpr = for (param: Statement <- parameters) yield {
      currentState = param.specialBackwardSemantics[S](currentState)
      currentState.expr
    }
    applyNativeBackwardSemanticsOnObject(calledMethod, objExpr, paramExpr, currentState, programPoint)
  }

  private def applyNativeForwardSemanticsOnObject[S <: State[S]](invokedMethod: String, thisExpr: ExpressionSet,
                                                                 parametersExpr: List[ExpressionSet], state: S,
                                                                 programpoint: ProgramPoint): S = {
    for (sem <- SystemParameters.nativeMethodsSemantics) {
      val res = sem.applyForwardNativeSemantics[S](thisExpr, invokedMethod, parametersExpr, parametricTypes,
        returnedType, programpoint, state)
      res match {
        case Some(s) =>
          return s
        case None => ()
      }
    }
    Reporter.reportImpreciseSemantics("Type " + thisExpr.typ + " with method " + invokedMethod + " not implemented", programpoint)
    state.top()
  }

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = {
    val body: Statement = method.normalize()
    val castedStatement = body.asInstanceOf[FieldAccess]
    val calledMethod = castedStatement.field
    backwardAnalyzeMethodCallOnObject[S](castedStatement.obj, calledMethod, state, getPC())
  }

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    val body: Statement = method.normalize()
    val castedStatement = body.asInstanceOf[FieldAccess]
    val calledMethod = castedStatement.field
    backwardAnalyzeMethodCallOnObject[S](castedStatement.obj, calledMethod, state, getPC())
  }

  private def applyNativeBackwardSemanticsOnObject[S <: State[S]](invokedMethod: String, thisExpr: ExpressionSet,
                                                                  parametersExpr: List[ExpressionSet], state: S,
                                                                  programPoint: ProgramPoint): S = {
    for (sem <- SystemParameters.nativeMethodsSemantics) {
      val res = sem.applyBackwardNativeSemantics[S](thisExpr, invokedMethod, parametersExpr, parametricTypes,
        returnedType, programPoint, state)
      res match {
        case Some(s) =>
          return s
        case None => ()
      }
    }
    Reporter.reportImpreciseSemantics("Type " + thisExpr.typ + " with method " + invokedMethod + " not implemented", programPoint)
    state.top()
  }

  override def toString: String =
    method.toString + ToStringUtilities.parametricTypesToString(parametricTypes) + "(" +
      ToStringUtilities.listToCommasRepresentation[Statement](parameters) + ")"

  override def toSingleLineString(): String =
    method.toSingleLineString() + ToStringUtilities.parametricTypesToString(parametricTypes) + "(" +
      ToStringUtilities.listStatementToCommasRepresentationSingleLine(parameters) + ")"

  override def getChildren: List[Statement] = List(method) ::: parameters

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = {
    val body: Statement = method.normalize()
    val (receiver, calledMethod) = body match {
      case body: FieldAccess => (body.obj, body.field)
      case body: Variable => (null, body.getName)
    }
    specialBackwardAnalyzeMethodCallOnObject[S](receiver, calledMethod, state, getPC())
  }

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
  override def forwardSemantics[S <: State[S]](state: S): S = state.createObject(typ, pp)

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = state.createObject(typ, pp)

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = {
    val ex = state.createObject(typ, pp).expr
    state.removeExpression().removeVariable(ex)
  }

  override def toSingleLineString(): String = toString

  override def toString: String = "new " + typ.toString

  override def getChildren: List[Statement] = Nil

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = state.createObject(typ, pp)
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

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = state.evalConstant(value, typ, pp)

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = state.evalConstant(value, typ, pp)

  override def toSingleLineString(): String = toString

  override def toString: String = value

  override def getChildren: List[Statement] = Nil

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = state.evalConstant(value, typ, pp)
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
    state1 throws thrownExpr
  }

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = ???

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = state.top()

  override def toString: String = "throw " + expr.toString

  override def toSingleLineString(): String = "throw " + expr.toSingleLineString()

  override def getChildren: List[Statement] = Nil

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = ???
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

  /**
    * The abstract backward semantics of the statement.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def backwardSemantics[S <: State[S]](state: S): S = state.removeExpression()

  override def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = state.removeExpression()

  override def toSingleLineString(): String = toString

  override def toString: String = "#empty statement#"

  override def getChildren: List[Statement] = Nil

  /**
    * The abstract special backward semantics of the statement.
    * User for quantified permission analysis where for the assignment, the semantics of the right side have to be
    * executed _after_ the actual assignment itself.
    * This method is introduced to avoid modification of the [[backwardSemantics]] function so that other parts of
    * sample inference depending on the backward semantics are not broken.
    *
    * @param state the post state
    * @return the state obtained before the execution of the statement
    */
  override def specialBackwardSemantics[S <: State[S]](state: S): S = state.removeExpression()
}
