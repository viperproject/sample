package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, SimpleAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.sil.SilAnalysisRunner
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

/** Reference variable
  *
  * @param id the identifier corresponding to the variable
  */
case class Ref(id: Identifier) {
  def name: String = id.getName
}

/** Wrapper that turns a `Ref` into an `Expression`
  *
  * @param ref the `Ref` to turn into an `Expression`
  */
case class RefExpression(ref: Ref) extends Expression {

  /** The type of the expression. */
  override def typ: Type = ref.id.typ
  /** Point in the program where the expression is located. */
  override def pp: ProgramPoint = ref.id.pp
  /** The string representation of the expression. */
  override def toString : String = ref.name

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = ???
  /** Runs `f` on the expression and all sub-expressions. */
  override def transform(f: (Expression) => Expression): Expression = f(this)
  /** Checks if `f` evaluates to `true` on the expression and all sub-expressions. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this)

}

/** Object created at object allocation site.
  *
  * @param typ the type of the object
  * @param pp the object allocation site
  */
case class Obj(typ: Type, pp: ProgramPoint) {

  /** The name of the object. */
  def name : String = "Object allocated " + pp.description
}

/** Wrapper that turns an `Obj` into an `Expression`
  *
  * @param obj the `Obj` to turn into an Expression
  */
case class ObjExpression(obj: Obj) extends Expression {

  /** The type of the expression. */
  override def typ: Type = obj.typ
  /** Point in the program where the expression is located. */
  override def pp: ProgramPoint = obj.pp
  /** The string representation of the expression. */
  override def toString : String = obj.name

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = ???
  /** Runs `f` on the expression and all sub-expressions. */
  override def transform(f: (Expression) => Expression): Expression = f(this)
  /** Checks if `f` evaluates to `true` on the expression and all sub-expressions. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this)

}

/** Permission Inference State
  *
  * @author Caterina Urban
  */
case class PermissionState(exprSet: ExpressionSet, refToObj: Map[Ref,Set[Obj]])
  extends SimpleState[PermissionState]
  with StateWithBackwardAnalysisStubs[PermissionState]
  with LazyLogging
{
  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): PermissionState = {
    logger.debug("*** assignField:")
    logger.debug(obj.toString)
    logger.debug(field.toString)
    logger.debug(right.toString)
    this
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): PermissionState = {
    logger.debug("*** assignVariable:")
    logger.debug(x.toString)
    logger.debug(right.toString)
    this
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): PermissionState = {
    logger.debug("*** assume:")
    logger.debug(cond.toString)
    this
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): PermissionState = {
    logger.debug("*** before:")
    logger.debug(pp.toString)
    this
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): PermissionState = {
    logger.debug("*** bottom()")
    PermissionState(exprSet.bottom(),refToObj.empty)
  }

  /** Creates an object at allocation site.
    *
    * @param typ The dynamic type of the created object
    * @param pp The allocation site of the object
    * @return The abstract state with updated exprSet after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** createObject(typ: " + typ.toString + ", pp: " + pp.toString + ")")
    val obj = Obj(typ,pp) // create new Obj
    val exp = ObjExpression(obj) // turn Obj into Expression
    this.copy(exprSet = ExpressionSet(exp)) // return the current PermissionState with updated exprSet
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the variable
    * @param typ The static type of the variable
    * @param pp The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** createVariable:")
    logger.debug(x.toString)
    logger.debug(typ.toString)
    logger.debug(pp.toString)
    this
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): PermissionState = {
    logger.debug("*** createVariableForArgument:")
    logger.debug(x.toString)
    logger.debug(typ.toString)
    this
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ The type of the numerical constant
    * @param pp The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** evalConstant:")
    logger.debug(value)
    logger.debug(typ.toString)
    logger.debug(pp.toString)
    this
  }

  /** Returns the current expression. */
  override def expr: ExpressionSet = {
    logger.debug("*** expr: " + this.exprSet.toString)
    this.exprSet // return exprSet
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): PermissionState = {
    logger.debug("*** factory:")
    this
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object on which the field access is performed
    * @param field the name of the field
    * @param typ the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field.
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): PermissionState = {
    logger.debug("*** getFieldValue:")
    logger.debug(obj.toString)
    logger.debug(field.toString)
    logger.debug(typ.toString)
    this
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): PermissionState = {
    logger.debug("*** getVariableValue:")
    logger.debug(id.toString)
    this
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: PermissionState): PermissionState = {
    logger.debug("*** glb:")
    logger.debug(other.toString)
    this
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return bottom
    */
  override def isBottom: Boolean = false

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return bottom
    */
  override def isTop: Boolean = false

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: PermissionState): Boolean = false

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: PermissionState): PermissionState = {
    logger.debug("*** lub:")
    logger.debug(other.toString)
    this
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): PermissionState = {
    logger.debug("*** pruneUnreachableHeap:")
    this
  }

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): PermissionState = {
    logger.debug("*** pruneVariables:")
    logger.debug(filter.toString)
    this
  }

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): PermissionState = {
    logger.debug("*** removeExpression()")
    this.copy(exprSet = ExpressionSet()) // return the current PermissionState with a new exprSet
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  override def removeVariable(varExpr: VariableIdentifier): PermissionState = {
    logger.debug("*** removeVariable:")
    logger.debug(varExpr.toString)
    this
  }

  /** Assigns an expression to an argument.
    *
    * @param x The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): PermissionState = {
    logger.debug("*** setArgument:")
    logger.debug(x.toString)
    logger.debug(right.toString)
    this
  }

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): PermissionState = {
    logger.debug("*** setExpression:")
    logger.debug(expr.toString)
    this
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): PermissionState = {
    logger.debug("*** setVariableToTop:")
    logger.debug(varExpr.toString)
    this
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  override def throws(t: ExpressionSet): PermissionState = {
    logger.debug("*** throws:")
    logger.debug(t.toString)
    this
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): PermissionState = {
    logger.debug("*** top:")
    this
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: PermissionState): PermissionState = {
    logger.debug("*** widening:")
    logger.debug(other.toString)
    this
  }
}

object PermissionAnalysisRunner extends SilAnalysisRunner[PermissionState] {
  val analysis = SimpleAnalysis[PermissionState](PermissionEntryStateBuilder)

  override def toString = "Stupid Analysis"
}

object PermissionEntryStateBuilder extends EntryStateBuilder[PermissionState] {
  override def topState: PermissionState = PermissionState(ExpressionSet(),Map())
}