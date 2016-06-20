package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

object Phase extends Enumeration {
  type Phase = Value
  val AliasAnalysisPhase = Value("Alias Analysis Phase")
  val PermissionAnalysisPhase = Value("Permission Inference Phase")
}

/**
  * A state of a multi phase permission inference. In the first phase the state
  * behaves like an alias analysis state. In the second phase the state behaves
  * like a permission alias state that is given the result of the alias analysis
  * of the first phase.
  *
  * @author Jerome Dohrau
  */
trait PermissionInferenceState[T <: PermissionInferenceState[T, A, P], A <: AliasAnalysisState[A], P <: PermissionAnalysisState[P, A]]
  extends SimplePermissionState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
  this: T =>

  type Phase = Phase.Value

  // indicates current phase
  val phase: Phase

  // state of the alias analysis
  val aliases: A

  // state of the permission analysis
  val permissions: P

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.exhale(acc))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.setAliases(aliases).exhale(acc))
  }

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): T = phase match{
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.inhale(acc))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.setAliases(aliases).inhale(acc))
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.createVariableForArgument(x, typ))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.createVariableForArgument(x, typ))
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  override def removeVariable(varExpr: VariableIdentifier): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.removeVariable(varExpr))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.removeVariable(varExpr))
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object on which the field access is performed
    * @param field the name of the field
    * @param typ   the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field.
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.getFieldValue(obj, field, typ))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.getFieldValue(obj, field, typ))
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.assume(cond))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.assume(cond))
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.createVariable(x, typ, pp))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.createVariable(x, typ, pp))
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.assignVariable(x, right))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.assignVariable(x, right))
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.setVariableToTop(varExpr))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.setVariableToTop(varExpr))
  }

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.assignField(obj, field, right))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.setAliases(aliases).assignField(obj, field, right))
  }

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.setArgument(x, right))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.setArgument(x, right))
  }

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.removeExpression())
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.removeExpression())
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  override def throws(t: ExpressionSet): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.throws(t))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.throws(t))
  }

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.pruneVariables(filter))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.pruneVariables(filter))
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.evalConstant(value, typ, pp))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.evalConstant(value, typ, pp))
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.before(pp))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.before(pp))
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.pruneUnreachableHeap())
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.pruneUnreachableHeap())
  }

  /** Returns the current expression. */
  override def expr: ExpressionSet = phase match {
    case Phase.AliasAnalysisPhase => aliases.expr
    case Phase.PermissionAnalysisPhase => permissions.expr
  }

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.createObject(typ, pp))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.createObject(typ, pp))
  }

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.setExpression(expr))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.setExpression(expr))
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.getVariableValue(id))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.getVariableValue(id))
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.bottom())
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.bottom())
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: T): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.widening(other.aliases))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.widening(other.permissions))
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: T): Boolean = phase match {
    case Phase.AliasAnalysisPhase => aliases.lessEqual(other.aliases)
    case Phase.PermissionAnalysisPhase => permissions.lessEqual(other.permissions)
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.top())
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.top())
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: T): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.lub(other.aliases))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.lub(other.permissions))
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.factory())
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.factory())
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: T): T = phase match {
    case Phase.AliasAnalysisPhase =>
      copy(aliases = aliases.glb(other.aliases))
    case Phase.PermissionAnalysisPhase =>
      copy(permissions = permissions.glb(other.permissions))
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return bottom
    */
  override def isBottom: Boolean = phase match {
    case Phase.AliasAnalysisPhase => aliases.isBottom
    case Phase.PermissionAnalysisPhase => permissions.isBottom
  }

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return bottom
    */
  override def isTop: Boolean = phase match {
    case Phase.AliasAnalysisPhase => aliases.isBottom
    case Phase.PermissionAnalysisPhase => permissions.isBottom
  }

  def copy(phase: Phase = phase,
           aliases: A = aliases,
           permissions: P = permissions): T
}