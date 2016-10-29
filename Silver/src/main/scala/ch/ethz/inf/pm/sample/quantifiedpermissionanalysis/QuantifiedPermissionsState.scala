package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverSpecification
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, MethodDeclaration, ProgramPoint, Type}

import scala.collection.immutable.Set
import scala.collection.mutable

/**
  * Abstract state for our analysis
  *
  * @author Severin Münger
  *         Added on 19/10/16.
  */

object QuantifiedPermissionsEntryStateBuilder extends EntryStateBuilder[QuantifiedPermissionsState] {

  var fields: Set[(Type, String)] = Set[(Type, String)]()

  override def build(method: MethodDeclaration): QuantifiedPermissionsState = {
    fields = Set[(Type, String)]()
    for (f <- method.classDef.fields) {
      fields = fields + ((f.typ, f.variable.toString))
    }
    method.initializeArgument[QuantifiedPermissionsState](topState.copy())
  }

  override def topState = QuantifiedPermissionsState(false, false)
}

object QuantifiedPermissionsState {
  object Top extends QuantifiedPermissionsState(true, false)

  object Bottom extends QuantifiedPermissionsState(false, true)
}

case class QuantifiedPermissionsState(isTop: Boolean = false,
                                      isBottom: Boolean = false,
                                      top: QuantifiedPermissionsState = QuantifiedPermissionsState.Top,
                                      bottom: QuantifiedPermissionsState = QuantifiedPermissionsState.Bottom,
                                      currentPP: ProgramPoint = DummyProgramPoint)
  extends SimplePermissionState[QuantifiedPermissionsState]
    with SilverSpecification {

  // FIELDS

  // result of the alias analysis before the current program point
  lazy val preAliases = Context.preAliases(currentPP)

  // result of the alias analysis after the current program point
  lazy val postAliases = Context.postAliases(currentPP)

  // result of the alias analysis before the current program point
  lazy val preNumericalInfo = Context.preNumericalInfo(currentPP)

  // result of the alias analysis after the current program point
  lazy val postNumericalInfo = Context.postNumericalInfo(currentPP)

  val permissionRecords: mutable.HashMap[String, Map[Expression, PermissionExpression]] = new mutable.HashMap[String, Map[Expression, PermissionExpression]]

  // BASIC METHODS

  def copy(isTop: Boolean = isTop, isBottom: Boolean = isBottom,
           currentPP: ProgramPoint = currentPP):
  QuantifiedPermissionsState = {
    QuantifiedPermissionsState(isTop, isBottom, currentPP = currentPP)
  }


  override def ids: IdentifierSet = ???

  // FORWARD (NUMERICAL) ANALYSIS

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission */
  override def inhale(acc: Expression): QuantifiedPermissionsState = {
    acc match {
      case acc: PermissionExpression => this
      case _ => assume(acc)
    }
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission */
  override def exhale(acc: Expression): QuantifiedPermissionsState = {
    this
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    if (!typ.isObject) copy()
    else this
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): QuantifiedPermissionsState = {
    if (!typ.isObject) copy()
    else this
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment */
  override def assignVariable(x: Expression, right: Expression): QuantifiedPermissionsState = {
    if (!x.typ.isObject) x match {
      case x: VariableIdentifier =>
        copy()
      case _ => throw new IllegalArgumentException("Not a variable identifier")
    } else {
      this
    }
  }

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment */
  override def assignField(obj: Expression, field: String, right: Expression): QuantifiedPermissionsState = {
    this
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable */
  override def setVariableToTop(varExpr: Expression): QuantifiedPermissionsState = {
    varExpr match {
      case varExpr: VariableIdentifier =>
        copy()
      case _ => this
    }
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable */
  override def removeVariable(varExpr: VariableIdentifier): QuantifiedPermissionsState = {
    copy()
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object on which the field access is performed
    * @param field the name of the field
    * @param typ   the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field. */
  override def getFieldValue(obj: Expression, field: String, typ: Type): QuantifiedPermissionsState = {
    this
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds */
  override def assume(cond: Expression): QuantifiedPermissionsState = {
    copy()
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified */
  override def before(pp: ProgramPoint): QuantifiedPermissionsState = {
    this.copy(currentPP = pp)
  }

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object */
  override def createObject(typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    // TODO: implement
    this
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    this
  }

  /** Returns the current expression. */
  override def expr: ExpressionSet = {
    // TODO: implement
    null
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable */
  override def getVariableValue(id: Identifier): QuantifiedPermissionsState = {
    this
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): QuantifiedPermissionsState = ???

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): QuantifiedPermissionsState = ???

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression */
  override def removeExpression(): QuantifiedPermissionsState = {
    this
  }

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): QuantifiedPermissionsState = ???

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one */
  override def setExpression(expr: ExpressionSet): QuantifiedPermissionsState = {
    this
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown */
  override def throws(t: ExpressionSet): QuantifiedPermissionsState = ???

  /** Undoes the effect of pruning the unreachable heap ids. That is,
    * all heap ids present in `preState` but not in this state are created
    * and set to top. Everything else stays the same as in the post state (this state).
    *
    * @param preState old pre state before heap pruning was applied
    * @return unpruned heap */
  override def undoPruneUnreachableHeap(preState: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  /** Undoes the effect of `pruneVariables`.
    *
    * All the variables that existed in `unprunedPreState` and that match the given filter are created and set to top.
    *
    * @param unprunedPreState state before pruning
    * @param filter           the filter that was used to prune variables
    * @return state with pruned variables created again */
  override def undoPruneVariables(unprunedPreState: QuantifiedPermissionsState, filter: (VariableIdentifier) =>
    Boolean): QuantifiedPermissionsState = ???

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object */
  def factory(): QuantifiedPermissionsState = {
    copy(isTop, isBottom, DummyProgramPoint)
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments */
  override def lub(other: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    copy(isTop = isTop || other.isTop, isBottom = isBottom && other.isBottom)
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments */
  override def glb(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other` */
  override def widening(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other` */
  override def lessEqual(other: QuantifiedPermissionsState): Boolean = ???

  // BACKWARD ANALYSIS

  def refiningWhileLoop(oldPreState: QuantifiedPermissionsState, beforeLoopState: QuantifiedPermissionsState,
                        loopBodyFirstState: QuantifiedPermissionsState, loopBodyLastState: QuantifiedPermissionsState,
                        afterLoopState: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    this
  }

  /** Performs refining backward assignment of variables.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param oldPreState the pre state to be refined
    * @param x           The assigned variable
    * @param right       The assigned expression
    * @return The abstract state before the assignment */
  override def refiningAssignVariable(oldPreState: QuantifiedPermissionsState, x: Expression, right: Expression):
  QuantifiedPermissionsState = {
    // TODO: implement
    this
  }

  /** Refining backward transformer for field assignments.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param oldPreState state before this operation
    * @param obj         field target object
    * @param field       field to be assigned
    * @param right       assigned expression
    * @return refined pre state before the field assignment */
  override def refiningAssignField(oldPreState: QuantifiedPermissionsState, obj: Expression, field: String, right:
  Expression): QuantifiedPermissionsState = {
    // TODO: implement
    this
  }

  /** Performs the backward semantics of a variable access.
    *
    * @param id The accessed variable
    * @return The abstract state obtained BEFORE accessing the variable */
  override def refiningGetVariableValue(id: Identifier): QuantifiedPermissionsState = {
    // TODO: implement
    this
  }

  /** Performs the backward semantics of a field access.
    *
    * @param obj   the object on which the field access is performed
    * @param field the name of the field
    * @param typ   the type of the field
    * @return the abstract state obtained before the field access */
  override def refiningGetFieldValue(obj: ExpressionSet, field: String, typ: Type): QuantifiedPermissionsState = {
    // TODO: implement
    this
  }

  /** Undoes the effect of object creation.
    *
    * Intended to be the backward version of createObject
    * and should only be used on a post state immediately after object creation.
    *
    * @param oldPreState
    * @param obj    the heap id of the object to be removed
    * @param fields the fields that were created
    * @return state without the object */
  override def removeObject(oldPreState: QuantifiedPermissionsState, obj: ExpressionSet, fields:
  Option[Set[Identifier]]): QuantifiedPermissionsState = {
    copy()
  }
}