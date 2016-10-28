package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverSpecification
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, MethodDeclaration, ProgramPoint, Type}

import scala.collection.immutable.Set

/**
  * Abstract state for our analysis
  *
  * @author Severin Münger
  *         Added on 19/10/16.
  */
object Top extends QuantifiedPermissionsState(true, false)

object Bottom extends QuantifiedPermissionsState(false, true)

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

case class QuantifiedPermissionsState(isTop: Boolean = false,
                                      isBottom: Boolean = false,
                                      top: QuantifiedPermissionsState = Top,
                                      bottom: QuantifiedPermissionsState = Bottom,
                                      numDom: Apron.Polyhedra = Apron.Polyhedra.Bottom,
                                      currentPP: ProgramPoint = DummyProgramPoint,
                                      fieldSet: Set[(Type, String)] = Set[(Type, String)]())
  extends SimplePermissionState[QuantifiedPermissionsState]
  with SilverSpecification {


  def copy(isTop: Boolean = isTop, isBottom: Boolean = isBottom, numDom: Apron.Polyhedra = numDom,
           currentPP: ProgramPoint = currentPP, fieldSet: Set[(Type, String)] = fieldSet):
  QuantifiedPermissionsState = {
    QuantifiedPermissionsState(isTop, isBottom)
  }

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
    if (!typ.isObject) copy(numDom = numDom.createVariable(x, typ))
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
    if (!typ.isObject) copy(numDom = numDom.createVariable(x, typ))
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
        copy(numDom = numDom.assign(x, right))
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
        copy(numDom = numDom.setToTop(varExpr))
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
    copy(numDom = numDom.removeVariable(varExpr))
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

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds */
  override def assume(cond: Expression): QuantifiedPermissionsState = {
    copy(numDom = numDom.assume(cond))
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

  /** Performs the backward semantics of a variable access.
    *
    * @param id The accessed variable
    * @return The abstract state obtained BEFORE accessing the variable */
  override def refiningGetVariableValue(id: Identifier): QuantifiedPermissionsState = ???

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression */
  override def removeExpression(): QuantifiedPermissionsState = {
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
    copy(numDom = oldPreState.numDom)
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
    val fields = Set[(Type, String)]()
    val num = numDom.factory()
    val currentPP = DummyProgramPoint
    copy(isTop, isBottom, num, currentPP, fields)
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments */
  override def lub(other: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    copy(isTop = isTop || other.isTop, isBottom = isBottom && other.isBottom, numDom = numDom.lub(other.numDom))
  }

  def backwardLub(falseBranchState: QuantifiedPermissionsState, oldPreState: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    // TODO: implement
    ???
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments */
  override def glb(other: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    copy(isTop = isTop && other.isTop, isBottom = isBottom || other.isBottom, numDom = numDom glb other.numDom)
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`*/
  override def widening(other: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    copy(isTop = isTop || other.isTop, isBottom = isBottom && other.isBottom, numDom = numDom widening other.numDom)
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`*/
  override def lessEqual(other: QuantifiedPermissionsState): Boolean = {
    if (isBottom || other.isTop) true
    else if (other.isBottom || isTop) false
    else this.numDom.lessEqual(other.numDom)
  }

  def refiningWhileLoop(oldPreState: QuantifiedPermissionsState, beforeLoopState: QuantifiedPermissionsState,
                        loopBodyFirstState: QuantifiedPermissionsState, loopBodyLastState: QuantifiedPermissionsState,
                        afterLoopState: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    this
  }

  override def ids: IdentifierSet = ???
}
