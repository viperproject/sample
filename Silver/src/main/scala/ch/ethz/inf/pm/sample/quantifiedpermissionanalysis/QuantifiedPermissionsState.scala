/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsState.{Bottom, Top}
import com.typesafe.scalalogging.LazyLogging

object QuantifiedPermissionsState {
  object Top extends QuantifiedPermissionsState(true, false)

  object Bottom extends QuantifiedPermissionsState(false, true)
}

case class QuantifiedPermissionsState(isTop: Boolean = false,
                                      isBottom: Boolean = false,
                                      expr: ExpressionSet = ExpressionSet(),
                                      visited: Set[ProgramPoint] = Set(),
                                      currentPP: ProgramPoint = DummyProgramPoint,
                                      permissions: PermissionRecords = PermissionRecords(),
                                      changingVars: Set[Identifier] = Set(),
                                      declaredBelowVars: Set[Identifier] = Set(),
                                      refSets: Map[(ProgramPoint, Expression), ReferenceSetDescription] = Map())
  extends SimplePermissionState[QuantifiedPermissionsState]
    with StateWithRefiningAnalysisStubs[QuantifiedPermissionsState]
    with SilverSpecification[Any]
    with LazyLogging {

  // BASIC METHODS

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): QuantifiedPermissionsState = QuantifiedPermissionsState()

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): QuantifiedPermissionsState = Top

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): QuantifiedPermissionsState = Bottom

  def copy(isTop: Boolean = isTop,
           isBottom: Boolean = isBottom,
           expr: ExpressionSet = expr,
           visited: Set[ProgramPoint] = visited,
           currentPP: ProgramPoint = currentPP,
           permissions: PermissionRecords = permissions,
           changingVars: Set[Identifier] = changingVars,
           declaredBelowVars: Set[Identifier] = declaredBelowVars,
           refSets: Map[(ProgramPoint, Expression), ReferenceSetDescription] = refSets): QuantifiedPermissionsState =
    QuantifiedPermissionsState(isTop, isBottom, expr, visited, currentPP, permissions, changingVars, declaredBelowVars, refSets) match {
      case newState if QuantifiedPermissionsParameters.useSetSimplifications && newState.canBeMerged => newState.mergeSetDescriptions
      case newState => newState
    }

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): QuantifiedPermissionsState = copy(expr = ExpressionSet())

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): QuantifiedPermissionsState = copy(expr = expr)

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: QuantifiedPermissionsState): QuantifiedPermissionsState = (this, other) match {
    case (Bottom, _) | (_, Top) => other
    case (_, Bottom) | (Top, _) => this
    case _ =>
      val newPermissions = (other.visited.subsetOf(visited), visited.subsetOf(other.visited)) match {
        case (true, _) => permissions
        case (_, true) => other.permissions
        case (false, false) => permissions.lub(other.permissions)
      }
      val newRefSets: Map[(ProgramPoint, Expression), ReferenceSetDescription] = refSets ++ other.refSets.transform {
        case (key, expressionCollection) => refSets.getOrElse(key, expressionCollection.bottom()).lub(expressionCollection)
      }
      copy(
        expr = expr lub other.expr,
        visited = visited ++ other.visited,
        permissions = newPermissions,
        changingVars = changingVars ++ other.changingVars,
        declaredBelowVars = declaredBelowVars ++ other.declaredBelowVars,
        refSets = newRefSets
      )
  }

  def lub(falseState: QuantifiedPermissionsState, cond: ExpressionSet): QuantifiedPermissionsState = (cond.getSingle.isDefined && !cond.getSingle.get.isInstanceOf[UnitExpression], this, falseState) match {
    case (false, _, _) => throw new IllegalArgumentException()
    case (true, Bottom, _) | (true, _, Top) => falseState
    case (true, _, Bottom) | (true, Top, _) => this
    case (true, _, _) =>
      val condSingle = cond.getSingle.get
      val newPermissions = (falseState.visited.subsetOf(visited), visited.subsetOf(falseState.visited)) match {
        case (true, _) => permissions
        case (_, true) => falseState.permissions
        case (false, false) => permissions.lub(condSingle, falseState.permissions)
      }
      val newRefSets: Map[(ProgramPoint, Expression), ReferenceSetDescription] = refSets.transform { case (_, setDescription) => setDescription.transformCondition(condSingle) } ++ falseState.refSets.transform {
        case (key, expressionCollection) => refSets.getOrElse(key, expressionCollection.bottom()).lub(expressionCollection.transformCondition(NegatedBooleanExpression(condSingle)))
      }
      copy(
        expr = expr lub falseState.expr,
        visited = visited ++ falseState.visited,
        permissions = newPermissions,
        changingVars = changingVars ++ falseState.changingVars,
        declaredBelowVars = declaredBelowVars ++ falseState.declaredBelowVars,
        refSets = newRefSets
      )
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: QuantifiedPermissionsState): QuantifiedPermissionsState = (this, other) match {
    case (Bottom, _) | (_, Bottom) => Bottom
    case (Top, _) => other
    case (_, Top) => this
    case _ =>
      copy(
        expr = expr glb other.expr,
        visited = visited ++ other.visited,
        permissions = permissions,
        changingVars = changingVars & other.changingVars,
        declaredBelowVars = declaredBelowVars & other.declaredBelowVars,
        refSets = refSets
      )
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: QuantifiedPermissionsState): Boolean = {
    (this, other) match {
      case (Bottom, _) | (_, Top) => true
      case (_, Bottom) | (Top, _) => false
      case (_, _) =>
        refSets.forall { case (key, elem) => elem.lessEqual(other.refSets.getOrElse(key, elem.bottom())) } &&
        changingVars.subsetOf(other.changingVars)
    }
  }

  private def canBeMerged: Boolean = refSets.foldLeft(Map[(ProgramPoint, Expression), ReferenceSetDescription]()) {
    case (collected, entry@(key, setDescription)) => collected + (collected.find(_._2.isEquivalentDescription(setDescription)) match {
      case Some((_, result)) => key -> result
      case None => entry
    })
  } != refSets

  private def mergeSetDescriptions: QuantifiedPermissionsState = {
    copy(refSets = refSets.foldLeft(Map[(ProgramPoint, Expression), ReferenceSetDescription]()) {
      case (collected, entry@(key, setDescription)) => collected + (collected.find(_._2.isEquivalentDescription(setDescription)) match {
        case Some((_, result)) => key -> result
        case None => entry
      })
    })
  }

  // ABSTRACT TRANSFORMERS

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): QuantifiedPermissionsState = x match {
    case left: VariableIdentifier =>
      val newRefSets: Map[(ProgramPoint, Expression), ReferenceSetDescription] = left.typ match {
        case _: RefType | _: DomType =>
          refSets.transform {
            case (_, setDescription) => setDescription.transformAssignVariable(left, right)
          }
        case IntType => refSets
      }
      copy(
        changingVars = changingVars + left,
        refSets = newRefSets
      )
    case _ => throw new IllegalStateException()
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
  override def assignField(obj: Expression, field: String, right: Expression): QuantifiedPermissionsState = {
    val receiver = obj match {
      case FieldExpression(_, `field`, rec) => rec
      case _ => throw new IllegalStateException()
    }
    val newPermissions =
      if (!visited.contains(currentPP)) permissions.undoLastRead(field).max(field, ExpressionDescription(currentPP, receiver), WritePermission)
      else permissions
    var newRefSets: Map[(ProgramPoint, Expression), ReferenceSetDescription] = right.typ match {
      case _: RefType | _: DomType =>
        refSets.transform {
          case (_, setDescription) => setDescription.transformAssignField(receiver, field, right)
        }
      case IntType => refSets
      case _ => refSets
    }
    newRefSets = newRefSets ++ extractExpressionDescriptions(receiver).transform((key, elem) => newRefSets.getOrElse(key, elem.bottom()).lub(elem))
    copy(
      permissions = newPermissions,
      refSets = newRefSets
    )
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  override def removeVariable(varExpr: VariableIdentifier): QuantifiedPermissionsState = {
    copy(
      expr = ExpressionSet(varExpr),
      declaredBelowVars = declaredBelowVars + varExpr
    )}

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
  override def getFieldValue(obj: Expression, field: String, typ: Type): QuantifiedPermissionsState = {
    val newPermissions =
      if (!visited.contains(currentPP)) permissions.max(field, ExpressionDescription(currentPP, obj), SymbolicReadPermission)
      else permissions
    val newRefSets = refSets ++ extractExpressionDescriptions(obj).transform((key, elem) => refSets.getOrElse(key, elem.bottom()).lub(elem))
    copy(
      expr = ExpressionSet(FieldExpression(typ, field, obj)),
      permissions = newPermissions,
      refSets = newRefSets
    )
  }

  private def extractExpressionDescriptions(expr: Expression, positive: Boolean = true): Map[(ProgramPoint, Expression), ReferenceSetDescription] = expr match {
    case functionCall@FunctionCallExpression(_, parameters, _, _) =>
      parameters.map {
        case param: RefType => extractExpressionDescriptions(param, positive)
        case param if param.typ.isInstanceOf[DomType] => extractExpressionDescriptions(param, positive)
        case _ => Map[(ProgramPoint, Expression), ReferenceSetDescription]()
      }.reduce(_ ++ _) +
      ((currentPP, functionCall) -> ReferenceSetDescription.Inner(currentPP, functionCall, positive))
    case _ => Map(((currentPP, expr), ReferenceSetDescription.Inner(currentPP, expr, positive)))
  }

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): QuantifiedPermissionsState = acc match {
    case FieldAccessPredicate(FieldExpression(_, field, receiver), num, denom, _) =>
      val newPermissions =
        if (!visited.contains(currentPP)) permissions.undoLastRead(field).sub(field, ExpressionDescription(currentPP, receiver), FractionalPermission(num, denom))
        else permissions
      val newRefSets = refSets ++ extractExpressionDescriptions(receiver, positive = false).transform((key, elem) => refSets.getOrElse(key, elem.bottom()).lub(elem))
      copy(
        permissions = newPermissions,
        refSets = newRefSets
      )
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): QuantifiedPermissionsState = acc match {
    case FieldAccessPredicate(FieldExpression(_, field, receiver), num, denom, _) =>
      val newPermissions =
        if (!visited.contains(currentPP)) permissions.undoLastRead(field).add(field, ExpressionDescription(currentPP, receiver), FractionalPermission(num, denom))
        else permissions
      val newRefSets = refSets ++ extractExpressionDescriptions(receiver).transform((key, elem) => refSets.getOrElse(key, elem.bottom()).lub(elem))
      copy(
        permissions = newPermissions,
        refSets = newRefSets
      )
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): QuantifiedPermissionsState = copy(currentPP = pp)

  def after(pp: ProgramPoint): QuantifiedPermissionsState = copy(visited = visited + pp)

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = copy(expr = ExpressionSet(Constant(value, typ, pp)))

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): QuantifiedPermissionsState = copy(expr = ExpressionSet(id))

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: QuantifiedPermissionsState): QuantifiedPermissionsState = (this, other) match {
    case (Top, _) | (_, Bottom) => this
    case (Bottom, _) | (_, Top) => other
    case _ =>
      copy(
        visited = visited ++ other.visited,
        refSets = refSets ++ other.refSets.transform {
          case (key, setDescription) =>
            if (refSets.contains(key))
              if (setDescription.lessEqual(refSets(key))) refSets(key).lub(setDescription)
              else refSets(key).widening(setDescription)
            else setDescription
        }
      )
  }

  // DO NOTHING ON THESE OPERATIONS

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = this

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): QuantifiedPermissionsState = this

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = this

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): QuantifiedPermissionsState = this

  override def command(cmd: Command): QuantifiedPermissionsState = this

  // STUBS

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  override def throws(t: ExpressionSet): QuantifiedPermissionsState = throw new UnsupportedOperationException

  override def ids: IdentifierSet = throw new UnsupportedOperationException

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): QuantifiedPermissionsState = throw new UnsupportedOperationException

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): QuantifiedPermissionsState = throw new UnsupportedOperationException

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): QuantifiedPermissionsState = throw new UnsupportedOperationException

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): QuantifiedPermissionsState = throw new UnsupportedOperationException()

  /**
    * Returns the inferred specifications.
    *
    * @return The inferred specifications.
    */
  override def specifications: Any = null
}

/** Trait adding Inhale/Exhale methods to a SimpleState.
  *
  * @author Caterina Urban
  */
trait SimplePermissionState[S <: SimplePermissionState[S]] extends SimpleState[S] {
  this: S =>

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  def inhale(acc: Expression): S

  def inhale(acc: ExpressionSet): S = unlessBottom(acc, {
    Lattice.bigLub(acc.toSetOrFail.map(inhale))
  })

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  def exhale(acc: Expression): S

  def exhale(acc: ExpressionSet): S = unlessBottom(acc, {
    Lattice.bigLub(acc.toSetOrFail.map(exhale))
  })
}