/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.SilverState
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Expression
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

/**
  * @param pp          The current program point.
  * @param expr        The expression representing the current result.
  * @param permissions A map from field names to the permission trees.
  * @param isTop       The top flag.
  * @param isBottom    The bottom flag.
  * @author Severin Münger
  * @author Jerome Dohrau
  */
case class QuantifiedPermissionsState(pp: ProgramPoint,
                                      expr: ExpressionSet,
                                      permissions: Map[String, PermissionTree],
                                      isTop: Boolean,
                                      isBottom: Boolean)
  extends SilverState[QuantifiedPermissionsState]
    with StateWithRefiningAnalysisStubs[QuantifiedPermissionsState]
    with LazyLogging {

  /* ------------------------------------------------------------------------- *
   * LATTICE FUNCTIONS
   */

  override def factory(): QuantifiedPermissionsState = ???

  override def top(): QuantifiedPermissionsState = {
    logger.trace("top()")
    copy(isTop = true, isBottom = false)
  }

  override def bottom(): QuantifiedPermissionsState = {
    logger.trace("bottom()")
    copy(isTop = false, isBottom = true)
  }

  override def lub(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  override def glb(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  override def widening(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  override def lessEqual(other: QuantifiedPermissionsState): Boolean = ???

  /* ------------------------------------------------------------------------- *
   *
   */

  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = this

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): QuantifiedPermissionsState =
    createVariable(variable, typ, DummyProgramPoint)

  override def removeVariable(varExpr: VariableIdentifier): QuantifiedPermissionsState = ???

  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    val result = Constant(value, typ, pp)
    copy(expr = ExpressionSet(result))
  }

  override def getFieldValue(obj: Expression, field: String, typ: Type): QuantifiedPermissionsState = {
    obj match {
      case receiver: VariableIdentifier =>
        val identifier = VariableIdentifier(field)(typ)
        val result = AccessPathIdentifier(List(receiver, identifier))
        copy(expr = ExpressionSet(result))
      case AccessPathIdentifier(path) =>
        val identifier = VariableIdentifier(field)(typ)
        val result = AccessPathIdentifier(path :+ identifier)
        copy(expr = ExpressionSet(result))
    }
  }

  override def assume(condition: Expression): QuantifiedPermissionsState = {
    val newPermissions = permissions.mapValues(_.assume(condition))
    copy(permissions = newPermissions)
  }

  override def assignVariable(x: Expression, right: Expression): QuantifiedPermissionsState = ???

  override def setVariableToTop(varExpr: Expression): QuantifiedPermissionsState = ???

  override def assignField(obj: Expression, field: String, right: Expression): QuantifiedPermissionsState = ???

  override def setArgument(x: ExpressionSet, right: ExpressionSet): QuantifiedPermissionsState = ???

  override def removeExpression(): QuantifiedPermissionsState = copy(expr = ExpressionSet())

  override def throws(t: ExpressionSet): QuantifiedPermissionsState = ???

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): QuantifiedPermissionsState = ???

  override def ids: IdentifierSet = ???

  override def before(pp: ProgramPoint): QuantifiedPermissionsState = copy(pp = pp)

  override def pruneUnreachableHeap(): QuantifiedPermissionsState = ???

  override def createObject(typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = ???

  override def setExpression(expr: ExpressionSet): QuantifiedPermissionsState = copy(expr = expr)

  override def getVariableValue(variable: Identifier): QuantifiedPermissionsState = {
    logger.trace(s"getVariableValue($variable)")
    copy(expr = ExpressionSet(variable))
  }

  /* ------------------------------------------------------------------------- *
   * STATE FUNCTIONS
   */

  override def inhale(expression: Expression): QuantifiedPermissionsState = {
    // TODO: assume pure part of expression.
    logger.trace(s"inhale($expression)")
    expression match {
      case FieldAccessPredicate(identifier, numerator, denominator, _) =>
        ???
    }
  }

  override def exhale(expression: Expression): QuantifiedPermissionsState = {
    // TODO: Maybe assert pure part of expression?
    logger.trace(s"exhale($expression)")
    expression match {
      case FieldAccessPredicate(identifier, numerator, denominator, _) =>
        ???
    }
  }

  /* ------------------------------------------------------------------------- *
   * COPY FUNCTION
   */

  def copy(pp: ProgramPoint = pp,
           expr: ExpressionSet = expr,
           permissions: Map[String, PermissionTree] = permissions,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): QuantifiedPermissionsState =
    QuantifiedPermissionsState(pp, expr, permissions, isTop, isBottom)
}
