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
  * @param pp       The current program point.
  * @param expr     The expression representing the current result.
  * @param records  A map from field names to the permission trees.
  * @param isTop    The top flag.
  * @param isBottom The bottom flag.
  * @author Jerome Dohrau
  * @author Severin Münger
  */
case class QuantifiedPermissionsState(pp: ProgramPoint,
                                      expr: ExpressionSet,
                                      records: PermissionRecords,
                                      isTop: Boolean,
                                      isBottom: Boolean)
  extends SilverState[QuantifiedPermissionsState]
    with StateWithRefiningAnalysisStubs[QuantifiedPermissionsState]
    with LazyLogging {

  /* ------------------------------------------------------------------------- *
   * LATTICE FUNCTIONS
   */

  override def factory(): QuantifiedPermissionsState = copy(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    records = PermissionRecords(),
    isTop = false,
    isBottom = false
  )

  override def top(): QuantifiedPermissionsState = {
    logger.trace("top()")
    copy(isTop = true, isBottom = false)
  }

  override def bottom(): QuantifiedPermissionsState = {
    logger.trace("bottom()")
    copy(isTop = false, isBottom = true)
  }

  override def lub(other: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    logger.trace("lub()")
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else copy(records = records.lub(other.records))
  }

  override def glb(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  override def widening(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  override def lessEqual(other: QuantifiedPermissionsState): Boolean = ???

  /* ------------------------------------------------------------------------- *
   * STATE FUNCTIONS
   */

  override def before(pp: ProgramPoint): QuantifiedPermissionsState = copy(pp = pp)

  override def createObject(typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = ???

  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    logger.trace(s"evalConstant($value)")
    val result = Constant(value, typ, pp)
    copy(expr = ExpressionSet(result))
  }

  override def getVariableValue(variable: Identifier): QuantifiedPermissionsState = {
    logger.trace(s"getVariableValue($variable)")
    copy(expr = ExpressionSet(variable))
  }

  override def pruneUnreachableHeap(): QuantifiedPermissionsState = ???

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): QuantifiedPermissionsState = ???

  override def setExpression(expr: ExpressionSet): QuantifiedPermissionsState = copy(expr = expr)

  override def removeExpression(): QuantifiedPermissionsState = copy(expr = ExpressionSet())

  override def setArgument(x: ExpressionSet, right: ExpressionSet): QuantifiedPermissionsState = ???

  override def throws(t: ExpressionSet): QuantifiedPermissionsState = ???

  override def ids: IdentifierSet = ???

  /* ------------------------------------------------------------------------- *
   * SIMPLE STATE FUNCTIONS
   */

  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = this

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): QuantifiedPermissionsState =
    createVariable(variable, typ, DummyProgramPoint)

  override def removeVariable(varExpr: VariableIdentifier): QuantifiedPermissionsState = this

  override def getFieldValue(receiver: Expression, field: String, typ: Type): QuantifiedPermissionsState = {
    logger.trace(s"getFieldValue($receiver, $field)")
    receiver match {
      case variable: VariableIdentifier =>
        val identifier = VariableIdentifier(field)(typ)
        val result = AccessPathIdentifier(List(variable, identifier))
        copy(expr = ExpressionSet(result))
      case AccessPathIdentifier(path) =>
        val identifier = VariableIdentifier(field)(typ)
        val result = AccessPathIdentifier(path :+ identifier)
        copy(expr = ExpressionSet(result))
    }
  }

  override def assume(condition: Expression): QuantifiedPermissionsState = {
    logger.trace(s"assume(condition)")
    val updated = records.assume(condition).read(condition)
    copy(records = updated)
  }

  override def assignVariable(variable: Expression, value: Expression): QuantifiedPermissionsState = {
    logger.trace(s"assignVariable($variable, $value)")
    val updated = records.assignVariable(variable, value).read(value)
    copy(records = updated)
  }

  override def setVariableToTop(varExpr: Expression): QuantifiedPermissionsState = ???

  override def assignField(target: Expression, field: String, value: Expression): QuantifiedPermissionsState = {
    logger.trace(s"assignField($target, $value)")
    val updated = records.assignField(target, value).write(target).read(value)
    copy(records = updated)
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
   * HELPER FUNCTIONS
   */


  /* ------------------------------------------------------------------------- *
   * COPY FUNCTION
   */

  def copy(pp: ProgramPoint = pp,
           expr: ExpressionSet = expr,
           records: PermissionRecords = records,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): QuantifiedPermissionsState =
    QuantifiedPermissionsState(pp, expr, records, isTop, isBottom)
}
