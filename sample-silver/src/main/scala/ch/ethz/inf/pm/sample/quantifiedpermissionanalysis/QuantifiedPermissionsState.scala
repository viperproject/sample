/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.{SilverState, Simplifications}
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Expression
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

/**
  * A stated used for the quantified permission analysis.
  *
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
                                      records: List[PermissionRecords],
                                      isTop: Boolean,
                                      isBottom: Boolean)
  extends SilverState[QuantifiedPermissionsState]
    with Simplifications[QuantifiedPermissionsState]
    with StateWithRefiningAnalysisStubs[QuantifiedPermissionsState]
    with LazyLogging {

  /* ------------------------------------------------------------------------- *
   * LATTICE FUNCTIONS
   */

  override def factory(): QuantifiedPermissionsState = copy(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    records = PermissionRecords() :: Nil,
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
    else {
      val newHead = records.head lub other.records.head
      copy(records = newHead :: records.tail)
    }
  }

  override def glb(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  override def widening(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  override def lessEqual(other: QuantifiedPermissionsState): Boolean = {
    logger.trace(s"lessEqual($this, $other)")
    if (isBottom || other.isTop) true
    else if (isTop || other.isBottom) false
    else {
      // TODO: Implement me if necessary.
      ???
    }
  }

  /* ------------------------------------------------------------------------- *
   * STATE FUNCTIONS
   */

  override def before(pp: ProgramPoint): QuantifiedPermissionsState = copy(pp = pp)

  override def createObject(typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = ???

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

  override def assume(condition: Expression): QuantifiedPermissionsState = {
    logger.trace(s"assume(condition)")
    val newHead = records.head.assume(condition).read(condition)
    copy(records = newHead :: records.tail)
  }

  override def assignVariable(target: Expression, value: Expression): QuantifiedPermissionsState = {
    logger.trace(s"assignVariable($target, $value)")
    target match {
      case variable: VariableIdentifier =>
        val newHead = records.head.assignVariable(variable, value).read(value)
        copy(records = newHead :: records.tail)
    }
  }

  override def setVariableToTop(varExpr: Expression): QuantifiedPermissionsState = ???

  override def assignField(target: Expression, field: String, value: Expression): QuantifiedPermissionsState = {
    logger.trace(s"assignField($target, $value)")
    target match {
      case expression: FieldAccessExpression =>
        val newHead = records.head.assignField(expression, value).write(expression).read(value)
        copy(records = newHead :: records.tail)
    }
  }

  /* ------------------------------------------------------------------------- *
   * SILVER STATE FUNCTIONS
   */

  override def inhale(expression: Expression): QuantifiedPermissionsState = {
    logger.trace(s"inhale($expression)")
    val newHead = records.head.inhale(expression).read(expression)
    copy(records = newHead :: records.tail)
  }

  override def exhale(expression: Expression): QuantifiedPermissionsState = {
    logger.trace(s"exhale($expression)")
    val newHead = records.head.exhale(expression).read(expression)
    copy(records = newHead :: records.tail)
  }

  override def enterLoop(): QuantifiedPermissionsState = {
    val inner :: outer :: rest = records
    val newRecords = (inner lub outer) :: rest
    copy(records = newRecords)
  }

  override def leaveLoop(): QuantifiedPermissionsState = {
    val newRecords = records.head.clear() :: records
    copy(records = newRecords)
  }

  /* ------------------------------------------------------------------------- *
   * COPY FUNCTION
   */

  def copy(pp: ProgramPoint = pp,
           expr: ExpressionSet = expr,
           records: List[PermissionRecords] = records,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): QuantifiedPermissionsState =
    QuantifiedPermissionsState(pp, expr, records, isTop, isBottom)
}
