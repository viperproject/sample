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
import ch.ethz.inf.pm.sample.util.SampleExpressions
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
case class QuantifiedPermissionState(pp: ProgramPoint,
                                     expr: ExpressionSet,
                                     records: List[PermissionRecords],
                                     isTop: Boolean,
                                     isBottom: Boolean)
  extends SilverState[QuantifiedPermissionState]
    with Simplifications[QuantifiedPermissionState]
    with StateWithRefiningAnalysisStubs[QuantifiedPermissionState]
    with LazyLogging {

  import SampleExpressions._

  type S = QuantifiedPermissionState

  /* ------------------------------------------------------------------------- *
   * LATTICE FUNCTIONS
   */

  override def factory(): S = copy(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    records = PermissionRecords() :: Nil,
    isTop = false,
    isBottom = false
  )

  override def top(): S = {
    logger.trace("top()")
    copy(isTop = true, isBottom = false)
  }

  override def bottom(): S = {
    logger.trace("bottom()")
    copy(isTop = false, isBottom = true)
  }

  override def lub(other: S): S = {
    logger.trace("lub()")
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {
      val bottom = records.head.bottom()
      val zipped = records.zipAll(other.records, bottom, bottom)
      val newRecords = zipped.map { case (a, b) => a lub b }
      copy(records = newRecords)
    }
  }

  override def glb(other: S): S = ???

  override def widening(other: S): S = ???

  override def lessEqual(other: S): Boolean = {
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

  override def before(pp: ProgramPoint): S = copy(pp = pp)

  override def createObject(typ: Type, pp: ProgramPoint): S = ???

  override def pruneUnreachableHeap(): S = ???

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): S = ???

  override def setExpression(expr: ExpressionSet): S = copy(expr = expr)

  override def removeExpression(): S = copy(expr = ExpressionSet())

  override def setArgument(x: ExpressionSet, right: ExpressionSet): S = ???

  override def throws(t: ExpressionSet): S = ???

  override def ids: IdentifierSet = ???

  /* ------------------------------------------------------------------------- *
   * SIMPLE STATE FUNCTIONS
   */

  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): S = this

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): S =
    createVariable(variable, typ, DummyProgramPoint)

  override def removeVariable(varExpr: VariableIdentifier): S = this

  override def assume(condition: Expression): S = {
    logger.trace(s"assume(condition)")
    val newHead = records.head.assume(condition).read(condition)
    copy(records = newHead :: records.tail)
  }

  override def assignVariable(target: Expression, value: Expression): S = {
    logger.trace(s"assignVariable($target, $value)")
    target match {
      case variable: VariableIdentifier =>
        val newHead = records.head.assignVariable(variable, value).read(value)
        copy(records = newHead :: records.tail)
    }
  }

  override def setVariableToTop(varExpr: Expression): S = ???

  override def assignField(target: Expression, field: String, value: Expression): S = {
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

  override def inhale(expression: Expression): S = {
    logger.trace(s"inhale($expression)")
    val newHead = records.head.inhale(expression).read(expression)
    copy(records = newHead :: records.tail)
  }

  override def exhale(expression: Expression): S = {
    logger.trace(s"exhale($expression)")
    val newHead = records.head.exhale(expression).read(expression)
    copy(records = newHead :: records.tail)
  }

  override def enterLoop(): S = {
    val inner :: outer :: rest = records
    val newRecords = (inner lub outer) :: rest
    copy(records = newRecords)
  }

  override def leaveLoop(): S = {
    val newRecords = records.head.bottom() :: records
    copy(records = newRecords)
  }

  def project(changing: Seq[VariableIdentifier]): S = {
    val inner :: rest = records

    // TODO: Filter constraints that do not mention any changing variable.
    // val filtered = constraints.filter(x => (x.ids.toSet intersect changing.toSet).nonEmpty)
    val numerical = Context.getNumericalResult()
    val domain = numerical.preStateAt(pp).domain
    val constraints = domain.getConstraints(domain.ids.toSet)
    val invariant = AndList(constraints)

    val forgotten = inner.forget(changing, invariant)

    val newRecords = forgotten :: rest
    copy(records = newRecords)
  }

  def second: S = {
    val _ :: outer :: Nil = records
    copy(records = outer :: Nil)
  }

  /* ------------------------------------------------------------------------- *
   * COPY FUNCTION
   */

  def copy(pp: ProgramPoint = pp,
           expr: ExpressionSet = expr,
           records: List[PermissionRecords] = records,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): S =
    QuantifiedPermissionState(pp, expr, records, isTop, isBottom)
}
