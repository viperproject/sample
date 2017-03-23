/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{BoxedNonRelationalNumericalDomain, IntegerInterval, NonRelationalNumericalDomain}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverAnalysisRunner
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Expression
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging

/**
  * A very simple state used for a non relational numerical analysis. Only the
  * values of variables are analyzed. Values of fields are ignored.
  *
  * @tparam S The type of the state.
  * @tparam N The type of the non relation numerical domain.
  * @author Jerome Dohrau
  */
trait NonRelationalNumericalAnalysisState[S <: NonRelationalNumericalAnalysisState[S, N], N <: NonRelationalNumericalDomain[N]]
  extends SilverState[S]
    with StateWithRefiningAnalysisStubs[S]
    with LazyLogging {
  this: S =>

  def pp: ProgramPoint

  def domain: BoxedNonRelationalNumericalDomain[N]

  override def inhale(expression: Expression): S = assume(expression)

  override def exhale(expression: Expression): S = {
    val assumed = assume(expression)
    val assumedFalse = assume(NegatedBooleanExpression(expression))
    if (!assumedFalse.lessEqual(bottom())) {
      Reporter.reportAssertionViolation("Possible assertion violation", pp)
    }
    assumed
  }

  override def before(pp: ProgramPoint): S = copy(pp = pp)

  override def createObject(typ: Type, pp: ProgramPoint): S = {
    logger.trace(s"createObject($typ)")
    this
  }

  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): S = {
    logger.trace(s"evalConstant($value)")
    copy(expr = ExpressionSet(Constant(value, typ, pp)))
  }

  override def getVariableValue(id: Identifier): S = {
    logger.trace(s"getVariableValue($id)")
    copy(expr = ExpressionSet(id))
  }

  override def pruneUnreachableHeap(): S = ???

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): S = ???

  override def removeExpression(): S = copy(expr = ExpressionSet())

  override def setArgument(x: ExpressionSet, right: ExpressionSet): S = ???

  override def setExpression(expr: ExpressionSet): S = copy(expr = expr)

  override def throws(t: ExpressionSet): S = ???

  override def ids: IdentifierSet = IdentifierSet.Inner(domain.map.keySet)

  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): S = {
    logger.trace(s"createVariable($x)")
    copy(domain = domain.createVariable(x, typ))
  }

  override def createVariableForArgument(x: VariableIdentifier, typ: Type): S = {
    logger.trace(s"createVariableForArgument($x)")
    copy(domain = domain.createVariable(x, typ))
  }

  override def assignVariable(x: Expression, right: Expression): S = {
    logger.trace(s"assignVariable($x, $right)")
    x match {
      case variable: VariableIdentifier => copy(domain = domain.assign(variable, right))
      case _ => throw new IllegalArgumentException(s"$x is not a variable identifier.")
    }
  }

  override def assignField(obj: Expression, field: String, right: Expression): S = {
    logger.trace(s"assignField($obj, $field, $right")
    this
  }

  override def setVariableToTop(x: Expression): S = {
    logger.trace(s"setVariableToTop($x)")
    x match {
      case variable: VariableIdentifier => copy(domain = domain.setToTop(variable))
      case _ => throw new IllegalArgumentException(s"$x is not a variable identifier.")
    }
  }

  override def removeVariable(x: VariableIdentifier): S = {
    logger.trace(s"removeVariable($x)")
    x match {
      case variable: VariableIdentifier => copy(domain = domain.removeVariable(variable))
      case _ => throw new IllegalArgumentException(s"$x is not a variable identifier.")
    }
  }

  override def getFieldValue(obj: Expression, field: String, typ: Type): S = {
    logger.trace(s"getFieldValue($obj, field, typ)")
    this
  }

  override def assume(cond: Expression): S = {
    logger.trace(s"assume($cond)")
    if (isTop) copy(domain = domain.top().assume(cond), isTop = false)
    else copy(domain = domain.assume(cond))
  }

  override def factory(): S = top()

  override def top(): S = copy(isTop = true, isBottom = false)

  override def bottom(): S = copy(isTop = false, isBottom = true)

  override def lub(other: S): S = {
    logger.trace(s"lub($this, $other)")
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else copy(domain = domain lub other.domain)
  }

  override def glb(other: S): S = {
    logger.trace(s"glb($this, $other)")
    if (isBottom || other.isTop) this
    else if (isTop || other.isBottom) other
    else copy(domain = domain glb other.domain)
  }

  override def widening(other: S): S = {
    logger.trace(s"widening($this, $other)")
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else copy(domain = domain widening other.domain)
  }

  override def lessEqual(other: S): Boolean = {
    logger.trace(s"lessEqual($this, $other)")
    if (isBottom || other.isTop) true
    else if (isTop || other.isBottom) false
    else domain lessEqual other.domain
  }

  def copy(pp: ProgramPoint = pp,
           expr: ExpressionSet = expr,
           domain: BoxedNonRelationalNumericalDomain[N] = domain,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): S

  override def toString: String = {
    if (isTop) "⊤"
    else if (isBottom) "⊥"
    else s"[${domain.map.map { case (k, v) => s"$k->$v" }.mkString(", ")}]"
  }
}

trait NonRelationalNumericalAnalysisRunner[S <: NonRelationalNumericalAnalysisState[S, N], N <: NonRelationalNumericalDomain[N]]
  extends SilverAnalysisRunner[S]

/**
  * A very simple state used for a numerical analysis using the integer interval
  * domain.
  *
  * @param pp       The program point before the statement.
  * @param expr     The expression representing the current result.
  * @param domain   The domain mapping identifiers to integer intervals.
  * @param isTop    The top flag.
  * @param isBottom The bottom flag.
  * @author Jerome Dohrau
  */
case class IntegerIntervalAnalysisState(pp: ProgramPoint,
                                        expr: ExpressionSet,
                                        domain: BoxedNonRelationalNumericalDomain[IntegerInterval],
                                        isTop: Boolean,
                                        isBottom: Boolean)
  extends NonRelationalNumericalAnalysisState[IntegerIntervalAnalysisState, IntegerInterval] {
  override def copy(pp: ProgramPoint, expr: ExpressionSet, domain: BoxedNonRelationalNumericalDomain[IntegerInterval], isTop: Boolean, isBottom: Boolean): IntegerIntervalAnalysisState = {
    val b = isBottom || (!isTop && domain.isBottom)
    IntegerIntervalAnalysisState(pp, expr, domain, isTop, b)
  }
}

/**
  * The entry state for a numerical analysis using the integer interval domain.
  *
  * @author Jerome Dohrau
  */
object IntegerIntervalAnalysisEntryState
  extends SimpleEntryStateBuilder[IntegerIntervalAnalysisState] {
  override def top: IntegerIntervalAnalysisState = IntegerIntervalAnalysisState(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    domain = BoxedNonRelationalNumericalDomain[IntegerInterval](IntegerInterval.Top, isTop = true),
    isTop = false,
    isBottom = false
  )
}

/**
  * A numerical analysis using the integer interval domain.
  *
  * @author Jerome Dohrau
  */
object IntegerIntervalAnalysis
  extends NonRelationalNumericalAnalysisRunner[IntegerIntervalAnalysisState, IntegerInterval] {
  override val analysis: SilverAnalysis[IntegerIntervalAnalysisState] = SimpleSilverForwardAnalysis(IntegerIntervalAnalysisEntryState)
}