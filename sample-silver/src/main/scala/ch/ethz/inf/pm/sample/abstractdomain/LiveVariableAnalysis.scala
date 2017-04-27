/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{FormalReturns, SilverAnalysisRunner, SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging

/**
  * A state for (strongly) live variable analysis. IdentifierSet is used as domain.
  * If an identifier exists in the domain then it is considered to be live. Widening is implemented as a join of the
  * two operands.
  *
  * This implementation treats fields as "always live". An assignment to a field will therefore set all identifiers
  * on the right hand side of the assignment to live.
  *
  * @tparam S The type of the state.
  * @author Flurin Rindisbacher
  */
trait LiveVariableAnalysisState[S <: LiveVariableAnalysisState[S]]
  extends SilverState[S]
    with StateWithRefiningAnalysisStubs[S]
    with LazyLogging {
  this: S =>

  def pp: ProgramPoint

  def domain: IdentifierSet

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

  override def ids: IdentifierSet = domain

  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): S = {
    logger.trace(s"createVariable($x)")
    this
  }

  override def createVariableForArgument(x: VariableIdentifier, typ: Type): S = {
    logger.trace(s"createVariableForArgument($x)")
    copy(domain = domain + x)
  }

  override def assignVariable(x: Expression, right: Expression): S = {
    logger.trace(s"assignVariable($x, $right)")
    x match {
      case left: VariableIdentifier if domain.contains(left) =>
        val d = domain - left // LIVE \ KILL
        if (right.ids.isBottom) copy(domain = d) else copy(domain = d ++ right.ids) // gen ∪ (live \ kill)
      case _: VariableIdentifier => this
      case _ => throw new IllegalArgumentException(s"$x is not a variable identifier.")
    }
  }

  override def assignField(obj: Expression, field: String, right: Expression): S = {
    logger.trace(s"assignField($obj, $field, $right")
    // we treat fields a being always live. that's why there's no "kill" set for this assignment
    if (right.ids.isBottom) copy(domain = domain) else copy(domain = domain ++ right.ids)
    this
  }

  override def setVariableToTop(x: Expression): S = {
    logger.trace(s"setVariableToTop($x)")
    x match {
      case variable: VariableIdentifier => copy(domain = domain + variable)
      case _ => throw new IllegalArgumentException(s"$x is not a variable identifier.")
    }
  }

  override def removeVariable(x: VariableIdentifier): S = {
    logger.trace(s"removeVariable($x)")
    x match {
      case variable: VariableIdentifier => copy(domain = domain - variable)
      case _ => throw new IllegalArgumentException(s"$x is not a variable identifier.")
    }
    this
  }

  override def getFieldValue(obj: Expression, field: String, typ: Type): S = {
    logger.trace(s"getFieldValue($obj, field, typ)")
    this
  }

  override def assume(cond: Expression): S = {
    copy(domain = domain ++ cond.ids)
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
    else copy(domain = domain ++ other.domain)
  }

  override def lessEqual(other: S): Boolean = {
    logger.trace(s"lessEqual($this, $other)")
    if (isBottom || other.isTop) true
    else if (isTop || other.isBottom) false
    else domain lessEqual other.domain
  }

  def copy(pp: ProgramPoint = pp,
           expr: ExpressionSet = expr,
           domain: IdentifierSet = domain,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): S

  override def toString: String = {
    if (isTop) "⊤"
    else if (isBottom) "⊥"
    else s"live variables: $domain"
  }
}

object LiveVariableAnalysisEntryState
  extends SimpleEntryStateBuilder[SimpleLiveVariableAnalysisState] {
  override def top: SimpleLiveVariableAnalysisState = SimpleLiveVariableAnalysisState(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    domain = IdentifierSet.Inner(Set()),
    isTop = false,
    isBottom = false
  )

  override def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): SimpleLiveVariableAnalysisState = method match {
    case r: FormalReturns => r.returns.foldLeft(top) { case (state, parameter) =>
      val result = parameter.variable.forwardSemantics(state)
      val expression = result.expr
      result.removeExpression().createVariableForArgument(expression, parameter.typ)
    }
    case _ => super.build(program, method)
  }
}

case class SimpleLiveVariableAnalysisState(pp: ProgramPoint,
                                           expr: ExpressionSet,
                                           domain: IdentifierSet,
                                           isTop: Boolean,
                                           isBottom: Boolean)
  extends LiveVariableAnalysisState[SimpleLiveVariableAnalysisState] {
  override def copy(pp: ProgramPoint, expr: ExpressionSet, domain: IdentifierSet, isTop: Boolean, isBottom: Boolean): SimpleLiveVariableAnalysisState = {
    val b = isBottom || (!isTop && domain.isBottom)
    SimpleLiveVariableAnalysisState(pp, expr, domain, isTop, b)
  }
}

object LiveVariableAnalysis
  extends SilverAnalysisRunner[SimpleLiveVariableAnalysisState] {
  override val analysis: SilverAnalysis[SimpleLiveVariableAnalysisState] = SimpleSilverForwardAnalysis(LiveVariableAnalysisEntryState)
}
