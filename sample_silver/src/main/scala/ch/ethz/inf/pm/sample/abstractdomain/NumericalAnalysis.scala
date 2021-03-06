/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.abstractdomain.Identifier.FieldIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.inference.SilverSpecification
import ch.ethz.inf.pm.sample.oorepresentation.silver.{InterproceduralSilverAnalysisRunner, SilverAnalysisRunner}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging

/**
  * A very simple state used for numerical analysis. Only the values of
  * variables are analyzed. Values of fields are ignored.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the numerical domain.
  * @author Jerome Dohrau
  */
trait NumericalAnalysisState[S <: NumericalAnalysisState[S, D], D <: NumericalDomain[D]]
  extends SilverState[S]
    with Simplifications[S]
    with SilverSpecification[Set[Expression]]
    with StateWithRefiningAnalysisStubs[S]
    with LazyLogging {
  this: S =>

  /**
    * The program point before the current statement.
    *
    * @return The program point.
    */
  def pp: ProgramPoint

  /**
    * The element of the numerical domain stored in the state.
    *
    * @return The element of the numerical domain.
    */
  def domain: D

  /* ------------------------------------------------------------------------- *
   * LATTICE FUNCTIONS
   */

  override def factory(): S = {
    logger.trace("factory()")
    top()
  }

  override def top(): S = {
    logger.trace("top()")
    copy(domain = domain.top(), isTop = true, isBottom = false)
  }

  override def bottom(): S = {
    logger.trace("bottom()")
    copy(domain = domain.bottom(), isTop = false, isBottom = true)
  }

  override def lub(other: S): S = {
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else copy(domain = domain lub other.domain)
  }

  override def glb(other: S): S = {
    if (isBottom || other.isTop) this
    else if (isTop || other.isBottom) other
    else copy(domain = domain glb other.domain)
  }

  override def widening(other: S): S = {
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else copy(domain = domain widening other.domain)
  }

  override def lessEqual(other: S): Boolean = {
    if (isBottom || other.isTop) true
    else if (isTop || other.isBottom) false
    else domain lessEqual other.domain
  }

  /* ------------------------------------------------------------------------- *
   * STATE FUNCTIONS
   */

  override def before(pp: ProgramPoint): S = copy(pp = pp)

  override def createObject(typ: Type, pp: ProgramPoint): S = this

  override def pruneUnreachableHeap(): S = ???

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): S = ???

  override def removeExpression(): S = copy(expr = ExpressionSet())

  override def setArgument(x: ExpressionSet, right: ExpressionSet): S = ???

  override def setExpression(expr: ExpressionSet): S = copy(expr = expr)

  override def throws(t: ExpressionSet): S = ???

  override def ids: IdentifierSet = domain.ids

  override def merge(r: Replacement): S = copy(domain = domain.merge(r))

  /* ------------------------------------------------------------------------- *
   * SIMPLE STATE FUNCTIONS
   */

  override def createVariable(id: VariableIdentifier, typ: Type, pp: ProgramPoint): S = {
    logger.trace(s"createVariable($id)")
    copy(domain = domain.createVariable(id, typ))
  }

  override def createVariableForArgument(id: VariableIdentifier, typ: Type): S = {
    logger.trace(s"createVariable($id)")
    copy(domain = domain.createVariable(id, typ))
  }

  override def assignVariable(left: Expression, right: Expression): S = {
    logger.trace(s"assignVariable($left, $right)")
    left match {
      case variable: VariableIdentifier =>
        val invalidRight = right.ids.exists(_.isInstanceOf[FieldIdentifier])
        if (invalidRight) setVariableToTop(variable)
        else copy(domain = domain.assign(variable, right))
      case _ => throw new IllegalArgumentException(s"$left is not a variable identifier.")
    }
  }

  override def assignField(obj: Expression, field: String, right: Expression): S = {
    logger.trace(s"assignField($obj, $field, $right")
    this
  }

  override def setVariableToTop(expr: Expression): S = {
    logger.trace(s"setVariableToTop($expr)")
    expr match {
      case variable: VariableIdentifier => copy(domain = domain.setToTop(variable))
      case _ => throw new IllegalArgumentException(s"$expr is not a variable identifier.")
    }
  }

  override def removeVariable(id: VariableIdentifier): S = {
    logger.trace(s"removeVariable($id)")
    copy(domain = domain.removeVariable(id))

  }

  override def assumeArithmeticExpression(condition: Expression): S = {
    val invalid = condition.ids.exists(_.isInstanceOf[FieldIdentifier])
    if (invalid) this
    else copy(domain = domain.assume(condition))
  }

  /* ------------------------------------------------------------------------- *
   * SILVER STATE FUNCTIONS
   */

  override def inhale(expression: Expression): S = expression match {
    case FieldAccessPredicate(_, _) => this
    case BinaryBooleanExpression(left, right, operator) => operator match {
      case BooleanOperator.&& => inhale(left).inhale(right)
      case BooleanOperator.|| => inhale(left) lub inhale(right)
    }
    case expression => assume(expression)
  }

  override def exhale(expression: Expression): S = expression match {
    case FieldAccessPredicate(_, _) => this
    case BinaryBooleanExpression(left, right, operator) => operator match {
      case BooleanOperator.&& => exhale(left).exhale(right)
      case BooleanOperator.|| => exhale(left) lub exhale(right)
    }
    case expression =>
      val assumed = assume(expression)
      val assumedFalse = assume(NegatedBooleanExpression(expression))
      if (!assumedFalse.lessEqual(bottom())) {
        Reporter.reportAssertionViolation("Possible assertion violation", pp)
      }
      assumed
  }

  /* ------------------------------------------------------------------------- *
   * SILVER SPECIFICATION FUNCTIONS
   */

  override def specifications: Set[Expression] = {
    val ids = domain.ids
    if (ids.isTop || ids.isBottom) Set()
    else domain.getConstraints(ids.toSet)
  }

  /* ------------------------------------------------------------------------- *
   * COPY FUNCTION
   */

  def copy(pp: ProgramPoint = pp,
           expr: ExpressionSet = expr,
           domain: D = domain,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): S
}

/**
  * A very simple state used for a non-relational numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the non-relational numerical domain.
  * @author Jerome Dohrau
  */
trait NonRelationalNumericalAnalysisState[S <: NonRelationalNumericalAnalysisState[S, D], D <: NonRelationalNumericalDomain[D]]
  extends NumericalAnalysisState[S, BoxedNonRelationalNumericalDomain[D]] {
  this: S =>
}

/**
  * A runner for a numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the numerical domain.
  * @author Jerome Dohrau
  */
trait NumericalAnalysisRunner[S <: NumericalAnalysisState[S, D], D <: NumericalDomain[D]]
  extends SilverAnalysisRunner[S]

/**
  * A runner for a non-relational numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the non-relational numerical domain.
  * @author Jerome Dohrau
  */
trait NonRelationalNumericalAnalysisRunner[S <: NonRelationalNumericalAnalysisState[S, D], D <: NonRelationalNumericalDomain[D]]
  extends NumericalAnalysisRunner[S, BoxedNonRelationalNumericalDomain[D]]


/**
  * A runner for an interprocedural numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the numerical domain.
  * @author Flurin Rindisbacher
  */
trait InterproceduralNumericalAnalysisRunner[S <: NumericalAnalysisState[S, D], D <: NumericalDomain[D]]
  extends InterproceduralSilverAnalysisRunner[S]

/**
  * A runner for an interprocedural non-relational numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the non-relational numerical domain.
  * @author Flurin Rindisbacher
  */
trait InterproceduralNonRelationalNumericalAnalysisRunner[S <: NonRelationalNumericalAnalysisState[S, D], D <: NonRelationalNumericalDomain[D]]
  extends InterproceduralNumericalAnalysisRunner[S, BoxedNonRelationalNumericalDomain[D]]

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

  /** Executes the given command.
    *
    * @param cmd The command to execute.
    * @return The abstract state after the execution of the given command.
    */
  override def command(cmd: Command): IntegerIntervalAnalysisState = cmd match {
    case UnifyCommand(other) => other match {
      case o: IntegerIntervalAnalysisState => this lub o
      case _ => super.command(cmd)
    }
    case _ => super.command(cmd)
  }
}

/**
  * The entry state for a numerical analysis using the integer interval domain.
  *
  * @author Jerome Dohrau
  */
object IntegerIntervalAnalysisEntryState
  extends SilverEntryStateBuilder[IntegerIntervalAnalysisState] {
  override def default: IntegerIntervalAnalysisState = IntegerIntervalAnalysisState(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    domain = BoxedNonRelationalNumericalDomain[IntegerInterval](IntegerInterval.Top, isTop = true),
    isTop = false,
    isBottom = false
  )
}

/**
  * A very simple state used for a numerical analysis using the integer octagon
  * domain.
  *
  * @param pp       The program point before the statement.
  * @param expr     The expression representing the current result.
  * @param domain   The element of the octagon domain.
  * @param isTop    The top flag.
  * @param isBottom The bottom flag.
  * @author Jerome Dohrau
  */
case class IntegerOctagonAnalysisState(pp: ProgramPoint,
                                       expr: ExpressionSet,
                                       domain: IntegerOctagons,
                                       isTop: Boolean,
                                       isBottom: Boolean)
  extends NumericalAnalysisState[IntegerOctagonAnalysisState, IntegerOctagons] {
  override def copy(pp: ProgramPoint, expr: ExpressionSet, domain: IntegerOctagons, isTop: Boolean, isBottom: Boolean): IntegerOctagonAnalysisState = {
    val b = isBottom || (!isTop && domain.isBottom)
    IntegerOctagonAnalysisState(pp, expr, domain, isTop, b)
  }

  /** Executes the given command.
    *
    * @param cmd The command to execute.
    * @return The abstract state after the execution of the given command.
    */
  override def command(cmd: Command): IntegerOctagonAnalysisState = cmd match {
    case UnifyCommand(other) => other match {
      case o: IntegerOctagonAnalysisState => {
        // we know that ''this'' and ''other'' may not have disjoint envs but that the constraints
        // do not conflict each other. domain.unify() assumes the environment to be disjoint which is why
        // we unify the domains ourselves.
        def unify(that: IntegerOctagons.Inner, other: IntegerOctagons.Inner): IntegerOctagons = {
          val newEnv = that.env ++ other.env
          val newClosed = Some(that.getDbm.factory(newEnv.size)
            .copy(that.getDbm, that.env.getIndices(that.env.ids), newEnv.getIndices(that.env.ids))
            .copy(other.getDbm, other.env.getIndices(other.env.ids), newEnv.getIndices(other.env.ids))
            .close())
          val newOpen = None
          that.factory(newEnv, newClosed, newOpen)
        }

        o.domain match {
          case _: Octagons.Top[IntegerOctagons] => o
          case _: Octagons.Bottom[IntegerOctagons] => this
          case other: IntegerOctagons.Inner => domain match {
            case _: Octagons.Top[IntegerOctagons] => this
            case _: Octagons.Bottom[IntegerOctagons] => o
            case that: IntegerOctagons.Inner => copy(domain = unify(that, other))
          }
        }
      }
      case _ => super.command(cmd)
    }
    case _ => super.command(cmd)
  }
}

/**
  * The entry state for a numerical analysis using the integer octagon domain.
  *
  * @author Jerome Dohrau
  */
object IntegerOctagonAnalysisEntryState
  extends SilverEntryStateBuilder[IntegerOctagonAnalysisState] {
  override def default: IntegerOctagonAnalysisState = IntegerOctagonAnalysisState(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    domain = IntegerOctagons.Top.factory(Set.empty[Identifier]),
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

/**
  * An interprocedural numerical analysis using the integer interval domain.
  *
  * @author Flurin Rindisbacher
  */
object InterproceduralIntegerIntervalAnalysis
  extends InterproceduralNonRelationalNumericalAnalysisRunner[IntegerIntervalAnalysisState, IntegerInterval] {
  override val analysis: InterproceduralSilverForwardAnalysis[IntegerIntervalAnalysisState] = SimpleInterproceduralSilverForwardAnalysis(IntegerIntervalAnalysisEntryState)
}

/**
  * An interprocedural numerical analysis using the integer interval domain.
  *
  * @author Flurin Rindisbacher
  */
object ContextInsensitiveInterproceduralIntegerIntervalAnalysis
  extends InterproceduralNonRelationalNumericalAnalysisRunner[IntegerIntervalAnalysisState, IntegerInterval] {
  override val analysis: InterproceduralSilverForwardAnalysis[IntegerIntervalAnalysisState] = SimpleInterproceduralSilverForwardAnalysis(IntegerIntervalAnalysisEntryState, CallString.ContextInsensitive)
}

/**
  * A numerical analysis using the integer octagon domain.
  */
object IntegerOctagonAnalysis
  extends NumericalAnalysisRunner[IntegerOctagonAnalysisState, IntegerOctagons] {
  override val analysis: SilverAnalysis[IntegerOctagonAnalysisState] = SimpleSilverForwardAnalysis(IntegerOctagonAnalysisEntryState)
}

/**
  * An interprocedural numerical analysis using the integer octagon domain.
  *
  * @author Jerome Dohrau
  */
object InterproceduralIntegerOctagonAnalysis
  extends InterproceduralNumericalAnalysisRunner[IntegerOctagonAnalysisState, IntegerOctagons] {
  override val analysis: InterproceduralSilverForwardAnalysis[IntegerOctagonAnalysisState] = SimpleInterproceduralSilverForwardAnalysis(IntegerOctagonAnalysisEntryState)
}