/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.elimination

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.util.Math.lcm
import ch.ethz.inf.pm.sample.util.SampleExpressions

/**
  * Some utility functions to eliminate extrema.
  *
  * @author Jerome Dohrau
  */
object MaximumElimination
  extends Elimination {

  import SampleExpressions._

  override def eliminate(expression: Expression): Expression = expression match {
    case BigMax(variables, body) =>
      variables.foldLeft(body) {
        case (eliminated, variable) =>
          eliminateMaximum(variable, eliminated)
      }
    case BigMin(variables, body) => eliminate(Negate(BigMax(variables, Negate(body))))
    case Negate(argument) => Negate(eliminate(argument))
  }

  private def eliminateMaximum(variable: VariableIdentifier, expression: Expression): Expression = {
    // normalize expression
    val normalized = normalize(variable, expression)
    // compute projections
    val negative = negativeInfiniteProjection(variable, normalized)
    val positive = positiveInfiniteProjection(variable, normalized)
    // compute set of interesting expressions and delta
    val (minExpressions, minDelta) = arithmeticMin(variable, normalized)
    val (maxExpressions, maxDelta) = arithmeticMax(variable, normalized)
    // pick smaller set of expressions and the corresponding delta and projection
    val (expressions, delta, projection) = if (minExpressions.size < maxExpressions.size) {
      (minExpressions, minDelta, negative)
    } else {
      (maxExpressions, maxDelta, positive)
    }
    // compute maximum corresponding to unbounded solutions
    val unbounded = for (i <- 0 until delta) yield
      projection.transform {
        case `variable` => Literal(i)
        case other => other
      }
    // compute maximum corresponding to bounded solutions
    val bounded = for (e <- expressions; i <- 0 until delta) yield
      normalized.transform {
        case `variable` => Plus(e, Literal(i))
        case other => other
      }
    // build and simplify final expression
    val maximum = Max(unbounded ++ bounded)
    simplify(maximum, collect = true)
  }

  protected def arithmeticMin(variable: VariableIdentifier, expression: Expression): (Set[Expression], Int) = expression match {
    // conditional expressions
    case ConditionalExpression(condition, _, _) =>
      // TODO: Left is constant.
      // TODO: Right is zero.
      booleanMin(variable, condition)
    // additions
    case Plus(left, right) =>
      val (s1, d1) = arithmeticMin(variable, left)
      val (s2, d2) = arithmeticMin(variable, right)
      (s1 ++ s2, lcm(d1, d2))
    // subtractions
    case Minus(term, ConditionalExpression(condition, _, _)) =>
      // negate condition since the conditional appears in a negative position
      val negated = toNegatedNormalForm(Not(condition))
      val (s1, d1) = arithmeticMin(variable, term)
      val (s2, d2) = booleanMin(variable, negated)
      // TODO: remove all expressions from s2 that make "term" zero
      (s1 ++ s2, lcm(d1, d2))
    // other expressions
    case _ => ???
  }

  protected def arithmeticMax(variable: VariableIdentifier, expression: Expression): (Set[Expression], Int) = expression match {
    // conditional expressions
    case ConditionalExpression(condition, _, _) => booleanMax(variable, condition)
    // additions
    case Plus(left, right) =>
      val (s1, d1) = arithmeticMax(variable, left)
      val (s2, d2) = arithmeticMax(variable, right)
      (s1 ++ s2, lcm(d1, d2))
    // subtractions
    case Minus(term, ConditionalExpression(condition, _, _)) =>
      // negate condition since the conditional appears in a negative position
      val negated = toNegatedNormalForm(Not(condition))
      val (s1, d1) = arithmeticMax(variable, term)
      val (s2, d2) = booleanMax(variable, negated)
      (s1 ++ s2, lcm(d1, d2))
    // other expressions
    case _ => ???
  }
}