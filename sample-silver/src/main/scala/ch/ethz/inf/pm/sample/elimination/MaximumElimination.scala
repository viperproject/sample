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
    val bounded = for ((expression, condition) <- expressions; i <- 0 until delta) yield {
      // TODO: Do not ignore condition.
      normalized.transform {
        case `variable` => Plus(expression, Literal(i))
        case other => other
      }
    }
    // build and simplify final expression
    val maximum = Max(unbounded ++ bounded)
    simplify(maximum, collect = true)
  }

  protected def arithmeticMin(variable: VariableIdentifier, expression: Expression): (Set[(Expression, Expression)], Int) = expression match {
    // conditional expressions
    case ConditionalExpression(condition, _, _) =>
      // TODO: Left is constant.
      // TODO: Right is zero.
      val (expressions, delta) = booleanMin(variable, condition)
      (toTuples(expressions), delta)
    // additions
    case Plus(left, right) =>
      val (tuples1, delta1) = arithmeticMin(variable, left)
      val (tuples2, delta2) = arithmeticMin(variable, right)
      (tuples1 ++ tuples2, lcm(delta1, delta2))
    // subtractions
    case Minus(term, ConditionalExpression(condition, _, _)) =>
      // negate condition since the conditional appears in a negative position
      val negated = toNegatedNormalForm(Not(condition))
      val (tuples, delta1) = arithmeticMin(variable, term)
      val (expressions, delta2) = booleanMin(variable, negated)
      // TODO: remove all expressions from s2 that make "term" zero
      (tuples ++ toTuples(expressions), lcm(delta1, delta2))
    // expressions not depending on the variable
    case _ =>
      if (expression.contains(_ == variable)) ???
      else (Set.empty, 1)
  }

  protected def arithmeticMax(variable: VariableIdentifier, expression: Expression): (Set[(Expression, Expression)], Int) = expression match {
    // conditional expressions
    case ConditionalExpression(condition, _, _) =>
      val (expressions, delta) = booleanMax(variable, condition)
      (toTuples(expressions), delta)
    // additions
    case Plus(left, right) =>
      val (tuples1, delta1) = arithmeticMax(variable, left)
      val (tuples2, delta2) = arithmeticMax(variable, right)
      (tuples1 ++ tuples2, lcm(delta1, delta2))
    // subtractions
    case Minus(term, ConditionalExpression(condition, _, _)) =>
      // negate condition since the conditional appears in a negative position
      val negated = toNegatedNormalForm(Not(condition))
      val (tuples, delta1) = arithmeticMax(variable, term)
      val (expressions, delta2) = booleanMax(variable, negated)
      (tuples ++ toTuples(expressions), lcm(delta1, delta2))
    // expressions not depending on the variable
    case _ =>
      if (expression.contains(_ == variable)) ???
      else (Set.empty, 1)
  }

  @inline
  protected def toTuples(expressions: Set[Expression]): Set[(Expression, Expression)] =
    for (expression <- expressions) yield (expression, True)
}