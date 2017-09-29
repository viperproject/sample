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

  type Tuples = Set[(Expression, Expression)]

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
    // TODO: currently we force the algorithm to select the min expressions
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
      val result = normalized.transform {
        case `variable` => Plus(expression, Literal(i))
        case constraint@Comparison(_, _, _) =>
          // TODO: More aggressive optimisation?
          val combined = simplify(And(constraint, condition))
          combined match {
            case False => False
            case _ => constraint
          }
        case other => other
      }
      result
    }
    // build and simplify final expression
    val maximum = Max(unbounded ++ bounded)
    simplify(maximum, collect = true)
  }

  protected def arithmeticMin(variable: VariableIdentifier, expression: Expression): (Tuples, Int) = expression match {
    // conditional expressions
    case ConditionalExpression(condition, term, No) =>
      val (tuples, delta) = arithmeticMin(variable, term)
      conditionalMin(variable, condition, tuples, delta)
    // additions
    case Plus(left, right) =>
      val (tuples1, delta1) = arithmeticMin(variable, left)
      val (tuples2, delta2) = arithmeticMin(variable, right)
      (tuples1 ++ tuples2, lcm(delta1, delta2))
    // subtractions
    case Minus(term, ConditionalExpression(condition, _, No)) =>
      // TODO: left of conditional is constant.
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

  protected def conditionalMin(variable: VariableIdentifier, condition: Expression, tuples0: Tuples, delta0: Int): (Tuples, Int) = condition match {
    case And(left, right) =>
      val (tuples1, delta1) = conditionalMin(variable, left, tuples0, delta0)
      val (tuples2, delta2) = conditionalMin(variable, right, tuples1, delta1)
      (tuples2, delta2)
    case Or(left, right) =>
      val (tuples1, delta1) = conditionalMin(variable, left, tuples0, delta0)
      val (tuples2, delta2) = conditionalMin(variable, right, tuples0, delta0)
      (tuples1 ++ tuples2, lcm(delta1, delta2))
    case _ =>
      val (expressions, delta1) = booleanMin(variable, condition)
      // TODO: Handle unsoundness
      if (tuples0.isEmpty) (toTuples(expressions), lcm(delta0, delta1))
      else if (expressions.isEmpty) (tuples0, lcm(delta0, delta1))
      else {
        val negated = Not(condition)
        val parts = for ((expression, _) <- tuples0) yield negated.transform {
          case `variable` => expression
          case other => other
        }
        val filter = Or(parts)
        val tuples1 = toTuples(expressions, filter)
        (tuples0 ++ tuples1, lcm(delta0, delta1))
      }
  }

  protected def arithmeticMax(variable: VariableIdentifier, expression: Expression): (Tuples, Int) = expression match {
    // conditional expressions
    case ConditionalExpression(condition, term, No) =>
      val (tuples, delta) = arithmeticMax(variable, term)
      conditionalMax(variable, condition, tuples, delta)
    // additions
    case Plus(left, right) =>
      val (tuples1, delta1) = arithmeticMax(variable, left)
      val (tuples2, delta2) = arithmeticMax(variable, right)
      (tuples1 ++ tuples2, lcm(delta1, delta2))
    // subtractions
    case Minus(term, ConditionalExpression(condition, _, No)) =>
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

  protected def conditionalMax(variable: VariableIdentifier, condition: Expression, tuples0: Tuples, delta0: Int): (Tuples, Int) = condition match {
    case And(left, right) =>
      val (tuples1, delta1) = conditionalMax(variable, left, tuples0, delta0)
      val (tuples2, delta2) = conditionalMax(variable, right, tuples1, delta1)
      (tuples2, delta2)
    case Or(left, right) =>
      val (tuples1, delta1) = conditionalMax(variable, left, tuples0, delta0)
      val (tuples2, delta2) = conditionalMax(variable, right, tuples0, delta0)
      (tuples1 ++ tuples2, lcm(delta1, delta2))
    case _ =>
      val (expressions, delta1) = booleanMax(variable, condition)
      // TODO: Handle unsoundness
      if (tuples0.isEmpty) (toTuples(expressions), lcm(delta0, delta1))
      else if (expressions.isEmpty) (tuples0, lcm(delta0, delta1))
      else {
        val negated = Not(condition)
        val parts = for ((expression, _) <- tuples0) yield negated.transform {
          case `variable` => expression
          case other => other
        }
        val filter = Or(parts)
        val tuples1 = toTuples(expressions, filter)
        (tuples0 ++ tuples1, lcm(delta0, delta1))
      }
  }

  @inline
  protected def toTuples(expressions: Set[Expression], condition: Expression = True): Tuples =
    for (expression <- expressions) yield (expression, condition)
}