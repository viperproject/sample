/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.elimination

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Context
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

  private def eliminateMaximum(variable: VariableIdentifier, expression: Expression): Expression =
    if (expression.contains(_ == variable)) {
      println(s"max $variable :: $expression")
      // normalize expression
      val normalized = normalize(variable, expression)
      // compute projections, set of interesting expressions and delta
      val (expressions, projection, delta) = analyzeArithmetic(variable, normalized, smallest = true)
      // compute maximum corresponding to unbounded solutions
      val unbounded = for (i <- 0 until delta) yield
        projection.transform {
          case `variable` => Literal(i)
          case other => other
        }
      // compute maximum corresponding to bounded solutions
      var count = 0
      val bounded = for ((expression, condition) <- expressions; i <- 0 until delta) yield {
        count = count + 1
        println(s"count: $count")
        println(s"($expression, $condition), delta=$i")
        val quantified = Context.getQuantified(condition)
        val result = normalized.transform {
          case `variable` => Plus(expression, Literal(i))
          case constraint@Comparison(_, _, _) =>
            // TODO: More aggressive optimisation?
            val formula = simplify(Exists(quantified, And(constraint, condition)))
            val eliminated = QuantifierElimination.eliminate(formula)
            eliminated match {
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
    } else expression

  override protected def normalizeCoefficient(variable: VariableIdentifier, expression: Expression): Expression = expression match {
    case ConditionalExpression(condition, term, ignore@(Zero | No)) =>
      val normalized = normalizeCoefficient(variable, condition)
      ConditionalExpression(normalized, term, ignore)
    case _ => super.normalizeCoefficient(variable, expression)
  }

  protected def analyzeArithmetic(variable: VariableIdentifier, expression: Expression, smallest: Boolean): (Tuples, Expression, Int) = expression match {
    // conditional expression
    case ConditionalExpression(condition, term, ignore@(Zero | No)) => condition match {
      case And(left, right) =>
        // rewrite to: left ? (right ? term : ignore) : ignore
        val partial = ConditionalExpression(right, term, ignore)
        val rewritten = ConditionalExpression(left, partial, ignore)
        analyzeArithmetic(variable, rewritten, smallest)
      case Or(left, right) =>
        // rewrite to: max(left ? term : ignore, right ? term : ignore)
        val conditional1 = ConditionalExpression(left, term, ignore)
        val conditional2 = ConditionalExpression(right, term, ignore)
        val rewritten = Max(conditional1, conditional2)
        analyzeArithmetic(variable, rewritten, smallest)
      case _ =>
        // analyze term and condition
        val (tuples2, pp, delta2) = analyzeArithmetic(variable, term, smallest)
        val (set, projection1, delta1) = analyzeBoolean(variable, condition, smallest)
        // check whether it is sound to optimize
        // TODO: Fix this.
        val transformed = pp.transform {
          case Divides(Literal(value: Int), `variable`) => False
          case NotDivides(Literal(value: Int), `variable`) => False
          case other => other
        }
        val simplified = simplify(transformed)
        val optimize = simplified match {
          case `ignore` => true
          case _ => false
        }
        // add conditions used to optimize
        val filter = if (optimize) {
          val negated = Not(condition)
          val parts = for ((expression, _) <- tuples2) yield negated.transform {
            case `variable` => expression
            case other => other
          }
          Ors(parts)
        } else True
        val tuples1 = toTuples(set, filter)
        // return result
        val tuples = tuples1 ++ tuples2
        val projection = ConditionalExpression(projection1, pp, ignore)
        val delta = lcm(delta1, delta2)
        (tuples, projection, delta)
    }
    // maximum
    case Max(left, right) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left, smallest)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right, smallest)
      val tuples = tuples1 ++ tuples2
      val projection = Max(projection1, projection2)
      val delta = lcm(delta1, delta2)
      (tuples, projection, delta)
    // additions
    case Plus(left, right) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left, smallest)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right, smallest)
      val tuples = tuples1 ++ tuples2
      val projection = Plus(projection1, projection2)
      val delta = lcm(delta1, delta2)
      (tuples, projection, delta)
    // subtractions
    case Minus(term, conditional@ConditionalExpression(condition, _, Zero | No)) =>
      // TODO: left of conditional is constant
      // TODO: Remove expressions from set2 that make term zero
      // negate condition since the conditional appears in a negative position
      val negated = toNegatedNormalForm(Not(condition))
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, term, smallest)
      val (set2, projection2, delta2) = analyzeBoolean(variable, negated, smallest)
      val tuples = tuples1 ++ toTuples(set2)
      val projection = Minus(projection1, ConditionalExpression(projection2, conditional.left, conditional.right))
      val delta = lcm(delta1, delta2)
      (tuples, projection, delta)
    // expressions not depending on the variable
    case _ =>
      if (expression.contains(_ == variable)) ???
      else (Set.empty, expression, 1)
  }

  @inline
  protected def toTuples(expressions: Set[Expression], condition: Expression = True): Tuples =
    for (expression <- expressions) yield (expression, condition)
}
