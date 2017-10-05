/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.elimination

import ch.ethz.inf.pm.sample.abstractdomain.{Exists, Expression, ForAll, VariableIdentifier}
import ch.ethz.inf.pm.sample.util.SampleExpressions

/**
  * Some utility functions to eliminate quantifiers.
  *
  * @author Jerome Dohrau
  * @author Severin Münger
  */
object QuantifierElimination
  extends Elimination {

  import SampleExpressions._

  override def eliminate(expression: Expression): Expression = expression match {
    case Exists(variables, body) =>
      variables.foldLeft(body) {
        case (eliminated, variable) =>
          eliminateExistential(variable, eliminated)
      }
    case ForAll(variables, body) => eliminate(Not(Exists(variables, Not(body))))
    case Not(argument) => Not(eliminate(argument))
    case And(left, right) => And(eliminate(left), eliminate(right))
    case Or(left, right) => Or(eliminate(left), eliminate(right))
  }

  private def eliminateExistential(variable: VariableIdentifier, expression: Expression): Expression =
    if (expression.contains(_ == variable)) {
      // normalize expression
      val normalized = normalize(variable, expression)
      // compute projections, set of interesting expressions and delta
      val (minExpressions, negative, minDelta) = analyzeBoolean(variable, normalized, smallest = true)
      val (maxExpressions, positive, maxDelta) = analyzeBoolean(variable, normalized, smallest = false)
      // check whether there is a trivial unbounded solution
      if (negative == True || positive == True) True
      else {
        // pick smaller set of expressions and the corresponding delta and projection
        val (expressions, delta, projection) = if (minExpressions.size < maxExpressions.size) {
          (minExpressions, minDelta, negative)
        } else {
          (maxExpressions, maxDelta, positive)
        }
        // compute disjuncts corresponding to unbounded solutions
        val unbounded = for (i <- 0 until delta) yield
          projection.transform {
            case `variable` => Literal(i)
            case other => other
          }
        // compute disjuncts corresponding to bounded solutions
        val bounded = for (e <- expressions; i <- 0 until delta) yield
          normalized.transform {
            case `variable` => Plus(e, Literal(i))
            case other => other
          }
        // build and simplify final expression
        val disjunction = Or(unbounded ++ bounded)
        simplify(disjunction, collect = true)
      }
    } else expression
}
