/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.IntType

/**
  * Some utility functions for quantifier elimination.
  *
  * @author Jerome Dohrau
  * @author Severin Münger
  */
object QuantifierElimination {

  import SampleExpressions._
  import Math._

  /**
    * Eliminates the quantifiers of the given expression and returns an
    * expression that does not contain a quantifier anymore but that is
    * semantically equivalent to the original expression.
    *
    * Currently, this method only handles quantifiers if they are the outermost
    * node of the expression.
    *
    * @param expression The expression.
    * @return The expression without quantifiers.
    */
  def eliminate(expression: Expression): Expression = expression match {
    // handle existential quantifier
    case Exists(variables, body) =>
      variables.foldLeft(body) {
        case (eliminated, variable) =>
          eliminate(variable, eliminated)
      }
    // reduce universal quantifier to existential quantifier
    case ForAll(variables, body) =>
      val reduced = Not(Exists(variables, Not(body)))
      eliminate(reduced)
    // negations
    case Not(argument) => Not(eliminate(argument))
    // conjunctions and disjunctions
    case And(left, right) => And(eliminate(left), eliminate(right))
    case Or(left, right) => Or(eliminate(left), eliminate(right))
    // fall through case
    case _ =>
      // check if there is a quantifier
      val quantified = expression.contains {
        case Exists(_, _) => true
        case ForAll(_, _) => true
        case _ => false
      }
      if (quantified) ???
      else expression
  }

  /**
    * The method assumes that the given variable is existentially quantified in
    * the given expression and then returns an expression does not contain the
    * quantified variable anymore but that is semantically equivalent to the
    * original expression.
    *
    * @param variable   The quantified variable to eliminate.
    * @param expression The expression.
    * @return The resulting expression.
    */
  private def eliminate(variable: VariableIdentifier, expression: Expression): Expression = {
    println(s"original: $expression")
    val nnf = toNegatedNormalForm(expression)
    println(s"nnf: $nnf")
    val collected = collectVariable(variable, nnf)
    println(s"collected: $collected")
    // TODO: Properly pick a fresh variable. Do we even need a new variable?
    val fresh = Variable("fresh", IntType)
    val normalized = normalizeCoefficient(variable, fresh, collected)
    println(s"normalized: $normalized")
    val result = constructEquivalent(fresh, normalized)
    println(s"result: $result")
    result
  }

  /**
    * Returns an expression that is in negated normal form and that is
    * semantically equivalent to the given expression. In fact, this method
    * eliminates all negations unless they are around divisibility literals.
    *
    * An expression is in negated normal form if it contains negations only as
    * leaf expression. The negated normal form is obtained by iteratively
    * applying de morgans laws and eliminating double negations.
    *
    * @param expression The expression.
    * @return The resulting expression.
    */
  private def toNegatedNormalForm(expression: Expression): Expression = expression match {
    // handle negations
    case Not(argument) => argument match {
      case BinaryBooleanExpression(left, right, operation) =>
        val newLeft = toNegatedNormalForm(Not(left))
        val newRight = toNegatedNormalForm(Not(right))
        val negated = BooleanOperator.negate(operation)
        BinaryBooleanExpression(newLeft, newRight, negated)
      case Comparison(left, right, operator) =>
        val newLeft = toNegatedNormalForm(left)
        val newRight = toNegatedNormalForm(right)
        val negated = ArithmeticOperator.negate(operator)
        Comparison(newLeft, newRight, negated)
      case Not(nested) => nested
      case original => toNegatedNormalForm(original)
    }
    // transform subexpressions
    case BinaryBooleanExpression(left, right, operator) =>
      val newLeft = toNegatedNormalForm(left)
      val newRight = toNegatedNormalForm(right)
      BinaryBooleanExpression(newLeft, newRight, operator)
    // leave unchanged
    case _ => expression
  }

  /**
    * Collects the given variable in all comparisons such that it is on the left
    * hand side and the rest on the right hand side. For instance, 2*x+3 < x+y
    * will be turned into x < y-3.
    *
    * TODO: Complete? Modulo?
    *
    * @param variable   The variable to collect.
    * @param expression The expression.
    * @return The resulting expression.
    */
  private def collectVariable(variable: VariableIdentifier, expression: Expression): Expression =
    expression.transform {
      case Comparison(left, right, operator) if left.ids.contains(variable) || right.ids.contains(variable) =>
        val collected = Collected(left) - Collected(right)
        val factor = collected.coefficients.getOrElse(variable, 0)
        val positive = if (factor >= 0) -collected else collected
        val newLeft = Times(Literal(math.abs(factor)), variable)
        val newRight = positive.drop(variable).toExpression
        val newOperator = if (factor >= 0) operator else ArithmeticOperator.negate(operator)
        simplify(Comparison(newLeft, newRight, newOperator))
      case original => original
    }

  /**
    * Normalizes the coefficients of the given variable.
    *
    * @param variable
    * @param fresh
    * @param expression
    * @return
    */
  private def normalizeCoefficient(variable: VariableIdentifier, fresh: VariableIdentifier, expression: Expression): Expression = {
    // collect all coefficients
    var coefficients = Set.empty[Int]
    expression.foreach {
      case Times(`variable`, Literal(value: Int)) => coefficients += value
      case Times(Literal(value: Int), `variable`) => coefficients += value
      case _ => // do nothing
    }
    // compute least common multiple
    val factor = lcm(coefficients)
    // normalize coefficients
    val transformed = expression.transform {
      case original@Comparison(left, right, operator) => left match {
        case `variable` =>
          Comparison(Times(Literal(factor), fresh), right, operator)
        case Times(Literal(value: Int), `variable`) =>
          Comparison(Times(Literal(factor / value), fresh), right, operator)
        case _ => original
      }
      case original => original
    }
    //
    val constraint = if (factor == 1) True else Divides(Literal(factor), fresh)
    simplify(And(transformed, constraint))
  }

  private def constructEquivalent(variable: VariableIdentifier, expression: Expression): Expression = {
    // compute projections
    val positive = positiveInfiniteProjection(variable, expression)
    val negative = negativeInfiniteProjection(variable, expression)
    // check whether there is a trivial unbounded solution
    if (positive == True || negative == True) True
    else {
      // collect factors and the sets A and B
      var factors = Set.empty[Int]
      var A = Set.empty[Expression]
      var B = Set.empty[Expression]
      expression.foreach {
        case Divides(Literal(value: Int), `variable`) => factors += value
        case Comparison(`variable`, right, operator) => operator match {
          case ArithmeticOperator.< =>
            A += Minus(right, One)
          case ArithmeticOperator.<= =>
            A += right
          case ArithmeticOperator.> =>
            B += Plus(right, One)
          case ArithmeticOperator.>= =>
            B += right
          case ArithmeticOperator.== =>
            A += right
            B += right
          case ArithmeticOperator.!= =>
            A += Minus(right, One)
            B += Plus(right, One)
          case _ => // do nothing
        }
        case Comparison(_, `variable`, _) => ???
        case _ => // do nothing
      }
      // compute delta and pick either A or B
      val delta = lcm(factors)
      val (projection, expressions) = if (A.size < B.size) (positive, A) else (negative, B)
      // disjuncts corresponding to a unbounded solution
      val unbounded = for (j <- 0 until delta) yield
        projection.transform {
          case `variable` => Literal(j)
          case other => other
        }
      // disjuncts corresponding to a bounded solution
      val bounded = for (j <- 0 until delta; b <- expressions) yield
        expression.transform {
          case `variable` => Plus(b, Literal(j))
          case other => other
        }
      // build and simplify final expression
      val result = Or(unbounded ++ bounded)
      simplify(result, collect = true)
    }
  }

  private def positiveInfiniteProjection(variable: VariableIdentifier, expression: Expression): Expression = {
    val projected = expression.transform {
      case Comparison(`variable`, _, operator) => operator match {
        case ArithmeticOperator.> | ArithmeticOperator.>= | ArithmeticOperator.!= => True
        case _ => False
      }
      case Comparison(_, `variable`, _) => ???
      case other => other
    }
    simplify(projected)
  }

  private def negativeInfiniteProjection(variable: VariableIdentifier, expression: Expression): Expression = {
    val projected = expression.transform {
      case Comparison(`variable`, _, operator) => operator match {
        case ArithmeticOperator.< | ArithmeticOperator.<= | ArithmeticOperator.!= => True
        case _ => False
      }
      case Comparison(_, `variable`, _) => ???
      case other => other
    }
    simplify(projected)
  }
}
