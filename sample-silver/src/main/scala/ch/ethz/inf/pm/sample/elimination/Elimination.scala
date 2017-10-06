/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.elimination

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.util.Math.lcm
import ch.ethz.inf.pm.sample.util.SampleExpressions._

trait Elimination {

  def eliminate(expression: Expression): Expression

  protected def normalize(variable: VariableIdentifier, expression: Expression): Expression = {
    val eliminated = eliminateDivisions(expression)
    val nnf = toNegatedNormalForm(eliminated)
    val collected = collectVariable(variable, nnf)
    normalizeCoefficient(variable, collected)
  }

  /**
    * Returns an expression that is semantically equivalent to the given
    * expression but ideally does not contain any divisions anymore. Currently
    * this method recursively multiplies out divisions by constants appearing on
    * either side of a comparison.
    *
    * @param expression The expression to process.
    * @return The resulting expression.
    */
  protected def eliminateDivisions(expression: Expression): Expression = {
    // helper function that determines by what factor the expression needs to
    // be multiplied in order to eliminate all divisions
    def quotient(expression: Expression): Int = expression match {
      case Negate(argument) => quotient(argument)
      case Divide(term, Literal(i: Int)) => i * quotient(term)
      case Operation(left, right, _) => lcm(quotient(left), quotient(right))
      case _ => 1
    }

    // multiplies the given expression by the given factor in order to eliminate
    // all divisions
    def multiply(expression: Expression, factor: Int): Expression =
      if (factor == 1) expression
      else expression match {
        case Negate(argument) => Negate(multiply(argument, factor))
        case Divide(term, Literal(i: Int)) => multiply(term, factor / i)
        case Operation(left, right, operator) => Operation(multiply(left, factor), multiply(right, factor), operator)
        case _ => Times(Literal(factor), expression)
      }

    expression.transform {
      case Comparison(left, right, operator) =>
        val factor = lcm(quotient(left), quotient(right))
        val newLeft = multiply(left, factor)
        val newRight = multiply(right, factor)
        Comparison(newLeft, newRight, operator)
      case other => other
    }
  }

  /**
    * Returns an expression that is semantically equivalent to the given
    * expression and in negated normal form. In fact, this method eliminates
    * all negations unless they are around literals that are not comparisons.
    *
    * @param expression The expression to transform into negated normal form.
    * @return The expression in negated normal form.
    */
  protected def toNegatedNormalForm(expression: Expression): Expression = expression match {
    // handle negations
    case Not(argument) => argument match {
      // apply de morgan's rule to conjuncts and disjuncts
      case BinaryBooleanExpression(left, right, operation) =>
        val newLeft = toNegatedNormalForm(Not(left))
        val newRight = toNegatedNormalForm(Not(right))
        val newOperator = BooleanOperator.negate(operation)
        BinaryBooleanExpression(newLeft, newRight, newOperator)
      // negate comparison operators (this also handles divisibility expressions)
      case Comparison(left, right, operator) =>
        val newOperator = ArithmeticOperator.negate(operator)
        Comparison(left, right, newOperator)
      // eliminate double negations
      case Not(nested) => nested
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
    * @param variable   The variable to collect.
    * @param expression The expression.
    * @return The resulting expression.
    */
  protected def collectVariable(variable: VariableIdentifier, expression: Expression): Expression = expression.transform {
    case Divides(left, right) => Divides(left, Collected.collect(variable, right))
    case NotDivides(left, right) => NotDivides(left, Collected.collect(variable, right))
    case Comparison(left, right, operator) if left.contains(_ == variable) || right.contains(_ == variable) =>
      val collected = Collected(left) - Collected(right)
      val factor = collected.coefficients.getOrElse(variable, 0)
      val positive = if (factor >= 0) -collected else collected
      val newLeft = Times(Literal(math.abs(factor)), variable)
      val newRight = positive.drop(variable).toExpression
      val newOperator = if (factor >= 0) operator else ArithmeticOperator.flip(operator)
      simplify(Comparison(newLeft, newRight, newOperator))
    case other => other
  }


  /**
    * Normalizes the coefficients of the given variable.
    *
    * @param variable
    * @param expression
    * @return
    */
  protected def normalizeCoefficient(variable: VariableIdentifier, expression: Expression): Expression = {
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
          Comparison(variable, Times(Literal(factor), right), operator)
        case Times(Literal(value: Int), `variable`) =>
          Comparison(variable, Times(Literal(factor / value), right), operator)
        case _ => original
      }
      case original => original
    }
    //
    val constraint = if (factor == 1) True else Divides(Literal(factor), variable)
    simplify(And(transformed, constraint))
  }

  /**
    * The first return value is the set of so-called interesting expressions.
    *
    * Depending on whether we are interested in a smallest or a largest
    * solution, the second return value is the negative or positive infinite
    * projection of the expression. The negative infinite projection of an
    * expression E with respect to a variable x is an expression E' that is
    * equivalent to E for sufficiently small values of x. Analogously, the
    * positive infinite projection E' is equivalent to E for sufficiently large
    * values of x.
    *
    * The third return value is the delta that indicates the least common
    * multiple of all values appearing in divisibility expressions.
    *
    * @param variable   The variable to eliminate.
    * @param expression The expression.
    * @param smallest   The flag indicating whether we are interested in a
    *                   smallest or largest solution.
    * @return
    */
  protected def analyzeBoolean(variable: VariableIdentifier, expression: Expression, smallest: Boolean): (Set[Expression], Expression, Int) = expression match {
    // divisibility expressions
    case Divides(Literal(value: Int), _) => (Set.empty, expression, value)
    case NotDivides(Literal(value: Int), _) => (Set.empty, expression, value)
    // comparison expressions
    case Comparison(`variable`, term, operator) =>
      if (smallest) operator match {
        case ArithmeticOperator.== => (Set(term), False, 1)
        case ArithmeticOperator.!= => (Set(Plus(term, One)), True, 1)
        case ArithmeticOperator.< => (Set.empty, True, 1)
        case ArithmeticOperator.<= => (Set.empty, True, 1)
        case ArithmeticOperator.> => (Set(Plus(term, One)), False, 1)
        case ArithmeticOperator.>= => (Set(term), False, 1)
      } else operator match {
        case ArithmeticOperator.== => (Set(term), False, 1)
        case ArithmeticOperator.!= => (Set(Minus(term, One)), True, 1)
        case ArithmeticOperator.< => (Set(Minus(term, One)), False, 1)
        case ArithmeticOperator.<= => (Set(term), False, 1)
        case ArithmeticOperator.> => (Set.empty, True, 1)
        case ArithmeticOperator.>= => (Set.empty, True, 1)
      }
    // conjunctions and disjunctions
    case BinaryBooleanExpression(left, right, operator) =>
      val (set1, projection1, delta1) = analyzeBoolean(variable, left, smallest)
      val (set2, projection2, delta2) = analyzeBoolean(variable, right, smallest)
      val set = set1 ++ set2
      val projection = BinaryBooleanExpression(projection1, projection2, operator)
      val delta = lcm(delta1, delta2)
      (set, projection, delta)
    // expressions not depending on the variable to eliminate.
    case _ =>
      if (expression.contains(_ == variable)) ???
      else (Set.empty, expression, 1)
  }
}

