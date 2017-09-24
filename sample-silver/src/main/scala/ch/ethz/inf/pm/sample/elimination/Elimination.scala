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
    val nnf = toNegatedNormalForm(expression)
    val collected = collectVariable(variable, nnf)
    normalizeCoefficient(variable, collected)
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
  protected def collectVariable(variable: VariableIdentifier, expression: Expression): Expression =
    expression.transform {
      case original@Divides(_, _) => original
      case original@NotDivides(_, _) => original
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

  protected def booleanMin(variable: VariableIdentifier, expression: Expression): (Set[Expression], Int) = expression match {
    // divisibility expressions
    case Divides(Literal(value: Int), `variable`) => (Set.empty, value)
    case NotDivides(Literal(value: Int), `variable`) => (Set.empty, value)
    // comparison expressions
    case Comparison(`variable`, term, operator) => operator match {
      case ArithmeticOperator.== => (Set(term), 1)
      case ArithmeticOperator.!= => (Set(Minus(term, One)), 1)
      case ArithmeticOperator.< => (Set(Minus(term, One)), 1)
      case ArithmeticOperator.<= => (Set(term), 1)
      case ArithmeticOperator.> => (Set.empty, 1)
      case ArithmeticOperator.>= => (Set.empty, 1)
    }
    // conjunctions and disjunctions
    case BinaryBooleanExpression(left, right, _) =>
      val (leftSet, leftDelta) = booleanMin(variable, left)
      val (rightSet, rightDelta) = booleanMin(variable, right)
      (leftSet ++ rightSet, lcm(leftDelta, rightDelta))
    // expressions not depending on the variable
    case _ =>
      if (expression.contains(_ == variable)) ???
      else (Set.empty, 1)
  }

  protected def booleanMax(variable: VariableIdentifier, expression: Expression): (Set[Expression], Int) = expression match {
    // divisibility expressions
    case Divides(Literal(value: Int), `variable`) => (Set.empty, value)
    case NotDivides(Literal(value: Int), `variable`) => (Set.empty, value)
    // comparison expressions
    case Comparison(`variable`, term, operator) => operator match {
      case ArithmeticOperator.== => (Set(term), 1)
      case ArithmeticOperator.!= => (Set(Plus(term, One)), 1)
      case ArithmeticOperator.< => (Set.empty, 1)
      case ArithmeticOperator.<= => (Set.empty, 1)
      case ArithmeticOperator.> => (Set(Plus(term, One)), 1)
      case ArithmeticOperator.>= => (Set(term), 1)
    }
    // conjunctions and disjunctions
    case BinaryBooleanExpression(left, right, _) =>
      val (leftSet, leftDelta) = booleanMax(variable, left)
      val (rightSet, rightDelta) = booleanMax(variable, right)
      (leftSet ++ rightSet, lcm(leftDelta, rightDelta))
    // expressions not depending on the variable
    case _ =>
      if (expression.contains(_ == variable)) ???
      else (Set.empty, 1)
  }

  /**
    * Returns the negative infinite projection of the given expression with
    * respect to the given variable.
    *
    * The negative infinite projection of an expression E with respect to a
    * variable x is an expression E' that is equivalent to E for sufficiently
    * small values of x.
    *
    * @param variable   The variable.
    * @param expression The expression.
    * @return The negative infinite projection of the expression.
    */
  protected def negativeInfiniteProjection(variable: VariableIdentifier, expression: Expression): Expression = {
    // projected expression
    val projected = expression.transform {
      // project comparisons
      case Comparison(`variable`, _, operator) => operator match {
        case ArithmeticOperator.< | ArithmeticOperator.<= | ArithmeticOperator.!= => True
        case _ => False
      }
      case Comparison(_, `variable`, operator) => operator match {
        case ArithmeticOperator.> | ArithmeticOperator.>= | ArithmeticOperator.!= => True
        case _ => False
      }
      // leave rest unchanged
      case other => other
    }
    // simplify projected expression in order to fold the constants that were
    // introduced by the projection
    simplify(projected)
  }

  /**
    * Returns the positive infinite projection of the given expression with
    * respect to the given variable.
    *
    * The positive infinite projection of an expression E with respect to a
    * variable x is an expression E' that is equivalent to E for sufficiently
    * large values of x.
    *
    * @param variable   The variable.
    * @param expression The expression.
    * @return The positive infinite projection of the expression.
    */
  protected def positiveInfiniteProjection(variable: VariableIdentifier, expression: Expression): Expression = {
    // project expression
    val projected = expression.transform {
      // project comparisons
      case Comparison(`variable`, _, operator) => operator match {
        case ArithmeticOperator.> | ArithmeticOperator.>= | ArithmeticOperator.!= => True
        case _ => False
      }
      case Comparison(_, `variable`, operator) => operator match {
        case ArithmeticOperator.< | ArithmeticOperator.<= | ArithmeticOperator.!= => True
        case _ => False
      }
      // leave rest unchanged
      case other => other
    }
    // simplify projection expression in order to fold the constants that were
    // introduced by the projection
    simplify(projected)
  }
}

