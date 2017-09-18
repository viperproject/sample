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
    * The method assumes that the given variable is existentially quantified in
    * the given expression and then returns an expression does not contain the
    * quantified variable anymore but that is semantically equivalent to the
    * original expression.
    *
    * @param variable   The quantified variable to eliminate.
    * @param expression The expression.
    * @return The resulting expression.
    */
  def eliminate(variable: VariableIdentifier, expression: Expression): Expression = {
    println(s"original: $expression")
    val nnf = toNegatedNormalForm(expression)
    println(s"nnf: $nnf")
    val collected = collectVariable(variable, nnf)
    println(s"collected: $collected")
    val normalized = normalizeCoefficient(variable, collected)
    println(s"normalized: $normalized")
    val fresh = Variable("fresh", IntType)
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
  def toNegatedNormalForm(expression: Expression): Expression = expression match {
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
    * TODO: Complete? Modulo?
    *
    * @param variable
    * @param expression
    * @return
    */
  def collectVariable(variable: VariableIdentifier, expression: Expression): Expression =
    expression.transform {
      case BinaryArithmeticExpression(left, right, operator) if left.ids.contains(variable) || right.ids.contains(variable) =>
        val collected = collect(left) - collect(right)
        val factor = collected.coefficients.getOrElse(variable, 0)
        val positive = if (factor >= 0) -collected else collected
        val newLeft = Times(Literal(math.abs(factor)), variable)
        val newRight = positive.drop(variable).toExpression
        simplify(Comparison(newLeft, newRight, operator))
      case original => original
    }

  def normalizeCoefficient(variable: VariableIdentifier, expression: Expression): Expression = {

    var factors = Set.empty[Int]
    expression.foreach {
      case Times(`variable`, Literal(value: Int)) => factors += value
      case Times(Literal(value: Int), `variable`) => factors += value
      case _ => // do nothing
    }

    val factor = lcm(factors)
    println(factor)

    val fresh = Variable("fresh", IntType)

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

    val constraint = Divides(Literal(factor), fresh)
    simplify(And(transformed, constraint))
  }

  def constructEquivalent(variable: VariableIdentifier, expression: Expression): Expression = {
    // compute projections
    val positive = positiveInfiniteProjection(variable, expression)
    val negative = negativeInfiniteProjection(variable, expression)

    if (positive == True || negative == True) True
    else {

      var deltas = Set.empty[Int]
      var A = Set.empty[Expression]
      var B = Set.empty[Expression]

      expression.foreach {
        case Divides(Literal(value: Int), `variable`) => deltas += value
        case Comparison(`variable`, right, operator) => operator match {
          case ArithmeticOperator.> =>
            A += right
          case ArithmeticOperator.>= =>
            A += Plus(right, One)
          case ArithmeticOperator.< =>
            B += right
          case ArithmeticOperator.>= =>
            B += Minus(right, One)
          case ArithmeticOperator.== =>
            A += Minus(right, One)
            B += Plus(right, One)
          case ArithmeticOperator.!= =>
            A += right
            B += right
          case _ => // do nothing
        }
        case Comparison(_, `variable`, _) => ???
        case _ => // do nothing
      }

      val delta = lcm(deltas)


      val (projection, expressions) = if (A.size < B.size) {
        (positive, A)
      } else {
        (negative, B)
      }

      val xs = for (j <- 1 to delta) yield
        projection.transform {
          case `variable` => Literal(j)
          case other => other
        }

      val ys = for (j <- 1 to delta; b <- expressions) yield
        expression.transform {
          case `variable` => Plus(b, Literal(j))
          case other => other
        }

      val result = simplify(Or(xs ++ ys))


      result
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

  def collect(expression: Expression): Collected = expression match {
    case Plus(left, right) => collect(left) + collect(right)
    case Minus(left, right) => collect(left) - collect(right)
    case Times(left, right) =>
      val Collected(leftCoefficients, leftRest) = collect(left)
      val Collected(rightCoefficients, rightRest) = collect(right)
      if (leftCoefficients.isEmpty && leftRest.isInstanceOf[Constant]) {
        val Literal(value: Int) = leftRest
        val coefficients = rightCoefficients.mapValues(value * _)
        val rest = simplify(Times(leftRest, rightRest))
        Collected(coefficients, rest)
      } else if (rightCoefficients.isEmpty && rightRest.isInstanceOf[Constant]) {
        val Literal(value: Int) = rightRest
        val coefficients = leftCoefficients.mapValues(value * _)
        val rest = simplify(Times(leftRest, rightRest))
        Collected(coefficients, rest)
      } else {
        // we cannot handle non-linear stuff
        ???
      }
    case variable: VariableIdentifier => Collected(Map(variable -> 1), Zero)
    case constant: Constant => Collected(Map.empty, constant)
    case permission: FractionalPermissionExpression => Collected(Map.empty, permission)
  }

  /**
    * Represents the sum of the rest and all variables contained in the map
    * multiplied with their corresponding coefficients.
    *
    * @param coefficients The coefficient map.
    * @param rest         The rest.
    */
   case class Collected(coefficients: Map[VariableIdentifier, Int], rest: Expression) {
    def +(other: Collected): Collected = {
      val newCoefficients = Maps.union[VariableIdentifier, Int](coefficients, other.coefficients, _ + _)
      val newRest = simplify(Plus(rest, other.rest))
      Collected(newCoefficients, newRest)
    }

    def -(other: Collected): Collected = this + -other

    def unary_+(): Collected = this

    def unary_-(): Collected = {
      val newCoefficients = coefficients.mapValues(-_)
      val newRest = simplify(Negate(rest))
      Collected(newCoefficients, newRest)
    }

    def drop(variable: VariableIdentifier): Collected = {
      val newCoefficient = coefficients - variable
      Collected(newCoefficient, rest)
    }

    def toExpression: Expression = {
      val filtered = coefficients.filter { case (_, coefficient) => coefficient != 0 }
      val parts = filtered.map { case (variable, coefficient) => Times(Literal(coefficient), variable) }
      simplify(Plus(Plus(parts), rest))
    }
  }

}
