/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.util.Maps
import ch.ethz.inf.pm.sample.util.Math._
import ch.ethz.inf.pm.sample.util.SampleExpressions._

object QpMath {

  def prettify(expression: Expression): Expression = expression.transform {
    case original@Comparison(left, right, operator) =>
      val (left1, right1, operator1) = operator match {
        case ArithmeticOperator.> | ArithmeticOperator.>= => (right, left, ArithmeticOperator.flip(operator))
        case _ => (left, right, operator)
      }
      ???
    case other => other
  }

  def simplify(expression: Expression): Expression = {
    expression.transform(simplification)
  }

  def toNnf(expression: Expression): Expression = expression match {
    // handle negations
    case Not(argument) => argument match {
      // boolean constants and  variables
      case _: Constant |
           _: VariableIdentifier => expression
      // eliminate double negations
      case Not(nested) => nested
      // apply de morgan's rule to conjunctions and disjunctions
      case And(left, right) => Or(toNnf(Not(left)), toNnf(Not(right)))
      case Or(left, right) => And(toNnf(Not(left)), toNnf(Not(right)))
      // negate comparison operators (this also handles divisibility expressions
      case Comparison(left, right, operator) => Comparison(left, right, ArithmeticOperator.negate(operator))
    }
    // transform subexpressions
    case BinaryBooleanExpression(left, right, operator) =>
      val newLeft = toNnf(left)
      val newRight = toNnf(right)
      BinaryBooleanExpression(newLeft, newRight, operator)
    // TODO: Do we need this once we do the rewriting?
    case Max(left, right) => Max(toNnf(left), toNnf(right))
    case Min(left, right) => Min(toNnf(left), toNnf(right))
    case ConditionalExpression(condition, left, No) =>
      ConditionalExpression(toNnf(condition), toNnf(left), No)
    // default
    case _ => expression
  }

  def simplification(expression: Expression): Expression = expression match {
    // simplify boolean negations
    case original@Not(argument) => argument match {
      // constant folding
      case True => False
      case False => True
      // negate arithmetic operator
      case Comparison(left, right, operator) =>
        val negated = ArithmeticOperator.negate(operator)
        Comparison(left, right, negated)
      // eliminate double negation
      case Not(nested) => nested
      // no simplification
      case _ => original
    }
    // simplify conjunctions
    case original@And(left, right) => (left, right) match {
      // constant folding
      case (True, _) => right
      case (_, True) => left
      case (False, _) => False
      case (_, False) => False
      // default action
      case _ => original
    }
    // simplify disjunctions
    case original@Or(left, right) => (left, right) match {
      // constant folding
      case (True, _) => True
      case (_, True) => True
      case (False, _) => right
      case (_, False) => left
      // default action
      case _ => original
    }
    // simplify negations
    case original@Negate(argument) => argument match {
      // constant folding
      case Literal(v: Int) => Literal(-v)
      case Permission(n, d) => Permission(-n, d)
      // drop double negations
      case Negate(negated) => negated
      // default action
      case _ => original
    }
    // simplify additions
    case original@Plus(left, right) => (left, right) match {
      // integers
      case (Zero, _) => right
      case (_, Zero) => left
      case (Literal(v1: Int), Literal(v2: Int)) => Literal(v1 + v2)
      case (Literal(v1: Int), Plus(e, Literal(v2: Int))) => Plus(e, Literal(v1 + v2))
      case (Plus(e, Literal(v1: Int)), Literal(v2: Int)) => Plus(e, Literal(v1 + v2))
      case (Plus(e1, Literal(v1: Int)), Plus(e2, Literal(v2: Int))) => Plus(Plus(e1, e2), Literal(v1 + v2))
      // permissions
      case (No, _) => right
      case (_, No) => left
      case (Permission(n1, d1), Permission(n2, d2)) =>
        val d = lcm(d1, d2)
        val n = n1 * d / d1 + n2 * d / d2
        val f = gcd(n, d)
        Permission(n / f, d / f)
      // default action
      case _ => original
    }
    // simplify subtractions
    case original@Minus(left, right) => (left, right) match {
      // integers
      case (Literal(v1: Int), Literal(v2: Int)) => Literal(v1 - v2)
      case (Zero, _) => Negate(right)
      case (_, Zero) => left
      // permissions
      case (Permission(n1, d1), Permission(n2, d2)) =>
        val d = lcm(d1, d2)
        val n = n1 * d / d1 - n2 * d / d2
        val f = gcd(n, d)
        Permission(n / f, d / f)
      case (No, _) => Negate(right)
      case (_, No) => left
      // default action
      case _ => original
    }
    // simplify multiplications
    case original@Times(left, right) => (left, right) match {
      // integers
      case (Literal(v1: Int), Literal(v2: Int)) => Literal(v1 * v2)
      case (Zero, _) => Zero
      case (_, Zero) => Zero
      case (One, _) => right
      case (_, One) => left
      case (Literal(-1), term) => simplification(Negate(right))
      case (_, Literal(-1)) => simplification(Negate(left))
      // default action
      case _ => original
    }
    // simplify modulo
    case original@Modulo(left, right) => (left, right) match {
      // TODO: necessary? case (_, One) => Zero
      // constant folding
      case (Literal(v1: Int), Literal(v2: Int)) => Literal(v1 % v2)
      case (Times(Literal(a: Int), term), Literal(b: Int)) =>
        val divisor = gcd(a, b)
        if (b == divisor) Zero
        else if (a == divisor) Modulo(term, Literal(b / divisor))
        else Modulo(Times(Literal(a / divisor), term), Literal(b / divisor))
      // no simplification
      case _ => original
    }
    // simplify maxima
    case original@Max(MaxList(ls), MaxList(rs)) =>

      def add(element: Expression, list: List[Expression]): List[Expression] = {
        val (keep, filtered) = list.foldLeft((true, List.empty[Expression])) {
          case ((keep, partial), current) =>
            val (leq, geq) = compare(element, current)
            if (geq) (keep, partial)
            else if (leq) (false, current :: partial)
            else (keep, current :: partial)
        }
        if (keep) element :: filtered else filtered
      }

      val merged = ls.foldLeft(rs) { case (list, element) => add(element, list) }
      MaxList(merged)
    // simplify minima
    case original@Min(left, right) =>
      if (left == right) left
      else {
        val (smaller, larger) = compare(left, right)
        if (smaller) left
        else if (larger) right
        else original
      }
    // simplify comparisons
    case Comparison(Literal(v1: Int), Literal(v2: Int), operator) => operator match {
      case ArithmeticOperator.== => Literal(v1 == v2)
      case ArithmeticOperator.!= => Literal(v1 != v2)
      case ArithmeticOperator.< => Literal(v1 < v2)
      case ArithmeticOperator.<= => Literal(v1 <= v2)
      case ArithmeticOperator.> => Literal(v1 > v2)
      case ArithmeticOperator.>= => Literal(v1 >= v2)
    }
    case Comparison(Permission(n1, d1), Permission(n2, d2), operator) => operator match {
      case ArithmeticOperator.== => Literal(n1 * d2 == n2 * d1)
      case ArithmeticOperator.!= => Literal(n1 * d2 != n2 * d1)
      case ArithmeticOperator.< => Literal(n1 * d2 < n2 * d1)
      case ArithmeticOperator.<= => Literal(n1 * d2 <= n2 * d1)
      case ArithmeticOperator.> => Literal(n1 * d2 > n2 * d1)
      case ArithmeticOperator.>= => Literal(n1 * d2 >= n2 * d1)
    }
    case Comparison(left, right, operator) if left == right => operator match {
      case ArithmeticOperator.== => True
      case ArithmeticOperator.!= => False
      case ArithmeticOperator.< => False
      case ArithmeticOperator.<= => True
      case ArithmeticOperator.> => False
      case ArithmeticOperator.>= => True
    }
    // simplify conditional expressions
    case ConditionalExpression(True, term, _) => term
    case ConditionalExpression(False, _, term) => term
    case ConditionalExpression(_, left, right) if left == right => left
    case ConditionalExpression(left, ConditionalExpression(right, term, No), No) => ConditionalExpression(And(left, right), term, No)
    // default: no simplification
    case other => other
  }

  /**
    * Computes a lower and an upper bound for the given expression.
    *
    * @param expression The expression.
    * @return A tuple representing the lower and the upper bound.
    */
  private def bounds(expression: Expression): (Expression, Expression) = expression match {
    case Literal(v: Int) => (Literal(v), Literal(v))
    case Permission(n, d) => (Permission(n, d), Permission(n, d))
    case ReadParameter(variable) => (variable, variable)
    case Negate(argument) =>
      val (lowerArgument, upperArgument) = bounds(argument)
      val lower = simplification(Negate(upperArgument))
      val upper = simplification(Negate(lowerArgument))
      (lower, upper)
    case Plus(left, right) =>
      val (leftLower, leftUpper) = bounds(left)
      val (rightLower, rightUpper) = bounds(right)
      val lower = simplification(Plus(leftLower, rightLower))
      val upper = simplification(Plus(leftUpper, rightUpper))
      (lower, upper)
    case Minus(left, right) =>
      val (leftLower, leftUpper) = bounds(left)
      val (rightLower, rightUpper) = bounds(right)
      val lower = simplification(Minus(leftLower, rightUpper))
      val upper = simplification(Minus(leftUpper, rightLower))
      (lower, upper)
    case ConditionalExpression(_, left, right) =>
      val (leftLower, leftUpper) = bounds(left)
      val (rightLower, rightUpper) = bounds(right)
      val lower = if (lessEqual(leftLower, rightLower)) leftLower else rightLower
      val upper = if (lessEqual(leftUpper, rightUpper)) rightUpper else leftUpper
      (lower, upper)
    case Max(left, right) =>
      val (leftLower, leftUpper) = bounds(left)
      val (rightLower, rightUpper) = bounds(right)
      val lower = if (lessEqual(leftLower, rightLower)) leftLower else rightLower
      val upper = if (lessEqual(leftUpper, rightUpper)) rightUpper else leftUpper
      (lower, upper)
    case Min(left, right) =>
      val (leftLower, leftUpper) = bounds(left)
      val (rightLower, rightUpper) = bounds(right)
      val lower = if (lessEqual(leftLower, rightLower)) leftLower else rightLower
      val upper = if (lessEqual(leftUpper, rightUpper)) rightUpper else leftUpper
      (lower, upper)
    case _ => ???
  }

  private def compare(left: Expression, right: Expression): (Boolean, Boolean) = (left, right) match {
    case (ConditionalExpression(c1, t1, No), ConditionalExpression(c2, t2, No)) if c1 == c2 => compare(t1, t2)
    case _ =>
      if (left == right) (true, true) else {
        val (leftLower, leftUpper) = bounds(left)
        val (rightLower, rightUpper) = bounds(right)
        val leq = lessEqual(leftUpper, rightLower)
        val geq = lessEqual(rightUpper, leftLower)
        (leq, geq)
      }
  }

  private def lessEqual(left: Expression, right: Expression): Boolean = (left, right) match {
    case (Literal(v1: Int), Literal(v2: Int)) => v1 <= v2
    case (Permission(n1, d1), Permission(n2, d2)) => n1.toLong * d2 <= n2.toLong * d1
    case (Permission(n, d), ReadParameter(_)) => n * d <= 0
    case (ReadParameter(_), Permission(n, d)) => 0 < n * d
    case _ => false
  }

  case class Collected(coefficients: Map[VariableIdentifier, Int], rest: Expression) {
    def plus(other: Collected): Collected = {
      val newCoefficients = Maps.union[VariableIdentifier, Int](coefficients, other.coefficients, _ + _)
      val newRest = simplification(Plus(rest, other.rest))
      Collected(newCoefficients, newRest)
    }

    def times(other: Collected): Collected = (this, other) match {
      case (_, Collected(map, Literal(v: Int))) if map.isEmpty =>
        val newCoefficients = coefficients.mapValues(v * _)
        val newRest = simplification(Times(Literal(v), rest))
        Collected(newCoefficients, newRest)
      case (Collected(map, Literal(v: Int)), _) if map.isEmpty => other times this
      case _ => ???
    }

    def negate(): Collected = {
      val newCoefficients = coefficients.mapValues(-_)
      val newRest = simplification(Negate(rest))
      Collected(newCoefficients, newRest)
    }

    def drop(variable: VariableIdentifier): Collected = Collected(coefficients - variable, rest)

    def toExpression: Expression = {
      val filtered = coefficients.filter { case (_, coefficient) => coefficient != 0 }
      val parts = filtered.map { case (variable, coefficient) => simplification(Times(Literal(coefficient), variable)) }
      simplification(Plus(Plus(parts), rest))
    }

    def toExpression(variable: VariableIdentifier): Expression = {
      val coefficient = coefficients.getOrElse(variable, 0)
      val left = simplification(Times(Literal(coefficient), variable))
      val right = drop(variable).toExpression
      simplification(Plus(left, right))
    }
  }

  object Collected {
    def apply(expression: Expression): Collected = expression match {
      case constant: Constant => Collected(Map.empty, constant)
      case permission: FractionalPermissionExpression => Collected(Map.empty, permission)
      case variable: VariableIdentifier => Collected(Map(variable -> 1), Zero)
      case Negate(argument) => Collected(argument).negate()
      case Plus(left, right) => Collected(left) plus Collected(right)
      case Minus(left, right) => Collected(left) plus Collected(right).negate()
      case Times(left, right) => Collected(left) times Collected(right)
      case other => Collected(Map.empty, other)
    }
  }

}
