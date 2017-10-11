/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.silver.{BoolType, IntType}

/**
  * Some utility functions for sample expressions.
  *
  * @author Jerome Dohrau
  */
object SampleExpressions {

  import Math._

  /**
    * An expression representing a true constant.
    */
  val True: Constant = Literal(true)

  /**
    * An expression representing a false constant.
    */
  val False: Constant = Literal(false)

  /**
    * An expression representing an integer constant with the value zero.
    */
  val Zero: Constant = Literal(0)

  /**
    * An expression representing an integer constant with the value one.
    */
  val One: Constant = Literal(1)

  /**
    * An expression representing an integer constant with the value three.
    */
  val Two: Constant = Literal(2)

  /**
    * An expression representing an integer constant with the value three.
    */
  val Three: Constant = Literal(3)

  /**
    * An expression representing an integer constant with the value four.
    */
  val Four: Constant = Literal(4)

  /**
    * An expression representing no permission.
    */
  val No: FractionalPermissionExpression = FractionalPermissionExpression(Zero, One)

  /**
    * An expression representing full permission.
    */
  val Full: FractionalPermissionExpression = FractionalPermissionExpression(One, One)

  object Literal {
    def apply(value: Int): Constant =
      Constant(value.toString, IntType)

    def apply(value: Boolean): Constant =
      Constant(value.toString, BoolType)

    def unapply(argument: Expression): Option[Any] = argument match {
      case Constant(value, IntType, _) => Some(value.toInt)
      case Constant(value, BoolType, _) => Some(value.toBoolean)
      case _ => None
    }
  }

  object Variable {
    def apply(name: String, typ: Type): VariableIdentifier =
      VariableIdentifier(name)(typ)

    def unapply(argument: Expression): Option[(String, Type)] = argument match {
      case variable@VariableIdentifier(name, _) => Some(name, variable.typ)
      case _ => None
    }
  }

  /* ---------------------------------------------------------------------------
   * Arithmetic Operations
   */

  object Negate {
    def apply(expression: Expression): Expression =
      UnaryArithmeticExpression(expression, ArithmeticOperator.-, expression.typ)

    def unapply(argument: Expression): Option[Expression] = argument match {
      case UnaryArithmeticExpression(expression, ArithmeticOperator.-, _) => Some(expression)
      case _ => None
    }
  }

  object Plus {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Operation(left, right, ArithmeticOperator.+)

    def apply(expressions: Iterable[Expression]): Expression =
      if (expressions.isEmpty) Zero
      else expressions.reduce((left, right) => Plus(left, right))

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Operation(left, right, ArithmeticOperator.+) => Some(left, right)
      case _ => None
    }
  }

  object Minus {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Operation(left, right, ArithmeticOperator.-)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Operation(left, right, ArithmeticOperator.-) => Some(left, right)
      case _ => None
    }
  }

  object Times {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Operation(left, right, ArithmeticOperator.*)

    def apply(expressions: Iterable[Expression]): Expression =
      if (expressions.isEmpty) One
      else expressions.reduce((left, right) => Times(left, right))

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Operation(left, right, ArithmeticOperator.*) => Some(left, right)
      case _ => None
    }
  }

  object Divide {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Operation(left, right, ArithmeticOperator./)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Operation(left, right, ArithmeticOperator./) => Some(left, right)
      case _ => None
    }
  }

  object Modulo {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Operation(left, right, ArithmeticOperator.%)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Operation(left, right, ArithmeticOperator.%) => Some(left, right)
      case _ => None
    }
  }

  object Operation {
    def apply(left: Expression, right: Expression, operator: ArithmeticOperator.Value): BinaryArithmeticExpression =
      BinaryArithmeticExpression(left, right, operator)

    def unapply(argument: Expression): Option[(Expression, Expression, ArithmeticOperator.Value)] = argument match {
      case BinaryArithmeticExpression(left, right, operator) if ArithmeticOperator.isArithmetic(operator) => Some(left, right, operator)
      case _ => None
    }
  }

  /* ---------------------------------------------------------------------------
   * Arithmetic Comparisons
   */

  object Equal {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Comparison(left, right, ArithmeticOperator.==)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Comparison(left, right, ArithmeticOperator.==) => Some(left, right)
      case _ => None
    }
  }

  object NotEqual {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Comparison(left, right, ArithmeticOperator.!=)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Comparison(left, right, ArithmeticOperator.!=) => Some(left, right)
      case _ => None
    }
  }

  object Less {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Comparison(left, right, ArithmeticOperator.<)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Comparison(left, right, ArithmeticOperator.<) => Some(left, right)
      case _ => None
    }
  }

  object LessEqual {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Comparison(left, right, ArithmeticOperator.<=)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Comparison(left, right, ArithmeticOperator.<=) => Some(left, right)
      case _ => None
    }
  }

  object Greater {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Comparison(left, right, ArithmeticOperator.>)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Comparison(left, right, ArithmeticOperator.>) => Some(left, right)
      case _ => None
    }
  }

  object GreaterEqual {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Comparison(left, right, ArithmeticOperator.>=)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Comparison(left, right, ArithmeticOperator.>=) => Some(left, right)
      case _ => None
    }
  }

  object Comparison {
    def apply(left: Expression, right: Expression, operator: ArithmeticOperator.Value): BinaryArithmeticExpression =
      BinaryArithmeticExpression(left, right, operator)

    def unapply(argument: Expression): Option[(Expression, Expression, ArithmeticOperator.Value)] = argument match {
      case BinaryArithmeticExpression(left, right, operator) if ArithmeticOperator.isComparison(operator) => Some(left, right, operator)
      case _ => None
    }
  }

  /* ---------------------------------------------------------------------------
   * Boolean Expressions
   */

  object Not {
    def apply(expression: Expression): NegatedBooleanExpression =
      NegatedBooleanExpression(expression)

    def unapply(argument: Expression): Option[Expression] = argument match {
      case NegatedBooleanExpression(expression) => Some(expression)
      case _ => None
    }
  }

  object And {
    def apply(left: Expression, right: Expression): BinaryBooleanExpression =
      BinaryBooleanExpression(left, right, BooleanOperator.&&)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&) => Some(left, right)
      case _ => None
    }
  }

  object AndList {
    def apply(expressions: Iterable[Expression]): Expression =
      if (expressions.isEmpty) True
      else expressions.reduce((left, right) => And(left, right))

    def unapply(argument: Expression): Option[List[Expression]] =  argument match {
      case And(AndList(left), AndList(right)) => Some(left ++ right)
      case _ if argument.typ.isBooleanType => Some(List(argument))
      case _ => None
    }
  }

  object Or {
    def apply(left: Expression, right: Expression): BinaryBooleanExpression =
      BinaryBooleanExpression(left, right, BooleanOperator.||)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case BinaryBooleanExpression(left, right, BooleanOperator.||) => Some(left, right)
      case _ => None
    }
  }

  object OrList {
    def apply(expressions: Iterable[Expression]): Expression =
      if (expressions.isEmpty) False
      else expressions.reduce((left, right) => Or(left, right))

    def unapply(argument: Expression): Option[List[Expression]] = argument match {
      case Or(OrList(left), OrList(right)) => Some(left ++ right)
      case _ if argument.typ.isBooleanType => Some(List(argument))
      case _ => None
    }
  }

  object Implies {
    def apply(left: Expression, right: Expression): BinaryBooleanExpression =
      Or(Not(left), right)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Or(Not(left), right) => Some(left, right)
      case _ => None
    }
  }

  object Divides {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      Equal(Modulo(right, left), Zero)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case Equal(Modulo(right, left), Zero) => Some(left, right)
      case _ => None
    }
  }

  object NotDivides {
    def apply(left: Expression, right: Expression): BinaryArithmeticExpression =
      NotEqual(Modulo(right, left), Zero)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case NotEqual(Modulo(right, left), Zero) => Some(left, right)
      case _ => None
    }
  }

  /* ---------------------------------------------------------------------------
   * Utility Functions
   */

  /**
    * Simplifies the given expression.
    *
    * This method assumes that permission amounts are not negative.
    *
    * @param expression The expression to simplify.
    * @return The simplified expression.
    */
  def simplify(expression: Expression, collect: Boolean = false): Expression = {
    // collect variables and constants if corresponding flag is set
    def collected = if (collect) {
      val transformed = expression.transform {
        case Comparison(left, right, operator) =>
          val collected = Collected(left) - Collected(right)
          Comparison(collected.toExpression, Zero, operator)
        case other => other
      }
      if (transformed.typ == IntType) Collected(transformed).toExpression
      else transformed
    } else expression

    // perform some simplifying rewritings that preserve equivalence
    collected.transform {
      // simplify unary operators
      case UnaryArithmeticExpression(argument, ArithmeticOperator.+, _) => argument
      case original@Negate(argument) =>
        println(original)
        argument match {
        case Literal(value: Int) => Literal(-value)
        case Times(Literal(value: Int), term) => Times(Literal(-value), term)
        case Negate(nested) => nested
        case _ => original
      }
      // simplify additions
      case original@Plus(left, right) => (left, right) match {
        case (Zero, _) => right
        case (_, Zero) => left
        case (Literal(a: Int), Literal(b: Int)) => Literal(a + b)
        case (Literal(a: Int), Plus(term, Literal(b: Int))) => Plus(term, Literal(a + b))
        case (Plus(term, Literal(b: Int)), Literal(a: Int)) => Plus(term, Literal(a + b))
        case (Literal(a: Int), _) if a < 0 => Minus(right, Literal(-a))
        case (_, Literal(a: Int)) if a < 0 => Minus(left, Literal(-a))
        case (Literal(_), _) => Plus(right, left)
        case (Negate(argument), _) => Minus(right, argument)
        case (_, Negate(argument)) => Minus(left, argument)
        case (Times(Literal(a: Int), term), _) if a < 0 => Minus(right, Times(Literal(-a), term))
        case (_, Times(Literal(a: Int), term)) if a < 0 => Minus(left, Times(Literal(-a), term))
        case _ => original
      }
      // simplify multiplications
      case original@Times(left, right) => (left, right) match {
        case (Zero, _) => Zero
        case (_, Zero) => Zero
        case (One, _) => right
        case (_, One) => left
        case (Literal(-1), Negate(term)) => term
        case (Negate(term), Literal(-1)) => term
        case (Literal(-1), _) => Negate(right)
        case (_, Literal(-1)) => Negate(left)
        case _ => original
      }
      // simplify modulo
      case original@Modulo(left, right) => (left, right) match {
        case (_, One) => Zero
        case (Literal(a: Int), Literal(b: Int)) => Literal(a % b)
        case (Times(Literal(a: Int), term), Literal(b: Int)) =>
          val divisor = gcd(a, b)
          if (b == divisor) Zero
          else if (a == divisor) Modulo(term, Literal(b / divisor))
          else Modulo(Times(Literal(a / divisor), term), Literal(b / divisor))
        case _ => original
      }
      // simplify comparisons
      case Comparison(Literal(a: Int), Literal(b: Int), operator) => operator match {
        case ArithmeticOperator.== => Literal(a == b)
        case ArithmeticOperator.!= => Literal(a != b)
        case ArithmeticOperator.< => Literal(a < b)
        case ArithmeticOperator.<= => Literal(a <= b)
        case ArithmeticOperator.> => Literal(a > b)
        case ArithmeticOperator.>= => Literal(a >= b)
      }
      case Comparison(left, right, operator) if left == right => operator match {
        case ArithmeticOperator.== => True
        case ArithmeticOperator.!= => False
        case ArithmeticOperator.< => False
        case ArithmeticOperator.<= => True
        case ArithmeticOperator.> => False
        case ArithmeticOperator.>= => True
      }
      case Comparison(Minus(left, right), Zero, operator) =>
        Comparison(left, right, operator)
      // simplify conjunctions
      case And(left, right) => (left, right) match {
        // constant folding
        case (True, _) => right
        case (_, True) => left
        case (False, _) => False
        case (_, False) => False
        // eliminate duplicates
        case (AndList(ls), AndList(rs)) => AndList((ls ++ rs).distinct)
      }
      // simplify disjunctions
      case Or(left, right) => (left, right) match {
        // constant folding
        case (True, _) => True
        case (_, True) => True
        case (False, _) => right
        case (_, False) => left
        // eliminate duplicates
        case (OrList(ls), OrList(rs)) => OrList((ls ++ rs).distinct)
      }
      // simplify boolean negations
      case original@Not(argument) => argument match {
        // constant folding
        case True => False
        case False => True
        // negate arithmetic operator
        case Comparison(left, right, operator) =>
          val negated = ArithmeticOperator.negate(operator)
          Comparison(left, right, negated)
        // eliminate double negations
        case Not(nested) => nested
        // no simplification
        case _ => original
      }
      // simplify minima
      case Min(left, right) => (left, right) match {
        case (Literal(a: Int), Literal(b: Int)) =>
          if (a < b) Literal(a)
          else Literal(b)
        case (No, _) => No
        case (_, No) => No
        case (MinList(ls), MinList(rs)) => MinList((ls ++ rs).distinct)
      }
      // simplify maxima
      case Max(left, right) => (left, right) match {
        case (Literal(a: Int), Literal(b: Int)) =>
          if (a < b) Literal(b)
          else Literal(a)
        case (No, _) => right
        case (_, No) => left
        // case (ConditionalExpression(c1, l1, No), ConditionalExpression(c2, l2, No)) if l1 == l2 =>
        //   ConditionalExpression(simplify(Or(c1, c2)), l1, No)
        case (MaxList(ls), MaxList(rs)) =>

          /*def add(x: Expression, xs: List[Expression]): List[Expression] = xs.foldLeft((x, List.empty)) {
            case ((x, ys),)
          }*/

          MaxList((ls ++ rs).distinct)
      }
      // simplify reference comparison expressions
      case ReferenceComparisonExpression(left, right, operator) if left == right =>
        Literal(operator == ReferenceOperator.==)
      // simplify conditional expressions
      case ConditionalExpression(_, left, right) if left == right => left
      case ConditionalExpression(True, term, _) => term
      case ConditionalExpression(False, _, term) => term
      case ConditionalExpression(left, ConditionalExpression(right, term, No), No) =>
        ConditionalExpression(And(left, right), term, No)
      // no simplification
      case original => original
    }
  }

  def pretty(expression: Expression, variables: Seq[VariableIdentifier]): Expression = expression.transform {
    case comparison@Comparison(left, right, operator) =>
      // put all variables of interest on one side
      val collected = Collected(left) - Collected(right)
      val keys = collected.coefficients.filter { case (v, c) => variables.contains(v) && c != 0 }.keys
      val c0 = if (keys.isEmpty) comparison else {

        val x = keys.foldLeft(1) { (f, k) => f * collected.coefficients(k) }
        val f = if (x < 0) -1 else 1

        val terms = keys.map { k => Times(Literal(f * collected.coefficients(k)), k) }
        val newLeft = Plus(terms)

        val b = if (x < 0) collected else -collected
        val rest = keys.foldLeft(b) { case (r, k) => r.drop(k) }

        val newRight = rest.toExpression
        simplify(Comparison(newLeft, newRight, operator))
      }

      // remove unnecessary negations
      val c1 = c0 match {
        case Comparison(Negate(left), Negate(right), operator) => Comparison(right, left, operator)
        case Comparison(Negate(term), Zero, operator) => Comparison(Zero, term, operator)
        case Comparison(Zero, Negate(term), operator) => Comparison(term, Zero, operator)
        case other => other
      }

      // modify inequalities s.t. the smaller term is always on the left hand side
      c1 match {
        case original@Comparison(left, right, operator) => operator match {
          case ArithmeticOperator.> | ArithmeticOperator.>= =>
            val flipped = ArithmeticOperator.flip(operator)
            Comparison(right, left, flipped)
          case _ => original
        }
        case other => other
      }
    case other => other
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
      val newRest = (rest, other.rest) match {
        case (Literal(a: Int), Literal(b: Int)) => Literal(a + b)
        case (Literal(a: Int), Plus(expression, Literal(b: Int))) => Plus(expression, Literal(a + b))
        case (Plus(expression, Literal(a: Int)), Literal(b: Int)) => Plus(expression, Literal(a + b))
        case (Plus(x, Literal(a: Int)), Plus(y, Literal(b: Int))) => Plus(Plus(x, y), Literal(a + b))
        case _ => Plus(rest, other.rest)
      }
      Collected(newCoefficients, newRest)
    }

    def -(other: Collected): Collected = this + -other

    def *(other: Collected): Collected = {
      // helper function to scale the rest
      def scale(factor: Int, rest: Expression): Expression =
        if (factor == 0) Zero
        else if (factor == 1) rest
        else if (factor == -1) Negate(rest)
        else rest match {
          case Literal(value: Int) => Literal(factor * value)
          case Plus(expression, Literal(value: Int)) => Plus(Times(Literal(factor), expression), Literal(factor * value))
          case Times(Literal(value: Int), expression) => Times(Literal(factor * value), expression)
          case Times(expression, Literal(value: Int)) => Times(Literal(factor * value), expression)
          case _ => Times(Literal(factor), rest)
        }

      // multiply this and other
      (this, other) match {
        case (Collected(map, Literal(value: Int)), collected) if map.isEmpty =>
          val coefficients = collected.coefficients.mapValues(value * _)
          val rest = scale(value, collected.rest)
          Collected(coefficients, rest)
        case (collected, Collected(map, Literal(value: Int))) if map.isEmpty =>
          val coefficients = collected.coefficients.mapValues(value * _)
          val rest = scale(value, collected.rest)
          Collected(coefficients, rest)
        case (collectedLeft, collectedRight) =>
          // TODO: Is it better to return the original expression?
          val leftExpression = collectedLeft.toExpression
          val rightExpression = collectedRight.toExpression
          Collected(Map.empty, Times(leftExpression, rightExpression))
      }
    }

    def unary_+(): Collected = this

    def unary_-(): Collected = {
      val newCoefficients = coefficients.mapValues(-_)
      val newRest = rest match {
        case Literal(value: Int) => Literal(-value)
        case Plus(expression, Literal(constant: Int)) => Plus(Times(Literal(-1), expression), Literal(-constant))
        case Times(Literal(factor: Int), expression) => Times(Literal(-factor), expression)
        case _ => Times(Literal(-1), rest)
      }
      Collected(newCoefficients, newRest)
    }

    def drop(variable: VariableIdentifier): Collected = {
      val newCoefficient = coefficients - variable
      Collected(newCoefficient, rest)
    }

    def toExpression: Expression = {
      val filtered = coefficients.filter { case (_, coefficient) => coefficient != 0 }
      val parts = filtered.map { case (variable, coefficient) => Times(Literal(coefficient), variable) }
      Plus(Plus(parts), rest)
    }

    def toExpression(variable: VariableIdentifier): Expression = {
      val coefficient = coefficients.getOrElse(variable, 0)
      val left = Times(Literal(coefficient), variable)
      val right = drop(variable).toExpression
      Plus(left, right)
    }
  }

  object Collected {
    def apply(expression: Expression): Collected = expression match {
      case Negate(argument) => -Collected(argument)
      case Plus(left, right) => Collected(left) + Collected(right)
      case Minus(left, right) => Collected(left) - Collected(right)
      case Times(left, right) => Collected(left) * Collected(right)
      case variable: VariableIdentifier => Collected(Map(variable -> 1), Zero)
      case constant: Constant => Collected(Map.empty, constant)
      case permission: FractionalPermissionExpression => Collected(Map.empty, permission)
      case other => Collected(Map.empty, other)
    }
  }

}
