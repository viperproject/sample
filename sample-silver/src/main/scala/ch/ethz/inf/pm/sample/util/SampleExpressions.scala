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

    def apply(expressions: Iterable[Expression]): Expression =
      if (expressions.isEmpty) True
      else expressions.reduce((left, right) => And(left, right))

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&) => Some(left, right)
      case _ => None
    }
  }

  object Or {
    def apply(left: Expression, right: Expression): BinaryBooleanExpression =
      BinaryBooleanExpression(left, right, BooleanOperator.||)

    def apply(expressions: Iterable[Expression]): Expression =
      if (expressions.isEmpty) False
      else expressions.reduce((left, right) => Or(left, right))

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case BinaryBooleanExpression(left, right, BooleanOperator.||) => Some(left, right)
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

  /* ---------------------------------------------------------------------------
   * Utility Functions
   */

  /**
    * Simplifies the given expression.
    *
    * @param expression The expression to simplify.
    * @return The simplified expression.
    */
  def simplify(expression: Expression): Expression = expression.transform {
    // simplify additions
    case original@Plus(left, right) => (left, right) match {
      case (Zero, _) => right
      case (_, Zero) => left
      case (Literal(a: Int), Literal(b: Int)) => Literal(a + b)
      case (Literal(a: Int), Plus(term, Literal(b: Int))) => Plus(term, Literal(a + b))
      case (Plus(term, Literal(b: Int)), Literal(a: Int)) => Plus(term, Literal(a + b))
      case (Literal(_), _) => Plus(right, left)
      case (Negate(argument), _) => Minus(right, argument)
      case (_, Negate(argument)) => Minus(left, argument)
      case _ => original
    }
    // simplify multiplications
    case original@Times(left, right) => (left, right) match {
      case (Zero, _) => Zero
      case (_, Zero) => Zero
      case (One, _) => right
      case (_, One) => left
      case (Literal(-1), _) => Negate(right)
      case (_, Literal(-1)) => Negate(left)
      case _ => original
    }
    // simplify unary operators
    case UnaryArithmeticExpression(argument, ArithmeticOperator.+, _) => argument
    case original@Negate(argument) => argument match {
      case Literal(value: Int) => Literal(-value)
      case Negate(nested) => nested
      case _ => original
    }
    // simplify conjunctions
    case original@And(left, right) => (left, right) match {
      // constant folding
      case (True, _) => right
      case (_, True) => left
      case (False, _) => False
      case (_, False) => False
      // syntactic simplification
      case _ if left == right => left
      // no simplification
      case _ => original
    }
    // simplify disjunctions
    case original@Or(left, right) => (left, right) match {
      // constant folding
      case (True, _) => True
      case (_, True) => True
      case (False, _) => right
      case (_, False) => left
      // syntactic simplification
      case _ if left == right => left
      // no simplification
      case _ => original
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
    // simplify reference comparision expressions
    case ReferenceComparisonExpression(left, right, operator) if left == right =>
      Literal(operator == ReferenceOperator.==)
    // simplify conditional expressions
    case original@ConditionalExpression(condition, left, right) => condition match {
      // constant folding
      case True => left
      case False => right
      // syntactic simplification
      case NegatedBooleanExpression(argument) => ConditionalExpression(argument, right, left)
      // no simplification
      case _ => original
    }
    // no simplification
    case original => original
  }

}
