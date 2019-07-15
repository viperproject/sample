/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.silver.{BoolType, IntType, PermType}

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
      Constant(value.toString, IntType)()

    def apply(value: Boolean): Constant =
      Constant(value.toString, BoolType)()

    def unapply(argument: Expression): Option[Any] = argument match {
      case Constant(value, IntType) => Some(value.toInt)
      // TODO: This is here since the numerator of fractional permissions sometimes is permission typed.
      case Constant(value, PermType) => Some(value.toInt)
      case Constant(value, BoolType) => Some(value.toBoolean)
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

  object Permission {
    def apply(numerator: Int, denominator: Int): FractionalPermissionExpression =
      FractionalPermissionExpression(Literal(numerator), Literal(denominator))

    def unapply(argument: Expression): Option[(Int, Int)] = argument match {
      case FractionalPermissionExpression(Literal(numerator: Int), Literal(denominator: Int)) => Some(numerator, denominator)
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

  object PlusList {
    def unapply(argument: Expression): Option[List[Expression]] = argument match {
      case Plus(PlusList(left), PlusList(right)) => Some(left ++ right)
      case _ if argument.typ.isNumericalType => Some(List(argument))
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

    def unapply(argument: Expression): Option[List[Expression]] = argument match {
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
   * Expressions for QP
   */

  object ReadParameter {
    def apply(): VariableIdentifier = {
      val name = "read" // QpContext.getReadParameter.name
      VariableIdentifier(name)(PermType)
    }

    def unapply(argument: Expression): Option[VariableIdentifier] = argument match {
      case variable: VariableIdentifier if variable.name == "read" => Some(variable) // QpContext.getReadParameter.nam
      case _ => None
    }
  }

  object Leaf {
    def apply(condition: Expression, permission: Expression): ConditionalExpression =
      ConditionalExpression(condition, permission, No)

    def unapply(argument: Expression): Option[(Expression, Expression)] = argument match {
      case ConditionalExpression(condition, ReadParameter(variable), No) => Some(condition, variable)
      case ConditionalExpression(condition, permission@Permission(_, _), No) => Some(condition, permission)
      case _ => None
    }
  }

  object NonLeaf {
    def apply(condition: Expression, left: Expression, right: Expression): ConditionalExpression =
      ConditionalExpression(condition, left, right)

    def unapply(argument: Expression): Option[(Expression, Expression, Expression)] = argument match {
      case Leaf(_, _) => None
      case ConditionalExpression(condition, left, right) => Some(condition, left, right)
      case _ => None
    }
  }
}
