/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{BoolType, IntType}

/**
  * Some utility functions for sample expressions.
  *
  * @author Jerome Dohrau
  */
object SampleExpressions {
  /**
    * An expression representing an integer constant with the value zero.
    */
  val zero: Constant = integer(0)

  /**
    * An expression representing an integer constant with the value one.
    */
  val one: Constant = integer(1)

  /**
    * Returns an expression representing an integer constant with the given
    * value.
    *
    * @param value The value.
    * @return The integer constant.
    */
  def integer(value: Int): Constant = Constant(value.toString, IntType)

  /**
    * Returns an expression representing the addition of the two given
    * expressions.
    *
    * @param left  The left expression.
    * @param right The right expression.
    * @return The addition.
    */
  def plus(left: Expression, right: Expression): BinaryArithmeticExpression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.+)

  /**
    * Returns an expression representing the subtraction of the two given
    * expressions.
    *
    * @param left  The left expression.
    * @param right The right expression.
    * @return The subtraction.
    */
  def minus(left: Expression, right: Expression): BinaryArithmeticExpression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.-)

  def equ(left: Expression, right: Expression): BinaryArithmeticExpression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.==)

  def neq(left: Expression, right: Expression): BinaryArithmeticExpression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.!=)

  def geq(left: Expression, right: Expression): BinaryArithmeticExpression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.>=)

  def leq(left: Expression, right: Expression): BinaryArithmeticExpression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.<=)

  /**
    * An expression representing a true constant.
    */
  val tt: Constant = boolean(value = true)

  /**
    * An expression representing a false constant.
    */
  val ff: Constant = boolean(value = false)

  /**
    * Returns an expression representing a boolean constant with the given
    * value.
    *
    * @param value The value.
    * @return The boolean constant.
    */
  def boolean(value: Boolean): Constant =
    Constant(value.toString, BoolType)

  /**
    * Returns an expression representing the negation of the given expression.
    *
    * @param argument The expression to negate.
    * @return The negation.
    */
  def not(argument: Expression): NegatedBooleanExpression =
    NegatedBooleanExpression(argument)

  /**
    * Returns an expression representing the conjunction of the two given
    * expressions.
    *
    * @param left  The left expression.
    * @param right The right expression.
    * @return The conjunction.
    */
  def and(left: Expression, right: Expression): BinaryBooleanExpression =
    BinaryBooleanExpression(left, right, BooleanOperator.&&)

  /**
    * Returns an expression representing the disjunction of the two given
    * expressions.
    *
    * @param left  The left expression.
    * @param right The right expression.
    * @return The disjunction.
    */
  def or(left: Expression, right: Expression): BinaryBooleanExpression =
    BinaryBooleanExpression(left, right, BooleanOperator.||)

  val none: FractionalPermissionExpression = fractional(zero, one)

  val write: FractionalPermissionExpression = fractional(one, one)

  def fractional(left: Expression, right: Expression): FractionalPermissionExpression =
    FractionalPermissionExpression(left, right)

  /**
    * Simplifies the given expression.
    *
    * @param expression The expression to simplify.
    * @return The simplified expression.
    */
  def simplify(expression: Expression): Expression = expression.transform {
    // simplify additions
    case original@BinaryArithmeticExpression(left, right, ArithmeticOperator.+) => (left, right) match {
      case (`zero`, _) => right
      case (_, `zero`) => left
      case _ => original
    }
    // simplify multiplications
    case original@BinaryArithmeticExpression(left, right, ArithmeticOperator.*) => (left, right) match {
      case (`one`, _) => right
      case (_, `one`) => left
      case _ => original
    }
    // simplify conjunctions
    case original@BinaryBooleanExpression(left, right, BooleanOperator.&&) => (left, right) match {
      // constant folding
      case (`tt`, _) => right
      case (_, `tt`) => left
      case (`ff`, _) => ff
      case (_, `ff`) => ff
      // syntactic simplification
      case _ if left == right => left
      // no simplification
      case _ => original
    }
    // simplify disjunctions
    case original@BinaryBooleanExpression(left, right, BooleanOperator.||) => (left, right) match {
      // constant folding
      case (`tt`, _) => tt
      case (_, `tt`) => tt
      case (`ff`, _) => right
      case (_, `ff`) => left
      // syntactic simplification
      case _ if left == right => left
      // no simplification
      case _ => original
    }
    // simplify negations
    case original@NegatedBooleanExpression(argument) => argument match {
      // constant folding
      case `tt` => ff
      case `ff` => tt
      // eliminate double negations
      case NegatedBooleanExpression(nested) => nested
      // no simplification
      case _ => original
    }
    // simplify reference comparision expressions
    case ReferenceComparisonExpression(left, right, operator) if left == right =>
      boolean(operator == ReferenceOperator.==)
    // simplify conditional expressions
    case original@ConditionalExpression(condition, left, right) => condition match {
      // constant folding
      case `tt` => left
      case `ff` => right
      // syntactic simplification
      case NegatedBooleanExpression(argument) => ConditionalExpression(argument, right, left)
      // no simplification
      case _ => original
    }
    // no simplification
    case original => original
  }
}
