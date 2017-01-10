/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.IntType
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.ExpressionBuilder._

/**
  * @author Severin Münger
  *         Added on 10.01.17.
  */
object QuantifierElimination {

  def eliminate(variable: VariableIdentifier, expr: Expression): Expression = expr

  // Step 1
  def toNNF(expr: Expression): Expression = Utils.toNNF(expr)

  // Step 2
  def toTzEquivalentFormula(expr: Expression): Expression = expr.transform {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.==) => and(lt(left, plusOne(right)), lt(right, plusOne(left)))
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, ArithmeticOperator.!=)) => and(lt(left, plusOne(right)), lt(right, plusOne(left)))
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, ArithmeticOperator.==)) => or(lt(left, right), lt(right, left))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=) => or(lt(left, right), lt(right, left))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.<=) => lt(left, plusOne(right))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.>=) => lt(right, plusOne(left))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.>) => lt(right, left)
  } match {
    case transformed if transformed == expr => transformed
    case transformed => toTzEquivalentFormula(transformed)
  }

  // Step 3
  def collectVariable(variable: VariableIdentifier, expr: Expression): Expression = {
    
    expr
  }

  private def collect(expr: Expression): Map[Any, Int] = expr match {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.+) => binOp(collect(left), collect(right), _ + _)
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.-) => binOp(collect(left), collect(right), _ - _)
    case BinaryArithmeticExpression(Constant(const, IntType, _), other, ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case BinaryArithmeticExpression(other, Constant(const, IntType, _), ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.-, _) => unOp(collect(left), - _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.+, _) => collect(left)
    case Constant(const, IntType, _) => Map((ConstPlaceholder, const.toInt))
    case v: VariableIdentifier => Map((v, 1))
  }

  private def binOp(a: Map[Any, Int], b: Map[Any, Int], op: (Int, Int) => Int): Map[Any, Int] = a ++ b.transform((key, value) => op(a.getOrElse(key, 0), value))

  private def unOp(a: Map[Any, Int], op: (Int) => Int): Map[Any, Int] = a.transform((_, value) => op(value))

  private case object ConstPlaceholder

}

object ExpressionBuilder {
  val one = Constant("1", IntType)

  def plus(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.+)

  def plusOne(expr: Expression): BinaryArithmeticExpression = plus(expr, one)

  def minus(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.-)

  def lt(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.<)

  def and(left: Expression, right: Expression): BinaryBooleanExpression = BinaryBooleanExpression(left, right, BooleanOperator.&&)

  def or(left: Expression, right: Expression): BinaryBooleanExpression = BinaryBooleanExpression(left, right, BooleanOperator.||)
}