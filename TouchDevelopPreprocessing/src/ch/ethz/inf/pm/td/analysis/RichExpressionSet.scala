/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.semantics.{TBoolean, TNumber, TString}

case class RichExpressionSet(thisExpr: ExpressionSet) extends RichExpressionSetImplicits {

  override def toString:String = thisExpr.toString

  def <=(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.<=, TBoolean))

  def >=(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.>=, TBoolean))

  def <(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.<, TBoolean))

  def >(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.>, TBoolean))

  def equal(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.==, TBoolean))

  def unequal(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.!=, TBoolean))

  def +(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.+, TNumber))

  def *(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.*, TNumber))

  def -(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.-, TNumber))

  def /(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator./, TNumber))

  def or(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createNondeterministicBinaryExpression(thisExpr, thatExpr, NondeterministicOperator.or, thisExpr.typ))

  def ndToIncl(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createNondeterministicBinaryExpression(thisExpr, thatExpr, NondeterministicOperator.toIncl, TNumber))

  def ndToExcl(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createNondeterministicBinaryExpression(thisExpr, thatExpr, NondeterministicOperator.toExcl, TNumber))

  def &&(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBooleanBinaryExpression(thisExpr, thatExpr, BooleanOperator.&&, TBoolean))

  def ||(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBooleanBinaryExpression(thisExpr, thatExpr, BooleanOperator.||, TBoolean))

  def concat(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createAbstractOperator(thisExpr, List(thatExpr), Nil, AbstractOperatorIdentifiers.stringConcatenation, TString))

  def not(): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createNegatedBooleanExpression(thisExpr))
}