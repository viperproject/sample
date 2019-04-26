/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.td.semantics.TString

case class RichExpressionSet(thisExpr: ExpressionSet) extends RichExpressionSetImplicits {

  override def toString:String = thisExpr.toString

  def <=(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.<=))

  def >=(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.>=))

  def <(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.<))

  def >(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.>))

  def equal(thatExpr: RichExpressionSet): RichExpressionSet = {

    if (this.typ.isStringType && thatExpr.typ.isStringType)
      RichExpressionSet(ExpressionSetFactory.createBinaryStringExpression(thisExpr, thatExpr, StringOperator.==))
    else if (this.typ.isNumericalType && thatExpr.typ.isNumericalType)
      RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.==))
    else
      RichExpressionSet(ExpressionSetFactory.createReferenceComparisonExpression(thisExpr, thatExpr, ReferenceOperator.==))

  }

  def unequal(thatExpr: RichExpressionSet): RichExpressionSet = {

    if (this.typ.isStringType && thatExpr.typ.isStringType)
      RichExpressionSet(ExpressionSetFactory.createBinaryStringExpression(thisExpr, thatExpr, StringOperator.!=))
    else if (this.typ.isNumericalType && thatExpr.typ.isNumericalType)
      RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.!=))
    else
      RichExpressionSet(ExpressionSetFactory.createReferenceComparisonExpression(thisExpr, thatExpr, ReferenceOperator.!=))

  }

  def +(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.+))

  def *(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.*))

  def -(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.-))

  def /(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator./))

  def or(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createNondeterministicBinaryExpression(thisExpr, thatExpr, NondeterministicOperator.or))

  def ndToIncl(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createNondeterministicBinaryExpression(thisExpr, thatExpr, NondeterministicOperator.toIncl))

  def ndToExcl(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createNondeterministicBinaryExpression(thisExpr, thatExpr, NondeterministicOperator.toExcl))

  def &&(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBooleanBinaryExpression(thisExpr, thatExpr, BooleanOperator.&&))

  def ||(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createBooleanBinaryExpression(thisExpr, thatExpr, BooleanOperator.||))

  def concat(thatExpr: RichExpressionSet): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createAbstractOperator(thisExpr, List(thatExpr), Nil, AbstractOperatorIdentifiers.stringConcatenation, TString))

  def not(): RichExpressionSet =
    RichExpressionSet(ExpressionSetFactory.createNegatedBooleanExpression(thisExpr))
}