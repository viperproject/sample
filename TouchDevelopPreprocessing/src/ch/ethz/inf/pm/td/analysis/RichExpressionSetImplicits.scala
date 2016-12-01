/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.td.compiler.{CFGGenerator, TouchType}
import ch.ethz.inf.pm.td.domain.{InvalidExpression, ValidExpression}
import ch.ethz.inf.pm.td.semantics.{TBoolean, TNumber, TString}

import scala.language.implicitConversions
import scala.collection.immutable.Range.Inclusive

object RichExpressionSetImplicits extends RichExpressionSetImplicits

trait RichExpressionSetImplicits {
  implicit def toRichExpression(value: ExpressionSet): RichExpressionSet =
    RichExpressionSet(value)

  implicit def toRichExpression(value: Inclusive): RichExpressionSet =
    toRichExpression(value.head) ndToIncl toRichExpression(value.last)

  implicit def toRichExpression(value: Int): RichExpressionSet =
    RichExpressionSet(new ExpressionSet(TNumber).add(Constant(value.toString, TNumber)))

  implicit def toRichExpression(value: Double): RichExpressionSet =
    RichExpressionSet(new ExpressionSet(TNumber).add(Constant(value.toString, TNumber)))

  implicit def toRichExpression(value: Expression): RichExpressionSet =
    RichExpressionSet(new ExpressionSet(value.typ).add(value))

  implicit def toExpressionSet(value: RichExpressionSet): ExpressionSet =
    value.thisExpr

  /*-- Constants --*/

  def String(a: String)(implicit pp: ProgramPoint): RichExpressionSet = toRichExpression(Constant(a, TString, pp))

  def True(implicit pp: ProgramPoint): RichExpressionSet = toRichExpression(Constant("true", TBoolean, pp))

  def Bottom(typ: TouchType): RichExpressionSet = toRichExpression(new ExpressionSet(typ).bottom())

  def PositiveInfinity(implicit pp: ProgramPoint): RichExpressionSet = toRichExpression(Constant("posinfty", TNumber, pp))

  def NegativeInfinity(implicit pp: ProgramPoint): RichExpressionSet = toRichExpression(Constant("neginfty", TNumber, pp))

  def Default(typ: Type, cause: String)(implicit pp: ProgramPoint): RichExpressionSet = {
    typ match {
      case TNumber =>   toRichExpression(ExpressionSet(Constant("0", TNumber, pp)))
      case TBoolean =>  toRichExpression(new ExpressionSet(TBoolean).add(False))
      case TString =>   toRichExpression(ExpressionSet(Constant("", TString, pp)))
      case _ =>         toRichExpression(Invalid(typ,cause))
    }
  }

  def False(implicit pp: ProgramPoint): RichExpressionSet = toRichExpression(Constant("false", TBoolean, pp))

  def Invalid(typ: Type, cause: String)(implicit pp: ProgramPoint): RichExpressionSet = toRichExpression(InvalidExpression(typ, cause, pp))

  def Valid(typ: Type)(implicit pp: ProgramPoint): RichExpressionSet = toRichExpression(ValidExpression(typ, pp))

  def Singleton(typ: Type)(implicit pp: ProgramPoint): RichExpressionSet = toRichExpression(VariableIdentifier(typ.name.toLowerCase)(typ, pp))

}
