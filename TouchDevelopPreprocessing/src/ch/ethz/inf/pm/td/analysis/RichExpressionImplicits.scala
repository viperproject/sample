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

object RichExpressionImplicits extends RichExpressionImplicits

trait RichExpressionImplicits {
  implicit def toRichExpression(value:ExpressionSet) : RichExpression =
    RichExpression(value)

  implicit def toRichExpression(value:Inclusive) : RichExpression =
    toRichExpression(value.head) ndToIncl toRichExpression(value.last)

  implicit def toRichExpression(value:Int) : RichExpression =
    RichExpression(new ExpressionSet(TNumber).add(Constant(value.toString, TNumber)))

  implicit def toRichExpression(value:Double) : RichExpression =
    RichExpression(new ExpressionSet(TNumber).add(Constant(value.toString, TNumber)))

  implicit def toRichExpression(value:Expression) : RichExpression =
    RichExpression(new ExpressionSet(value.typ).add(value))

  implicit def toExpressionSet(value:RichExpression) : ExpressionSet =
    value.thisExpr

  /*-- Constants --*/

  def String(a:String)(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant(a,TString,pp))
  def True(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant("true",TBoolean,pp))
  def False(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant("false",TBoolean,pp))
  def Bottom(typ:TouchType): RichExpression = toRichExpression(new ExpressionSet(typ).bottom())
  def PositiveInfinity(implicit pp:ProgramPoint) :RichExpression = toRichExpression(Constant("posinfty", TNumber, pp))
  def NegativeInfinity(implicit pp:ProgramPoint) :RichExpression = toRichExpression(Constant("neginfty", TNumber, pp))

  def Invalid(typ: Type, cause: String)(implicit pp: ProgramPoint): RichExpression = toRichExpression(InvalidExpression(typ, cause, pp))
  def Valid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = toRichExpression(ValidExpression(typ,pp))
  def Singleton(typ: Type)(implicit pp: ProgramPoint): RichExpression = toRichExpression(VariableIdentifier(typ.name.toLowerCase)(typ, pp))

}
