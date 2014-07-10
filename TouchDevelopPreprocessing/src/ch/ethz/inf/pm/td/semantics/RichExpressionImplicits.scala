package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{Constant, VariableIdentifier, _}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.domain.{InvalidExpression, ValidExpression}

import scala.collection.immutable.Range.Inclusive

object RichExpressionImplicits extends RichExpressionImplicits

trait RichExpressionImplicits {
  implicit def toRichExpression(value:ExpressionSet) : RichExpression =
    RichExpression(value)

  implicit def toRichExpression(value:Inclusive) : RichExpression =
    toRichExpression(value.head) ndTo toRichExpression(value.last)

  implicit def toRichExpression(value:Int) : RichExpression =
    RichExpression(new ExpressionSet(TNumber.typ).add(new Constant(value.toString,TNumber.typ)))

  implicit def toRichExpression(value:Double) : RichExpression =
    RichExpression(new ExpressionSet(TNumber.typ).add(new Constant(value.toString,TNumber.typ)))

  implicit def toRichExpression(value:Expression) : RichExpression =
    RichExpression(new ExpressionSet(value.typ).add(value))

  implicit def toExpressionSet(value:RichExpression) : ExpressionSet =
    value.thisExpr

  /*-- Constants --*/

  def String(a:String)(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant(a,TString.typ,pp))
  def True(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant("true",TBoolean.typ,pp))
  def False(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant("false",TBoolean.typ,pp))
  def Bottom(typ:TouchType): RichExpression = toRichExpression(new ExpressionSet(typ).bottom())
  def PositiveInfinity(implicit pp:ProgramPoint) :RichExpression = toRichExpression(new Constant("posinfty",TNumber.typ,pp))
  def NegativeInfinity(implicit pp:ProgramPoint) :RichExpression = toRichExpression(new Constant("neginfty",TNumber.typ,pp))

  def Invalid(typ: Type, cause: String)(implicit pp: ProgramPoint): RichExpression = toRichExpression(InvalidExpression(typ, cause, pp))
  def Valid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = toRichExpression(ValidExpression(typ,pp))
  def Singleton(typ: Type)(implicit pp: ProgramPoint): RichExpression = toRichExpression(VariableIdentifier(typ.name.toLowerCase)(typ, pp))
}
