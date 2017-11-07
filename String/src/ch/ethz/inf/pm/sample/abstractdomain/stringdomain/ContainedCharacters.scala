/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain.SetDomain.Default
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.BooleanExpressionSimplifier
import ch.ethz.inf.pm.sample.oorepresentation._

case class InvertedCharacterSet(wrapped: SetDomain.Default[Char] = SetDomain.Default.Bottom[Char]())
  extends InvertedLatticeWrapper[SetDomain.Default[Char], InvertedCharacterSet] {

  def add(c:Char) = wrapperFactory(wrapped.+(c))

  def remove(c:Char) = wrapperFactory(wrapped.-(c))

  override def wrapperFactory(wrapped: Default[Char]): InvertedCharacterSet = InvertedCharacterSet(wrapped)

}

class SurelyContainedCharacters (val map: Map[Identifier, InvertedCharacterSet] = Map.empty[Identifier, InvertedCharacterSet],
                                 override val isBottom: Boolean = false,
                                 val isTop: Boolean = false)
  extends BoxedDomain[InvertedCharacterSet, SurelyContainedCharacters]
  with BooleanExpressionSimplifier[SurelyContainedCharacters]
  with StringDomain[SurelyContainedCharacters] {

  def functionalFactory(_value: Map[Identifier, InvertedCharacterSet] = Map.empty[Identifier, InvertedCharacterSet],
                        _isBottom: Boolean = false,
                        _isTop: Boolean = false): SurelyContainedCharacters =
    new SurelyContainedCharacters(_value, _isBottom, _isTop)

  def setToTop(variable: Identifier): SurelyContainedCharacters = this.add(variable,InvertedCharacterSet().top())

  def assign(variable: Identifier, expr: Expression): SurelyContainedCharacters = this.add(variable, this.eval(expr))

  override def assumeSimplified(expr: Expression): SurelyContainedCharacters = expr match {
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, _m,
    AbstractOperatorIdentifiers.stringIndexof, _), Constant("0", typ2), ArithmeticOperator.>=) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).add(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, _,
    AbstractOperatorIdentifiers.stringLastindexof, _), Constant("0", typ2), ArithmeticOperator.>=) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).add(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, _,
    AbstractOperatorIdentifiers.stringIndexof, _), Constant("0", typ2), ArithmeticOperator.<) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).remove(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, _,
    AbstractOperatorIdentifiers.stringLastindexof, _), Constant("0", typ2), ArithmeticOperator.<) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).remove(c));
        case _ => this;
      }
    case AbstractOperator(thisExpr: Identifier, parameters, _, AbstractOperatorIdentifiers.stringContains, _) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, typ2) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).add(c));
        case _ => this;
      }
    case _ => this;
  }

  def get(variable: Identifier) = map.get(variable) match {
    case Some(x) => x;
    case None => InvertedCharacterSet().top();
  }

  def createVariable(variable: Identifier, typ: Type): SurelyContainedCharacters = this.add(variable,InvertedCharacterSet().top())

  def removeVariable(variable: Identifier): SurelyContainedCharacters = this.remove(variable)

  private def eval(expr: Expression): InvertedCharacterSet = expr match {
    case x: Identifier => this.get(x)
    case x: Constant if x.constant.isInstanceOf[String] =>
      var result = InvertedCharacterSet()
      for (c <- x.constant.toCharArray)
        result = result.add(c)
      result;
    case AbstractOperator(thisExpr, parameters, _, AbstractOperatorIdentifiers.stringConcatenation, _) =>
      parameters match {
        case p1 :: Nil =>
          val left = this.eval(thisExpr)
          val right = this.eval(p1)
          left.glb(right)
        case _ =>
          InvertedCharacterSet().top()
      }
    case AbstractOperator(thisExpr, parameters, _, AbstractOperatorIdentifiers.stringSubstring, _) =>
      InvertedCharacterSet().top();
    case _ => InvertedCharacterSet().top();
  }
}