/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.BooleanExpressionSimplifier
import ch.ethz.inf.pm.sample.oorepresentation._

trait CharacterSet extends SetDomain[Char, CharacterSet] {
  override def factory(value: Set[Char]): CharacterSet =
    if (value.isEmpty) BottomCharacterSet
    else InnerCharacterSet(value)
  override def bottom(): CharacterSet = BottomCharacterSet
  override def top(): CharacterSet = TopCharacterSet
}
object BottomCharacterSet extends CharacterSet with SetDomain.Bottom[Char, CharacterSet]
object TopCharacterSet extends CharacterSet with SetDomain.Top[Char, CharacterSet]
case class InnerCharacterSet(value:Set[Char]) extends CharacterSet with SetDomain.Inner[Char, CharacterSet, InnerCharacterSet]


class MaybeContainedCharacters(val map: Map[Identifier, CharacterSet] = Map.empty[Identifier, CharacterSet],
                               override val isBottom: Boolean = false,
                               val isTop: Boolean = false)
  extends BoxedDomain[CharacterSet, MaybeContainedCharacters]
  with BooleanExpressionSimplifier[MaybeContainedCharacters]
  with StringDomain[MaybeContainedCharacters] {

  def functionalFactory(_value: Map[Identifier, CharacterSet] = Map.empty[Identifier, CharacterSet], _isBottom: Boolean = false, _isTop: Boolean = false): MaybeContainedCharacters =
    new MaybeContainedCharacters(_value, _isBottom, _isTop)

  def setToTop(variable: Identifier): MaybeContainedCharacters = this.add(variable, TopCharacterSet)

  def assign(variable: Identifier, expr: Expression): MaybeContainedCharacters = this.add(variable, this.eval(expr))

  override def assumeSimplified(expr: Expression): MaybeContainedCharacters = expr match {
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, _,
    AbstractOperatorIdentifiers.stringIndexof, _), Constant("0", typ2), ArithmeticOperator.>=) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).+(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, _,
    AbstractOperatorIdentifiers.stringLastindexof, _), Constant("0", typ2), ArithmeticOperator.>=) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).+(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, _,
    AbstractOperatorIdentifiers.stringIndexof, _), Constant("0", typ2), ArithmeticOperator.<) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).-(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, _,
    AbstractOperatorIdentifiers.stringLastindexof, _), Constant("0", typ2), ArithmeticOperator.<) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).-(c));
        case _ => this;
      }
    case AbstractOperator(thisExpr: Identifier, parameters, _, AbstractOperatorIdentifiers.stringContains, _) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.head match {
        case Constant(s, typ2) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          this.add(thisExpr, this.get(thisExpr).+(c));
        case _ => this;
      }
    case _ => this;
  }

  def get(variable: Identifier) = map.get(variable) match {
    case Some(x) => x;
    case None => TopCharacterSet
  }

  def createVariable(variable: Identifier, typ: Type): MaybeContainedCharacters = this.add(variable, TopCharacterSet)

  def removeVariable(variable: Identifier): MaybeContainedCharacters = this.remove(variable)

  private def eval(expr: Expression): CharacterSet = expr match {
    case x: Identifier => this.get(x)
    case x: Constant if x.constant.isInstanceOf[String] =>
      var result:CharacterSet = BottomCharacterSet
      for (c <- x.constant.toCharArray)
        result = result.+(c)
      result;
    case AbstractOperator(thisExpr, parameters, _, AbstractOperatorIdentifiers.stringConcatenation, _) =>
      parameters match {
        case p1 :: Nil =>
          val left = this.eval(thisExpr)
          val right = this.eval(p1)
          left.lub(right)
        case _ => TopCharacterSet
      }
    case AbstractOperator(thisExpr, _, _, AbstractOperatorIdentifiers.stringSubstring, _) =>
      this.eval(thisExpr);
    case _ => TopCharacterSet
  }
}