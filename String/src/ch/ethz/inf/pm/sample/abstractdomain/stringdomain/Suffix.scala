/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.BooleanExpressionSimplifier
import ch.ethz.inf.pm.sample.oorepresentation._

class SuffixDomain extends Lattice[SuffixDomain] {
  var isBottom: Boolean = false
  var isTop: Boolean = false
  var stringValue: String = ""

  override def factory(): SuffixDomain = new SuffixDomain()

  def top(): SuffixDomain = {
    val result: SuffixDomain = this.factory()
    result.isTop = true
    result.isBottom = false
    result
  }

  def bottom(): SuffixDomain = {
    val result: SuffixDomain = this.factory()
    result.isBottom = true
    result.isTop = false
    result
  }

  def foo(cnt: Int, lengthLeft: Int, left: String, lengthRight: Int, right: String): Int = {
    if (cnt >= lengthLeft || cnt >= lengthRight)
      cnt
    else if (left.charAt(lengthLeft - 1 - cnt).equals(right.charAt(lengthRight - 1 - cnt)))
      foo(cnt + 1, lengthLeft, left, lengthRight, right)
    else
      cnt
  }

  def lub(other: SuffixDomain): SuffixDomain = {
    val leftLength = stringValue.length()
    val otherLength = other.stringValue.length()
    if (leftLength == 0 || otherLength == 0)
      return top()
    val cnt: Int = foo(0, leftLength, stringValue, otherLength, other.stringValue)
    if (cnt <= 0)
      top()
    else {
      val result = this.factory()
      //Console.println("left: " + stringValue + "; other: " + other.stringValue + "; leftlength: "  + leftLength + "; cnt: " + cnt);
      result.stringValue = stringValue.substring(leftLength - cnt, leftLength)
      result
    }
  }

  def glb(other: SuffixDomain): SuffixDomain = {
    if (lessEqual(other))
      this
    else if (other.lessEqual(this))
      other
    else
      bottom()
  }

  def widening(other: SuffixDomain): SuffixDomain =
    lub(other)

  def lessEqual(r: SuffixDomain): Boolean =
    stringValue.endsWith(r.stringValue)

  override def toString: String = {
    if (isBottom) "⊥"
    else if (isTop) "⊤"
    else stringValue
  }
}

class Suffix (val map: Map[Identifier, SuffixDomain] = Map.empty[Identifier, SuffixDomain],
              override val isBottom: Boolean = false,
              val isTop: Boolean = false)
  extends BoxedDomain[SuffixDomain, Suffix]
  with BooleanExpressionSimplifier[Suffix]
  with StringDomain[Suffix] {

  def functionalFactory(_value: Map[Identifier, SuffixDomain] = Map.empty[Identifier, SuffixDomain], _isBottom: Boolean = false, _isTop: Boolean = false): Suffix =
    new Suffix(_value, _isBottom, _isTop)

  def setToTop(variable: Identifier): Suffix = this.add(variable, new SuffixDomain().top())

  def assign(variable: Identifier, expr: Expression): Suffix = this.add(variable, this.eval(expr))

  override def assumeSimplified(expr: Expression): Suffix = this

  def createVariable(variable: Identifier, typ: Type): Suffix = this.add(variable, new SuffixDomain().top())

  def removeVariable(variable: Identifier): Suffix = this.remove(variable)

  def get(variable: Identifier) = map.get(variable) match {
    case Some(x) => x;
    case None => new SuffixDomain().top();
  }

  override def getStringOfId(id: Identifier): String = {
    get(id).toString
  }

  private def eval(expr: Expression): SuffixDomain = expr match {
    case x: Identifier => this.get(x)
    case x: Constant if x.constant.isInstanceOf[String] =>
      //Console.println("costruzione string: " + x.constant.asInstanceOf[String]);
      val result = new SuffixDomain()
      result.stringValue = x.constant
      result
    case AbstractOperator(thisExpr, parameters, typeParameters, AbstractOperatorIdentifiers.stringConcatenation, returnTyp) =>
      parameters match {
        case p1 :: Nil => this.eval(p1);
        case _ => new SuffixDomain().top();
      }
    case AbstractOperator(thisExpr, parameters, typeParameters, AbstractOperatorIdentifiers.stringSubstring, returnTyp) =>
      new SuffixDomain().top();
    case _ => new SuffixDomain().top();
  }

  override def getPossibleConstants(id: Identifier) = ???
}