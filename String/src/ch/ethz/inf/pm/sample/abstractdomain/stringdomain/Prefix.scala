/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.BooleanExpressionSimplifier
import ch.ethz.inf.pm.sample.oorepresentation._

class PrefixDomain extends Lattice[PrefixDomain] {
  var isBottom: Boolean = false
  var isTop: Boolean = false
  var stringValue: String = ""

  override def factory(): PrefixDomain = new PrefixDomain()

  def top(): PrefixDomain = {
    val result: PrefixDomain = this.factory()
    result.isTop = true
    result.isBottom = false
    result
  }

  def bottom(): PrefixDomain = {
    val result: PrefixDomain = this.factory()
    result.isBottom = true
    result.isTop = false
    result
  }

  def foo(cnt: Int, left: String, right: String, max: Int): Int = {
    if (cnt >= max)
      cnt
    else if (left.charAt(cnt).equals(right.charAt(cnt)))
      foo(cnt + 1, left, right, max)
    else
      cnt
  }

  def lub(other: PrefixDomain): PrefixDomain = {
    if (stringValue.length() == 0 || other.stringValue.length() == 0)
      return top()
    val cnt: Int = foo(0, stringValue, other.stringValue, stringValue.length().min(other.stringValue.length()))
    if (cnt == 0)
      top()
    else {
      val result = this.factory()
      result.stringValue = stringValue.substring(0, cnt)
      result
    }
  }

  def glb(other: PrefixDomain): PrefixDomain = {
    if (lessEqual(other))
      this
    else if (other.lessEqual(this))
      other
    else
      bottom()
  }

  def widening(other: PrefixDomain): PrefixDomain = lub(other)

  def lessEqual(r: PrefixDomain): Boolean =
    this.stringValue.startsWith(r.stringValue)

  override def toString: String = {
    if (isBottom) "⊥"
    else if (isTop) "⊤"
    else stringValue
  }
}

class Prefix (val map: Map[Identifier, PrefixDomain] = Map.empty[Identifier, PrefixDomain],
              override val isBottom: Boolean = false,
              val isTop: Boolean = false)
  extends BoxedDomain[PrefixDomain, Prefix]
  with BooleanExpressionSimplifier[Prefix]
  with StringDomain[Prefix] {

  def functionalFactory(_value: Map[Identifier, PrefixDomain] = Map.empty[Identifier, PrefixDomain],
                        _isBottom: Boolean = false,
                        _isTop: Boolean = false): Prefix =
    new Prefix(_value, _isBottom, _isTop)

  def setToTop(variable: Identifier): Prefix = this.add(variable, new PrefixDomain().top())

  def assign(variable: Identifier, expr: Expression): Prefix = this.add(variable, this.eval(expr))

  override def assumeSimplified(expr: Expression): Prefix = this

  def createVariable(variable: Identifier, typ: Type): Prefix = this.add(variable, new PrefixDomain().top())

  def removeVariable(variable: Identifier): Prefix = this.remove(variable)

  def get(variable: Identifier) = map.get(variable) match {
    case Some(x) => x;
    case None => new PrefixDomain().top();
  }

  override def getStringOfId(id: Identifier): String = {
    get(id).toString
  }

  private def eval(expr: Expression): PrefixDomain = expr match {
    case x: Identifier => this.get(x)
    case x: Constant if x.constant.isInstanceOf[String] =>
      val result = new PrefixDomain()
      result.stringValue = x.constant
      result
    case AbstractOperator(thisExpr, parameters, typeParameters, AbstractOperatorIdentifiers.stringConcatenation, returnTyp) =>
      this.eval(thisExpr);
    case AbstractOperator(thisExpr, parameters, typeParameters, AbstractOperatorIdentifiers.stringSubstring, returnTyp) =>
      val l: List[Expression] = parameters
      if (l.size != 2) return new PrefixDomain().top()
      l.head match {
        case Constant(s1, _) =>
          l.apply(1) match {
            case Constant(s2, _) =>
              val beginIndex = Integer.decode(s1).intValue()
              val endIndex = Integer.decode(s2).intValue()
              val str = this.eval(thisExpr).stringValue
              val len = str.length()
              if (endIndex <= len) {
                val newStr = str.substring(beginIndex, endIndex)
                val result = new PrefixDomain()
                result.stringValue = newStr
                result
              } else if (beginIndex < len) {
                val newStr = str.substring(beginIndex, len)
                val result = new PrefixDomain()
                result.stringValue = newStr
                result
              } else
                new PrefixDomain().top();
            case _ => new PrefixDomain().top();
          }
        case _ => new PrefixDomain().top();
      }
    case _ => new PrefixDomain().top();
  }

  override def getPossibleConstants(id: Identifier) = ???
}