/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.sample.oorepresentation.Type

/**
  * Represents a pair-like expression (e.g. for multiple return values)
  *
  * @param left  The left operand
  * @param right The right operand
  * @author Lucas Brutschy
  */
case class MultiValExpression(left: Expression, right: Expression, returnTyp: Type) extends Expression {

  def pp = left.pp

  def typ = returnTyp

  def ids = left.ids ++ right.ids

  override def hashCode(): Int = left.hashCode() + right.hashCode()

  override def equals(o: Any) = o match {
    case MultiValExpression(l, r, t) => left.equals(l) && right.equals(r) && returnTyp.equals(t)
    case _ => false
  }

  override def toString = left.toString + "," + right.toString

  override def transform(f: (Expression => Expression)): Expression =
    f(MultiValExpression(left.transform(f), right.transform(f), returnTyp))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || left.contains(f) || right.contains(f)

}