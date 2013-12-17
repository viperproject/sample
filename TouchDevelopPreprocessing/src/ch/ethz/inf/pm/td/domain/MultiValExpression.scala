package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, Expression}

/**
 * Represents a pair-like expression (e.g. for multiple return values)
 *
 * @param left The left operand
 * @param right The right operand
 * @author Lucas Brutschy
 */
case class MultiValExpression(left : Expression, right:Expression, returnTyp : Type) extends Expression {

  def getProgramPoint = left.getProgramPoint
  def getType = returnTyp
  def getIdentifiers : Set[Identifier] = left.getIdentifiers ++ right.getIdentifiers

  override def hashCode() : Int = left.hashCode()+right.hashCode()
  override def equals(o : Any) = o match {
    case MultiValExpression(l, r, t) => left.equals(l) && right.equals(r) && returnTyp.equals(t)
    case _ => false
  }
  override def toString = left.toString+","+right.toString

  override def transform(f:(Expression => Expression)):Expression =
    f(MultiValExpression(left.transform(f),right.transform(f),returnTyp))

}