/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, FieldExpression, Lattice, VariableIdentifier}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.SetDescription.{Bottom, InnerSetDescription, Top}

import scala.collection.mutable

/**
  * @author Severin Münger
  *         Added on 03.12.16.
  */
object SetDescription {
  case object Top extends SetDescription with Lattice.Top[SetDescription]

  case object Bottom extends SetDescription with Lattice.Bottom[SetDescription]

  object InnerSetDescription {
    def apply(initExpression: Expression) = new InnerSetDescription(initExpression)
  }

  case class InnerSetDescription(widened: Boolean = false, concreteExpressions: Map[Expression, Set[Expression]] = Map(), abstractExpressions: Set[SetElementDescriptor] = Set()) extends SetDescription {

    def this(initExpression: Expression) = this(concreteExpressions = Map(initExpression -> Set(initExpression)), abstractExpressions = Set(RootElement(initExpression)))

    def copy(widened: Boolean = widened,
             concreteExpressions: Map[Expression, Set[Expression]] = concreteExpressions,
             abstractExpressions: Set[SetElementDescriptor] = abstractExpressions): InnerSetDescription = InnerSetDescription(widened, concreteExpressions, abstractExpressions)

    override def update: InnerSetDescription = {
      val newAbstractExpressions: mutable.Set[SetElementDescriptor] = mutable.Set() ++ abstractExpressions
      val newConcreteExpressions: mutable.Map[Expression, Set[Expression]] = mutable.Map()
      concreteExpressions.foreach {
        case (expr, mapsTo) =>
            newConcreteExpressions.put(expr, Set(expr))
            mapsTo.foreach {
              e =>
                newConcreteExpressions.put(e, Set(e))
                newAbstractExpressions.union(extractRules(e))
            }
        }
      copy(concreteExpressions = newConcreteExpressions.toMap, abstractExpressions = newAbstractExpressions.toSet)
    }

    private def extractRules(expr: Expression): Set[SetElementDescriptor] = {
      expr match {
        case FieldExpression(_, field, receiver) => extractRules(receiver) + AddField(field)
        case id: VariableIdentifier => Set(VariableElement(id))
      }
    }

    override def transformAssignField(left: FieldExpression, right: Expression): SetDescription = {
      this
    }

    override def transformAssignVariable(left: VariableIdentifier, right: Expression): SetDescription = {
      val newConcreteExpressions = concreteExpressions.transform {
        case (_, set) => set.map(expr => expr.transform(e => if (e.equals(left)) right else e))
      }
      val newAbstractExpressions = abstractExpressions
      copy(concreteExpressions = newConcreteExpressions, abstractExpressions = newAbstractExpressions)
    }

    /** Computes the least upper bound of two elements.
      *
      * @param other The other value
      * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
      *         and less than or equal to any other upper bound of the two arguments
      */
    override def lub(other: SetDescription): SetDescription = {
      this
    }

    /** Computes the greatest lower bound of two elements.
      *
      * @param other The other value
      * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
      *         and greater than or equal to any other lower bound of the two arguments
      */
    override def glb(other: SetDescription): SetDescription = throw new UnsupportedOperationException()

    /** Computes the widening of two elements.
      *
      * @param other The new value
      * @return The widening of `this` and `other`
      */
    override def widening(other: SetDescription): SetDescription = {
      copy(widened = true)
    }

    /** Returns true if and only if `this` is less than or equal to `other`.
      *
      * @param other The value to compare
      * @return true if and only if `this` is less than or equal to `other`
      */
    override def lessEqual(other: SetDescription): Boolean = other match {
      case Top => true
      case Bottom => false
      case other: InnerSetDescription =>
        (abstractExpressions.subsetOf(other.abstractExpressions)
          && ((widened && other.widened) || concreteExpressions.forall { case (expr, set) => set.subsetOf(other.concreteExpressions.getOrElse(expr, Set())) }))
      case _ => throw new IllegalStateException()
    }

    /** Checks whether the given domain element is equivalent to bottom.
      *
      * @return bottom
      */
    override def isBottom: Boolean = false

    /** Checks whether the given domain element is equivalent to top.
      *
      * @return bottom
      */
    override def isTop: Boolean = false
  }
}

trait SetDescription extends Lattice[SetDescription] {
  override def factory() = InnerSetDescription()

  override def top() = Top

  override def bottom() = Bottom

  def update: SetDescription = this

  def transformAssignField(left: FieldExpression, right: Expression): SetDescription = this

  def transformAssignVariable(left: VariableIdentifier, right: Expression): SetDescription = this
}

trait SetElementDescriptor {

}

case class RootElement(expr: Expression) extends SetElementDescriptor

case class VariableElement(expr: VariableIdentifier) extends SetElementDescriptor

case class AddField(field: String) extends SetElementDescriptor