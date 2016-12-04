/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, Lattice, VariableIdentifier}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.SetDescription.{Bottom, InnerSetDescription, Top}

/**
  * @author Severin Münger
  *         Added on 03.12.16.
  */
object SetDescription {
  case object Top extends SetDescription with Lattice.Top[SetDescription]

  case object Bottom extends SetDescription with Lattice.Bottom[SetDescription]

  case class InnerSetDescription() extends SetDescription {
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
    override def widening(other: SetDescription): SetDescription = throw new UnsupportedOperationException()

    /** Returns true if and only if `this` is less than or equal to `other`.
      *
      * @param other The value to compare
      * @return true if and only if `this` is less than or equal to `other`
      */
    override def lessEqual(other: SetDescription): Boolean = other match {
      case Top => true
      case Bottom => false
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
}

trait SetElementDescriptor {

}

case class RootElement(expr: Expression) extends SetElementDescriptor

case class VariableElement(expr: VariableIdentifier) extends SetElementDescriptor

case class AddField(field: String) extends SetElementDescriptor

case class IfFieldAddField(existing: String, addAlso: String) extends SetElementDescriptor