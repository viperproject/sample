/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain._

/**
  * A heap domain that tracks information about the heap structure.
  *
  * @tparam T The type of the elements of the heap domain.
  * @tparam I The type of the identifiers used by the heap domain.
  * @author Jerome Dohrau
  */
trait HeapDomain[T <: HeapDomain[T, I], I <: Identifier]
  extends Lattice[T] {
  this: T =>

  /**
    * Adds the given variable to the domain.
    *
    * @param variable The variable to add.
    * @return The resulting domain and the corresponding replacement.
    */
  def addVariable(variable: VariableIdentifier): (T, Replacement)

  /**
    * Removes the given variable from the domain.
    *
    * @param variable The variable to remove.
    * @return THe resulting domain and the corresponding replacement.
    */
  def removeVariable(variable: VariableIdentifier): (T, Replacement)

  /**
    * Performs a variable assignment with the given target and value.
    *
    * @param variable   The variable representing the target.
    * @param expression The expression representing the value.
    * @return The resulting domain and the corresponding replacement.
    */
  def assignVariable(variable: VariableIdentifier, expression: Expression): (T, Replacement)

  /**
    * Performs a field assignment with the given target and value.
    *
    * @param target     The target.
    * @param expression The expression representing the value.
    * @return The resulting domain and the corresponding replacement.
    */
  def assignField(target: AccessPathIdentifier, expression: Expression): (T, Replacement)

  /**
    * Havocs the location corresponding to the given expression.
    *
    * @param expression The expression.
    * @return The resulting domain and the corresponding replacement.
    */
  def havoc(expression: Expression): (T, Replacement)

  /**
    * Inhales the given condition.
    *
    * @param condition The condition to be inhaled.
    * @return The resulting domain and the corresponding replacement.
    */
  def inhale(condition: Expression): (T, Replacement)

  /**
    * Exhales the given condition.
    *
    * @param condition The condition to be exhaled.
    * @return The resulting domain and the corresponding replacement.
    */
  def exhale(condition: Expression): (T, Replacement)

  /**
    * Performs an abstract garbage collection by pruning all unreachable heap
    * locations.
    *
    * @return The resulting domain and the corresponding replacement.
    */
  def garbageCollect(): (T, Replacement)

  /**
    * Returns the set of possible values of the given expression.
    *
    * @param expression The expression.
    * @return The set of possible values.
    */
  def getValue(expression: Expression): Set[I]
}