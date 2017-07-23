/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.domain.util.Substitution

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
    * Creates an element of the heap domain with the given fields.
    *
    * @param fields The fields
    * @return The element of the heap domain.
    */
  def factory(fields: Seq[Identifier]): T

  /**
    * Adds the given variable to the domain.
    *
    * @param variable The variable to add.
    * @return The resulting domain and the corresponding substitution.
    */
  def createVariable(variable: VariableIdentifier): (T, Substitution)

  /**
    * Removes the given variable from the domain.
    *
    * @param variable The variable to remove.
    * @return THe resulting domain and the corresponding substitution.
    */
  def removeVariable(variable: VariableIdentifier): (T, Substitution)

  /**
    * Performs a variable assignment with the given target and value.
    *
    * @param variable   The variable representing the target.
    * @param expression The expression representing the value.
    * @return The resulting domain and the corresponding substitution.
    */
  def assignVariable(variable: VariableIdentifier, expression: Expression): (T, Substitution)

  /**
    * Performs a field assignment with the given target and value.
    *
    * @param target     The target.
    * @param expression The expression representing the value.
    * @return The resulting domain and the corresponding substitution.
    */
  def assignField(target: AccessPathIdentifier, expression: Expression): (T, Substitution)

  /**
    * Havocs the location corresponding to the given expression.
    *
    * @param expression The expression.
    * @return The resulting domain and the corresponding substitution.
    */
  def havoc(expression: Expression): (T, Substitution)

  /**
    * Inhales the given condition.
    *
    * @param condition The condition to be inhaled.
    * @return The resulting domain and the corresponding substitution.
    */
  def inhale(condition: Expression): (T, Substitution)

  /**
    * Exhales the given condition.
    *
    * @param condition The condition to be exhaled.
    * @return The resulting domain and the corresponding substitution.
    */
  def exhale(condition: Expression): (T, Substitution)

  /**
    * Performs an abstract garbage collection by pruning all unreachable heap
    * locations.
    *
    * @return The resulting domain and the corresponding substitution.
    */
  def garbageCollect(): (T, Substitution)

  /**
    * Returns the set of possible values of the given expression.
    *
    * @param expression The expression.
    * @return The set of possible values.
    */
  def getValue(expression: Expression): Set[I]

  /**
    * Returns the set of all heap locations.
    *
    * @return The set of all heap locations.
    */
  def locations: Set[I]
}