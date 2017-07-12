/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type

/**
  * A domain that keeps track of information about the heep.
  *
  * @tparam T The type of a heap domain element.
  * @tparam I The type of the identifiers used by the domain.
  * @author Jerome Dohrau
  */
trait HeapDomain[T <: HeapDomain[T, I], I <: Identifier]
  extends Lattice[T] {
  this: T =>

  /**
    * The type used to represent sets of heap identifiers.
    */
  type HeapIdentifierSet = SetDomain.Default[I]

  def createVariable(variable: Identifier, typ: Type): (T, Replacement)

  def removeVariable(variable: Identifier): (T, Replacement)

  def assign(target: Identifier, value: Expression): (T, Replacement)

  def assignField(receiver: Identifier, field: String, value: Expression): (T, Replacement)

  def assume(condition: Expression): (T, Replacement)

  def getHeapIdentifiers(receiver: Identifier, field: String): (HeapIdentifierSet, T, Replacement)
}

/**
  * A domain that keeps track of alias information.
  *
  * @tparam T The type of an alias domain element.
  * @tparam L The type of a location.
  * @tparam I The type of the identifiers used by the domain.
  * @author Jerome Dohrau
  */
trait AliasDomain[T <: AliasDomain[T, L, I], L, I <: Identifier]
  extends HeapDomain[T, I] {
  this: T =>

  /**
    * Returns whether the two given locations may alias.
    *
    * @param first  The first location.
    * @param second The second location.
    * @return True if the two given locations may alias.
    */
  def mayAlias(first: L, second: L): Boolean

  /**
    * Returns whether the two given locations must alias.
    *
    * @param first  The first location.
    * @param second The second location.
    * @return True if the two given locations must alias.
    */
  def mustAlias(first: L, second: L): Boolean
}