/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain.util

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, Replacement}

import scala.collection.mutable

/**
  * A helper object providing useful methods to construct replacements.
  *
  * @author Jerome Dohrau
  */
object Replacements {
  /**
    * Returns the identity replacement.
    *
    * @return The identity.
    */
  def identity: Replacement = new Replacement()

  /**
    * Returns the replacement that expands the given identifier into the given
    * set of identifier.
    *
    * @param from The identifier to expand.
    * @param to   The set of identifiers to expand int.
    * @return The replacement.
    */
  def expand(from: Identifier, to: Set[Identifier]): Replacement = new Replacement(mutable.Map(Set(from) -> to))

  /**
    * Returns the replacements that removes all identifiers of the given set.
    *
    * @param set The set of identifiers to remove.
    * @return The replacement.
    */
  def remove(set: Set[Identifier]): Replacement = new Replacement(mutable.Map(set -> Set.empty))
}
