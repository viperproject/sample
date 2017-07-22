/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain.util

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, Replacement, State}
import ch.ethz.inf.pm.sample.domain.util.Substitution.{And, Atom, Identity, Or}

import scala.collection.mutable

/**
  * A substitution is defined recursively and is either the identity, a
  * replacement, the composition of two substitutions, or the least upper bound
  * of two substitutions.
  *
  * NOTE: This class is used to avoid the broken implementation of the
  * composition of replacements [[Replacement.>>]].
  *
  * TODO: Make [[Replacement]] a case class since we use it as arguments to
  * case classes.
  *
  * @author Jerome Dohrau
  */
sealed trait Substitution {
  def compose(other: Substitution): Substitution = (this, other) match {
    case (Identity, _) => other
    case (_, Identity) => this
    case (left, right) => And(left, right)
  }

  def lub(other: Substitution): Substitution = (this, other) match {
    case (Identity, Identity) => Identity
    case (left, right) => Or(left, right)
  }

  def apply[S <: State[S]](state: S): S = this match {
    case Identity => state
    case Atom(replacement) => state.merge(replacement)
    case And(left, right) => right(left(state))
    case Or(left, right) => left(state) lub right(state)
  }
}

object Substitution {
  /**
    * Returns the identity substitution.
    *
    * @return The identity substitution.
    */
  def identity: Substitution = Identity

  /**
    * Returns the substitution that replaces the given identifier with the given
    * set of identifiers.
    *
    * @param from The identifier to replace.
    * @param to   The set of identifiers with which the original identifier is
    *             replaced.
    * @return The substitution.
    */
  def expand(from: Identifier, to: Set[Identifier]): Substitution = Atom(new Replacement(mutable.Map(Set(from) -> to)))

  /**
    * Returns the substitution that removes the given set of identifiers.
    *
    * @param set The set of identifiers to remove.
    * @return The substitution.
    */
  def remove(set: Set[Identifier]): Substitution = Atom(new Replacement(mutable.Map(set -> Set.empty)))

  /**
    * The identity substitution.
    */
  private case object Identity extends Substitution

  /**
    * A substitution wrapping a replacement.
    *
    * @param replacement The replacement.
    */
  private case class Atom(replacement: Replacement) extends Substitution

  /**
    * A substitution that corresponds to the composition of two substitutions.
    *
    * @param left  The first substitution.
    * @param right The second substitution.
    */
  private case class And(left: Substitution, right: Substitution) extends Substitution

  /**
    * A substitution that corresponds to the least upper bound of two
    * substitutions.
    *
    * @param left  The first substitution.
    * @param right The second substitution.
    */
  private case class Or(left: Substitution, right: Substitution) extends Substitution

}