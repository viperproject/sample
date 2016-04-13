/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.SemanticDomain
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._

/**
 *
 * Defines the abstract domains specific to TouchDevelops
 *
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:54 AM
 * 
 */

/**
 * A concrete variant of a domain for touch develop: Numerical with Invalid values.
 */
case class InvalidAnd[T <: SemanticDomain[T]](
    _1: T,
    _2: BooleanInvalidDomainWithSource = BooleanInvalidDomainWithSource())
  extends SemanticWithInvalidDomain[T, BooleanInvalidDomainWithSource, InvalidAnd[T]] {
  override def factory(a:T,b:BooleanInvalidDomainWithSource) = new InvalidAnd(a,b)
}

case class StringsAnd[
    T <: SemanticDomain[T],
    S <: StringDomain[S]](
    _1: T,
    _2: S)
  extends SemanticWithStringDomain[T, S, StringsAnd[T, S]] {
  override def factory(a:T,b:S) = new StringsAnd(a,b)
}
