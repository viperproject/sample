/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.test.LatticeTest

/**
  * An example of our test infrastructure, implemented for the set domain.
  */
class SetDomainTest extends LatticeTest[SetDomain.Default[Int]] {

  override lazy val factory = SetDomain.Default.Bottom[Int]()

  override lazy val instances: Set[SetDomain.Default[Int]] = super.instances ++ Set(
    SetDomain.Default.Inner(Set(1,2,9)),
    SetDomain.Default.Inner(Set(1,3,10))
  )

}
