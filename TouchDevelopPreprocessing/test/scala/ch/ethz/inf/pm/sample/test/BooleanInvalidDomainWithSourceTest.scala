/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td.domain.{BooleanInvalidDomainWithSource, InvalidExpression, ValidExpression}

/**
  * Does NOT have most precise assignment, as potentially Valid != Valid
  */
class BooleanInvalidDomainWithSourceTest
  extends SemanticDomainTest[BooleanInvalidDomainWithSource] {

  override lazy val values = super.values ++ Set(
    InvalidExpression(typ, "dummy1", DummyProgramPoint),
    ValidExpression(typ, DummyProgramPoint)
  )

  override lazy val typ = SystemParameters.tm.Int
  override lazy val factory = BooleanInvalidDomainWithSource()
}
