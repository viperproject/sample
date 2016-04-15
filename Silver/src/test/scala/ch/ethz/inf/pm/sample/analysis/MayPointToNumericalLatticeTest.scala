/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.permissionanalysis._
import ch.ethz.inf.pm.sample.test.LatticeTest

class MayPointToIntervalsLatticeTest extends LatticeTest[MayPointToIntervalsState] {
  SystemParameters.typ = DummyRefType
  override def factory: MayPointToIntervalsState = MayPointToIntervalsEntryStateBuilder.topState
}

class MayPointToPolyhedraLatticeTest extends LatticeTest[MayPointToPolyhedraState] {
  SystemParameters.typ = DummyRefType
  override def factory: MayPointToPolyhedraState = MayPointToPolyhedraEntryStateBuilder.topState
}
