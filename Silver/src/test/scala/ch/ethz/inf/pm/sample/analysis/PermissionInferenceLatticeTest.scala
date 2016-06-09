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

class MayPointToPolyhedraLatticeTest extends LatticeTest[MayPointToAPolyhedraState] {
  SystemParameters.typ = DummyRefType
  override def factory: MayPointToAPolyhedraState = MayPointToAPolyhedraEntryStateBuilder.topState
}

//class AccessIntervalsLatticeTest extends LatticeTest[AccessIntervalsState] {
//  SystemParameters.typ = DummyRefType
//  override def factory: AccessIntervalsState = AccessIntervalsEntryStateBuilder.topState
//}

//class AccessPolyhedraLatticeTest extends LatticeTest[AccessPolyhedraState] {
//  SystemParameters.typ = DummyRefType
//  override def factory: AccessPolyhedraState = AccessPolyhedraEntryStateBuilder.topState
//}