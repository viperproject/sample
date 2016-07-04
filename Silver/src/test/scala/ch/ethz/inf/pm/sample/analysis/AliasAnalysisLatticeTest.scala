/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.permissionanalysis.{AliasAnalysisEntryState, AliasAnalysisState, DummyRefType}
import ch.ethz.inf.pm.sample.test.LatticeTest

/**
  * Property-based testing of lattice elements for Alias Analysis.
  *
  * @author Caterina Urban
  */
class AliasAnalysisLatticeTest extends LatticeTest[AliasAnalysisState.Default] {
  SystemParameters.typ = DummyRefType
  override def factory: AliasAnalysisState.Default = AliasAnalysisEntryState.topState
}
