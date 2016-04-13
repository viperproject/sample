
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.SkipSemantics
import ch.ethz.inf.pm.td.defsemantics.Default_TApp_Logger

/**
 * Customizes the abstract semantics of App Logger
 *
 * A custom logger
 *
 * @author Lucas Brutschy
 */

object TApp_Logger extends Default_TApp_Logger {

  override lazy val member_log = super.member_log.copy(semantics = SkipSemantics)
  override lazy val member_debug = super.member_debug.copy(semantics = SkipSemantics)

}
          
