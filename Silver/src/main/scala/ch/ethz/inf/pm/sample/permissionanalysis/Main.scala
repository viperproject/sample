/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.IntegerIntervalAnalysis
import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}

/** Main for the permission inference.
  *
  * @author Lucas Brutschy, Caterina Urban
  */
object Main {

  def main(args: Array[String]) = {

    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()

    IntegerIntervalAnalysis.main(args)
    //PermissionInference.main(args)
  }

}
