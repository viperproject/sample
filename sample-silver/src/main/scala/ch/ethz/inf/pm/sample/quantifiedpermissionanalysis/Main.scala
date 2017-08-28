/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.IntegerOctagonAnalysisState
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.IntegerOctagons
import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}

/**
  * @author Severin Münger
  *         Added on 28.08.17.
  */
object Main {
  def main(args: Array[String]): Unit = {
    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()
    SystemParameters.wideningLimit = 10
    QuantifiedPermissionsAnalysisRunner.main(args)
  }
}

object QuantifiedPermissionsParameters {

  val useShortHelperVariableNames = true

  val useQESimplifications = true

  type NumericalDomainType = IntegerOctagons

  type NumericalStateType = IntegerOctagonAnalysisState

}
