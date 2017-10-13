/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{IntegerOctagonAnalysisEntryState, IntegerOctagonAnalysisState}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{Apron, IntegerOctagons}
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

  val OVERAPPROXIMATE_FIELD_ACCESSES_IN_SPECIFICATIONS = true

  val ADD_COLLECTED_CONSTRAINTS = true

  val ADD_ALL_INVARIANTS = false

  val numericalEntryStateBuilder = ApronPolyhedraAnalysisEntryState
  type NumericalDomainType = Apron.Polyhedra
  type NumericalStateType = ApronPolyhedraAnalysisState

  /*val numericalEntryStateBuilder = IntegerOctagonAnalysisEntryState
  type NumericalDomainType = IntegerOctagons
  type NumericalStateType = IntegerOctagonAnalysisState*/
}
