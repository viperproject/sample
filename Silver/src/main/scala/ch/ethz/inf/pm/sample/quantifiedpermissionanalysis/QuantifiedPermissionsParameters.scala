/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.IntegerOctagons
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.NumericalAnalysisState.OctagonAnalysisState

/**
  * @author Severin Münger
  *         Added on 09/01/17.
  */
object QuantifiedPermissionsParameters {

  val applyIntegerQuantificationWherePossible = true

  val useHeapAnalysis = true

  val useExpressionsSimplifications = true

  val useSetSimplifications = true

  val includeBranchConditions = true

  val useQESimplifications = true

  /**
    * Depending on which types are defined here, either the Polyhedra domain from the Apron library will be used or
    * the natively supported IntegerOctagon domain for the numerical analysis.
    */

//  type NumericalDomainType = Apron.Polyhedra
//
//  type NumericalStateType = PolyhedraAnalysisState
//
//  type NumericalStateBuilderType = PolyhedraAnalysisEntryState.type
//
//  val numericalStateBuilder = PolyhedraAnalysisEntryState

    type NumericalDomainType = IntegerOctagons

    type NumericalStateType = OctagonAnalysisState

    type NumericalStateBuilderType = OctagonAnalysisEntryState.type

    val numericalStateBuilder = OctagonAnalysisEntryState

}
