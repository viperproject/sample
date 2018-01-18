/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, NumericalAnalysisState}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution.SilverEntryStateBuilder
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint}

case class ApronPolyhedraAnalysisState(pp: ProgramPoint,
                                       expr: ExpressionSet,
                                       domain: Apron.Polyhedra,
                                       isTop: Boolean,
                                       isBottom: Boolean)
  extends NumericalAnalysisState[ApronPolyhedraAnalysisState, Apron.Polyhedra] {

  override def copy(pp: ProgramPoint,
                    expr: ExpressionSet,
                    domain: Apron.Polyhedra,
                    isTop: Boolean,
                    isBottom: Boolean): ApronPolyhedraAnalysisState ={
    val b = isBottom || (!isTop && domain.isBottom)
    ApronPolyhedraAnalysisState(pp, expr, domain, isTop, b)
  }
}

object ApronPolyhedraAnalysisEntryState
  extends SilverEntryStateBuilder[ApronPolyhedraAnalysisState] {

  override def default: ApronPolyhedraAnalysisState = ApronPolyhedraAnalysisState(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    domain = Apron.Polyhedra.Top.factory(),
    isTop = false,
    isBottom = false
  )
}