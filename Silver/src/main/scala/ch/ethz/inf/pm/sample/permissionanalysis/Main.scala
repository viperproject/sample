/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{Apron, BoxedNonRelationalNumericalDomain, DoubleInterval}
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}

object NumDom {
  type I = BoxedNonRelationalNumericalDomain[DoubleInterval]
  type P = Apron.Polyhedra
}

/** Main for the permission inference.
  *
  * @author Lucas Brutschy, Caterina Urban
  */
object Main {

  def main(args:Array[String]) = {

    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()

    //PointsToIntervalsAnalysisRunner.main(args)
    //PointsToPolyhedraAnalysisRunner.main(args)
    //PermissionIntervalsAnalysisRunner.main(args)
    //PermissionPolyhedraAnalysisRunner.main(args)

    //MayPointToPolyhedraAnalysisRunner.main(args)
    AccessPolyhedraAnalysisRunner.main(args)

    //println()
    //for (w <- Reporter.seenInfos) {
    //  println(w)
    //}
    //if (Reporter.seenErrors.isEmpty) println("No errors")
    //for (e <- Reporter.seenErrors) {
    //  println(e)
    //}

  }

}
