/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}

object NumDom {
  type I = BoxedNonRelationalNumericalDomain[DoubleInterval]
  type IO = IntegerOctagons
  type DO = DoubleOctagons
}

/** Main for the permission inference.
  *
  * @author Lucas Brutschy, Caterina Urban
  */
object Main {

  def main(args:Array[String]) = {

    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()

    //MayPointToOctagonsAnalysisRunner.main(args)
    //MayPointToAOctagonsAnalysisRunner.main(args)
    //MayPointToAPolyhedraAnalysisRunner.main(args)

    // AliasAnalysis.main(args)
    PermissionInference.main(args)

    //println("\n******************\n* AnalysisResult *\n******************\n")
    //if (Reporter.seenErrors.isEmpty) println("No errors")
    //for (e <- Reporter.seenErrors) { println(e) } // error report
    //println()
    //if (Reporter.seenInfos.isEmpty) println("No warnings")
    //for (w <- Reporter.seenInfos) { println(w) } // warning report

  }

}
