package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.reporting.{SampleMessage, Reporter}
import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}

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
    PermissionPolyhedraAnalysisRunner.main(args)

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
