package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}

/** Main for the permission inference.
  *
  * @author Lucas Brutschy, Caterina Urban
  */
object Main {

  def main(args:Array[String]) = {

    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()

    //PointsToNumericalAnalysisRunner.main(args)
    PermissionAnalysisRunner.main(args)

    for (r <- Reporter.seenErrors) {
      println(r)
    }
    if (Reporter.seenErrors.isEmpty) {
      println("No errors")
    }

  }

}
