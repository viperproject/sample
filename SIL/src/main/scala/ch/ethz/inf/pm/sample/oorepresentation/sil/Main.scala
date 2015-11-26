package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}

/**
  * @author Lucas Brutschy
  */
object Main {

  def main(args:Array[String]) = {

    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()
    DefaultAnalysisRunner.main(args)

    for (r <- Reporter.seenErrors) {
      println(r)
    }

    if (Reporter.seenErrors.isEmpty) {
      println("No errors")
    }

  }

}
