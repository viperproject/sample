package ch.ethz.inf.pm.td

import ch.ethz.inf.pm.td.domain.TouchApronRun
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

object Main {

  def main(args: Array[String]) {

    val nonOptions = (for (arg <- args) yield {
      arg match {
        case "-json" => TouchAnalysisParameters.printJsonErrorRecords = true; None
        case "-njson" => TouchAnalysisParameters.printJsonErrorRecords = false; None
        case _ => Some(arg)
      }
    }).flatten

    TouchApronRun.main(nonOptions)

  }

}