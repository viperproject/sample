package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis.{ReportingParams, TouchAnalysisParameters, TouchRun}

class TouchBoostTests extends TouchBoostTestSuite {

  def testDirectories: Seq[String] = Seq("automated_tests")

  def runOnFile(file: String): Seq[SampleMessage] = TouchRun.runSingle(file, Some(touchBoostOptions))

}
