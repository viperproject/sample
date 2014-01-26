package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis.TouchApronRun

class TouchBoostTests extends TouchBoostTestSuite {
  def testDirectories: Seq[String] = Seq("TouchDevelop/automated_tests")

  def runOnFile(file: String): Seq[SampleMessage] = TouchApronRun.runSingle(file, Some(touchBoostOptions))
}