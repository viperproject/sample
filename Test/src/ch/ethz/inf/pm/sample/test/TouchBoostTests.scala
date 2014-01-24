package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.td.domain.TouchApronRun
import ch.ethz.inf.pm.sample.reporting.SampleMessage

class TouchBoostTests extends TouchBoostTestSuite {
  def testDirectories: Seq[String] = Seq("TouchDevelop/automated_tests")

  def runOnFile(file: String): Seq[SampleMessage] = TouchApronRun.runSingle(file)
}
