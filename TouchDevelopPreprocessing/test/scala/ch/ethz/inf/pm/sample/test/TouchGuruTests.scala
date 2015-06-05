package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis.TouchRun

class TouchGuruTests extends TouchGuruTestSuite {

  def testDirectories: Seq[String] = Seq("automated_tests")

  def runOnFile(file: String): Seq[SampleMessage] = {
    val res = TouchRun.runSingle(file, Some(touchGuruOptions))
    assert(!TouchRun.threadFailed)
    res
  }

}
