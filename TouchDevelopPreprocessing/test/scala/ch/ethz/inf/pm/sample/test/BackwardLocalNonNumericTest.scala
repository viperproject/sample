package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.analysis.ReportingParams
import ch.ethz.inf.pm.td.analysis.DomainParams

class BackwardLocalNonNumericTest extends TouchBoostTestSuite {

  def testDirectories: Seq[String] = Seq("backward_tests/local_nonnumeric")

  def runOnFile(file: String): Seq[SampleMessage] = TouchApronRun.runSingle(file, Some(touchBoostOptions))

  override def touchBoostOptions: TouchAnalysisParameters = {
    val default = TouchAnalysisParameters()
    val customTopFields = default.topFields -- Set("width", "height")
    val customBackward = BackwardParams(enableBackwardAnalysis = true,
      rewriteChainCalls = true)
    val customExecution = ExecutionModelParams(singleExecution = true)
    val customReporting = ReportingParams(silent = false)

    TouchAnalysisParameters(
      execution = customExecution,
      topFields = customTopFields,
      backward = customBackward,
      reporting = customReporting
    )
  }
}
