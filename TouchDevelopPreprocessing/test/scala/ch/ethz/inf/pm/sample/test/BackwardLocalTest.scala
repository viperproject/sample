package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.analysis.ReportingParams
import ch.ethz.inf.pm.td.analysis.DomainParams

class BackwardLocalTest extends TouchBoostTestSuite {

  def testDirectories: Seq[String] = Seq("backward_tests/local")

  def runOnFile(file: String): Seq[SampleMessage] = TouchApronRun.runSingle(file, Some(touchBoostOptions))

  override def touchBoostOptions: TouchAnalysisParameters = {
    val default = TouchAnalysisParameters()
    val customReporting = ReportingParams(
      reportNoncriticalParameterBoundViolations = true,
      reportNumericalErrors = true,
      silent = false)
    val customDomain = DomainParams(numericalDomain = NumericDomainChoice.StrictPolyhedra)
    val customTopFields = default.topFields -- Set("width", "height")
    val customBackward = BackwardParams(enableBackwardAnalysis = true)
    val customExecution = ExecutionModelParams(singleExecution = true)//, contextSensitiveInterproceduralAnalysis = true)

    TouchAnalysisParameters(
      execution = customExecution,
      reporting = customReporting,
      domains = customDomain,
      topFields = customTopFields,
      backward = customBackward
    )
  }
}
