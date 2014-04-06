package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.td.analysis.interpreter.ConcreteRunner
import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis.{ReportingParams, TouchAnalysisParameters}

class InterpreterTestSuite extends TouchBoostTestSuite {
  def testDirectories: Seq[String] = Seq("interpreter_tests")

  def runOnFile(file: String): Seq[SampleMessage] = {
    TouchAnalysisParameters.set(touchBoostOptions)
    ConcreteRunner.runScriptFile(file)
  }

}
