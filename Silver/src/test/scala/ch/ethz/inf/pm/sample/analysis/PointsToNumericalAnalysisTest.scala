package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Path

import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.permissionanalysis.{PointsToIntervalsAnalysisRunner, PointsToPolyhedraAnalysisRunner, PointsToNumericalAnalysisRunner}
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.silver.ast.{Program, Position}
import viper.silver.frontend.{Frontend, SilFrontend}
import viper.silver.testing.SilSuite
import viper.silver.verifier._

class PointsToIntervalsAnalysisTestSuite extends SilSuite {
  override def testDirectories = Seq("silver/heap/intervals")

  def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilFrontend()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override def buildTestInput(file: Path, prefix: String) = {
    val input = super.buildTestInput(file, prefix)
    input.copy(annotations = input.annotations.filterByKeyIdPrefix("sample"))
  }

  override def verifiers: Seq[Verifier] = Seq(new PointsToIntervalsAnalysisAnalyzer())
}

class PointsToPolyhedraAnalysisTestSuite extends SilSuite {
  override def testDirectories = Seq("silver/heap/polyhedra")

  def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilFrontend()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override def buildTestInput(file: Path, prefix: String) = {
    val input = super.buildTestInput(file, prefix)
    input.copy(annotations = input.annotations.filterByKeyIdPrefix("sample"))
  }

  override def verifiers: Seq[Verifier] = Seq(new PointsToPolyhedraAnalysisAnalyzer())
}

class PointsToIntervalsAnalysisAnalyzer() extends SampleAnalyzer {
  def name: String = "sample"

  def verify(program: Program): VerificationResult = {
    PointsToIntervalsAnalysisRunner.run(program)

    if (Reporter.seenErrors.isEmpty)
      Success
    else {
      Failure(Reporter.seenErrors.map(error => {
        SampleFailure(DefaultSampleConverter.convert(error.pp))
      }).toSeq)
    }
  }

  override def start(): Unit = () //???

  override def stop(): Unit = () //???
}

class PointsToPolyhedraAnalysisAnalyzer() extends SampleAnalyzer {
  def name: String = "sample"

  def verify(program: Program): VerificationResult = {
    PointsToPolyhedraAnalysisRunner.run(program)

    if (Reporter.seenErrors.isEmpty)
      Success
    else {
      Failure(Reporter.seenErrors.map(error => {
        SampleFailure(DefaultSampleConverter.convert(error.pp))
      }).toSeq)
    }
  }

  override def start(): Unit = () //???

  override def stop(): Unit = () //???
}

trait SampleAnalyzer extends Verifier {
  def version: String = "0.1"

  def buildVersion: String = ""

  def copyright: String = ""

  def debugInfo(info: Seq[(String, Any)]): Unit = {}

  def dependencies: Seq[Dependency] = Nil

  def parseCommandLine(args: Seq[String]): Unit = {}
}

case class SampleFailure(pos: Position) extends AbstractError {
  def fullId: String = "sample.assert.failed"

  def readableMessage: String = "the assertion may not hold"
}

final case class DummySilFrontend() extends SilFrontend {
  def createVerifier(fullCmd: String) = ???

  def configureVerifier(args: Seq[String]) = ???
}