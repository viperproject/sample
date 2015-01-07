package ch.ethz.inf.pm.sample.oorepresentation.sil

import java.nio.file.Path
import viper.silver.frontend.{Frontend, SilFrontendConfig, SilFrontend}
import viper.silver.verifier._
import viper.silver.ast.{Position, Program}
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.silver.verifier.Failure
import viper.silver.testing.SilSuite

class SampleAnalysisTestSuite extends SilSuite {
  def testDirectories = Seq("sil/issues", "sil/translation")

  def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilFrontend()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  def verifiers: Seq[Verifier] = Seq(new SampleVerifier())

  override def buildTestInput(file: Path, prefix: String) = {
    val input = super.buildTestInput(file, prefix)
    input.copy(annotations = input.annotations.filterByKeyIdPrefix("sample"))
  }
}

/**
 * Just a dummy front-end such that we gain easy access to the fully parsed
 * and type-checked SIL program.
 */
final case class DummySilFrontend() extends SilFrontend {
  def createVerifier(fullCmd: String) = ???

  def configureVerifier(args: Seq[String]) = ???
}

trait SimpleVerifier extends Verifier {
  def version: String = "0.1"

  def buildVersion: String = ""

  def copyright: String = ""

  def debugInfo(info: Seq[(String, Any)]): Unit = {}

  def dependencies: Seq[Dependency] = Nil

  def parseCommandLine(args: Seq[String]): Unit = {}
}

class SampleVerifier() extends SimpleVerifier {
  def name: String = "sample"

  def verify(program: Program): VerificationResult = {
    PreciseAnalysisRunner.run(program)

    // TODO: Maybe check for bottom at the end
    if (Reporter.seenErrors.isEmpty)
      Success
    else {
      Failure(Reporter.seenErrors.map(error => {
        SampleAssertFailed(DefaultSampleConverter.convert(error.pp))
      }).toSeq)
    }
  }

  override def start(): Unit = ???

  override def stop(): Unit = ???
}

case class SampleAssertFailed(pos: Position) extends AbstractError {
  def fullId: String = "sample.assert.failed"

  def readableMessage: String = "the assertion may not hold"
}