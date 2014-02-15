package ch.ethz.inf.pm.sample.oorepresentation.sil

import java.nio.file.Path
import semper.sil.frontend.{Frontend, SilFrontendConfig, SilFrontend}
import semper.sil.verifier._
import semper.sil.ast.{Position, Program}
import ch.ethz.inf.pm.sample.reporting.Reporter
import semper.sil.verifier.Failure
import semper.sil.testing.SilSuite

/**
 * Just a dummy front-end such that we gain easy access to the fully parsed
 * and type-checked SIL program.
 */
class SampleFrontend extends SilFrontend {
  def createVerifier(fullCmd: String): Verifier =
    new SampleVerifier()

  def configureVerifier(args: Seq[String]): SilFrontendConfig =
    new SilFrontendConfig(args, "sample")
}

class SampleVerifier() extends Verifier {
  def name: String = "sample"

  def version: String = "0.1"

  def buildVersion: String = ""

  def copyright: String = ""

  def debugInfo(info: Seq[(String, Any)]): Unit = {}

  def dependencies: Seq[Dependency] = Nil

  def parseCommandLine(args: Seq[String]): Unit = {}

  def verify(program: Program): VerificationResult = {
    AnalysisRunner.run(program)

    // TODO: Maybe check for bottom at the end
    if (Reporter.seenErrors.isEmpty)
      Success
    else {
      Failure(Reporter.seenErrors.map(error => {
        SampleAssertFailed(DefaultSampleConverter.convert(error.pp))
      }).toSeq)
    }
  }
}

case class SampleAssertFailed(pos: Position) extends AbstractError {
  def fullId: String = "sample.assert.failed"

  def readableMessage: String = "the assertion may not hold"
}

class SampleEndToEndTestSuite extends SilSuite {
  def testDirectories = Seq("sil/issues")

  def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new SampleFrontend()
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