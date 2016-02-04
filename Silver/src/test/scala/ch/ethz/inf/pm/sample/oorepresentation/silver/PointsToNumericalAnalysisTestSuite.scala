package ch.ethz.inf.pm.sample.oorepresentation.sil

import java.nio.file.Path
import ch.ethz.inf.pm.sample.oorepresentation.sil.DefaultSampleConverter
import ch.ethz.inf.pm.sample.permissionanalysis.{PointsToNumericalAnalysisRunner, PermissionAnalysisRunner}
import viper.silver.frontend.{Frontend, SilFrontend}
import viper.silver.verifier._
import viper.silver.ast.{Position, Program}
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.silver.verifier.Failure
import viper.silver.testing.SilSuite

class PointsToNumericalAnalysisTestSuite extends SilSuite {
  override def testDirectories = Seq("silver/heap")

  def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilverFrontend()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override def buildTestInput(file: Path, prefix: String) = {
    val input = super.buildTestInput(file, prefix)
    input.copy(annotations = input.annotations.filterByKeyIdPrefix("sample"))
  }

  override def verifiers: Seq[Verifier] = Seq(new PointsToNumericalAnalysisVerifier())
}

class PointsToNumericalAnalysisVerifier() extends SampleVerifier {
  def name: String = "samplePointsToNumericalAnalysis"

  def verify(program: Program): VerificationResult = {
    PointsToNumericalAnalysisRunner.run(program)

    if (Reporter.seenErrors.isEmpty)
      Success
    else {
      Failure(Reporter.seenErrors.map(error => {
        SampleAssertFailure(DefaultSampleConverter.convert(error.pp))
      }).toSeq)
    }
  }

  override def start(): Unit = () //???

  override def stop(): Unit = () //???
}