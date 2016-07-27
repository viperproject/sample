/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Path

import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.permissionanalysis.{PointsToIntervalsAnalysisRunner, PointsToPolyhedraAnalysisRunner, PointsToNumericalAnalysisRunner}
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.silver.ast.{Program, Position}
import viper.silver.frontend.{Frontend, SilFrontend}
import viper.silver.testing.SilSuite
import viper.silver.verifier._

/*
class PointsToIntervalsAnalysisTestSuite extends SilSuite {
  override def testDirectories = Seq("silver/heap/intervals")

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

  override def verifiers: Seq[Verifier] = Seq(new PointsToIntervalsAnalysisAnalyzer())
}

class PointsToPolyhedraAnalysisTestSuite extends SilSuite {
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
        AssertFailure(DefaultSampleConverter.convert(error.pp))
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
        AssertFailure(DefaultSampleConverter.convert(error.pp))
      }).toSeq)
    }
  }

  override def start(): Unit = () //???

  override def stop(): Unit = () //???
}
*/