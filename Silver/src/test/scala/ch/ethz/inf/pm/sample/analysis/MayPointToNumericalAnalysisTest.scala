/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Path

import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.permissionanalysis.{MayPointToIntervalsAnalysisRunner, MayPointToAPolyhedraAnalysisRunner, PointsToIntervalsAnalysisRunner}
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.silver.ast.{Position, Program}
import viper.silver.frontend.{Frontend, SilFrontend}
import viper.silver.testing.SilSuite
import viper.silver.verifier.{AbstractError, _}

/*
class MayPointToIntervalsAnalysisTest extends SilSuite {
  override def testDirectories = Seq("silver/heap/intervals")

  def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilverFrontend()
    fe.init(verifier); fe.reset(files); fe
  }

  override def buildTestInput(file: Path, prefix: String) = {
    val input = super.buildTestInput(file, prefix)
    input.copy(annotations = input.annotations.filterByKeyIdPrefix("sample"))
  }

  override def verifiers: Seq[Verifier] = Seq(new MayPointToIntervalsAnalyzer())
}

class MayPointToPolyhedraAnalysisTest extends SilSuite {
  override def testDirectories = Seq("silver/heap")

  def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilverFrontend()
    fe.init(verifier); fe.reset(files); fe
  }

  override def buildTestInput(file: Path, prefix: String) = {
    val input = super.buildTestInput(file, prefix)
    input.copy(annotations = input.annotations.filterByKeyIdPrefix("sample"))
  }

  override def verifiers: Seq[Verifier] = Seq(new MayPointToPolyhedraAnalyzer())
}



class MayPointToIntervalsAnalyzer() extends Verifier {
  def name: String = "Sample[MayPointTo+Intervals]"

  def verify(program: Program): VerificationResult = {
    MayPointToIntervalsAnalysisRunner.run(program)
    if (Reporter.seenErrors.isEmpty)
      Success
    else
      Failure(Reporter.seenErrors.map(error => AssertFailure(DefaultSampleConverter.convert(error.pp))).toSeq)
  }

  override def buildVersion: String = ???
  override def copyright: String = ???
  override def debugInfo(info: Seq[(String, Any)]): Unit = ???
  override def dependencies: Seq[Dependency] = ???
  override def parseCommandLine(args: Seq[String]): Unit = ???
  override def start(): Unit = ()
  override def stop(): Unit = ()
  override def version: String = ""
}

class MayPointToPolyhedraAnalyzer() extends Verifier {
  def name: String = "Sample[MayPointTo+Polyhedra]"

  def verify(program: Program): VerificationResult = {
    MayPointToAPolyhedraAnalysisRunner.run(program)
    if (Reporter.seenErrors.isEmpty)
      Success
    else
      Failure(Reporter.seenErrors.map(error => AssertFailure(DefaultSampleConverter.convert(error.pp))).toSeq)
  }

  override def buildVersion: String = ???
  override def copyright: String = ???
  override def debugInfo(info: Seq[(String, Any)]): Unit = ???
  override def dependencies: Seq[Dependency] = ???
  override def parseCommandLine(args: Seq[String]): Unit = ???
  override def start(): Unit = ()
  override def stop(): Unit = ()
  override def version: String = ""
}
*/



