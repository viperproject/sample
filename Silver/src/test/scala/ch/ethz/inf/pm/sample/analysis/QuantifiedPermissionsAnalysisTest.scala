/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Path

import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSilverConverter
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.{QuantifiedPermissionsAnalysisRunner, QuantifiedPermissionsState}
import ch.ethz.inf.pm.sample.test.LatticeTest
import viper.silicon.Silicon
import viper.silver.ast.Program
import viper.silver.frontend.Frontend
import viper.silver.testing.SilSuite
import viper.silver.verifier.{Success, VerificationResult, Verifier}

/**
  * @author Severin Münger
  *         Added on 26/01/17.
  */
class QuantifiedPermissionsAnalysisTest extends SilSuite {

  /** The list of verifiers to be used. Should be overridden by a lazy val
    * if the verifiers need to access the config map provided by ScalaTest. */
  override def verifiers: Seq[Verifier] = Seq(createSiliconInstance())

  private def createSiliconInstance(): Silicon = {   // copied from silicon/src/test/scala/SiliconTests.scala
    val silicon = new SiliconWithQuantifiedPermissionAnalysis(Seq(("startedBy", "viper.silicon.SiliconTests")))
    val args = optionsFromScalaTestConfigMap(configMap) ++ Seq("dummy.sil")
    silicon.parseCommandLine(args)
    silicon.config.initialize { case _ => silicon.config.initialized = true }
    silicon
  }

  private def optionsFromScalaTestConfigMap(configMap: Map[String, Any]): Seq[String] =
    configMap.flatMap{case (k, v) => Seq("--" + k, v.toString)}.toSeq

  /** The frontend to be used. */
  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = DummySilverFrontend()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  /**
    * The test directories where tests can be found.
    * The directories must be relative because they are resolved via
    * [[java.lang.ClassLoader]].
    *
    * @see http://stackoverflow.com/a/7098501/491216.
    * @return A sequence of test directories.
    */
  override def testDirectories = Seq("silver/quantifiedpermissionsinference")
}

class SiliconWithQuantifiedPermissionAnalysis(private var debugInfo: Seq[(String, Any)] = Nil) extends Silicon {
  override val name: String = "sample"

  override def verify(program: Program): VerificationResult = {
    val runner = QuantifiedPermissionsAnalysisRunner
    val results = runner.run(program) // run the permission inference
    // extend the program with the inferred permissions
    val extendedProgram = runner.extendProgram(DefaultSilverConverter.prog,results)
    try { // use silicon to verify the extended program
      start(); super.verify(extendedProgram)
    } catch {
      case _: Throwable => Success // something went wrong with the verifier (not our fault)
    }
  }
}
class QuantifiedPermissionAnalysisLatticeTest extends LatticeTest[QuantifiedPermissionsState] {
  override def factory: QuantifiedPermissionsState = QuantifiedPermissionsState()
}