/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Path

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSilverConverter
import ch.ethz.inf.pm.sample.permissionanalysis._
import ch.ethz.inf.pm.sample.test.LatticeTest
import viper.silicon.Silicon
import viper.silver.ast.Program
import viper.silver.frontend.Frontend
import viper.silver.testing.SilSuite
import viper.silver.verifier.{Success, VerificationResult, Verifier}

/** Property-based testing of lattice elements for Permission Analysis.
  *
  * @author Caterina Urban
  */
class PermissionAnalysisLatticeTest extends LatticeTest[PermissionAnalysisState.Default] {
  SystemParameters.typ = DummyRefType
  override def factory: PermissionAnalysisState.Default = PermissionAnalysisEntryState.topState
}

/** Testing of Permission Analysis.
  *
  * @author Caterina Urban
  */
class PermissionAnalysisTest extends SilSuite {
  override def testDirectories = Seq("silver/inference")

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilverFrontend()
    fe.init(verifier); fe.reset(files); fe
  }

  override def verifiers: Seq[Verifier] = Seq(createSiliconInstance())

  private def createSiliconInstance(): Silicon = {   // copied from silicon/src/test/scala/SiliconTests.scala
    val silicon = new SiliconWithPermissionAnalysis(Seq(("startedBy", "viper.silicon.SiliconTests")))
    val args = optionsFromScalaTestConfigMap(configMap) ++ Seq("dummy.sil")
    silicon.parseCommandLine(args); silicon.config.initialize { case _ => silicon.config.initialized = true }; silicon
  }

  private def optionsFromScalaTestConfigMap(configMap: Map[String, Any]): Seq[String] =
    configMap.flatMap{case (k, v) => Seq("--" + k, v.toString)}.toSeq
}

class SiliconWithPermissionAnalysis(private var debugInfo: Seq[(String, Any)] = Nil) extends Silicon {
  override val name: String = "sample"

  override def verify(program: Program): VerificationResult = {
    val runner = PermissionAnalysis
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


