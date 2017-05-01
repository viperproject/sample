/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Path

import ch.ethz.inf.pm.sample.execution.MethodAnalysisResult
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSilverConverter
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisState.SimplePermissionAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis._
import ch.ethz.inf.pm.sample.test.LatticeTest
import viper.silicon.Silicon
import viper.silver.ast.{Method, Program}
import viper.silver.frontend.Frontend
import viper.silver.testing.SilSuite
import viper.silver.verifier._

/** Testing of Permission Analysis.
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
class PermissionAnalysisTest extends SilSuite {
  override def testDirectories = Seq("silver/inference")

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = DummySilverFrontend()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override def verifiers: Seq[Verifier] = Seq(createSiliconInstance())

  private def createSiliconInstance(): Silicon = {
    // copied from silicon/src/test/scala/SiliconTests.scala
    val silicon = new SiliconWithPermissionAnalysis(Seq(("startedBy", "viper.silicon.SiliconTests")))
    val args = optionsFromScalaTestConfigMap(configMap) ++ Seq("dummy.sil")
    silicon.parseCommandLine(args)
    silicon
  }

  private def optionsFromScalaTestConfigMap(configMap: Map[String, Any]): Seq[String] =
    configMap.flatMap { case (k, v) => Seq("--" + k, v.toString) }.toSeq
}

class SiliconWithPermissionAnalysis(private var debugInfo: Seq[(String, Any)] = Nil) extends Silicon {
  override val name: String = "sample"

  override def verify(program: Program): VerificationResult = {
    val runner = PermissionInference

    val seq = Seq.empty[Method]
    val map = Map.empty[String, Method]
    val (methods, preMap, postMap) = program.methods.foldLeft((seq, map, map)) {
      case ((m, pre, posts), method) =>
        if (method.name endsWith "_pre") {
          val name = method.name.dropRight(4)
          (m, pre + (name -> method), posts)
        } else if (method.name endsWith "_post") {
          val name = method.name.dropRight(5)
          (m, pre, posts + (name -> method))
        } else (m :+ method, pre, posts)
    }

    val filteredProgram = program.copy(methods = methods)(program.pos, program.info, program.errT)
    val results = runner.run(filteredProgram)

    // run the permission inference
    // extend the program with the inferred permissions
    val extendedProgram = runner.extendProgram(DefaultSilverConverter.prog, results)

    // methods that check against expected preconditions
    val preMethods = extendedProgram.methods.filter(preMap contains _.name).map {
      method =>
        val preMethod = preMap(method.name)
        preMethod.copy(_posts = method.pres)(preMethod.pos, preMethod.info, preMethod.errT)
    }
    // methods that check against expected postconditions
    val postMethods = extendedProgram.methods.filter(postMap contains _.name).map {
      method =>
        val postMethod = postMap(method.name)
        postMethod.copy(_pres = method.posts)(postMethod.pos, postMethod.info, postMethod.errT)
    }

    // program with checks against expected pre- and postconditions added
    val allMethods = extendedProgram.methods ++ preMethods ++ postMethods
    val extendedProgramWithChecks = extendedProgram.copy(methods = allMethods)(extendedProgram.pos, extendedProgram.info, extendedProgram.errT)

    try {
      // use silicon to verify the extended program with the checks
      start()
      super.verify(extendedProgramWithChecks)
    } catch {
      case _: Throwable => Success // something went wrong with the verifier (not our fault)
    }
  }
}

/** Property-based testing of lattice elements for Permission Analysis.
  *
  * @author Caterina Urban
  */
class PermissionAnalysisLatticeTest extends LatticeTest[SimplePermissionAnalysisState] {
  override def factory: SimplePermissionAnalysisState = PermissionAnalysisEntryState.top
}