/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.sil

import viper.silver.testing.SilSuite
import viper.silver.verifier.{Success, Verifier}
import viper.silver.{ast => sil}
import viper.silver.ast.Program
import scala.reflect.io.File
import java.nio.file._
//import viper.silicon.Silicon
//
//class SiliconWithInference(private var debugInfo: Seq[(String, Any)] = Nil)
//  extends Silicon {
//
//  // For verification errors, the specification inference is to blame.
//  // Override the verifier name such that one can use `UnexpectedError`
//  // annotations etc. and refer to issues in the Sample issue tracker.
//  override val name: String = "sample"
//
//  /** Extend the given program with inferred specifications and verify it. */
//  override def verify(program: sil.Program) = {
//    val runner = PredicateAnalysisRunner
//    val results = runner.run(program)
//
//    val programExtender = ProgramExtender[ApronInterface.Default]()
//    val extendedProgram = programExtender.extend(program, results)
//
//    assert(isWellFormed(extendedProgram),
//      "the extended program is not well-formed")
//
//    // Silicon sets the log level for logger of the current package to "OFF"
//    // unless specified otherwise using a command line argument.
//    // Since we're not interested in Silicon log messages anyway, that's okay.
//    // However, the original log level should be restored again later.
//    // TODO: Find a more elegant solution.
//    val result = super.verify(extendedProgram)
//
//    result
//  }
//
//  /** Returns whether the given program can be parsed and type-checked.
//    *
//    * Invalid changes to a program are not always caught by the SIL AST.
//    */
//  def isWellFormed(program: sil.Program): Boolean = {
//    val tempFile = File.makeTemp()
//    tempFile.writeAll(program.toString())
//
//    val frontend = new DummySilFrontend
//    frontend.init(DummyVerifier)
//
//    // TODO: Find a better way of turning the File into a Path
//    frontend.reset(Seq(Paths.get(tempFile.toString())))
//
//    frontend.run() == Success
//  }
//}

//class SampleInferenceTestSuite extends SilSuite {
//  def testDirectories = Seq("sil/inference")
//
//  def frontend(verifier: Verifier, files: Seq[Path]) = {
//    val frontend = new DummySilFrontend
//    frontend.init(verifier)
//    frontend.reset(files)
//    frontend
//  }
//
//  def verifiers: Seq[Verifier] = Seq(DummyVerifier) //Seq(createSiliconInstance())
//
//  // Copied from silicon/src/test/scala/SiliconTests.scala
////  private def createSiliconInstance(): Silicon = {
////    val silicon = new SiliconWithInference(Seq(("startedBy", "viper.silicon.SiliconTests")))
////    val args = optionsFromScalaTestConfigMap(configMap) ++ Seq("dummy.sil")
////
////    silicon.parseCommandLine(args)
////    silicon.config.initialize {case _ =>}
////
////    silicon
////  }
//
//  private def optionsFromScalaTestConfigMap(configMap: Map[String, Any]): Seq[String] =
//    configMap.flatMap{case (k, v) => Seq("--" + k, v.toString)}.toSeq
//}

object DummyVerifier extends SimpleVerifier {
  def name = "dummy"

  def verify(program: Program) = Success

  override def start(): Unit = ()

  override def stop(): Unit = ()
}