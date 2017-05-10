/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import viper.silver.{ast => sil}
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.carbon.CarbonVerifier


trait AbstractAnalysisRunner[S <: State[S]] {
  val compiler: SilverCompiler

  val analysis: SilverAnalysis[S]

  def program: SilverProgramDeclaration = compiler.program

  def run(compilable: Compilable): ProgramResult[S] = {
    compiler.compile(compilable)
    _run()
  }

  protected def prepareContext(): Unit = {
    SystemParameters.analysisOutput = new StringCollector
    SystemParameters.progressOutput = new StringCollector
    SystemParameters.resetOutput()

    SystemParameters.wideningLimit = 3
    //SystemParameters.compiler = compiler

    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodSemantics)
  }

  protected def _run(): ProgramResult[S] = {
    prepareContext()
    analysis.analyze(program)
  }

  def main(args: Array[String]): Unit = {
    run(Compilable.Path(new File(args(0)).toPath))
  }
}

/** Analysis runner for Silver programs. */
trait SilverAnalysisRunner[S <: State[S]]
  extends AbstractAnalysisRunner[S] {
  val compiler = new SilverCompiler()

  /** Runs the analysis on a given Silver program. */
  def run(program: sil.Program): ProgramResult[S] = {
    compiler.compileProgram(program)
    _run()
  }

  /** Runs the analysis on the Silver program whose name is passed as first argument and reports errors and warnings. */
  override def main(args: Array[String]): Unit = {
    run(Compilable.Path(new File(args(0)).toPath)) // run the analysis

    println("\n******************\n* AnalysisResult *\n******************\n")
    if (Reporter.assertionViolations.isEmpty) println("No errors")
    for (e <- Reporter.assertionViolations) {
      println(e)
    } // error report
    println()
    if (Reporter.genericWarnings.isEmpty) println("No warnings")
    for (w <- Reporter.genericWarnings) {
      println(w)
    } // warning report
  }
}

/** Specification Inference for Silver Programs.
  *
  * @author Caterina Urban
  * @tparam T The type of the specification.
  * @tparam S The type of the state.
  */
trait SilverInferenceRunner[T, S <: State[S] with SilverSpecification[T]]
  extends SilverAnalysisRunner[S] with SilverExtender[T, S] {

  /**
    * Extends a Silver program whose name is passed as first argument with
    * specifications inferred by the analysis.
    */
  def extend(args: Array[String]): sil.Program = {
    val results = run(Compilable.Path(new File(args(0)).toPath)) // run the analysis
    // extend the Silver program with inferred permission
    extendProgram(DefaultSilverConverter.prog, results)
  }

  /**
    * Exports a Silver program extended with inferred specifications.
    */
  def export(args: Array[String]): Unit = {
    val program = extend(args) // extend the program with permission inferred by the analysis
    println("\n********************\n* Extended Program *\n********************\n\n" + program)
    // create a file with the extended program
    val outName = args(0).split('.')(0) + "X.sil"
    val pw = new PrintWriter(new File(outName))
    pw.write(program.toString())
    pw.close()
  }

  /** Verifies a Silver program extended with inferred specifications using the Viper symbolic execution backend. */
  def verify(args: Array[String]): Unit = {
    val program = extend(args)
    // extend the program with permission inferred by the analysis
    // verified the extended program with Silicon/Carbon
    //val verifier = new Silicon()
    val verifier = CarbonVerifier()
    verifier.parseCommandLine(Seq("dummy.sil"))
    verifier.start()
    val result: viper.silver.verifier.VerificationResult = verifier.verify(program)
    println("\n***********************\n* Verification Result * " + result + "\n***********************")
  }

  override def main(args: Array[String]) {
    // run the analysis
    val extended = extend(args)

    // report errors and warnings
    println("\n******************\n* AnalysisResult *\n******************\n")
    if (Reporter.assertionViolations.isEmpty) println("No errors")
    for (e <- Reporter.assertionViolations) {
      println(e)
    } // error report
    println()
    if (Reporter.genericWarnings.isEmpty) println("No warnings")
    for (w <- Reporter.genericWarnings) {
      println(w)
    } // warning report

    // report extended program
    println("\n********************\n* Extended Program *\n********************\n\n" + extended)
    // create a file with the extended program
    //val copyName = args(0).split('.')(0) + ".sil.orig"
    //val cw = new PrintWriter(new File(copyName))
    //cw.write(DefaultSilverConverter.prog.toString); cw.close
    val outName = args(0).split('.')(0) + "X.sil"
    val ow = new PrintWriter(new File(outName))
    ow.write(extended.toString())
    ow.close()

    // verify the extended program with Silicon/Carbon
    // val verifier = new Silicon()
    var verifier = CarbonVerifier()
    verifier.parseCommandLine(Seq("dummy.sil"))
    verifier.start()
    val result: viper.silver.verifier.VerificationResult = verifier.verify(extended)
    println("\n***********************\n* Verification Result *\n***********************\n\n" + result)
  }

  override def toString = "Specification Inference"
}
