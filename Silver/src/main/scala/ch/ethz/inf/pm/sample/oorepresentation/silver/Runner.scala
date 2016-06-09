/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.sample.execution.{AnalysisResult, AnalysisRunner}
import ch.ethz.inf.pm.sample.abstractdomain._
import viper.silver.{ast => sil}
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.silicon.Silicon

/** Analysis runner for Silver programs. */
trait SilverAnalysisRunner[S <: State[S]] extends AnalysisRunner[S] {
  val compiler = new SilCompiler()

  /** Runs the analysis on a given Silver program. */
  def run(program: sil.Program): List[AnalysisResult[S]] = {
    compiler.compileProgram(program)
    _run()
  }

  /** Runs the analysis on the Silver program whose name is passed as first argument and reports errors and warnings. */
  override def main(args: Array[String]): Unit = {
    run(new File(args(0)).toPath) // run the analysis

    println("\n******************\n* AnalysisResult *\n******************\n")
    if (Reporter.seenErrors.isEmpty) println("No errors")
    for (e <- Reporter.seenErrors) { println(e) } // error report
    println()
    if (Reporter.seenInfos.isEmpty) println("No warnings")
    for (w <- Reporter.seenInfos) { println(w) } // warning report
  }
}

/** Specification Inference for Silver Programs.
  *
  * @author Caterina Urban
  */
trait SilverInferenceRunner[S <: State[S] with SilverSpecification]
  extends SilverAnalysisRunner[S] with SilverExtender[S] {

  /** Extends a Silver program whose name is passed as first argument with specifications inferred by the analysis. */
  def extend(args: Array[String]): sil.Program = {
    val results: List[AnalysisResult[S]] = run(new File(args(0)).toPath) // run the analysis
    // extend the Silver program with inferred permission
    extendProgram(DefaultSilverConverter.prog,results)
  }

  /** Exports a Silver program extended with inferred specifications. */
  def export(args: Array[String]): Unit = {
    val program = extend(args) // extend the program with permission inferred by the analysis
    println("\n********************\n* Extended Program *\n********************\n\n" + program)
    // create a file with the extended program
    val outName = args(0).split('.')(0) + "X.sil"
    val pw = new PrintWriter(new File(outName))
    pw.write(program.toString); pw.close
  }

  /** Verifies a Silver program extended with inferred specifications using the Viper symbolic execution backend. */
  def verify(args: Array[String]): Unit = {
    val program = extend(args) // extend the program with permission inferred by the analysis
    // verified the extended program with Silicon
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.config.initialize { case _ => silicon.config.initialized = true }
    silicon.start()
    val result: viper.silver.verifier.VerificationResult = silicon.verify(program)
    println("\n***********************\n* Verification Result * " + result + "\n***********************")
  }

  override def main(args: Array[String]) {
    // run the analysis and report errors and warnings
    val results: List[AnalysisResult[S]] = run(new File(args(0)).toPath)
    println("\n******************\n* AnalysisResult *\n******************\n")
    if (Reporter.seenErrors.isEmpty) println("No errors")
    for (e <- Reporter.seenErrors) { println(e) } // error report
    println()
    if (Reporter.seenInfos.isEmpty) println("No warnings")
    for (w <- Reporter.seenInfos) { println(w) } // warning report

    // extend program with inferred permission
    val extended = extendProgram(DefaultSilverConverter.prog,results)
    println("\n********************\n* Extended Program *\n********************\n\n" + extended)
    // create a file with the extended program
    val outName = args(0).split('.')(0) + "X.sil"
    val pw = new PrintWriter(new File(outName))
    pw.write(extended.toString); pw.close

    // verify the extended program with Silicon
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.config.initialize { case _ => silicon.config.initialized = true }
    silicon.start()
    val result: viper.silver.verifier.VerificationResult = silicon.verify(extended)
    println("\n***********************\n* Verification Result * " + result + "\n***********************")
  }

  override def toString = "Specification Inference"
}
