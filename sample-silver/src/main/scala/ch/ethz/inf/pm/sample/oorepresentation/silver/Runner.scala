/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import viper.carbon.CarbonVerifier
import viper.silver.ast.Program
import viper.silver.{ast => sil}

//import viper.silicon.Silicon

trait AbstractAnalysisRunner[S <: State[S]] {
  val compiler: SilverCompiler

  val analysis: SilverAnalysis[S]

  /**
    * Returns the sequence of functions to analyze. By default these are all
    * functions.
    *
    * @return The sequence of functions to analyze.
    */
  def functionsToAnalyze: Seq[SilverFunctionDeclaration] = compiler.allFunctions

  /**
    * Returns the sequence of methods to analyze. By default these are all
    * methods.
    *
    * @return The sequence of methods to analyze.
    */
  def methodsToAnalyze: Seq[SilverMethodDeclaration] = compiler.allMethods

  def run(compilable: Compilable): ProgramResult[S] = {
    compiler.compile(compilable)
    _run(compiler.program)
  }

  def run(program: sil.Program): ProgramResult[S] = {
    compiler.compileProgram(program)
    _run(compiler.program)
  }

  def run(program: SilverProgramDeclaration): ProgramResult[S] = {
    SystemParameters.tm = SilverTypeMap
    _run(program)
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

  protected def _run(program: SilverProgramDeclaration): ProgramResult[S] = {
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

  /** Runs the analysis on the Silver program whose name is passed as first argument and reports errors and warnings. */
  override def main(args: Array[String]): Unit = {
    val result = run(Compilable.Path(new File(args(0)).toPath)) // run the analysis

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
    println("\n***************\n* Entry States *\n***************\n")
    result.identifiers.foreach(ident => println(ident.name + "() -> " + result.getResult(ident).entryState()))
    println("\n***************\n* Exit States *\n***************\n")
    result.identifiers.foreach(ident => println(ident.name + "() -> " + result.getResult(ident).exitState()))
  }
}

/**
  * Interprocedural analysis runner for Silver programs.
  *
  * Methods are added to the worklist according to the callgraph (top down)
  *
  * @author Flurin Rindisbacher
  *
  **/
trait InterproceduralSilverAnalysisRunner[S <: State[S]]
  extends SilverAnalysisRunner[S] {

  override val analysis: InterproceduralSilverAnalysis[S]

  override protected def _run(program: SilverProgramDeclaration): ProgramResult[S] = {
    prepareContext()
    analysis.analyze(program)
  }
}

/**
  * Interprocedural bottom up analysis runner for Silver programs.
  *
  * Methods at the  bottom of the topologically ordered call-graph (callee's) are analyzed first. Then their analysis result is reused for
  * the analysis of the callers
  *
  * @author Flurin Rindisbacher
  *
  **/
trait InterproceduralSilverBottomUpAnalysisRunner[S <: State[S]]
  extends InterproceduralSilverAnalysisRunner[S] {

  override val analysis: BottomUpAnalysis[S]

  override protected def _run(program: SilverProgramDeclaration): ProgramResult[S] = {
    prepareContext()
    // run the analysis starting at the bottom of the topological order
    analysis.analyze(program)
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

/** Interprocedural Specification Inference for Silver Programs.
  *
  * @author Flurin Rindisbacher
  * @tparam T The type of the specification.
  * @tparam S The type of the state.
  */
trait InterproceduralSilverInferenceRunner[T, S <: State[S] with SilverSpecification[T]]
  extends SilverInferenceRunner[T, S] with InterproceduralSilverAnalysisRunner[S] {

  //
  // The SilverExtender usually only works with one CFG
  // But for the interprocedural case we need to access all the available CfgResults
  //
  var resultsToWorkWith: Seq[CfgResult[S]] = Seq()

  /**
    * Extends the given program using all results (meaning for different call-strings) seen in the analysis.
    *
    * @param program The program to extend.
    * @param results The result of the analysis.
    * @return The extended program
    */
  override def extendProgram(program: sil.Program, results: ProgramResult[S]): sil.Program = {
    // extend methods
    val extendedMethods = program.methods.map { method =>
      val identifier = SilverIdentifier(method.name)
      resultsToWorkWith = results.getTaggedResults(identifier).map(_._2).toSeq
      extendMethod(method, results.getResult(identifier))
    }

    // return extended program
    program.copy(methods = extendedMethods)(program.pos, program.info, program.errT)
  }
}

/**
  * Extend the given program using the results of a bottom-up analysis. In bottom-up analysis
  * we're not interested in all CfgResults but only those that are untagged or tagged with an empty call-string.
  * Example:
  * for the recursive function foo() the following CfgResults may exist:
  * (Untagged, Result1)
  * (foo, Result2)
  * (foo.foo, Result3)
  * etc.
  * We're only interested in the Untagged result. Meaning the result that assumes entryState = initial-state.
  *
  * @tparam T The type of the specification.
  * @tparam S The type of the state.
  */
trait InterproceduralSilverBottomUpInferenceRunner[T, S <: State[S] with SilverSpecification[T]]
  extends InterproceduralSilverInferenceRunner[T, S] {
  override def extendProgram(program: Program, results: ProgramResult[S]): Program = {
    val extendedMethods = program.methods.map { method =>
      val identifier = SilverIdentifier(method.name)
      resultsToWorkWith = Seq(results.getResult(identifier))
      extendMethod(method, results.getResult(identifier))
    }

    // return extended program
    program.copy(methods = extendedMethods)(program.pos, program.info, program.errT)
  }
}