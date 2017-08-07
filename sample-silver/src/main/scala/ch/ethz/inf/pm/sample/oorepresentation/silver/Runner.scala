/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.inference.{SilverExporter, SilverExtender, SilverInferenceRunner}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import viper.silver.ast.Program
import viper.silver.{ast => sil}

trait AbstractAnalysisRunner[S <: State[S]] {
  val compiler: SilverCompiler

  val analysis: SilverAnalysis[S]

  def compile(compilable: Compilable): sil.Program =
    compiler.compile(compilable)

  def run(compilable: Compilable): ProgramResult[S] = {
    val program = compiler.compile(compilable)
    run(program)
  }

  def run(program: sil.Program): ProgramResult[S] = {
    val translated = compiler.toSample(program)
    _run(translated)
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

/** Interprocedural Specification Inference for Silver Programs.
  *
  * @author Flurin Rindisbacher
  * @tparam S The type of the state.
  */
trait InterproceduralSilverInferenceRunner[S <: State[S]]
  extends SilverInferenceRunner[S]
    with SilverExtender[S]
    with SilverExporter[S]
    with InterproceduralSilverAnalysisRunner[S] {

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

  override def exportProgram(program: Program, results: ProgramResult[S]): Unit = {
    program.methods.foreach { method =>
      val identifier = SilverIdentifier(method.name)
      resultsToWorkWith = results.getTaggedResults(identifier).map(_._2).toSeq
      exportMethod(method, results.getResult(identifier))
    }
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
  * @tparam S The type of the state.
  */
trait InterproceduralSilverBottomUpInferenceRunner[S <: State[S]]
  extends InterproceduralSilverInferenceRunner[S]
   with InterproceduralSilverBottomUpAnalysisRunner[S]{

  override def extendProgram(program: Program, results: ProgramResult[S]): Program = {
    val extendedMethods = program.methods.map { method =>
      val identifier = SilverIdentifier(method.name)
      val result = results.getResult(identifier)
      resultsToWorkWith = Seq(result)
      extendMethod(method, result)
    }

    // return extended program
    program.copy(methods = extendedMethods)(program.pos, program.info, program.errT)
  }

  override def exportProgram(program: Program, results: ProgramResult[S]): Unit = {
    program.methods.foreach { method =>
      val identifier = SilverIdentifier(method.name)
      val result = results.getResult(identifier)
      resultsToWorkWith = Seq(result)
      exportMethod(method, result)
    }
  }
}