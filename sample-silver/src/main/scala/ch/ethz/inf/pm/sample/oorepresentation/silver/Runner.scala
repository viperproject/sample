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
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import viper.silver.ast.Program
import viper.silver.{ast => sil}

/**
  * An analysis runner for silver programs.
  *
  * @tparam S The type of the state used by the analysis.
  * @author Jerome Dohrau
  */
trait SilverAnalysisRunner[S <: State[S]] {
  /**
    * The compiler.
    */
  val compiler: SilverCompiler = new SilverCompiler()

  /**
    * The analysis to run.
    */
  val analysis: SilverAnalysis[S]

  /**
    * The main method that runs the analysis with the given arguments.
    *
    * @param arguments The arguments.
    */
  def main(arguments: Array[String]): Unit = {
    require(arguments.nonEmpty, "No file specified")
    val result = run(arguments)
    printEntryStates(result)
    printExitStates(result)
  }

  /**
    * Runs the analysis with the given arguments.
    *
    * @param arguments The arguments.
    * @return The result of the analysis.
    */
  def run(arguments: Array[String]): ProgramResult[S] = {
    val compilable = compile(arguments(0))
    run(compilable)
  }

  /**
    * Runs the analysis with the given compilable as input.
    *
    * @param compilable The compilable.
    * @return The result of the analysis.
    */
  def run(compilable: Compilable): ProgramResult[S] = {
    val program = compiler.compile(compilable)
    run(program)
  }

  /**
    * Runs the analysis with the given program as input.
    *
    * @param program The program.
    * @return The result of the analysis.
    */
  def run(program: sil.Program): ProgramResult[S] = {
    val translated = compiler.toSample(program)
    run(translated)
  }

  /**
    * Runs the analysis with the given program as input.
    *
    * @param program The program.
    * @return The result of the analysis.
    */
  def run(program: SilverProgramDeclaration): ProgramResult[S] = {
    prepareContext()
    analysis.analyze(program)
  }

  def compile(file: String): sil.Program = {
    val path = new File(file).toPath
    val compilable = Compilable.Path(path)
    compile(compilable)
  }

  def compile(compilable: Compilable): sil.Program =
    compiler.compile(compilable)

  protected def prepareContext(): Unit = {
    SystemParameters.tm = SilverTypeMap
    SystemParameters.analysisOutput = new StringCollector
    SystemParameters.progressOutput = new StringCollector
    SystemParameters.wideningLimit = 3

    SystemParameters.resetOutput()
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodSemantics)
  }

  def printHeader(header: String): Unit = {
    println("--------------------------------------------------")
    println(header)
    println("--------------------------------------------------")
  }

  /**
    * Prints all entry states of the given analysis result.
    *
    * @param result The result of the analysis.
    */
  def printEntryStates(result: ProgramResult[S]): Unit = {
    printHeader("Entry States")
    result.identifiers.foreach { identifier =>
      val entry = result.getResult(identifier).entryState()
      println(s"$identifier: $entry")
    }
  }

  /**
    * Prints all exit states of the given analysis result.
    *
    * @param result The result of the analysis.
    */
  def printExitStates(result: ProgramResult[S]): Unit = {
    printHeader("Exit States")
    result.identifiers.foreach { identifier =>
      val exit = result.getResult(identifier).exitState()
      println(s"$identifier: $exit")
    }
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
      resultsToWorkWith = results.getTaggedResults(identifier).values.toSeq
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
  * @tparam S The type of the state.
  */
trait InterproceduralSilverBottomUpInferenceRunner[S <: State[S]]
  extends InterproceduralSilverInferenceRunner[S] {

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

  protected override def exportProgram(program: Program, results: ProgramResult[S]): Unit = {
    program.methods.foreach { method =>
      val identifier = SilverIdentifier(method.name)
      val result = results.getResult(identifier)
      resultsToWorkWith = Seq(result)
      exportMethod(method, result)
    }
  }
}