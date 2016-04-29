/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, Compiler}
import java.nio.file.Path
import ch.ethz.inf.pm.sample.{AnalysisUnitContext, StringCollector, SystemParameters}
import java.io.File

trait AnalysisRunner[S <: State[S]] {
  val compiler: Compiler

  val analysis: Analysis[S]

  /** Which methods to analyze (by default: all of them). */
  def methodsToAnalyze: List[MethodDeclaration] =
    compiler.allMethods

  def run(path: Path): List[AnalysisResult[S]] = {
    compiler.compileFile(path.toAbsolutePath.toString)
    _run()
  }

  protected def prepareContext() {
    SystemParameters.analysisOutput = new StringCollector
    SystemParameters.progressOutput = new StringCollector
    SystemParameters.resetOutput()

    SystemParameters.isValueDrivenHeapAnalysis = true
    SystemParameters.wideningLimit = 3
    SystemParameters.compiler = compiler

    // Set up native methods
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics())
  }

  protected def _run(): List[AnalysisResult[S]] = {
    prepareContext()
    methodsToAnalyze.map(analysis.analyze)
  }

  def main(args: Array[String]) {
    run(new File(args(0)).toPath)
  }
}

case class AnalysisResult[S <: State[S]](method: MethodDeclaration, cfgState: TrackingCFGState[S]) {}

/** EntryState Builder. Builds analysis entry states for given method declarations.
  *
  * @tparam S the abstract state
  */
trait EntryStateBuilder[S <: State[S]] {
  def topState: S

  def build(method: MethodDeclaration): S = method.initializeArgument[S](topState)
}

/** Analysis.
  *
  * @tparam S the abstract state
  * @author Caterina Urban
  */
trait Analysis[S <: State[S]] {
  def analyze(method: MethodDeclaration): AnalysisResult[S]
}

/** Forward Analysis.
  *
  * @tparam S the abstract state
  */
trait ForwardAnalysis[S <: State[S]] extends Analysis[S] {

  /** Analyzes the given method with a `TrackingForwardInterpreter` starting
    * from the given entry state.
    */
  protected def analyze(method: MethodDeclaration, entryState: S): AnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val interpreter = TrackingForwardInterpreter[S](entryState)
      val cfgState = interpreter.forwardExecute(method.body, entryState)
      AnalysisResult(method, cfgState)
    }
  }

  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }
}

/** Backward Analysis.
  *
  * @tparam S the abstract state
  * @author Caterina Urban
  */
trait BackwardAnalysis[S <: State[S]] extends Analysis[S] {

  protected def analyze(method: MethodDeclaration, exitState: S): AnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val interpreter = TrackingBackwardInterpreter[S](exitState)
      val cfgState = interpreter.backwardExecute(method.body, exitState)
      AnalysisResult(method, cfgState)
    }
  }

  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }
}

/** Forward Analysis Runner with EntryStateBuilder. */
case class SimpleForwardAnalysis[S <: State[S]](entryStateBuilder: EntryStateBuilder[S]) extends ForwardAnalysis[S] {
  def analyze(method: MethodDeclaration): AnalysisResult[S] = analyze(method, entryStateBuilder.build(method))
}

/** Backward Analysis Runner with EntryStateBuilder. */
case class SimpleBackwardAnalysis[S <: State[S]](entryStateBuilder: EntryStateBuilder[S]) extends BackwardAnalysis[S] {
  def analyze(method: MethodDeclaration): AnalysisResult[S] = analyze(method, entryStateBuilder.build(method))
}