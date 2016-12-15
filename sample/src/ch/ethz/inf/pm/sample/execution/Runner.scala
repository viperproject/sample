/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.{AnalysisUnitContext, StringCollector, SystemParameters}

trait AnalysisRunner[S <: State[S]] {
  val compiler: Compiler

  val analysis: Analysis[S]

  def main(args: Array[String]) {
    run(Compilable.Path(new File(args(0)).toPath))
  }

  def run(comp: Compilable): List[AnalysisResult] = {
    compiler.compile(comp)
    _run()
  }

  protected def _run(): List[AnalysisResult] = {
    prepareContext()
    methodsToAnalyze.map(analysis.analyze)
  }

  /** Which methods to analyze (by default: all of them). */
  def methodsToAnalyze: List[MethodDeclaration] = compiler.allMethods

  protected def prepareContext() {
    SystemParameters.analysisOutput = new StringCollector
    SystemParameters.progressOutput = new StringCollector
    SystemParameters.resetOutput()

    SystemParameters.isValueDrivenHeapAnalysis = true
    SystemParameters.wideningLimit = 3
    SystemParameters.compiler = compiler

    // Set up native methods
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics)
  }
}

trait AnalysisResult {

  /** A decriptive name for the result */
  def displayName:String

  /** A descriptive, unique identifier without special characters */
  def uniqueID:String = displayName.replaceAll("[^a-z0-9A-Z_]*","")
}

trait NodeWithState[S <: State[S]] {

  def state:S

}

case class WeightedGraphAnalysisResult[S, W](name: String, shortName: String, graph: WeightedGraph[S, W]) extends AnalysisResult {
  override def displayName: String = name
}

case class MethodAnalysisResult[S <: State[S]](method: MethodDeclaration, cfgState: TrackingCFGState[S]) extends AnalysisResult {
  override def displayName: String = "Method " + method.name
}

/** Entry State Builder. Builds analysis entry states for given method declarations.
  *
  * @tparam S the abstract state
  */
trait EntryStateBuilder[S <: State[S]] {
  def topState: S
  def build(method: MethodDeclaration): S
}

/** Forward Entry State Builder. Builds forward analysis entry states for given method declarations.
  *
  * @tparam S the abstract state
  */
trait ForwardEntryStateBuilder[S <: State[S]] extends EntryStateBuilder[S] {
  override def build(method: MethodDeclaration): S = method.initializeArgument[S](topState)
}

/** Forward Entry State Builder. Builds forward analysis entry states for given method declarations.
  *
  * @tparam S the abstract state
  */
trait BackwardEntryStateBuilder[S <: State[S]] extends EntryStateBuilder[S] {
  override def build(method: MethodDeclaration): S = topState
}

/** Analysis.
  *
  * @tparam S the abstract state
  * @author Caterina Urban
  */
trait Analysis[S <: State[S]] {
  def analyze(method: MethodDeclaration): MethodAnalysisResult[S]

  def time[A](a: => A): A = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }
}

/** Forward Analysis.
  *
  * @tparam S the abstract state
  */
trait ForwardAnalysis[S <: State[S]] extends Analysis[S] {

  /** Analyzes the given method with a `TrackingForwardInterpreter` starting
    * from the given entry state.
    */
  protected def analyze(method: MethodDeclaration, entryState: S): MethodAnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val interpreter = TrackingForwardInterpreter[S](entryState)
      val cfgState = interpreter.forwardExecute(method.body, entryState)
      MethodAnalysisResult(method, cfgState)
    }
  }
}

/** Backward Analysis.
  *
  * @tparam S the abstract state
  * @author Caterina Urban
  */
trait BackwardAnalysis[S <: State[S]] extends Analysis[S] {

  protected def analyze(method: MethodDeclaration, exitState: S): MethodAnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val interpreter = TrackingBackwardInterpreter[S](exitState)
      val cfgState = interpreter.backwardExecute(method.body, exitState)
      MethodAnalysisResult(method, cfgState)
    }
  }
}

/** Forward Analysis Runner with EntryStateBuilder. */
case class SimpleForwardAnalysis[S <: State[S]](entryStateBuilder: ForwardEntryStateBuilder[S]) extends ForwardAnalysis[S] {
  def analyze(method: MethodDeclaration): MethodAnalysisResult[S] = analyze(method, entryStateBuilder.build(method))
}

/** Backward Analysis Runner with EntryStateBuilder. */
case class SimpleBackwardAnalysis[S <: State[S]](entryStateBuilder: ForwardEntryStateBuilder[S]) extends BackwardAnalysis[S] {
  def analyze(method: MethodDeclaration): MethodAnalysisResult[S] = analyze(method, entryStateBuilder.build(method))
}