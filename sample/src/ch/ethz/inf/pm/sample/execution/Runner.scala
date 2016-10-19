/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation._
import java.nio.file.Path

import ch.ethz.inf.pm.sample.{AnalysisUnitContext, StringCollector, SystemParameters}
import java.io.File

import com.sun.corba.se.impl.orbutil.graph.Graph

trait AnalysisRunner[S <: State[S]] {
  val compiler: Compiler

  val analysis: Analysis[S]

  /** Which methods to analyze (by default: all of them). */
  def methodsToAnalyze: List[MethodDeclaration] = compiler.allMethods

  def run(comp: Compilable): List[AnalysisResult] = {
    compiler.compile(comp)
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
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics)
  }

  protected def _run(): List[AnalysisResult] = {
    prepareContext()
    methodsToAnalyze.map(analysis.analyze)
  }

  def main(args: Array[String]) {
    run(Compilable.Path(new File(args(0)).toPath))
  }
}

trait AnalysisResult {
  def displayName:String
}

trait NodeWithState[S <: State[S]] {

  def state:S

}

case class WeightedGraphAnalysisResult[S, W](name:String, graph: WeightedGraph[S,W]) extends AnalysisResult {
  override def displayName = name
}

case class MethodAnalysisResult[S <: State[S]](method: MethodDeclaration, cfgState: TrackingCFGState[S]) extends AnalysisResult {
  override def displayName = "Method "+method.name
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

  def time[A](a: => A) = {
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

/** Adds a previous analysis result to a state.
  *
  * @tparam S the state used for the previous analysis
  * @tparam T the current state
  */
trait PreviousResult[S <: State[S], T <: State[T]] {

  /** Adds to the current state the result of a previous analysis
    *
    * @param result control flow graph storing the analysis result
    * @return the current state updated with the previous analysis result
    */
  def addPreviousResult(result: TrackingCFGState[S]): T

  private def positionFromPP(cfg: ControlFlowGraph, pp: ProgramPoint): Option[CFGPosition] = {
    var blockIdx = 0
    for (block: List[Statement]  <- cfg.nodes) {
      var stmtIdx = 0
      for (stmt: Statement <- block) {
        if (stmt.getPC() == pp)
          return Some(CFGPosition(blockIdx, stmtIdx))
        stmtIdx = stmtIdx + 1
      }
      blockIdx = blockIdx + 1
    }; None
  }

  /** Retrieves the result of an analysis before a given program point.
    *
    * @param result control flow graph storing the analysis result
    * @param pp given program point
    * @return the state before the given program point
    *   state <-- returned state
    * pp: <-- given program point
    *   state
    */
  def preStateAtPP(result: TrackingCFGState[S], pp: ProgramPoint): S = {
    // retrieve the position in the control flow graph corresponding to the program point
    val position: CFGPosition = positionFromPP(result.cfg, pp).get
    // retrieve the result of the analysis before the program point
    result.preStateAt(position)
  }

  /** Retrieves the result of an analysis after a given program point.
    *
    * @param result control flow graph storing the analysis result
    * @param pp given program point
    * @return the state after the given program point
    *   state
    * pp: <-- given program point
    *   state <-- returned state
    */
  def postStateAtPP(result: TrackingCFGState[S], pp: ProgramPoint): S = {
    // retrieve the position in the control flow graph corresponding to the program point
    val position: CFGPosition = positionFromPP(result.cfg, pp).get
    // retrieve the result of the analysis after the program point
    result.postStateAt(position)
  }

  /** Retrieves the result of an analysis after a given program point.
    *
    * @param result control flow graph storing the analysis result
    * @param pp given program point
    * @return the state after the given program point
    *   state <-- returned state
    * pp:
    *   state
    * pp: <-- given program point
    *   state
    */
  def preStateBeforePP(result: TrackingCFGState[S], pp: ProgramPoint): S = {
    // retrieve the position in the control flow graph corresponding to the program point
    val position: CFGPosition = positionFromPP(result.cfg, pp).get
    // retrieve the position in the control flow graph corresponding to the previous program point
    val previous: CFGPosition = CFGPosition(position.blockIdx, position.stmtIdx - 1)
    // retrieve the result of the analysis before the previous program point
    result.postStateAt(previous)
  }

}

/** Forward + Backward Analysis.
  *
  * @tparam S the abstract forward state
  * @tparam T the abstract backward state
  */
trait ForwardBackwardAnalysis[S <: State[S], T <: State[T] with PreviousResult[S, T]] extends Analysis[T] {

  protected def analyze(method: MethodDeclaration, entryState: S, exitState: T): MethodAnalysisResult[T] = {

    // run the forward analysis and retrieve its result
    val result = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val interpreter = TrackingForwardInterpreter[S](entryState)
      val cfgState = interpreter.forwardExecute(method.body, entryState)
      MethodAnalysisResult(method, cfgState)
    }
    // store the result of the forward analysis in the entry state of the backward analysis
    val exit = exitState.addPreviousResult(result.cfgState)
    // run the backward analysis
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val interpreter = TrackingBackwardInterpreter[T](exit)
      val cfgState = interpreter.backwardExecute(method.body, exit)
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

/** Forward Backward Analysis Runner with EntryStateBuilder */
case class SimpleForwardBackwardAnalysis[S <: State[S], T <: State[T] with PreviousResult[S, T]](fwdBuilder: ForwardEntryStateBuilder[S], bwdBuilder: BackwardEntryStateBuilder[T])
  extends ForwardBackwardAnalysis[S, T] {
  def analyze(method: MethodDeclaration): MethodAnalysisResult[T] = analyze(method, fwdBuilder.build(method), bwdBuilder.build(method))
}