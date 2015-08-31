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

case class AnalysisResult[S <: State[S]](
                                          method: MethodDeclaration,
                                          cfgState: TrackingCFGState[S]) {
}

/** Builds analysis entry states for given method declarations. */
trait EntryStateBuilder[S <: State[S]] {
  def topState: S

  def build(method: MethodDeclaration): S =
    method.initializeArgument[S](topState)
}

trait Analysis[S <: State[S]] {
  def analyze(method: MethodDeclaration): AnalysisResult[S]

  /** Analyzes the given method with a `TrackingForwardInterpreter` starting
    * from the given entry state.
    */
  protected def analyze(method: MethodDeclaration, entryState: S): AnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val interpreter = TrackingForwardInterpreter[S](entryState.top())
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

/** Simple analysis that uses an `EntryStateBuilder` to build the entry states
  * for methods to analyze.
  */
case class SimpleAnalysis[S <: State[S]](
                                          entryStateBuilder: EntryStateBuilder[S])
  extends Analysis[S] {

  def analyze(method: MethodDeclaration): AnalysisResult[S] =
    analyze(method, entryStateBuilder.build(method))
}