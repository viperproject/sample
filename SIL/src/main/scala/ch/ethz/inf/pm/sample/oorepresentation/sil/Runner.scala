package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.execution.{TrackingForwardInterpreter, TrackingCFGState}
import apron.Polka
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.MethodDeclaration
import ch.ethz.inf.pm.sample.AnalysisUnitContext
import semper.sil.ast.Program
import java.nio.file.Path
import ch.ethz.inf.pm.sample.oorepresentation.sil.AnalysisRunner.S
import java.io.File

case class AnalysisRunner[S <: State[S]](analysis: Analysis[S]) {
  def run(path: Path): List[AnalysisResult[_]] = {
    val compiler = new SilCompiler
    compiler.compileFile(path.toAbsolutePath.toString)
    _run(compiler)
  }

  def run(program: Program): List[AnalysisResult[S]] = {
    val compiler = new SilCompiler
    compiler.compileProgram(program)
    _run(compiler)
  }

  def _run(compiler: SilCompiler): List[AnalysisResult[S]] = {
    SystemParameters.analysisOutput = new StringCollector
    SystemParameters.progressOutput = new StringCollector
    SystemParameters.resetOutput()

    SystemParameters.isValueDrivenHeapAnalysis = true
    SystemParameters.typ = TopType
    SystemParameters.wideningLimit = 3
    SystemParameters.compiler = compiler

    // Set up native methods
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics())

    // Analyze
    compiler.allMethods.map(analysis.analyze)
  }

  def main(args: Array[String]) {
    run(new File(args(0)).toPath)
  }
}

object AnalysisRunner {
  type S = ApronInterface.Default
}

object DefaultAnalysisRunner extends AnalysisRunner(
  Analysis[ValueDrivenHeapState.Default[S]](DefaultEntryStateBuilder)) {
  override def toString = "Default Analysis"
}

object PreciseAnalysisRunner extends AnalysisRunner(
  Analysis[PreciseValueDrivenHeapState.Default[S]](PreciseEntryStateBuilder)) {
  override def toString = "Precise Analysis"
}

trait EntryStateBuilder[S <: State[S]] {
  def topState: S

  def build(method: MethodDeclaration): S =
    method.initializeArgument[S](topState)
}

trait ValueDrivenHeapEntryStateBuilder[
    Q <: SemanticDomain[Q],
    S <: ValueDrivenHeapState[Q, S]]
  extends EntryStateBuilder[S] {

  protected def topApronInterface: ApronInterface.Default =
    ApronInterface.Default(None, new Polka(false), env = Set.empty[Identifier]).top()

  protected def topHeapGraph: HeapGraph[Q] =
    HeapGraph[Q]()
}

object DefaultEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  ApronInterface.Default,
  ValueDrivenHeapState.Default[ApronInterface.Default]] {

  def topState = {
    ValueDrivenHeapState.Default[ApronInterface.Default](topHeapGraph, topApronInterface, ExpressionSet())
  }
}

object PreciseEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PreciseValueDrivenHeapState.EdgeStateDomain[ApronInterface.Default],
  PreciseValueDrivenHeapState.Default[ApronInterface.Default]] {

  def topState = {
    val generalValState = PreciseValueDrivenHeapState.makeTopEdgeState(topApronInterface)
    PreciseValueDrivenHeapState.Default(topHeapGraph, generalValState, ExpressionSet())
  }
}

case class AnalysisResult[S <: State[S]](method: MethodDeclaration, cfgState: TrackingCFGState[S])

case class Analysis[S <: State[S]](entryStateBuilder: EntryStateBuilder[S]) {
  def analyze(method: MethodDeclaration): AnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val entryState = entryStateBuilder.build(method)
      val interpreter = TrackingForwardInterpreter[S](entryState.top())
      val cfgState = interpreter.forwardExecuteFrom(method.body, entryState)
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
