package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.{execution, StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import scala.reflect.io.File
import ch.ethz.inf.pm.sample.execution.TrackingCFGState
import apron.Polka
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.MethodDeclaration
import ch.ethz.inf.pm.sample.AnalysisUnitContext
import semper.sil.ast.Program

object AnalysisRunner {
  val analysis = DefaultAnalysis[DefaultValueDrivenHeapState[ApronInterface]](DefaultEntryStateBuilder)
  // val analysis = DefaultAnalysis[DefaultPreciseValueDrivenHeapState[ApronInterface]](PreciseEntryStateBuilder)

  def run(file: File): List[AnalysisResult[_]] = {
    val compiler = new SilCompiler
    compiler.compileFile(file.toAbsolute.toString())
    _run(compiler)
  }

  def run(program: Program): List[AnalysisResult[_]] = {
    val compiler = new SilCompiler
    compiler.compileProgram(program)
    _run(compiler)
  }

  def _run(compiler: SilCompiler): List[AnalysisResult[_]] = {
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

    // Experimental
    ValueDrivenHeapProperty.materializeOnlyAcyclic = false

    // Analyze
    compiler.allMethods.map(analysis.analyze)
  }
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

  protected def topApronInterface: ApronInterface =
    new ApronInterface(None, new Polka(false), env = Set.empty[Identifier]).top()

  protected def topHeapGraph: HeapGraph[Q] =
    new HeapGraph[Q]()
}

object DefaultEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  ApronInterface,
  DefaultValueDrivenHeapState[ApronInterface]] {

  def topState = {
    DefaultValueDrivenHeapState[ApronInterface](topHeapGraph, topApronInterface, ExpressionSet())
  }
}

object PreciseEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  SemanticAndGhostCartesianProductDomain[ApronInterface],
  DefaultPreciseValueDrivenHeapState[ApronInterface]] {

  def topState = {
    val generalValState = SemanticAndGhostCartesianProductDomain(topApronInterface)
    DefaultPreciseValueDrivenHeapState(topHeapGraph, generalValState, ExpressionSet())
  }
}

trait Analysis[S <: State[S]] {
  def analyze(method: MethodDeclaration): AnalysisResult[S]
}

case class AnalysisResult[S <: State[S]](method: MethodDeclaration, cfgState: TrackingCFGState[S])

case class DefaultAnalysis[S <: State[S]](entryStateBuilder: EntryStateBuilder[S]) extends Analysis[S] {
  def analyze(method: MethodDeclaration): AnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val entryState = entryStateBuilder.build(method)
      val cfgState = execution.TrackingForwardInterpreter[S](entryState.top()).forwardExecuteFrom(method.body, entryState)
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
