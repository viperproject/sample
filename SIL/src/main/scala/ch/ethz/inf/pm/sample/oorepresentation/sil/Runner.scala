package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.execution.TrackingCFGState
import apron.Polka
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.MethodDeclaration
import java.nio.file.Path
import ch.ethz.inf.pm.sample.oorepresentation.sil.AnalysisRunner.S
import java.io.File
import ch.ethz.inf.pm.sample.execution.TrackingForwardInterpreter
import ch.ethz.inf.pm.sample.abstractdomain.vdha.HeapGraph
import ch.ethz.inf.pm.sample.AnalysisUnitContext
import semper.sil.ast.Program
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState.EdgeStateDomain

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

    NullVertex.typ = compiler.refType

    // Set up native methods
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics())

    // Experimental
    ValueDrivenHeapProperty.materializeOnlyAcyclic = false

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
  SimpleAnalysis[ValueDrivenHeapState.Default[S]](DefaultEntryStateBuilder)) {}

object PreciseAnalysisRunner extends AnalysisRunner(
  SimpleAnalysis[PreciseValueDrivenHeapState.Default[S]](PreciseEntryStateBuilder)) {}

object InitialSymbolicPredicateAnalysisRunner extends AnalysisRunner(
  SimpleAnalysis[PredicateDrivenHeapState[S]](SymbolicPredicateEntryStateBuilder)) {}

object RefiningSymbolicPredicateAnalysisRunner extends AnalysisRunner(
  RefiningSymbolicPredicateAnalysis[S](SymbolicPredicateEntryStateBuilder)) {}

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

object SymbolicPredicateEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PredicateDrivenHeapState.EdgeStateDomain[ApronInterface.Default],
  PredicateDrivenHeapState[ApronInterface.Default]] {

  def topState = {
    val generalValState = PredicateDrivenHeapState.makeTopEdgeState(topApronInterface)
    PredicateDrivenHeapState(topHeapGraph, generalValState, ExpressionSet())
  }
}

case class AnalysisResult[S <: State[S]](method: MethodDeclaration, cfgState: TrackingCFGState[S])

trait Analysis[S <: State[S]] {
  def analyze(method: MethodDeclaration): AnalysisResult[S]

  protected def analyze(method: MethodDeclaration, entryState: S): AnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
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

case class SimpleAnalysis[S <: State[S]](
    entryStateBuilder: EntryStateBuilder[S])
  extends Analysis[S] {

  def analyze(method: MethodDeclaration): AnalysisResult[S] =
    analyze(method, entryStateBuilder.build(method))
}

case class RefiningSymbolicPredicateAnalysis[S <: SemanticDomain[S]](
    entryStateBuilder: EntryStateBuilder[PredicateDrivenHeapState[S]])
  extends Analysis[PredicateDrivenHeapState[S]] {

  type T = PredicateDrivenHeapState[S]

  def analyze(method: MethodDeclaration): AnalysisResult[T] = {
    def defs(state: T) =
      state.generalValState.valueState.predicateState.definitions

    // Rather than building the entry state from scratch,
    // adapt the one of the first iteration.
    // A benefit is that we are sure that predicate instance IDs do not change
    val firstEntryState = entryStateBuilder.build(method)
    val firstResult = analyze(method, firstEntryState)
    val firstExitState = firstResult.cfgState.exitState()
    val firstEntryDefs = defs(firstEntryState)
    val firstExitDefs = defs(firstExitState)
    val secondEntryDefs = firstEntryDefs.copy(map = {
      firstExitDefs.map.filterKeys(firstEntryDefs.map.contains)
    })

    val secondEntryStateCondHeap = CondHeapGraph[EdgeStateDomain[S], T](firstEntryState).map(state => {
      var isBottom = false

      // TODO: Find a more concise way of altering the state
      val newState = state.copy(valueState = {
        state.valueState.copy[S](predicateState = {
          state.valueState.predicateState.copy(
            definitions = secondEntryDefs.copy(map = secondEntryDefs.map.filterNot(_._2.isBottom)),
            instances = {
              import PredicateDefinition._
              import PredicateInstancesDomain._

              var instances = state.valueState.predicateState.instances
              val foldedInstIds = instances.certainlyFoldedIds
              val nonRecursiveDefIds = secondEntryDefs.nonRecursiveIds

              // Auto-unfold every folded, non-recursive predicate instance
              // and remove the edge altogether if one of the folded predicate
              // instances has a false body
              for (foldedInstId <- foldedInstIds) {
                val predDefId = extractPredInstId(foldedInstId).toPredicateDefId
                if (nonRecursiveDefIds.contains(predDefId)) {
                  val predDef = secondEntryDefs.get(predDefId)
                  if (predDef.isBottom) {
                    isBottom = true
                  } else {
                    instances = instances.assign(foldedInstId, Unfolded)
                  }
                }
              }

              instances
            })
        })
      })

      if (isBottom) newState.bottom()
      else newState
    }).prune

    val secondEntryState = firstEntryState.copy(
      abstractHeap = secondEntryStateCondHeap.heap,
      generalValState = secondEntryStateCondHeap.cond)

    val secondResult = analyze(method, secondEntryState)
    secondResult
  }
}