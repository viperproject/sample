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
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState.EdgeStateDomain

class AnalysisRunner[S <: State[S]](analysis: Analysis[S]) {
  def run(path: Path): List[AnalysisResult[_]] = {
    val compiler = new SilCompiler
    compiler.compileFile(path.toAbsolutePath.toString)
    _run(compiler)
  }

  def run(program: sil.Program): List[AnalysisResult[S]] = {
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

    // Experimental
    PredicatesDomain.resetId()
    
    // TODO: Get rid of this ugly hack
    vdha.glbPreservingIdsStrategy = CustomGlbPreservingIdsStrategy

    // Analyze
    methodsToAnalyze(compiler).map(analysis.analyze)
  }

  /** Which methods to analyze (by default: all of them). */
  def methodsToAnalyze(compiler: SilCompiler): List[MethodDeclaration] =
    compiler.allMethods

  def main(args: Array[String]) {
    run(new File(args(0)).toPath)
  }
}

object AnalysisRunner {
  type S = ApronInterface.Default
}

object DefaultAnalysisRunner extends AnalysisRunner(
  SimpleAnalysis[ValueDrivenHeapState.Default[S]](DefaultEntryStateBuilder)) {
  override def toString = "Default Analysis"
}

object PreciseAnalysisRunner extends AnalysisRunner(
  SimpleAnalysis[PreciseValueDrivenHeapState.Default[S]](PreciseEntryStateBuilder)) {
  override def toString = "Precise Analysis"
}

object OnePhasePredicateAnalysisRunner extends AnalysisRunner(
    SimpleAnalysis[PredicateDrivenHeapState[S]](PredicateEntryStateBuilder)) {
  override def toString = "Analysis with Predicates: One-Phase"

  /** Only analyze the first method. */
  override def methodsToAnalyze(compiler: SilCompiler) =
    List(compiler.allMethods.head)
}

object TwoPhasePredicateAnalysisRunner extends AnalysisRunner(
  RefiningPredicateAnalysis[S](PredicateEntryStateBuilder)) {
  override def toString = "Analysis with Predicates: Two-Phase"

  /** Only analyze the first method. */
  override def methodsToAnalyze(compiler: SilCompiler) =
    List(compiler.allMethods.head)
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
  ValueDrivenHeapState.Default[S]] {

  def topState = {
    ValueDrivenHeapState.Default[ApronInterface.Default](topHeapGraph, topApronInterface, ExpressionSet())
  }
}

object PreciseEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PreciseValueDrivenHeapState.EdgeStateDomain[S],
  PreciseValueDrivenHeapState.Default[S]] {

  def topState = {
    val generalValState = PreciseValueDrivenHeapState.makeTopEdgeState(topApronInterface)
    PreciseValueDrivenHeapState.Default(topHeapGraph, generalValState, ExpressionSet())
  }
}

object PredicateEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PredicateDrivenHeapState.EdgeStateDomain[S],
  PredicateDrivenHeapState[S]] {

  def topState = {
    val generalValState = PredicateDrivenHeapState.makeTopEdgeState(topApronInterface)
    PredicateDrivenHeapState(topHeapGraph, generalValState, ExpressionSet())
  }

  override def build(method: MethodDeclaration) = {
    // Very naive support for existing predicate access predicates
    // in the method condition.
    // Mostly experimental.

    val program = SystemParameters.compiler.asInstanceOf[SilCompiler].program
    val silMethod = program.methods.find(_.name == method.name.toString).get

    import PredicateDrivenHeapState._

    var initialState = method.initializeArgument(topState)

    silMethod.pres.foreach({
      case sil.PredicateAccessPredicate(sil.PredicateAccess(args, pred), sil.FullPerm()) =>
        val paramLocalVars = silMethod.formalArgs.map(_.localVar)
        paramLocalVars.find(p => Seq(p) == args) match {
          case Some(paramLocalVar) =>
            val existingPreds = DefaultSilConverter.convert(Seq(pred))
            if (!existingPreds.map.isEmpty) {
              val heap = initialState.abstractHeap
              // Here we can assume that there is a single non-null edge
              // for the parameter variable vertex with a single folded
              // predicate instance that we can merge the existing
              // definition into.
              val freshPredId = heap.outEdges(heap.localVarVertex(paramLocalVar.name))
                .filter(_.target != NullVertex).head.state.predInsts.foldedIds.head
              val repl = new Replacement()
              repl.value += Set(freshPredId, existingPreds.map.keySet.head) -> Set(freshPredId)
              val condHeap = initialState.toCondHeapGraph.map(state => {
                state.transformPreds(_ lub existingPreds).merge(repl)
              })

              initialState = initialState.factory(condHeap.heap, condHeap.cond, ExpressionSet())
            }
          case None =>
        }
    })

    initialState
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

case class RefiningPredicateAnalysis[S <: SemanticDomain[S]](
    entryStateBuilder: EntryStateBuilder[PredicateDrivenHeapState[S]])
  extends Analysis[PredicateDrivenHeapState[S]] {

  type T = PredicateDrivenHeapState[S]

  import PredicateInstanceState.Unfolded

  def analyze(method: MethodDeclaration): AnalysisResult[T] = {
    def preds(state: T) =
      state.generalValState.valueState.predicateState.predicates

    // Rather than building the entry state from scratch,
    // adapt the one of the first iteration.
    // A benefit is that we are sure that predicate instance IDs do not change
    val firstEntryState = entryStateBuilder.build(method)
    val firstResult = analyze(method, firstEntryState)
    val firstExitState = firstResult.cfgState.exitState()
    val firstEntryDefs = preds(firstEntryState)
    val firstExitDefs = preds(firstExitState)
    val secondEntryDefs = firstEntryDefs.copy(map = {
      firstExitDefs.map.filterKeys(firstEntryDefs.map.contains)
    })

    val secondEntryStateCondHeap = CondHeapGraph[EdgeStateDomain[S], T](firstEntryState).mapEdges(edge => {
      var isBottom = false
      val state = edge.state

      // TODO: Find a more concise way of altering the state
      val newState = state.copy(valueState = {
        state.valueState.copy[S](predicateState = {
          state.valueState.predicateState.copy(
            predicates = secondEntryDefs.copy(map = secondEntryDefs.map.filterNot(_._2.isBottom)),
            instances = {
              var instances = state.valueState.predicateState.instances

              // Remove the edge altogether if one of the folded predicate
              // instances has a false body
              for (foldedId <- instances.foldedIds) {
                val predDef = secondEntryDefs.get(foldedId)
                if (predDef.isBottom) {
                  isBottom = true
                } else {
                  val edgeLocalId = EdgeLocalIdentifier(List(edge.field), foldedId)
                  instances = instances.assign(edgeLocalId, Unfolded)
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