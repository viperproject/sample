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
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState._
import com.weiglewilczek.slf4s.Logging

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

object SimplePredicateAnalysisRunner extends AnalysisRunner(
    SimpleAnalysis[PredicateDrivenHeapState[S]](PredicateEntryStateBuilder)) {
  override def toString = "Analysis with Predicates: Simple"

  /** Only analyze the first method. */
  override def methodsToAnalyze(compiler: SilCompiler) =
    List(compiler.allMethods.head)
}

object RefiningPredicateAnalysisRunner extends AnalysisRunner(
  RefiningPredicateAnalysis[S](PredicateEntryStateBuilder)) {
  override def toString = "Analysis with Predicates: Refining"

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
      case _ => // Ignore precondition
    })

    initialState
  }
}

case class AnalysisResult[S <: State[S]](method: MethodDeclaration, cfgState: TrackingCFGState[S])

trait Analysis[S <: State[S]] extends Logging {
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
    logger.debug("%d microseconds".format(micros))
    result
  }
}

case class SimpleAnalysis[S <: State[S]](
    entryStateBuilder: EntryStateBuilder[S])
  extends Analysis[S] {

  def analyze(method: MethodDeclaration): AnalysisResult[S] =
    analyze(method, entryStateBuilder.build(method))
}

/** Restarts the analysis whenever the predicates are made stronger. */
case class RefiningPredicateAnalysis[S <: SemanticDomain[S]](
    entryStateBuilder: EntryStateBuilder[PredicateDrivenHeapState[S]])
  extends Analysis[PredicateDrivenHeapState[S]] with Logging {

  type T = PredicateDrivenHeapState[S]

  def analyze(method: MethodDeclaration): AnalysisResult[T] = {
    var initialState = entryStateBuilder.build(method)
    var resultOption: Option[AnalysisResult[T]] = None

    while (resultOption.isEmpty) {
      // Set up the hook
      val initialPreds = initialState.generalValState.preds
      val restartSubscriber = AnalysisRestartSubscriber[S](initialPreds)
      val initialStateWithSubscriber = initialState.subscribe(restartSubscriber)

      try {
        resultOption = Some(analyze(method, initialStateWithSubscriber))
      } catch {
        case AnalysisRestartException(preds) =>
          // Apply the predicates that were present in the stated when
          // the analysis was aborted to the entry state
          initialState = initialState.map(_.transformPreds(_ lub preds))

          logger.info(s"Restarting analysis of method ${method.name}.")
      }
    }

    resultOption.get
  }
}

/** Exception that is thrown so that the analysis is restarted with a more
  * constrained initial state.
  *
  * @param preds the predicates that were present when the analysis was aborted
  */
case class AnalysisRestartException(preds: PredicatesDomain) extends Exception {
  override def toString = "Restart of analysis with refined initial state"
}

/** Subscriber that aborts the analysis when a predicate merge happens during the
  * analysis inside of the `PredicateDrivenHeapState`.
  * @tparam S type of the semantic domain
  */
case class AnalysisRestartSubscriber[S <: SemanticDomain[S]](
    initialPreds: PredicatesDomain)
  extends GhostOpSubscriber[S] {

  def notify(state: PredicateDrivenHeapState[S], event: GhostOpEvent) = event match {
    case event: PredMergeGhostOpEvent =>
      // Only abort the analysis if the merge affects the predicate IDs
      // in the original state
      val affectedPredIds = event.repl.value.flatMap(r => r._1 union r._2).toSet
      if (!affectedPredIds.intersect(initialPreds.ids).isEmpty) {
        val preds = state.generalValState.preds
        throw new AnalysisRestartException(preds)
      }
    case _ =>
  }
}