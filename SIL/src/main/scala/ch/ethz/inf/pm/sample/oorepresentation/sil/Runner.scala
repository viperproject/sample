package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, AnalysisResult, SimpleAnalysis, AnalysisRunner, Analysis}
import ch.ethz.inf.pm.sample.abstractdomain._
import semper.sil.{ast => sil}
import com.weiglewilczek.slf4s.Logging
import ch.ethz.inf.pm.sample.oorepresentation.MethodDeclaration
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState._

trait SilAnalysisRunner[S <: State[S]] extends AnalysisRunner[S] {
  val compiler = new SilCompiler()

  def run(program: sil.Program): List[AnalysisResult[S]] = {
    compiler.compileProgram(program)
    _run()
  }
}

object AnalysisRunner {
  type S = ApronInterface.Default
}

object DefaultAnalysisRunner extends SilAnalysisRunner[ValueDrivenHeapState.Default[ApronInterface.Default]] {
  val analysis = SimpleAnalysis[ValueDrivenHeapState.Default[ApronInterface.Default]](DefaultHeapEntryStateBuilder)

  override def toString = "Default Analysis"
}

object PreciseAnalysisRunner extends SilAnalysisRunner[PreciseValueDrivenHeapState.Default[ApronInterface.Default]] {
  val analysis = SimpleAnalysis[PreciseValueDrivenHeapState.Default[ApronInterface.Default]](PreciseHeapEntryStateBuilder)

  override def toString = "Precise Analysis"
}

object PredicateAnalysisRunner extends SilAnalysisRunner[PredicateDrivenHeapState[ApronInterface.Default]] {
  val analysis = PredicateAnalysis[ApronInterface.Default](PredicateEntryStateBuilder)

  override def toString = "Predicate Analysis"

  /** Only analyze the first method. */
  override def methodsToAnalyze =
    List(compiler.allMethods.head)
}

object PredicateEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PredicateDrivenHeapState.EdgeStateDomain[ApronInterface.Default],
  PredicateDrivenHeapState[ApronInterface.Default]] {

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

              val predIdMerge = PredicateIdentifierMerge(Set(freshPredId, existingPreds.map.keySet.head))
              val condHeap = initialState.toCondHeapGraph.map(state => {
                state.transformPreds(_.lub(existingPreds))
              })

              initialState = initialState.factory(condHeap.heap, condHeap.cond, ExpressionSet())
              initialState = initialState.mergePredicates(predIdMerge)
            }
          case None =>
        }
      case _ => // Ignore precondition
    })

    initialState
  }
}

/** Restarts the analysis whenever the predicates are made stronger. */
case class PredicateAnalysis[S <: SemanticDomain[S]](
    entryStateBuilder: EntryStateBuilder[PredicateDrivenHeapState[S]])
  extends Analysis[PredicateDrivenHeapState[S]] with Logging {

  type T = PredicateDrivenHeapState[S]

  def analyze(method: MethodDeclaration): AnalysisResult[T] = {
    // Experimental
    PredicateIdentifier.reset()
    PredicateInstanceIdentifier.resetVersion()

    vdha.withGlbPreservingIdsStrategy(CustomGlbPreservingIdsStrategy, () => {
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
    })
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
    case event: PredicateIdentifierMergeEvent =>
      // Only abort the analysis if the merge affects the predicate IDs
      // in the original state
      if (!initialPreds.ids.intersect(event.predIdMerge.predIds.toSet).isEmpty) {
        val preds = state.generalValState.preds
        throw new AnalysisRestartException(preds)
      }
    case _ =>
  }
}