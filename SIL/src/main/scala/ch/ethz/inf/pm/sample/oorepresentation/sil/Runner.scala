/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, AnalysisResult, SimpleForwardAnalysis, AnalysisRunner, ForwardAnalysis}
import ch.ethz.inf.pm.sample.abstractdomain._
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}
import ch.ethz.inf.pm.sample.oorepresentation.MethodDeclaration
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState._

/** Analysis runner for SIL programs. */
trait SilAnalysisRunner[S <: State[S]] extends AnalysisRunner[S] {
  val compiler = new SilCompiler()

  /** Analyze a program that has already been parsed and type-checked. */
  def run(program: sil.Program): List[AnalysisResult[S]] = {
    compiler.compileProgram(program)
    _run()
  }
}

/** SIL analysis runner that uses the default heap analysis. */
object DefaultAnalysisRunner extends SilAnalysisRunner[ValueDrivenHeapState.Default[Apron.Polyhedra]] {
  val analysis = SimpleForwardAnalysis[ValueDrivenHeapState.Default[Apron.Polyhedra]](DefaultHeapEntryStateBuilder)

  override def toString = "Default Analysis"
}

/** SIL analysis runner that uses the heap analysis
  * with edge disambiguation ghost states.
  */
object PreciseAnalysisRunner extends SilAnalysisRunner[PreciseValueDrivenHeapState.Default[Apron.Polyhedra]] {
  val analysis = SimpleForwardAnalysis[PreciseValueDrivenHeapState.Default[Apron.Polyhedra]](PreciseHeapEntryStateBuilder)

  override def toString = "Precise Analysis"
}

/** SIL analysis runner used for inferring SIL specifications. */
object PredicateAnalysisRunner extends SilAnalysisRunner[PredicateDrivenHeapState[Apron.Polyhedra]] {
  val analysis = PredicateAnalysis[Apron.Polyhedra](ReusingPredicateEntryStateBuilder)

  override def toString = "Predicate Analysis"

  /** Only analyze methods whose name does not begin with 'test'.
    * Test methods contain method calls to ensure that the right method
    * preconditions and postconditions have been inferred.
    */
  override def methodsToAnalyze =
    compiler.allMethods.filterNot(_.name.toString.startsWith("test"))
}

/** Builds the entry state of a method for the predicate analysis. */
trait PredicateEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PredicateDrivenHeapState.EdgeStateDomain[Apron.Polyhedra],
  PredicateDrivenHeapState[Apron.Polyhedra]] {

  def topState = {
    val generalValState = PredicateDrivenHeapState.makeTopEdgeState(emptyApronState)
    PredicateDrivenHeapState(emptyHeapGraph, generalValState, ExpressionSet())
  }
}

object DefaultPredicateEntryStateBuilder extends PredicateEntryStateBuilder

/** Entry state builder with very naïve support for pre-existing
  * predicate access predicates in the method precondition.
  *
  * It only supports method preconditions of the form 'acc(p(param), write)'
  * for a reference parameter 'param' and where predicate 'p' can be
  * represented in our analysis.
  *
  * Experimental.
  */
object ReusingPredicateEntryStateBuilder extends PredicateEntryStateBuilder {
  override def build(method: MethodDeclaration) = {
    // Find the SIL method (requires a look-up in the compiler)
    val program = SystemParameters.compiler.asInstanceOf[SilCompiler].program
    val silMethod = program.methods.find(_.name == method.name.toString).get

    import PredicateDrivenHeapState._

    var initialState = method.initializeArgument(topState)

    silMethod.pres.foreach({
      case sil.PredicateAccessPredicate(sil.PredicateAccess(args, pred), sil.FullPerm()) =>
        val paramLocalVars = silMethod.formalArgs.map(_.localVar)
        paramLocalVars.find(Seq(_) == args) match {
          case Some(paramLocalVar) =>
            val predX = program.findPredicate(pred)
            // Try to convert the predicate
            val existingPreds = DefaultSilConverter.convert(Seq(predX))
            if (existingPreds.map.nonEmpty) {
              val heap = initialState.abstractHeap
              // Find the predicate identifier that created by the analysis
              // for this reference parameter, so the existing definition
              // can be merged into it
              val freshPredId = heap.outEdges(heap.localVarVertex(paramLocalVar.name))
                .filter(_.target != NullVertex).head.state.predInsts.foldedIds.head

              val predIdMerge = PredicateIdentifierMerge(Set(freshPredId, existingPreds.map.keySet.head))
              val condHeap = initialState.toCondHeapGraph.map(state => {
                state.transformPredDefs(_.lub(existingPreds))
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

/** Analysis that infers predicates for SIL methods.
  * Restarts whenever predicated that specify parameters are merged.
  */
case class PredicateAnalysis[S <: SemanticDomain[S]](
    entryStateBuilder: EntryStateBuilder[PredicateDrivenHeapState[S]])
  extends ForwardAnalysis[PredicateDrivenHeapState[S]] with LazyLogging {

  type T = PredicateDrivenHeapState[S]

  def analyze(method: MethodDeclaration): AnalysisResult[T] = {
    PredicateIdentifier.reset()
    PredicateInstanceIdentifier.resetVersion()

    vdha.withGlbPreservingIdsStrategy(CustomGlbPreservingIdsStrategy, () => {
      var initialState = entryStateBuilder.build(method)
      var resultOption: Option[AnalysisResult[T]] = None

      while (resultOption.isEmpty) {
        // Set up the subscriber that triggers the restart
        val initialPreds = initialState.generalValState.predDefs
        val restartSubscriber = AnalysisRestartSubscriber[S](initialPreds)
        val initialStateWithSubscriber = initialState.subscribe(restartSubscriber)

        try {
          resultOption = Some(analyze(method, initialStateWithSubscriber))
        } catch {
          case AnalysisRestartException(preds) =>
            // Apply the predicates that were present in the stated when
            // the analysis was aborted to the entry state
            initialState = initialState.map(_.transformPredDefs(_ lub preds))

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
case class AnalysisRestartException(preds: PredicateDefinitionsDomain) extends Exception {
  override def toString = "Restart of analysis with refined initial state"
}

/** Subscriber that aborts the analysis when a predicate merge happens during the
  * analysis inside of the `PredicateDrivenHeapState`.
 *
  * @tparam S type of the semantic domain
  */
case class AnalysisRestartSubscriber[S <: SemanticDomain[S]](
    initialPreds: PredicateDefinitionsDomain)
  extends GhostOpSubscriber[S] {

  def notify(state: PredicateDrivenHeapState[S], event: GhostOpEvent) = event match {
    case event: PredicateIdentifierMergeEvent =>
      // Only abort the analysis if the merge affects the predicate IDs
      // in the original state
      if (initialPreds.ids.getNonTopUnsafe.intersect(event.predIdMerge.predIds.toSet).nonEmpty) {
        val preds = state.generalValState.predDefs
        throw new AnalysisRestartException(preds)
      }
    case _ =>
  }
}