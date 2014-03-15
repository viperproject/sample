package ch.ethz.inf.pm.sample.oorepresentation.sil

import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha
import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.execution.AbstractCFGState
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.oorepresentation.CFGPosition
import ch.ethz.inf.pm.sample.oorepresentation.CFGPosition
import ch.ethz.inf.pm.sample.oorepresentation.sil.WrappedProgramPoint
import scala.Some
import ch.ethz.inf.pm.sample.oorepresentation.sil.PredicateRegistryBuilder
import ch.ethz.inf.pm.sample.oorepresentation.sil.AnalysisResult
import ch.ethz.inf.pm.sample.oorepresentation.sil.AssertionExtractor
import ch.ethz.inf.pm.sample.abstractdomain.vdha.UnfoldGhostOpEvent
import ch.ethz.inf.pm.sample.abstractdomain.SemanticDomain
import com.weiglewilczek.slf4s.Logging

case class ProgramExtender[S <: ApronInterface[S]]() extends Logging {
  type T = PredicateDrivenHeapState[S]
  type StateType = PredicateDrivenHeapState.EdgeStateDomain[S]

  import PredicateDrivenHeapState._

  def extend(p: sil.Program, results: List[AnalysisResult[T]]): sil.Program = {
    vdha.withGlbPreservingIdsStrategy(CustomGlbPreservingIdsStrategy, () => {
      val methodNameToCfgState = results.map(result =>
        result.method.name.toString -> result.cfgState).toMap

      // Only extend methods for which there is an analysis result
      var (newMethods, newPredicates) = p.methods.map(m => {
        methodNameToCfgState.get(m.name) match {
          case Some(cfgState) => extendMethod(p, m, cfgState)
          case None => (m, Seq())
        }
      }).unzip

      // Ensure that all method calls in the program refer to the extended methods
      newMethods = newMethods.map(_.transform({
        case mc @ sil.MethodCall(m, _, _) =>
          mc.copy(method = newMethods.find(_.name == m.name).get)(mc.pos, mc.info)
      })())

      // Now build the new program
      val result = p.copy(
        methods = newMethods,
        predicates = p.predicates ++ newPredicates.flatten)(p.pos, p.info)

      logger.info(s"Extended Program:\n $result")

      result
    })
  }

  def extendMethod(program: sil.Program, method: sil.Method, cfgState: AbstractCFGState[T]): (sil.Method, Seq[sil.Predicate]) = {
    var entryState = cfgState.entryState()
    var exitState = cfgState.exitState().tryToFoldAllLocalVars()

    // Remove all return variables from the entry state,
    // since we cannot refer to them in the precondition
    val returnVarIds = method.formalReturns.map(DefaultSilConverter.convert).map(_.variable.id)
    entryState = returnVarIds.foldLeft(entryState)(_.removeVariable(_))

    // Remove all local variables from the exit state,
    // since we cannot refer to them in the postcondition
    val localVarIds = method.locals.map(DefaultSilConverter.convert).map(_.variable.id)
    exitState = localVarIds.foldLeft(exitState)(_.removeVariable(_))

    val predRegistry = PredicateRegistryBuilder().build(
      extractedPreds = exitState.generalValState.preds,
      existingSilPreds = program.predicates
    )

    val entryExtractor = AssertionExtractor[S](entryState.toCondHeapGraph, predRegistry)
    val exitExtractor = AssertionExtractor[S](exitState.toCondHeapGraph, predRegistry)

    // Detect unfold operations by hooking into the forward interpretation
    // of every statement in the CFG
    // Assumes that there is a bijection between SIL positions and
    // Sample CFG statements
    var unfoldMap: Map[sil.Position, Seq[UnfoldGhostOpEvent]] = Map.empty
    var foldMap: Map[sil.Position, Seq[FoldGhostOpEvent]] = Map.empty

    for ((block, blockIdx) <- cfgState.cfg.nodes.zipWithIndex) {
      for ((stmt, stmtIdx) <- block.zipWithIndex) {
        val cfgPosition = CFGPosition(blockIdx, stmtIdx)
        val preState = cfgState.preStateAt(cfgPosition)
        val collector = CollectingGhostOpSubscriber[S]()
        val preStateWithCollector = preState.subscribe(collector)

        stmt.forwardSemantics(preStateWithCollector)

        val pos = DefaultSampleConverter.convert(stmt.getPC())
        unfoldMap += pos -> collector.unfoldGhostOps
      }

      if (!block.isEmpty) {
        // TODO: Can currently only detect folds at the end of
        // non-empty blocks, but that is fine, right?
        val lastStmt = block.last
        val lastStmtIdx = block.size - 1
        val cfgPosition = CFGPosition(blockIdx, lastStmtIdx)
        val postState = cfgState.postStateAt(cfgPosition)
        val collector = CollectingGhostOpSubscriber[S]()
        val postStateWithCollector = postState.subscribe(collector)

        // TODO: Currently does not work because predicates
        // may be renamed when heaps are joined. The fold statement
        // should use the new name of the predicate after the merge
        postStateWithCollector.tryToFoldAllLocalVars()

        val pos = DefaultSampleConverter.convert(lastStmt.getPC())
        foldMap += pos -> collector.foldGhostOps
      }
    }

    val newMethod = method.transform()(post = {
      case m: sil.Method =>
        // TODO: At the moment, the existing preconditions are ignored.
        // Some of them will be inferred from the initial state anyway.
        // We would need to ensure that we do not output duplicate access
        // predicates.
        m.copy(
          _pres = entryExtractor.assertionTree.simplify.toExps,
          _posts = exitExtractor.assertionTree.simplify.toExps)(m.pos, m.info)
      case w: sil.While =>
        val pp = DefaultSilConverter.convert(w.cond.pos)
        // Find the loop guard block in the CFG so we can extract
        // a loop invariant from it
        val cfgPositions = cfgState.cfg.nodes.zipWithIndex.flatMap({
          case (stmts, blockIdx) => stmts.zipWithIndex.flatMap({
            case (stmt, stmtIdx) =>
              if (stmt.getPC() == pp) Some(CFGPosition(blockIdx, stmtIdx))
              else None
          })
        })

        assert (cfgPositions.size == 1,
          "there must be exactly one statement for the while condition")

        val cfgPosition = cfgPositions.head
        val state = cfgState.postStateAt(cfgPosition)
        val extractor = AssertionExtractor[S](state.toCondHeapGraph, predRegistry)

        w.copy(invs = w.invs ++ extractor.assertionTree.simplify.toExps)(w.pos, w.info)
      case s: sil.Stmt =>
        // Add unfold statements in front of this statement if necessary
        val newS = unfoldMap.get(s.pos) match {
          case Some(sampleUnfolds) =>
            val unfoldStmts = sampleUnfolds.flatMap(unfold => {
              predRegistry.predAccessPred(unfold.variable, unfold.predicateId) match {
                case Some(predAccessPred) =>
                  Some(sil.Unfold(predAccessPred)(s.pos, InferredInfo))
                case None => None
              }
            })

            sil.Seqn(unfoldStmts :+ s)(s.pos)
          case None => s
        }

        // Add fold statement after this statement if necessary
        foldMap.get(newS.pos) match {
          case Some(sampleFold) =>
            val foldStmts = sampleFold.flatMap(fold => {
              predRegistry.predAccessPred(fold.variable, fold.predicateId) match {
                case Some(predAccessPred) =>
                  Some(sil.Fold(predAccessPred)(newS.pos, InferredInfo))
                case None => None
              }
            })

            sil.Seqn(newS +: foldStmts)(newS.pos)
          case None => newS
        }
    })

    val existingPredNames = program.predicates.map(_.name).toSet
    val newPredicates = predRegistry.predicates.filter(p => !existingPredNames.contains(p.name))

    (newMethod, newPredicates.toSeq)
  }
}

/** Info associated with newly inferred specification expressions. */
object InferredInfo extends sil.SimpleInfo(Seq("Inferred"))