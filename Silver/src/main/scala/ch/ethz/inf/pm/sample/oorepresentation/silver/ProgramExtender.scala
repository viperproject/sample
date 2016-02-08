package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.{vdha}
import ch.ethz.inf.pm.sample.abstractdomain.valueheap._
import ch.ethz.inf.pm.sample.execution.{AnalysisResult, AbstractCFGState}
import ch.ethz.inf.pm.sample.oorepresentation.CFGPosition
import ch.ethz.inf.pm.sample.abstractdomain.valueheap.UnfoldGhostOpEvent

case class ProgramExtender[S <: Apron[S]]() extends LazyLogging {
  type T = PredicateDrivenHeapState[S]

  import PredicateDrivenHeapState._

  /** Extends the given SIL program with specifications extracted from the
    * results of the analysis of that program.
    *
    * @param p the program to extend
    * @param results the analysis results to use
    * @return the extended program
    */
  def extend(p: sil.Program, results: List[AnalysisResult[T]]): sil.Program = {
    vdha.withGlbPreservingIdsStrategy(CustomGlbPreservingIdsStrategy, () => {
      // Only extend methods for which there is an analysis result
      val methodNameToCfgState = results.map(result =>
        result.method.name.toString -> result.cfgState).toMap
      var (newMethods, newPredicates) = p.methods.map(m => {
        methodNameToCfgState.get(m.name) match {
          case Some(cfgState) => extendMethod(p, m, cfgState)
          case None => (m, Seq())
        }
      }).unzip

      // Now build the new program
      val result = p.copy(
        methods = newMethods,
        predicates = p.predicates ++ newPredicates.flatten)(p.pos, p.info)

      logger.info(s"Extended Program:\n $result")

      result
    })
  }

  /** Extends a single method in a SIL program with specifications
    * extracted from the given CFG state.
    *
    * @param program the program that the method is a part of
    *                (to get access to the existing predicates)
    * @param method the method to extend
    * @param cfgState the CFG state for that method
    * @return the extended method and new predicates to be added to the program
    */
  def extendMethod(
      program: sil.Program,
      method: sil.Method,
      cfgState: AbstractCFGState[T]): (sil.Method, Seq[sil.Predicate]) = {
    require(program.methods.contains(method),
      "method does not belong to the given program")

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
      extractedPreds = exitState.generalValState.predDefs,
      existingSilPreds = program.predicates
    )

    val entryExtractor = AssertionExtractor[S](entryState.toCondHeapGraph, predRegistry)
    val exitExtractor = AssertionExtractor[S](exitState.toCondHeapGraph, predRegistry)

    // Detect unfold operations by hooking into the forward interpretation
    // of every statement in the CFG
    var unfoldMap: Map[sil.Position, Seq[UnfoldGhostOpEvent]] = Map.empty

    // Detect fold operations at the end of basic blocks by trying to
    // fold eagerly
    var foldMap: Map[sil.Position, Seq[FoldGhostOpEvent]] = Map.empty

    for ((block, blockIdx) <- cfgState.cfg.nodes.zipWithIndex) {
      for ((stmt, stmtIdx) <- block.zipWithIndex) {
        val cfgPosition = CFGPosition(blockIdx, stmtIdx)
        val preState = cfgState.preStateAt(cfgPosition)
        val collector = GhostOpCollector[S]()
        val preStateWithCollector = preState.subscribe(collector)

        stmt.forwardSemantics(preStateWithCollector)

        val pos = DefaultSampleConverter.convert(stmt.getPC())
        unfoldMap += pos -> collector.unfoldGhostOps
      }

      if (!block.isEmpty) {
        val lastStmt = block.last
        val lastStmtIdx = block.size - 1
        val cfgPosition = CFGPosition(blockIdx, lastStmtIdx)
        val postState = cfgState.postStateAt(cfgPosition)
        val collector = GhostOpCollector[S]()
        val postStateWithCollector = postState.subscribe(collector)

        postStateWithCollector.tryToFoldAllLocalVars()

        val pos = DefaultSampleConverter.convert(lastStmt.getPC())
        foldMap += pos -> collector.foldGhostOps
      }
    }

    // Build a map of predicate aliases from the merge have taken place
    // during the analysis
    val predMergeOps = exitState.ghostOpSubscribers
      .collectFirst({ case c: GhostOpCollector[S] => c }).get.predIdMergeGhostOps

    val predIdAliases: Map[PredicateIdentifier, PredicateIdentifier] =
      predMergeOps.flatMap(predMergeOp => {
      val predIdMerge = predMergeOp.predIdMerge
      (predIdMerge.predIds - predIdMerge.target).map(_ -> predIdMerge.target)
    }).toMap

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
        // When there is a field access in the condition of an if statement,
        // then the ghost operation is associated with the position of the
        // condition rather than the statement. Thus, if the current statement
        // is an if statement, use the position of the condition to check
        // whether there are any ghost operations.
        val ghostOpPos = s match {
          case sil.If(cond, _, _) => cond.pos
          case _ => s.pos
        }

        // Add unfold statements in front of this statement if necessary
        val newS = unfoldMap.get(ghostOpPos) match {
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
        foldMap.get(ghostOpPos) match {
          case Some(sampleFold) =>
            val foldStmts = sampleFold.flatMap(fold => {
              val predId = predIdAliases.getOrElse(fold.predicateId, fold.predicateId)

              predRegistry.predAccessPred(fold.variable, predId) match {
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
    val newPredicates = predRegistry.predicates.filter(p =>
      !existingPredNames.contains(p.name))

    (newMethod, newPredicates.toSeq)
  }
}

/** Info associated with newly inferred specification expressions. */
object InferredInfo extends sil.SimpleInfo(Seq("Inferred"))