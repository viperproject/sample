package ch.ethz.inf.pm.sample.oorepresentation.sil

import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.{UnfoldGhostOp, GhostOpHook, CondHeapGraph, PredicateDrivenHeapState}
import ch.ethz.inf.pm.sample.execution.AbstractCFGState
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.oorepresentation.CFGPosition

case class ProgramExtender[S <: ApronInterface[S]]() {
  type T = PredicateDrivenHeapState[S]
  type StateType = PredicateDrivenHeapState.EdgeStateDomain[S]

  def extend(p: sil.Program, results: List[AnalysisResult[T]]): sil.Program = {
    val methodNameToCfgState = results.map(result =>
      result.method.name.toString -> result.cfgState).toMap

    // Only extend methods for which there is an analysis result
    var (newMethods, newPredicates) = p.methods.map(m => {
      methodNameToCfgState.get(m.name) match {
        case Some(cfgState) => extendMethod(m, cfgState)
        case None => (m, Seq())
      }
    }).unzip

    // Ensure that all method calls in the program refer to the extended methods
    newMethods = newMethods.map(_.transform({
      case mc @ sil.MethodCall(m, _, _) =>
        mc.copy(method = newMethods.find(_.name == m.name).get)(mc.pos, mc.info)
    })())

    // Now build the new program
    p.copy(
      methods = newMethods,
      predicates = p.predicates ++ newPredicates.flatten)(p.pos, p.info)
  }

  def extendMethod(method: sil.Method, cfgState: AbstractCFGState[T]): (sil.Method, Seq[sil.Predicate]) = {
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

    // Use same predicate definitions in entry state as in exit state.
    // TODO: Could just supply predicate definitions separately
    val entryCondHeapGraph = CondHeapGraph[StateType, T](entryState).map(state => {
      state.copy(valueState =
        state.valueState.copy(predicateState = {
          val predState = state.valueState.predicateState
          predState.copy(definitions =
            predState.definitions.lub(
              exitState.generalValState.valueState.predicateState.definitions)
          )
        })
      )
    }).join

    val entryExtractor = AssertionExtractor[S](entryCondHeapGraph)
    val exitExtractor = AssertionExtractor[S](exitState.toCondHeapGraph)

    // Detect unfold operations by hooking into the forward interpretation
    // of every statement in the CFG
    // Assumes that there is a bijection between SIL positions and
    // Sample CFG statements
    var unfoldMap: Map[sil.Position, Seq[UnfoldGhostOp]] = Map.empty

    for ((block, blockIdx) <- cfgState.cfg.nodes.zipWithIndex) {
      for ((stmt, stmtIdx) <- block.zipWithIndex) {
        val cfgPosition = CFGPosition(blockIdx, stmtIdx)
        val preState = cfgState.preStateAt(cfgPosition)
        val hook = new CollectingGhostOpHook
        val preStateWithHook = preState.setGhostOpHook(hook)

        stmt.forwardSemantics(preStateWithHook)

        val pos = stmt.getPC() match {
          case WrappedProgramPoint(p) => p.asInstanceOf[sil.Position]
        }

        unfoldMap += pos -> hook.unfolds
      }
    }

    val newMethod = method.transform()(post = {
      case m: sil.Method =>
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
        val extractor = AssertionExtractor[S](state.toCondHeapGraph)

        w.copy(invs = w.invs ++ extractor.assertionTree.simplify.toExps)(w.pos, w.info)
      case s: sil.Stmt =>
        // Add unfold statements in front of this statement if necessary
        unfoldMap.get(s.pos) match {
          case Some(sampleUnfolds) =>
            val unfoldStmts = sampleUnfolds.flatMap(sampleUnfold => {
              val samplePred = exitExtractor.samplePredDefs.get(sampleUnfold.predicateId)

              if (!samplePred.isShallow) {
                val pred = exitExtractor.predicateRegistry(sampleUnfold.predicateId)
                val localVar = DefaultSampleConverter.convert(sampleUnfold.variable)
                val predAccessPred = sil.PredicateAccessPredicate(
                  sil.PredicateAccess(Seq(localVar), pred)(s.pos), sil.FullPerm()(s.pos))(s.pos)
                Some(sil.Unfold(predAccessPred)(s.pos))
              } else {
                None
              }
            })

            sil.Seqn(unfoldStmts :+ s)(s.pos)
          case None => s
        }
    })

    val predicates = exitExtractor.predicates

    (newMethod, predicates)
  }
}

class CollectingGhostOpHook extends GhostOpHook {
  private[this] var _unfolds: Seq[UnfoldGhostOp] = Seq.empty

  def unfolds = _unfolds

  def onUnfold(unfold: UnfoldGhostOp) = {
    _unfolds = _unfolds :+ unfold
  }
}
