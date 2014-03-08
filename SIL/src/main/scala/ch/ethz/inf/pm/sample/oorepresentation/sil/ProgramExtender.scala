package ch.ethz.inf.pm.sample.oorepresentation.sil

import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.{CondHeapGraph, PredicateDrivenHeapState}
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

    val newMethod = method.transform()(post = {
      case m: sil.Method =>
        m.copy(
          _pres = entryExtractor.assertionTree.simplify.toExps,
          _posts = exitExtractor.assertionTree.simplify.toExps)(m.pos, m.info)
      case w: sil.While =>
        val pp = DefaultSilConverter.convert(w.cond.pos)
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
    })

    val predicates = exitExtractor.predicates

    (newMethod, predicates)
  }
}
