package ch.ethz.inf.pm.sample.oorepresentation.sil

import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.{CondHeapGraph, PredicateDrivenHeapState}
import ch.ethz.inf.pm.sample.abstractdomain.SemanticDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState._
import ch.ethz.inf.pm.sample.execution.AbstractCFGState

// TODO: It's odd to pass the compiler as a constructor argument,
// as it is program-specific
case class ProgramExtender[S <: SemanticDomain[S]](compiler: SilCompiler) {
  type T = PredicateDrivenHeapState[S]
  type StateType = PredicateDrivenHeapState.EdgeStateDomain[S]

  def extend(program: sil.Program, results: List[AnalysisResult[T]]): sil.Program = {
    val methodNameToCfgState = results.map(result =>
      result.method.name.toString -> result.cfgState).toMap

    // Only extend methods for which there is an analysis result
    val result = program.copy(methods = program.methods.map(method => {
        methodNameToCfgState.get(method.name) match {
        case Some(cfgState) => extendMethod(method, cfgState)
        case None => method
      }
    }))(pos = program.pos, info = program.info)

    // Ensure that all method calls in the program refer to the extended methods
    result.copy(methods = result.methods.map(_.transform({
      case mc @ sil.MethodCall(method, _, _) =>
        mc.copy(method = result.methods.find(_.name == method.name).get)(pos = mc.pos, info = mc.info)
    })()))(result.pos, result.info)
  }

  def extendMethod(method: sil.Method, cfgState: AbstractCFGState[T]): sil.Method = {
    val entryState = cfgState.entryState()
    val exitState = cfgState.exitState().tryToFoldAllLocalVars()

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
    val exitCondHeapGraph = CondHeapGraph[EdgeStateDomain[S], PredicateDrivenHeapState[S]](exitState)

    val predicateBuilder = DefaultPredicateBuilder(compiler.refType)
    val entryExtractor = AssertionExtractor[S](entryCondHeapGraph, predicateBuilder)
    val exitExtractor = AssertionExtractor[S](exitCondHeapGraph, predicateBuilder)

    method.copy(
      _pres = entryExtractor.assertions.toSeq,
      _posts = exitExtractor.assertions.toSeq)(
      pos = method.pos,
      info = method.info)
  }
}
