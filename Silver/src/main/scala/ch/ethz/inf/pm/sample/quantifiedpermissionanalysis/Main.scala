package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraph, MethodDeclaration}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSilverConverter, SilverInferenceRunner, TopType}
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState.SimpleAliasAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.{AliasAnalysisEntryState, AliasAnalysisStateBuilder}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.NumericalAnalysisState.PolyhedraAnalysisState
import ch.ethz.inf.pm.sample.{AnalysisUnitContext, StdOutOutput, SystemParameters}
import com.typesafe.scalalogging.LazyLogging
import viper.silver.ast.Program

import scala.collection.mutable

/**
  * @author Severin Münger
  *         Added on 19/10/16.
  */
object Main {
  def main(args: Array[String]) = {

    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()
    QuantifiedPermissionsAnalysisRunner.main(args)
  }
}

/** SIL analysis runner that uses the default QuantifiedPermissions analysis. */
object QuantifiedPermissionsAnalysisRunner extends SilverInferenceRunner[QuantifiedPermissionsState] {
  SystemParameters.isValueDrivenHeapAnalysis = false
  SystemParameters.typ = TopType

  /**
    * Extends a sil.Program with inferred specifications.
    **/
  override def extendProgram(prog: Program, results: List[MethodAnalysisResult[QuantifiedPermissionsState]]): Program = {
    val tempProg = super.extendProgram(prog, results)
    tempProg.copy(functions = tempProg.functions ++ Context.auxiliaryFunctions.values)(pos = tempProg.pos, info = tempProg.info)
  }

  val analysis = ForwardAndBackwardAnalysis(AliasAnalysisEntryState, NumericalAnalysisEntryState, QuantifiedPermissionsEntryStateBuilder)
}

case class ForwardAndBackwardAnalysis(aliasAnalysisBuilder: AliasAnalysisStateBuilder[SimpleAliasAnalysisState],
                                      numericalEntryStateBuilder: NumericalAnalysisStateBuilder[Apron.Polyhedra, PolyhedraAnalysisState],
                                      entryStateBuilder: EntryStateBuilder[QuantifiedPermissionsState])
  extends Analysis[QuantifiedPermissionsState] with LazyLogging {

  var loopHeads = Set[Int]()

  def preprocessGraph(cfg: ControlFlowGraph): (ControlFlowGraph, mutable.LinkedHashSet[Int]) = {
    val (loopHeads, flowOrder) = findLoops(ForwardInterpreter.startBlockId, cfg, Set())
    this.loopHeads = loopHeads
    (cfg, flowOrder)
  }

  def findLoops(currentNode: Int, cfg: ControlFlowGraph, visited: Set[Int]): (Set[Int], mutable
  .LinkedHashSet[Int]) = {
    if (visited.contains(currentNode)) {
      return (Set[Int](currentNode), mutable.LinkedHashSet[Int]())
    }
    val successors: Set[Int] = cfg.getDirectSuccessors(currentNode)
    var loopHeads = Set[Int]()
    var flowOrder = mutable.LinkedHashSet[Int]()
    for (nextNode <- successors) {
      val (lNodes, fOrder) = findLoops(nextNode, cfg, visited + currentNode)
      loopHeads = loopHeads ++ lNodes
      flowOrder = flowOrder ++ fOrder
    }
    flowOrder += currentNode
    (loopHeads, flowOrder)
  }

  def analyze(method: MethodDeclaration): MethodAnalysisResult[QuantifiedPermissionsState] = {

    Context.setProgram(DefaultSilverConverter.prog)

//    val aliasAnalysisResult = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
//      val entryState = aliasAnalysisBuilder.build(method)
//      val interpreter = TrackingForwardInterpreter[SimpleAliasAnalysisState](entryState)
//      val cfgState = interpreter.forwardExecute(method.body, entryState)
//      MethodAnalysisResult(method, cfgState)
//    }
//
//    Context.setAliases(aliasAnalysisResult.cfgState)

//    val numericalAnalysisResult = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
//      val entryState = numericalEntryStateBuilder.build(method)
//      val interpreter = TrackingForwardInterpreter[PolyhedraAnalysisState](entryState)
//      val cfgState: TrackingCFGState[PolyhedraAnalysisState] = interpreter.forwardExecute(method.body, entryState)
//      MethodAnalysisResult[PolyhedraAnalysisState](method, cfgState)
//    }
//
//    Context.setNumericalInfo[Apron.Polyhedra, PolyhedraAnalysisState](numericalAnalysisResult.cfgState)

    val quantifiedPermissionAnalysisResult = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val entryState = entryStateBuilder.build(method)
      val interpreter = TrackingQPInterpreter(loopHeads, entryState)
      val (cfgWithoutCycles, flowOrder) = preprocessGraph(method.body)
      val cfgState = interpreter.simpleBackwardExecute(cfgWithoutCycles, flowOrder, entryState)
      MethodAnalysisResult(method, cfgState)
    }

    loopHeads = Set[Int]()

    Context.clearMethodSpecificInfo()

    quantifiedPermissionAnalysisResult
  }
}