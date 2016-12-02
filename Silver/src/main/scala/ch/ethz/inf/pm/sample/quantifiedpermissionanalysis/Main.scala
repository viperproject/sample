/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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
object QuantifiedPermissionsAnalysisRunner extends SilverInferenceRunner[QuantifiedPermissionsState2] {
  SystemParameters.isValueDrivenHeapAnalysis = false
  SystemParameters.typ = TopType

  /**
    * Extends a sil.Program with inferred specifications.
    **/
  override def extendProgram(prog: Program, results: List[MethodAnalysisResult[QuantifiedPermissionsState2]]): Program = {
    val tempProg = super.extendProgram(prog, results)
    tempProg.copy(functions = tempProg.functions ++ Context.auxiliaryFunctions.values)(pos = tempProg.pos, info = tempProg.info)
  }

  val analysis = ForwardAndBackwardAnalysis(AliasAnalysisEntryState, NumericalAnalysisEntryState, QuantifiedPermissionsEntryStateBuilder, QuantifiedPermissionsEntryStateBuilder2)
}

case class ForwardAndBackwardAnalysis(aliasAnalysisBuilder: AliasAnalysisStateBuilder[SimpleAliasAnalysisState],
                                      numericalEntryStateBuilder: NumericalAnalysisStateBuilder[Apron.Polyhedra, PolyhedraAnalysisState],
                                      entryStateBuilder: EntryStateBuilder[QuantifiedPermissionsState],
                                      entryStateBuilder2: EntryStateBuilder[QuantifiedPermissionsState2])
  extends Analysis[QuantifiedPermissionsState2] with LazyLogging {

  var loopHeads = Set[Int]()

  def preprocessGraph(cfg: ControlFlowGraph): mutable.LinkedHashSet[Int] = {
    val (loopHeads, flowOrder) = findLoops(ForwardInterpreter.startBlockId, cfg, Set())
    this.loopHeads = loopHeads
    flowOrder
  }

  def findLoops(currentNode: Int, cfg: ControlFlowGraph, visited: Set[Int]): (Set[Int], mutable.LinkedHashSet[Int]) = {
    if (visited.contains(currentNode)) {
      return (Set[Int](currentNode), mutable.LinkedHashSet[Int]())
    }
    val successors: List[Int] = cfg.exitEdges(currentNode).toList.sortWith{
      case ((_, _, Some(false)), _) => true
      case _ => false
    }.map(edge => edge._2)
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

  def analyze(method: MethodDeclaration): MethodAnalysisResult[QuantifiedPermissionsState2] = {


    loopHeads = Set[Int]()

    Context.clearMethodSpecificInfo()

    Context.setProgram(DefaultSilverConverter.prog)

//    val aliasAnalysisResult = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
//      val entryState = aliasAnalysisBuilder.build(method)
//      val interpreter = TrackingForwardInterpreter[SimpleAliasAnalysisState](entryState)
//      val cfgState = interpreter.forwardExecute(method.body, entryState)
//      MethodAnalysisResult(method, cfgState)
//    }
//
//    Context.setAliases(aliasAnalysisResult.cfgState)

    val numericalAnalysisResult = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val entryState = numericalEntryStateBuilder.build(method)
      val interpreter = TrackingForwardInterpreter[PolyhedraAnalysisState](entryState)
      val cfgState: TrackingCFGState[PolyhedraAnalysisState] = interpreter.forwardExecute(method.body, entryState)
      MethodAnalysisResult[PolyhedraAnalysisState](method, cfgState)
    }

    Context.setNumericalInfo[Apron.Polyhedra, PolyhedraAnalysisState](numericalAnalysisResult.cfgState)

    val quantifiedPermissionAnalysisResult = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val entryState = entryStateBuilder.build(method)
      val interpreter = TrackingQPInterpreter(loopHeads, entryState)
      val flowOrder = preprocessGraph(method.body)
      val cfgState = interpreter.simpleBackwardExecute(method.body, flowOrder, entryState)
      MethodAnalysisResult(method, cfgState)
    }

    Context.setFirstRunInfo(quantifiedPermissionAnalysisResult.cfgState)

    val quantifiedPermissionAnalysisResult2 = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val entryState = entryStateBuilder2.build(method)
      val interpreter = TrackingQPInterpreter2(entryState)
      val cfgState = interpreter.backwardExecute(method.body, entryState)
      MethodAnalysisResult(method, cfgState)
    }

    quantifiedPermissionAnalysisResult2
  }
}