/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSilverConverter, SilverInferenceRunner}
import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraph, MethodDeclaration}
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState.SimpleAliasAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.{AliasAnalysisEntryState, AliasAnalysisState, AliasAnalysisStateBuilder}
import ch.ethz.inf.pm.sample.{AnalysisUnitContext, StdOutOutput, SystemParameters}
import com.typesafe.scalalogging.LazyLogging
import viper.silver.ast.{Method, Program}

import scala.collection.mutable

/**
  * @author Severin Münger
  *         Added on 19/10/16.
  */
object Main {
  def main(args: Array[String]): Unit = {
    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()
    SystemParameters.wideningLimit = 10
    QuantifiedPermissionsAnalysisRunner.main(args)
  }
}

/** SIL analysis runner that uses the default QuantifiedPermissions analysis. */
object QuantifiedPermissionsAnalysisRunner extends SilverInferenceRunner[QuantifiedPermissionsState] {
  SystemParameters.isValueDrivenHeapAnalysis = false

  /**
    * Extends a sil.Program with inferred specifications.
    **/
  override def extendProgram(prog: Program, results: List[MethodAnalysisResult[QuantifiedPermissionsState]]): Program = {
    val tempProg = super.extendProgram(prog, results)
    tempProg.copy(functions = tempProg.functions ++ Context.auxiliaryFunctions.values)(pos = tempProg.pos, info = tempProg.info)
  }

  override def extendMethod(method: Method, cfgState: AbstractCFGState[QuantifiedPermissionsState]): Method = {
    Context.loadAliasesForMethod(method.name)
    Context.loadNumericalInfoForMethod(method.name)
    super.extendMethod(method, cfgState)
  }

  val analysis = ForwardAndBackwardAnalysis(AliasAnalysisEntryState, Context.numericalStateBuilder, QuantifiedPermissionsEntryStateBuilder)
}

case class ForwardAndBackwardAnalysis(aliasAnalysisBuilder: AliasAnalysisStateBuilder[SimpleAliasAnalysisState],
                                      numericalEntryStateBuilder: Context.NumericalStateBuilderType,
                                      entryStateBuilder2: EntryStateBuilder[QuantifiedPermissionsState])
  extends Analysis[QuantifiedPermissionsState] with LazyLogging {

  var loopHeads: Set[Int] = Set[Int]()

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

  def analyze(method: MethodDeclaration): MethodAnalysisResult[QuantifiedPermissionsState] = {


    loopHeads = Set[Int]()

    Context.clearMethodSpecificInfo()

    Context.setProgram(DefaultSilverConverter.prog)

    // TODO: just catching the exception is a hack, better perform a search through the program if there exists a field access with a function as receiver
    val aliasAnalysisResult: Option[TrackingCFGState[_ <: AliasAnalysisState[_]]] =
      try {
        Some(SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
          val entryState = aliasAnalysisBuilder.build(method)
          val interpreter = TrackingForwardInterpreter[SimpleAliasAnalysisState](entryState)
          val cfgState = interpreter.forwardExecute(method.body, entryState)
          MethodAnalysisResult(method, cfgState)
        }.cfgState)
      } catch {
        case _: IllegalArgumentException => None
      }

    Context.setAliases(method.name.toString, aliasAnalysisResult)

    val numericalAnalysisResult = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val entryState = numericalEntryStateBuilder.build(method)
      val interpreter = TrackingForwardInterpreter[Context.NumericalStateType](entryState)
      val cfgState: TrackingCFGState[Context.NumericalStateType] = interpreter.forwardExecute(method.body, entryState)
      MethodAnalysisResult[Context.NumericalStateType](method, cfgState)
    }

    Context.setNumericalInfo(method.name.toString, numericalAnalysisResult.cfgState)

    val quantifiedPermissionAnalysisResult2 = SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val entryState = entryStateBuilder2.build(method)
      val interpreter = TrackingQPInterpreter(entryState)
      val cfgState = interpreter.backwardExecute(method.body, entryState)
      MethodAnalysisResult(method, cfgState)
    }

    quantifiedPermissionAnalysisResult2
  }
}