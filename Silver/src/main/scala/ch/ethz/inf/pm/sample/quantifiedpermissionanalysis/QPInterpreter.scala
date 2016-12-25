/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Severin Münger
  *         Added on 29.11.16.
  */
trait QPInterpreter extends Interpreter[QuantifiedPermissionsState] with LazyLogging {

  var blocksLastInLoop: Set[Int] = Set()
  var nestingLevels: Map[Int, Int] = Map()
  var sequenceNumbers: Map[Int, Int] = Map()
  var currentSequenceNumbers: Map[Int, Int] = Map()

  def determineBlockTypes(cfg: ControlFlowGraph, nestingLevel: Int = 0, blockIndex: Int = ForwardInterpreter.startBlockId, visited: Set[Int] = Set()): Boolean = {
    if (visited.contains(blockIndex)) {
      blocksLastInLoop += blockIndex
      true
    } else {
      nestingLevels += blockIndex -> nestingLevel
      val sequenceNumber = currentSequenceNumbers.getOrElse(nestingLevel, -1) + 1
      sequenceNumbers += blockIndex -> sequenceNumber
      currentSequenceNumbers += nestingLevel -> sequenceNumber
      val exitEdges = cfg.exitEdges(blockIndex)
      exitEdges.size match {
        case 2 =>
          val ((_, toTrue, _), (_, toFalse, _)) = exitEdges.head match {
            case (_, _, Some(true)) => (exitEdges.head, exitEdges.last)
            case (_, _, Some(false)) => (exitEdges.last, exitEdges.head)
            case _ => throw new IllegalStateException()
          }
          if (cfg.initialBlockInLoop(blockIndex)) {
            determineBlockTypes(cfg, nestingLevel + 1, toTrue, visited + blockIndex)
            determineBlockTypes(cfg, nestingLevel, toFalse, visited + blockIndex)
          } else {
            determineBlockTypes(cfg, nestingLevel + 1, toTrue, visited + blockIndex)
            determineBlockTypes(cfg, nestingLevel + 1, toFalse, visited + blockIndex)
          }
        case 1 =>
          val successor = cfg.getDirectSuccessors(blockIndex).head
          determineBlockTypes(cfg, if (nestingLevel > 0 && cfg.entryEdges(successor).size > 1) nestingLevel - 1 else nestingLevel, successor, visited + blockIndex)
        case 0 =>
          false
      }
    }
  }

  def backwardExecute(cfg: ControlFlowGraph, finalState: QuantifiedPermissionsState): TrackingCFGState[QuantifiedPermissionsState] = {
    determineBlockTypes(cfg)
    val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState](finalState)
    val cfgState = cfgStateFactory.allBottom(cfg)
    val leavesIds: Set[Int] = cfg.getLeavesIds
    val ordering: Ordering[Int] = Ordering.by(blockIndex => (-nestingLevels(blockIndex), -sequenceNumbers(blockIndex)))
    var blocksToProcessIds: mutable.SortedSet[Int] = mutable.SortedSet[Int]()(ordering) ++ leavesIds
    var iterationAtBlock = Map[Int, Int]()
    while (blocksToProcessIds.nonEmpty) {
      val currentId: Int = blocksToProcessIds.head; blocksToProcessIds.remove(currentId)
      val currentCount: Int = iterationAtBlock.getOrElse(currentId, 0)
      val exitEdges = cfg.exitEdges(currentId)
      val currentState: QuantifiedPermissionsState =
        if (leavesIds.contains(currentId)) finalState
        else exitEdges.size match {
          case 1 => cfgState.statesOfBlock(cfg.exitEdges(currentId).head._2).head
          case 2 =>
            val (trueState, falseState) =
              (exitEdges.head, exitEdges.last) match {
                case ((_, toTrue, Some(true)), (_, toFalse, Some(false))) => (cfgState.statesOfBlock(toTrue).head, cfgState.statesOfBlock(toFalse).head)
                case ((_, toFalse, Some(false)), (_, toTrue, Some(true))) => (cfgState.statesOfBlock(toTrue).head, cfgState.statesOfBlock(toFalse).head)
                case _ => throw new IllegalStateException()
              }
            // val tempCurrentState =  trueState.lub(falseState)
            // val states = computeExpressions(tempCurrentState, currentId, cfgState)
            // trueState.lub(falseState, states.init.last.expr)
            trueState.lub(falseState)
          case _ => throw new IllegalStateException()
        }
      val blockStates: List[QuantifiedPermissionsState] = cfgState.statesOfBlock(currentId)
      val oldState: QuantifiedPermissionsState = if (blockStates.isEmpty) cfgState.stateFactory.bottom() else blockStates.last
      if (!currentState.lessEqual(oldState)) {
        backwardExecuteBlock(currentState, currentId, currentCount, cfgState)
        blocksToProcessIds ++= cfg.getDirectPredecessors(currentId)
        iterationAtBlock += currentId -> (currentCount + 1)
      }
    }
    cfgState
  }

  private def computeExpressions(exitState: QuantifiedPermissionsState, id: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState]): List[QuantifiedPermissionsState] = {
    var newStates = ListBuffer[QuantifiedPermissionsState]()
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(id)
    var nextState: QuantifiedPermissionsState = exitState
    for ((stmt: Statement, _: Int) <- stmts.zipWithIndex.reverse) {
      newStates = nextState +: newStates
      val pp = ProgramPointUtils.identifyingPP(stmt)
      val prevState: QuantifiedPermissionsState = stmt.specialBackwardSemantics(nextState.before(pp)).after(pp)
      nextState = prevState
    }
    newStates = nextState +: newStates
    newStates.toList
  }

  private def backwardExecuteBlock(exitState: QuantifiedPermissionsState, id: Int, count: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState]()
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(id)
    var nextState: QuantifiedPermissionsState = exitState
    for ((stmt: Statement, _: Int) <- stmts.zipWithIndex.reverse) {
      newStates = nextState +: newStates
      val pp = ProgramPointUtils.identifyingPP(stmt)
      val prevState: QuantifiedPermissionsState = stmt.specialBackwardSemantics(nextState.before(pp)).after(pp)
//      logger.info(nextState.toString)
//      logger.info(stmt.toString)
//      logger.info(prevState.toString)
      nextState = prevState
    }
    if (cfgState.cfg.exitEdges(id).size > 1 && count > SystemParameters.wideningLimit) {
      val blockStates: List[QuantifiedPermissionsState] = cfgState.statesOfBlock(id)
      nextState = blockStates.head widening nextState
      newStates = nextState +: newStates
    } else {
      newStates = nextState +: newStates
    }
    cfgState.setStatesOfBlock(id, newStates.toList)
  }

}

case class TrackingQPInterpreter(stateFactory: QuantifiedPermissionsState) extends QPInterpreter {
  type C = TrackingCFGState[QuantifiedPermissionsState]
  val cfgStateFactory: CFGStateFactory[QuantifiedPermissionsState, TrackingCFGState[QuantifiedPermissionsState]] = TrackingCFGStateFactory[QuantifiedPermissionsState](stateFactory)
}