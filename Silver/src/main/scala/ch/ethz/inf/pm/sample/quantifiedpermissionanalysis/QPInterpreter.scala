/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.BlockType.BlockType
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Severin Münger
  *         Added on 29.11.16.
  */
object BlockType extends Enumeration {
  type BlockType = Value
  val Default, LoopHead, Loop, LoopHeadInLoop = Value
}
trait QPInterpreter extends Interpreter[QuantifiedPermissionsState] with LazyLogging {

  var blockTypes: Map[Int, BlockType] = Map()
  var blocksLastInLoop: Set[Int] = Set()

  def determineBlockTypes(cfg: ControlFlowGraph, blockIndex: Int = ForwardInterpreter.startBlockId, visited: Set[Int] = Set()): Boolean = {
    if (visited.contains(blockIndex)) {
      blocksLastInLoop += blockIndex
      true
    } else {
      val exitEdges = cfg.exitEdges(blockIndex)
      exitEdges.size match {
        case 2 =>
          val ((_, toTrue, _), (_, toFalse, _)) = exitEdges.head match {
            case (_, _, Some(true)) => (exitEdges.head, exitEdges.last)
            case (_, _, Some(false)) => (exitEdges.last, exitEdges.head)
            case _ => throw new IllegalStateException()
          }
          val (trueBranchLoop, falseBranchLoop) = (determineBlockTypes(cfg, toTrue, visited + blockIndex), determineBlockTypes(cfg, toFalse, visited + blockIndex))
          val blockType = (trueBranchLoop, falseBranchLoop) match {
            case (true, true) => BlockType.LoopHeadInLoop
            case (true, false) => BlockType.LoopHead
            case (false, true) => BlockType.Loop
            case (false, false) => BlockType.Default
          }
          blockTypes += (blockIndex -> blockType)
          falseBranchLoop
        case 1 =>
          val isInLoop = determineBlockTypes(cfg, cfg.getDirectSuccessors(blockIndex).head, visited + blockIndex)
          blockTypes += (blockIndex -> (if (isInLoop) BlockType.Loop else BlockType.Default))
          isInLoop
        case 0 =>
          blockTypes += (blockIndex -> BlockType.Default)
          false
      }
    }
  }

  def backwardExecute(cfg: ControlFlowGraph, finalState: QuantifiedPermissionsState): TrackingCFGState[QuantifiedPermissionsState] = {
    determineBlockTypes(cfg)
    val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState](finalState)
    val cfgState = cfgStateFactory.allBottom(cfg) // set the whole cfg to bottom
    val leavesIds: Set[Int] = cfg.getLeavesIds // get the ids of the leaves of the cfg
    // process the blocks of the cfg
    var blocksToProcessIds: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet[Int]() ++ leavesIds // initial blocks to be processes
    var iterationAtBlock = Map[Int, Int]() // initially empty map from ids to iteration counts
    while (blocksToProcessIds.nonEmpty) { // while there still are blocks to be processed...
      val currentId: Int = blocksToProcessIds.head; blocksToProcessIds.remove(currentId) // extract the current block
      val currentCount: Int = iterationAtBlock.getOrElse(currentId, 0) // extract the corresponding iteration count
      // figure out the current exit state
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
            val tempCurrentState =  trueState.lub(falseState)
            val states = computeExpressions(tempCurrentState, currentId, cfgState)
//            trueState.lub(falseState, states.init.last.expr)
            trueState.lub(falseState)
          case _ => throw new IllegalStateException()
        }
          // figure out the exit state of the previous iteration
      val blockStates: List[QuantifiedPermissionsState] = cfgState.statesOfBlock(currentId) // get the result of the previous iteration
      val oldState: QuantifiedPermissionsState = if (blockStates.isEmpty) cfgState.stateFactory.bottom() else blockStates.last
      // backward execute the current block
      if (!currentState.lessEqual(oldState)) {
        backwardExecuteBlock(currentState, currentId, currentCount, cfgState)
        val entryEdges = cfg.entryEdges(currentId)
        blocksToProcessIds = entryEdges.size match {
          case 2 =>
            val ((fromLoopBody, _, _), (fromRest, _, _)) =
              if (blocksLastInLoop.contains(entryEdges.head._1)) (entryEdges.head, entryEdges.last)
              else (entryEdges.last, entryEdges.head)
            mutable.LinkedHashSet(fromLoopBody, fromRest) ++ (blocksToProcessIds -- Set(fromLoopBody, fromRest))
          case 1 =>
            mutable.LinkedHashSet(cfg.getDirectPredecessors(currentId).head) ++ (blocksToProcessIds -- cfg.getDirectPredecessors(currentId))
          case 0 => blocksToProcessIds
        }
        iterationAtBlock = iterationAtBlock + (currentId -> (currentCount + 1))
      }
    }
    cfgState
  }

  private def computeExpressions(exitState: QuantifiedPermissionsState, id: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState]): List[QuantifiedPermissionsState] = {
    var newStates = ListBuffer[QuantifiedPermissionsState]() // initially empty list of new states
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(id) // get the statements within the block
    var nextState: QuantifiedPermissionsState = exitState // initial next state
    for ((stmt: Statement, _: Int) <- stmts.zipWithIndex.reverse) { // for each statement (in reverse order)...
      newStates = nextState +: newStates // prepend the next state to the list of new states
      val pp = ProgramPointUtils.identifyingPP(stmt)
      val prevState: QuantifiedPermissionsState = stmt.specialBackwardSemantics(nextState.before(pp).setBlockType(blockTypes(id))).after(pp) // compute the previous state
      nextState = prevState // update the next state
    }
    newStates = nextState +: newStates
    newStates.toList
  }

  private def backwardExecuteBlock(exitState: QuantifiedPermissionsState, id: Int, count: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState]() // initially empty list of new states
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(id) // get the statements within the block
    var nextState: QuantifiedPermissionsState = exitState // initial next state
    for ((stmt: Statement, _: Int) <- stmts.zipWithIndex.reverse) { // for each statement (in reverse order)...
      newStates = nextState +: newStates // prepend the next state to the list of new states
      val pp = ProgramPointUtils.identifyingPP(stmt)
      val prevState: QuantifiedPermissionsState = stmt.specialBackwardSemantics(nextState.before(pp).setBlockType(blockTypes(id))).after(pp) // compute the previous state
//      logger.info(nextState.toString)
//      logger.info(stmt.toString)
//      logger.info(prevState.toString)
      nextState = prevState // update the next state
    }
    // perform widening where needed
    if (cfgState.cfg.exitEdges(id).size > 1 && count > SystemParameters.wideningLimit) {
      val blockStates: List[QuantifiedPermissionsState] = cfgState.statesOfBlock(id) // get the result of the previous iteration
      nextState = blockStates.head widening nextState
      newStates = nextState +: newStates // prepend the widened state to the list of new states
    } else {
      newStates = nextState +: newStates // prepend the next state to the list of new states
    }
    cfgState.setStatesOfBlock(id, newStates.toList) // update the cfg with the new block states
  }

}

case class TrackingQPInterpreter(stateFactory: QuantifiedPermissionsState) extends QPInterpreter {
  type C = TrackingCFGState[QuantifiedPermissionsState]
  val cfgStateFactory: CFGStateFactory[QuantifiedPermissionsState, TrackingCFGState[QuantifiedPermissionsState]] = TrackingCFGStateFactory[QuantifiedPermissionsState](stateFactory)
}