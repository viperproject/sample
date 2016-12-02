/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.execution.{Interpreter, TrackingCFGState, TrackingCFGStateFactory}
import ch.ethz.inf.pm.sample.oorepresentation._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Severin Münger
  *         Added on 29.11.16.
  */
trait QPInterpreter2 extends Interpreter[QuantifiedPermissionsState2] with LazyLogging {

  def backwardExecute(cfg: ControlFlowGraph, finalState: QuantifiedPermissionsState2): TrackingCFGState[QuantifiedPermissionsState2] = {
    val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState2](finalState)
    val cfgState = cfgStateFactory.allBottom(cfg) // set the whole cfg to bottom
    val leavesIds: Set[Int] = cfg.getLeavesIds // get the ids of the leaves of the cfg
    // process the blocks of the cfg
    var blocksToProcessIds = mutable.LinkedHashSet[Int]() ++ leavesIds // initial blocks to be processes
    var iterationAtBlock = Map[Int, Int]() // initially empty map from ids to iteration counts
    while (blocksToProcessIds.nonEmpty) { // while there still are blocks to be processed...
      val currentId: Int = blocksToProcessIds.head; blocksToProcessIds.remove(currentId) // extract the current block
      val currentCount: Int = iterationAtBlock.getOrElse(currentId, 0) // extract the corresponding iteration count
      // figure out the current exit state
      val currentState: QuantifiedPermissionsState2 =
        if (leavesIds.contains(currentId)) finalState
        else cfg.exitEdges(currentId).map { case (_: Int, to: Int, _: Option[Boolean]) => cfgState.statesOfBlock(to).head }.reduceLeft [QuantifiedPermissionsState2]{ case (a: QuantifiedPermissionsState2, b: QuantifiedPermissionsState2) => a lub b }
      // figure out the exit state of the previous iteration
      val blockStates: List[QuantifiedPermissionsState2] = cfgState.statesOfBlock(currentId) // get the result of the previous iteration
      val oldState: QuantifiedPermissionsState2 = if (blockStates.isEmpty) cfgState.stateFactory.bottom() else blockStates.last
      // backward execute the current block
      if (!currentState.lessEqual(oldState)) {
        backwardExecuteBlock(currentState, currentId, cfgState)
        blocksToProcessIds = blocksToProcessIds ++ cfg.getDirectPredecessors(currentId)
        iterationAtBlock = iterationAtBlock + (currentId -> (currentCount + 1))
      }
    }
    cfgState
  }

  private def backwardExecuteBlock(exitState: QuantifiedPermissionsState2, id: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState2]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState2]() // initially empty list of new states
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(id) // get the statements within the block
    var nextState: QuantifiedPermissionsState2 = exitState // initial next state
    for ((stmt: Statement, _: Int) <- stmts.zipWithIndex.reverse) { // for each statement (in reverse order)...
      newStates = nextState +: newStates // prepend the next state to the list of new states
      val pp = ProgramPointUtils.identifyingPP(stmt)
      val prevState: QuantifiedPermissionsState2 = stmt.backwardSemantics(nextState).before(pp) // compute the previous state
      logger.trace(nextState.toString)
      logger.trace(stmt.toString)
      logger.trace(prevState.toString)
      nextState = prevState // update the next state
    }
    newStates = nextState +: newStates // prepend the next state to the list of new states
    cfgState.setStatesOfBlock(id, newStates.toList) // update the cfg with the new block states
  }

}

case class TrackingQPInterpreter2(stateFactory: QuantifiedPermissionsState2) extends QPInterpreter2 {
  type C = TrackingCFGState[QuantifiedPermissionsState2]
  val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState2](stateFactory)
}