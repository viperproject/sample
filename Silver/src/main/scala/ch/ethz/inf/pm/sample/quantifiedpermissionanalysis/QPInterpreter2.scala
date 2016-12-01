/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.execution.{Interpreter, TrackingCFGState, TrackingCFGStateFactory}
import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraph, ProgramPointUtils, Statement}
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
    while (blocksToProcessIds.nonEmpty) { // while there still are blocks to be processed...
      val currentId: Int = blocksToProcessIds.head; blocksToProcessIds.remove(currentId) // extract the current block
      // figure out the current exit state
      val currentState: QuantifiedPermissionsState2 = if (leavesIds.contains(currentId)) finalState else {
        var exitState: QuantifiedPermissionsState2 = cfgStateFactory.stateFactory.bottom()
        val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(currentId)
        val stmt: Statement = stmts.last
        for ((_, to, weight) <- cfg.exitEdges(currentId)) { // for each exit edge...
          val postState = cfgState.statesOfBlock(to).head // get the first state of the successor block
          // filter the state if needed
          val filteredState = weight match {
            case Some(cond) =>
              var tempState: QuantifiedPermissionsState2 = cfgState.stateFactory.factory()
              val pp = ProgramPointUtils.identifyingPP(stmt)
              tempState = stmt.forwardSemantics(tempState.before(pp))
              //val pp = ProgramPointUtils.identifyingPP(stmts.last)
              if (cond) { postState.before(pp).setExpression(tempState.expr).testTrue() }
              else { postState.before(pp).setExpression(tempState.expr).testFalse() }
            case None => postState
          }
          exitState = exitState lub filteredState // join the successor states
        }
        exitState
      }
      // figure out the exit state of the previous iteration
      val blockStates: List[QuantifiedPermissionsState2] = cfgState.statesOfBlock(currentId) // get the result of the previous iteration
      val oldState: QuantifiedPermissionsState2 = if (blockStates.isEmpty) cfgState.stateFactory.bottom() else blockStates.last
      // backward execute the current block
      if (!currentState.lessEqual(oldState)) {
        backwardExecuteBlock(currentState, currentId, cfgState)
        blocksToProcessIds = blocksToProcessIds ++ cfg.getDirectPredecessors(currentId)
      }
    }
    cfgState
  }

  private def backwardExecuteBlock(exitState: QuantifiedPermissionsState2, id: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState2]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState2]() // initially empty list of new states

    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(id) // get the statements within the block
    var nextState: QuantifiedPermissionsState2 = exitState // initial next state
    for ((stmt: Statement, idx: Int) <- stmts.zipWithIndex.reverse) { // for each statement (in reverse order)...
      newStates = nextState +: newStates // prepend the next state to the list of new states
      val pp = ProgramPointUtils.identifyingPP(stmt)
      val tempState = nextState.before(pp)
      val prevState: QuantifiedPermissionsState2 = stmt.backwardSemantics(tempState) // compute the previous state
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