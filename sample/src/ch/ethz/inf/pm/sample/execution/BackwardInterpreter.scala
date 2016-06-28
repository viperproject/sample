/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraph, ProgramPointUtils, Statement}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Backward Interpreter.
  *
  * @tparam S the abstract state
  * @author Caterina Urban
  */
trait BackwardInterpreter[S <: State[S]] extends Interpreter[S] with LazyLogging {

  def backwardExecute(cfg: ControlFlowGraph, finalState: S): C = {
    val cfgState = cfgStateFactory.allBottom(cfg) // set the whole cfg to bottom

    val leavesIds: Set[Int] = cfg.getLeavesIds // get the ids of the leaves of the cfg
    // process the blocks of the cfg
    var blocksToProcessIds = mutable.LinkedHashSet[Int]() ++ leavesIds // initial blocks to be processes
    var iterationAtBlock = Map[Int, Int]() // initially empty map from ids to iteration counts
    while (blocksToProcessIds.nonEmpty) { // while there still are blocks to be processed...
      val currentId: Int = blocksToProcessIds.head; blocksToProcessIds.remove(currentId) // extract the current block
      val currentCount: Int = iterationAtBlock.getOrElse(currentId, 0) // extract the corresponding iteration count
      // figure out the current exit state
      val currentState: S = if (leavesIds.contains(currentId)) finalState else {
        var exitState: S = cfgStateFactory.stateFactory.bottom()
        for ((_, to, weight) <- cfg.exitEdges(currentId)) { // for each exit edge...
        val postState = cfgState.statesOfBlock(to).head // get the first state of the successor block
        val filteredState = weight match { // filter the state if needed
            case Some(true) => postState.testTrue()
            case Some(false) => postState.testFalse()
            case None => postState
          }
          exitState = exitState lub filteredState // join the successor states
        }
        exitState
      }
      // figure out the exit state of the previous iteration
      val blockStates: List[S] = cfgState.statesOfBlock(currentId) // get the result of the previous iteration
      val oldState: S = if (blockStates.isEmpty) cfgState.stateFactory.bottom() else blockStates.last
      // backward execute the current block
      if (!currentState.lessEqual(oldState)) {
        backwardExecuteBlock(currentState, currentId, currentCount, cfgState)
        blocksToProcessIds = blocksToProcessIds ++ cfg.getDirectPredecessors(currentId)
        iterationAtBlock = iterationAtBlock + (currentId -> (currentCount + 1))
      }
    }

    cfgState
  }

  private def backwardExecuteBlock(exitState: S, id: Int, count: Int, cfgState: C): Unit = {
    var newStates = ListBuffer[S]() // initially empty list of new states

    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(id) // get the statements within the block
    var nextState: S = exitState // initial next state
    for ((stmt: Statement, idx: Int) <- stmts.zipWithIndex.reverse) { // for each statement (in reverse order)...
      newStates = nextState +: newStates // prepend the next state to the list of new states
      val tempState = nextState.before(ProgramPointUtils.identifyingPP(stmt))
      val prevState: S = stmt.backwardSemantics(tempState) // compute the previous state
      logger.trace(nextState.toString)
      logger.trace(stmt.toString)
      logger.trace(prevState.toString)
      nextState = prevState // update the next state
    }
    // perform widening where needed
    if (cfgState.cfg.exitEdges(id).size > 1 && count > SystemParameters.wideningLimit) {
      val blockStates: List[S] = cfgState.statesOfBlock(id) // get the result of the previous iteration
      nextState = blockStates.head widening nextState
      newStates = nextState +: newStates // prepend the widened state to the list of new states
    } else {
      newStates = nextState +: newStates // prepend the next state to the list of new states
    }
    cfgState.setStatesOfBlock(id, newStates.toList) // update the cfg with the new block states
  }

}

/** Backward Interpreter over TrackingCFGState.
  *
  * @author Caterina Urban
  */
case class TrackingBackwardInterpreter[S <: State[S]](stateFactory: S) extends BackwardInterpreter[S] {
  type C = TrackingCFGState[S]
  val cfgStateFactory = TrackingCFGStateFactory[S](stateFactory)
}
