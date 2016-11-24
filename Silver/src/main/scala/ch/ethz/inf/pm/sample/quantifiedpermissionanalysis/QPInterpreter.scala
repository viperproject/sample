/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.execution.{Interpreter, TrackingCFGState, TrackingCFGStateFactory}
import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraph, Statement}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Severin Münger
  *         Added on 23/10/16.
  */
trait QPInterpreter extends Interpreter[QuantifiedPermissionsState] with LazyLogging {
  def loopHeads: Set[Int]

  def simpleBackwardExecute(cfg: ControlFlowGraph, flowOrder: mutable.LinkedHashSet[Int], entryState: QuantifiedPermissionsState): TrackingCFGState[QuantifiedPermissionsState] = {
    val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState](entryState)
    val cfgState: TrackingCFGState[QuantifiedPermissionsState] = cfgStateFactory.allBottom(cfg)
    // process the blocks of the cfg
    while (flowOrder.nonEmpty) {
      // while there still are blocks to be processed...
      val currentBlockId: Int = flowOrder.head
      flowOrder.remove(currentBlockId) // extract the current block
      // figure out the current exit state
      val postState: QuantifiedPermissionsState = if (cfg.getLeavesIds.contains(currentBlockId)) {
        entryState
      } else {
        val exitEdges = cfg.exitEdges(currentBlockId)
        exitEdges.size match {
          case 0 => cfgState.statesOfBlock(currentBlockId).last
          case 1 => cfgState.statesOfBlock(exitEdges.head._2).head
          case 2 => val (trueState, falseState) = (exitEdges.head, exitEdges.last) match {
            case ((_, toTrue, Some(true)), (_, toFalse, Some(false))) =>
              (cfgState.statesOfBlock(toTrue).head, cfgState.statesOfBlock(toFalse).head)
            case ((_, toFalse, Some(false)), (_, toTrue, Some(true))) =>
              (cfgState.statesOfBlock(toTrue).head, cfgState.statesOfBlock(toFalse).head)
            case _ => throw new IllegalStateException("There should be exactly one true and one false edge.")
          }
            trueState.lub(falseState)
          case _ => throw new IllegalStateException("A node cannot have more than 2 exit edges.")
        }
      }

      if (cfg.entryEdges(currentBlockId).exists{ case (from, _, Some(true)) => loopHeads.contains(from)}) {

      }

      // backward execute the current block
      backwardExecuteBlock(postState, currentBlockId, cfgState)
    }
    cfgState
  }

  private def backwardExecuteBlock(postBlockState: QuantifiedPermissionsState, blockId: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState]()
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(blockId)
    var postState = postBlockState
    for ((stmt: Statement, idx: Int) <- stmts.zipWithIndex.reverse) {
      newStates = postState +: newStates
      logger.info("Execute " + stmt)
      var preState = stmt.specialBackwardSemantics(postState)
      logger.trace(postState.toString)
      logger.trace(stmt.toString)
      logger.trace(preState.toString)
      postState = preState // update the next state
    }
    if (loopHeads.contains(blockId)) {
      val exitEdges = cfgState.cfg.exitEdges(blockId)
      val (_, exitIndex1, weight1) = exitEdges.head
      val (_, exitIndex2, _) = exitEdges.last
      val (loopBodyIdx, afterLoopIdx) = if (weight1.contains(true)) (exitIndex1, exitIndex2) else (exitIndex2, exitIndex1)
      assert(cfgState.cfg.entryEdges(blockId).exists { case (from, _, _) => from == loopBodyIdx })
      val (loopBodyFirstState, loopBodyLastState, afterLoopState) = (cfgState.statesOfBlock(loopBodyIdx).head,
        cfgState.statesOfBlock(loopBodyIdx).last, cfgState.statesOfBlock(afterLoopIdx).head)
//      postState = postState.refiningWhileLoop(postBlockState, loopBodyFirstState, loopBodyLastState, afterLoopState)
    }
    newStates = postState +: newStates // prepend the next state to the list of new states
    cfgState.setStatesOfBlock(blockId, newStates.toList) // update the cfg with the new block states
  }

}

case class TrackingQPInterpreter(loopHeads: Set[Int], stateFactory: QuantifiedPermissionsState) extends QPInterpreter {
  type C = TrackingCFGState[QuantifiedPermissionsState]
  val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState](stateFactory)
}
