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

  def simpleBackwardExecute(cfgWithoutCycles: ControlFlowGraph, flowOrder: mutable.LinkedHashSet[Int], entryState: QuantifiedPermissionsState): TrackingCFGState[QuantifiedPermissionsState] = {
    val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState](entryState)
    val cfgState: TrackingCFGState[QuantifiedPermissionsState] = cfgStateFactory.allBottom(cfgWithoutCycles)
    // process the blocks of the cfg
    while (flowOrder.nonEmpty) {
      // while there still are blocks to be processed...
      val currentBlockId: Int = flowOrder.head
      flowOrder.remove(currentBlockId) // extract the current block
      // figure out the current exit state
      val postState: QuantifiedPermissionsState = if (cfgWithoutCycles.getLeavesIds.contains(currentBlockId)) {
        entryState
      } else {
        val exitEdges = cfgWithoutCycles.exitEdges(currentBlockId)
        exitEdges.size match {
          case 0 => cfgState.statesOfBlock(currentBlockId).last
          case 1 => cfgState.statesOfBlock(exitEdges.head._2).head
          case 2 => val (trueState, falseState) = (exitEdges.head, exitEdges.last) match {
            case ((_, toTrue, Some(true)), (_, toFalse, Some(false))) =>
              (cfgState.statesOfBlock(toTrue).head, cfgState.statesOfBlock(toFalse).head)
            case ((_, toFalse, Some(false)), (_, toTrue, Some(true))) =>
              (cfgState.statesOfBlock(toTrue).head, cfgState.statesOfBlock(toFalse).head)
            case _ => throw new IllegalStateException("should only have one true and one false edge.")
          }
            trueState.lub(falseState)
          case _ => throw new IllegalStateException("A node cannot have more than 2 exit edges.")
        }
      }

      // backward execute the current block
      backwardExecuteBlock(postState, currentBlockId, cfgState)
    }
    cfgState
  }

  private def backwardExecuteBlock(lastBlockState: QuantifiedPermissionsState, blockId: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState]() // initially empty list of new states
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(blockId) // get the statements within the block
    var postState = lastBlockState // initial next state
    for ((stmt: Statement, idx: Int) <- stmts.zipWithIndex.reverse) {
      // for each statement (in reverse order)...
      newStates = postState +: newStates // prepend the next state to the list of new states
      logger.info("Execute " + stmt)
      var preState = stmt.specialBackwardSemantics(postState) // compute the previous state
      //if we are at the top of a while loop condition apply the whileLoopRules
      if (loopHeads.contains(blockId) && idx == 0) {
        val exitEdges = cfgState.cfg.exitEdges(blockId)
        val (_, exitIndex1, weight1) = exitEdges.head
        val (_, exitIndex2, _) = exitEdges.last
        val (loopBodyIdx, afterLoopIdx) = if (weight1.contains(true)) (exitIndex1, exitIndex2) else (exitIndex2, exitIndex1)
        val (loopBodyFirstState, loopBodyLastState, afterLoopState) = (cfgState.statesOfBlock(loopBodyIdx).head,
          cfgState.statesOfBlock(loopBodyIdx).last, cfgState.statesOfBlock(afterLoopIdx).head)
        //strengthen the numerical domain by taking the numDomain before the loop and widen it with the exit state of the loop
        // TODO: implement loop handling
        // preState = preState.refiningWhileLoop(lastBlockState, loopBodyFirstState, loopBodyLastState, afterLoopState)
      }
      logger.trace(postState.toString)
      logger.trace(stmt.toString)
      logger.trace(preState.toString)
      postState = preState // update the next state
    }
    newStates = postState +: newStates // prepend the next state to the list of new states
    cfgState.setStatesOfBlock(blockId, newStates.toList) // update the cfg with the new block states
  }

}

case class TrackingQPInterpreter(loopHeads: Set[Int], stateFactory: QuantifiedPermissionsState) extends QPInterpreter {
  type C = TrackingCFGState[QuantifiedPermissionsState]
  val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState](stateFactory)
}
