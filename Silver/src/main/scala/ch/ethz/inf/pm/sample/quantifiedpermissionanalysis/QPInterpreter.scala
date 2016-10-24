package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionSet
import ch.ethz.inf.pm.sample.execution.{ForwardInterpreter, Interpreter, TrackingCFGState, TrackingCFGStateFactory}
import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraph, Statement}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Severin Münger
  *         Added on 23/10/16.
  */
trait QPInterpreter extends Interpreter[QuantifiedPermissionsState] with LazyLogging {
  var whileLoopCond = Set[Int]()

  /**
    * Transforms the CFG in such a way that we do not iterate through loops anymore. For this analysis it is enough
    * to iterate through the loop body only once. This pre-processing will remove the edge from the last block
    * inside the loop back to the head.
    */
  def preprocessGraph(cfg: ControlFlowGraph): (ControlFlowGraph, mutable.LinkedHashSet[Int]) = {
    var flowOrder = mutable.LinkedHashSet[Int]()
    val (whileLoopCond1, flowOrder1) = traverseGraphDFSFindLoop(ForwardInterpreter.startBlockId, cfg, Set())
    whileLoopCond = whileLoopCond1
    flowOrder = flowOrder1
    (cfg, flowOrder)
  }

  def traverseGraphDFSFindLoop(currentNode: Int, cfg: ControlFlowGraph, path: Set[Int]): (Set[Int], mutable
  .LinkedHashSet[Int]) = {
    var loopNodes = Set[Int]()
    var flowOrder = mutable.LinkedHashSet[Int]()
    if (path.contains(currentNode)) {
      return (loopNodes + currentNode, mutable.LinkedHashSet[Int]())
    }
    val exitNodes: Set[Int] = cfg.getDirectSuccessors(currentNode)
    for (nextNode <- exitNodes) {
      val (lNodes, fOrder) = traverseGraphDFSFindLoop(nextNode, cfg, path + currentNode)
      loopNodes = lNodes ++ loopNodes
      flowOrder = flowOrder ++ fOrder
    }
    flowOrder = flowOrder ++ mutable.LinkedHashSet[Int](currentNode)
    (loopNodes, flowOrder)
  }

  def refiningExecute(p_cfg: ControlFlowGraph, finalState: QuantifiedPermissionsState, forwardStates: TrackingCFGState[QuantifiedPermissionsState]): TrackingCFGState[QuantifiedPermissionsState] = {
    val (cfg, blocksToProcessIds) = preprocessGraph(p_cfg)
    val cfgState = forwardStates //cfgStateFactory.allBottom
    // (cfg) // start with the forwardStates
    val leavesIds: Set[Int] = cfg.getLeavesIds // get the ids of the leaves of the cfg
    // process the blocks of the cfg
    while (blocksToProcessIds.nonEmpty) {
      // while there still are blocks to be processed...
      val currentId: Int = blocksToProcessIds.head
      blocksToProcessIds.remove(currentId) // extract the current block
      // figure out the current exit state
      val postState: QuantifiedPermissionsState = if (leavesIds.contains(currentId)) cfgState.statesOfBlock(currentId).last
      else {
        val exitEdges = cfg.exitEdges(currentId)
        val oldPreState = forwardStates.statesOfBlock(currentId).last
        exitEdges.size match {
          case 0 => cfgState.statesOfBlock(currentId).last
          case 1 => //Remove the expression from the oldPreState, there might be some expressions left and backwardLub would
            // transform it into a branch cond
            cfgState.statesOfBlock(exitEdges.head._2).head.backwardLub(finalState.bottom(), oldPreState.setExpression(ExpressionSet()))
          case 2 => val (trueState, falseState) = (exitEdges.head, exitEdges.last) match {
            case ((_, toTrue, Some(true)), (_, toFalse, Some(false))) =>
              (cfgState.statesOfBlock(toTrue).head, cfgState.statesOfBlock(toFalse).head)
            case ((_, toFalse, Some(false)), (_, toTrue, Some(true))) =>
              (cfgState.statesOfBlock(toTrue).head, cfgState.statesOfBlock(toFalse).head)
            case _ => throw new IllegalStateException("should only have one true and one false edge.")
          }
            trueState.backwardLub(falseState, oldPreState)
          case _ => throw new IllegalStateException("A node cannot have more than 2 exit edges.")
        }
      }

      // backward execute the current block
      refiningExecuteBlock(postState, currentId, cfgState, forwardStates)
    }
    cfgState
  }

  private def refiningExecuteBlock(exitState: QuantifiedPermissionsState, id: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState], forwardStates: TrackingCFGState[QuantifiedPermissionsState]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState]() // initially empty list of new states
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(id) // get the statements within the block
    val blockForwardPreStates = forwardStates.statesOfBlock(id) //Get the provided states from the forward execution
    var postState = exitState // initial next state
    for ((stmt: Statement, idx: Int) <- stmts.zipWithIndex.reverse) {
      val forwardPreState = blockForwardPreStates(idx) // preState of the forward execution
      // for each statement (in reverse order)...
      newStates = postState +: newStates // prepend the next state to the list of new states
      var preState = stmt.refiningSemantics(postState, forwardPreState) // compute the previous state
      //if we are at the top of a while loop condition apply the whileLoopRules
      if (whileLoopCond.contains(id) && idx == 0) {
        val set = cfgState.cfg.exitEdges(id)
        val (_, to1, weight1) = set.head
        val (_, to2, _) = set.last
        val (loopBodyNr, afterLoopNr) = if (weight1.contains(true)) (to1, to2) else (to2, to1)
        val (loopBodyFirstState, loopBodyLastState, afterLoopState) = (cfgState.statesOfBlock(loopBodyNr).head,
          cfgState.statesOfBlock(loopBodyNr).last, cfgState.statesOfBlock(afterLoopNr).head)
        val beforeLoopState = exitState //strengthen the
        // numerical domain by taking the numDomain before the loop and widen it with the exit state of the loop
        preState = preState.refiningWhileLoop(forwardPreState, beforeLoopState, loopBodyFirstState, loopBodyLastState, afterLoopState)
      }
      logger.trace(postState.toString)
      logger.trace(stmt.toString)
      logger.trace(preState.toString)
      postState = preState // update the next state
    }
    newStates = postState +: newStates // prepend the next state to the list of new states
    cfgState.setStatesOfBlock(id, newStates.toList) // update the cfg with the new block states
  }

}

case class TrackingQPInterpreter(stateFactory: QuantifiedPermissionsState) extends QPInterpreter {
  val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState](stateFactory)
}
