package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionSet
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
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
  var loopHeads = Set[Int]()

  /**
    * Transforms the CFG in such a way that we do not iterate through loops anymore. For this analysis it is enough
    * to iterate through the loop body only once. This pre-processing will remove the edge from the last block
    * inside the loop back to the head.
    */
  def preprocessGraph(cfg: ControlFlowGraph): (ControlFlowGraph, mutable.LinkedHashSet[Int]) = {
    val (loopHeads, flowOrder) = findLoops(ForwardInterpreter.startBlockId, cfg, Set())
    this.loopHeads = loopHeads
    (cfg, flowOrder)
  }

  def findLoops(currentNode: Int, cfg: ControlFlowGraph, visited: Set[Int]): (Set[Int], mutable
  .LinkedHashSet[Int]) = {
    if (visited.contains(currentNode)) {
      return (Set[Int](currentNode), mutable.LinkedHashSet[Int]())
    }
    val successors: Set[Int] = cfg.getDirectSuccessors(currentNode)
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

  def refiningExecute(p_cfg: ControlFlowGraph, forwardStates: TrackingCFGState[Apron.Polyhedra]): TrackingCFGState[QuantifiedPermissionsState] = {
    val (cfg, flowOrder) = preprocessGraph(p_cfg)
    val cfgState = forwardStates //cfgStateFactory.allBottom
    // (cfg) // start with the forwardStates
    // process the blocks of the cfg
    while (flowOrder.nonEmpty) {
      // while there still are blocks to be processed...
      val currentBlockId: Int = flowOrder.head
      flowOrder.remove(currentBlockId) // extract the current block
      // figure out the current exit state
      val postState: Apron.Polyhedra = if (cfg.getLeavesIds.contains(currentBlockId)) cfgState.statesOfBlock(currentBlockId).last
      else {
        val exitEdges = cfg.exitEdges(currentBlockId)
        val oldPreState = forwardStates.statesOfBlock(currentBlockId).last
        exitEdges.size match {
          case 0 => cfgState.statesOfBlock(currentBlockId).last
          case 1 => //Remove the expression from the oldPreState, there might be some expressions left and backwardLub would
            // transform it into a branch cond
            cfgState.statesOfBlock(exitEdges.head._2).head.backwardLub(QuantifiedPermissionsState.Bottom, oldPreState.setExpression(ExpressionSet()))
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
      refiningExecuteBlock(postState, currentBlockId, cfgState, forwardStates)
    }
    cfgState
  }

  private def refiningExecuteBlock(exitState: QuantifiedPermissionsState, blockId: Int, cfgState: TrackingCFGState[QuantifiedPermissionsState], forwardStates: TrackingCFGState[QuantifiedPermissionsState]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState]() // initially empty list of new states
    val stmts: List[Statement] = cfgState.cfg.getBasicBlockStatements(blockId) // get the statements within the block
    val blockForwardPreStates = forwardStates.statesOfBlock(blockId) //Get the provided states from the forward execution
    var postState = exitState // initial next state
    for ((stmt: Statement, idx: Int) <- stmts.zipWithIndex.reverse) {
      val forwardPreState = blockForwardPreStates(idx) // preState of the forward execution
      // for each statement (in reverse order)...
      newStates = postState +: newStates // prepend the next state to the list of new states
      var preState = stmt.refiningSemantics(postState, forwardPreState) // compute the previous state
      //if we are at the top of a while loop condition apply the whileLoopRules
      if (loopHeads.contains(blockId) && idx == 0) {
        val set = cfgState.cfg.exitEdges(blockId)
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
    cfgState.setStatesOfBlock(blockId, newStates.toList) // update the cfg with the new block states
  }

}

case class TrackingQPInterpreter(stateFactory: QuantifiedPermissionsState) extends QPInterpreter {
  type C = TrackingCFGState[QuantifiedPermissionsState]
  val cfgStateFactory = TrackingCFGStateFactory[QuantifiedPermissionsState](stateFactory)
}