package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.SystemParameters
import scala.collection.mutable.ListBuffer
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPointUtils, CFGPosition, ControlFlowGraph}

object ForwardInterpreter {
  var currentLocation: Option[CFGPosition] = None
}

trait ForwardInterpreter[S <: State[S]] extends Interpreter[S] {
  val startBlockId: Int = 0

  def forwardExecuteFrom(cfg: ControlFlowGraph, initialState: S): C = {
    implicit val cfgState = cfgStateFactory.allBottom(cfg)

    var blocksToProcessIds = Set(startBlockId)
    var iterationCountAtBlock = Map.empty[Int, Int]

    while (!blocksToProcessIds.isEmpty) {
      val currentBlockId = blocksToProcessIds.min
      blocksToProcessIds = blocksToProcessIds - currentBlockId
      val itNumber = iterationCountAtBlock.getOrElse(currentBlockId, 0)
      val entry = if (currentBlockId == 0) initialState else computeEntryState(currentBlockId, itNumber)
      val blockStates = cfgState.statesOfBlock(currentBlockId)
      val previousEntry = if (blockStates.isEmpty) cfgState.stateFactory.bottom() else blockStates.head
      if (!entry.lessEqual(previousEntry)) {
        forwardExecuteBlock(entry, currentBlockId)
        blocksToProcessIds = blocksToProcessIds ++ cfg.getDirectSuccessors(currentBlockId)
        iterationCountAtBlock = iterationCountAtBlock + ((currentBlockId, itNumber + 1))
      }
    }
    cfgState
  }

  private def computeEntryState(index: Int, it: Int)(implicit cfgState: C): S = {
    var result: S = cfgStateFactory.stateFactory.bottom()

    // Join incoming states
    val cfg = cfgState.cfg
    for ((from, to, weight) <- cfg.entryEdges(index)) {
      val preState = cfgState.statesOfBlock(from).last
      val filteredState =
        weight match {
          case Some(true) => preState.testTrue()
          case Some(false) => preState.testFalse()
          case None =>
            preState
        }

      result = result.lub(filteredState)
    }

    // Widen with previous result
    val currentBlockStates = cfgState.statesOfBlock(index)
    val previousEntry = currentBlockStates.head
    if (it > SystemParameters.wideningLimit) {
       result =  previousEntry.widening(result)
    }

    result
  }

  private def forwardExecuteBlock(entryState: S, currentBlockId: Int)(implicit cfgState: C) = {
    val resultingStates = ListBuffer(entryState)

    val blockStmts = cfgState.cfg.getBasicBlockStatements(currentBlockId)
    var previousState = resultingStates.head
    for ((stmt, stmtIdx) <- blockStmts.zipWithIndex)  {
      ForwardInterpreter.currentLocation = Some(CFGPosition(currentBlockId, stmtIdx))
      // Need to call this to make trace partitioning possible
      val tempState = previousState.before(ProgramPointUtils.identifyingPP(stmt))
      val transformedState = stmt.forwardSemantics(tempState)
      previousState = transformedState
      resultingStates append transformedState
    }

    cfgState.setStatesOfBlock(currentBlockId, resultingStates.toList)
  }
}

/** Forward interpreter that operates on `DefaultCFGState`s. */
case class DefaultForwardInterpreter[S <: State[S]](stateFactory: S) extends ForwardInterpreter[S] {
  type C = DefaultCFGState[S]

  val cfgStateFactory = DefaultCFGStateFactory[S](stateFactory)
}
