package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, ControlFlowGraph, ProgramPointUtils}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object ForwardInterpreter {
  val startBlockId: Int = 0
}

trait ForwardInterpreter[S <: State[S]] extends Interpreter[S] with LazyLogging {

  /**
   * Perform forward abstract interpretation, given a previous `CFGState` but starting from a new initial state.
   *
   * @param cfg the `ControlFlowGraph` to execute
   * @param initialState the new entry state, may differ from old one in cfgState
   * @param oldCfgState optional state to initialise the execution with
   * @return states in CFG after reaching fixed point
   */
  def forwardExecute(cfg: ControlFlowGraph, initialState: S, oldCfgState: Option[C] = None): C = {
    val cfgState = oldCfgState match {
      case Some(x) => cfgStateFactory.makeFrom(cfg, x)
      case None    => cfgStateFactory.allBottom(cfg)
    }

    // Model the blocks to process as a queue
    var blocksToProcessIds = mutable.LinkedHashSet(ForwardInterpreter.startBlockId)
    var iterationCountAtBlock = Map.empty[Int, Int]

    while (blocksToProcessIds.nonEmpty) {
      val currentBlockId = blocksToProcessIds.head
      blocksToProcessIds.remove(currentBlockId)

      val itNumber = iterationCountAtBlock.getOrElse(currentBlockId, 0)
      val entry = if (currentBlockId == 0) initialState else computeEntryState(currentBlockId, itNumber, cfgState)
      val blockStates = cfgState.statesOfBlock(currentBlockId)
      val previousEntry = if (blockStates.isEmpty) cfgState.stateFactory.bottom() else blockStates.head
      if (!entry.lessEqual(previousEntry)) {
        forwardExecuteBlock(entry, currentBlockId, cfgState)
        blocksToProcessIds = blocksToProcessIds ++ cfg.getDirectSuccessors(currentBlockId)
        iterationCountAtBlock = iterationCountAtBlock + ((currentBlockId, itNumber + 1))
      }
    }
    cfgState
  }

  private def computeEntryState(index: Int, it: Int, cfgState: C): S = {
    var result: S = cfgStateFactory.stateFactory.bottom()

    // Join incoming states
    val cfg = cfgState.cfg
    for ((from, to, weight) <- cfg.entryEdges(index)) {
      val preState = cfgState.statesOfBlock(from).last
      val filteredState =
        weight match {
          case Some(true) =>  preState.testTrue()
          case Some(false) => preState.testFalse()
          case None =>        preState
        }

      result = result.lub(filteredState)
    }

    // Taking the least upper bound with the previous result here may seem
    // redundant, but some analyses (for example the SIL spec inference) do
    // depend on it, in order to trigger certain changes to the state.
    // Also, in the case of must-analyses, taking the least upper bound
    // here may lead to reaching the fix-point faster.
    if (SystemParameters.isValueDrivenHeapAnalysis) {
      val currentBlockStates = cfgState.statesOfBlock(index)
      val previousEntry = currentBlockStates.head
      if (it > SystemParameters.wideningLimit) {
        // Widen with previous result
        result = previousEntry.widening(result)
      } else {
        // The old ControlFlowExecution code also used to perform this operation.
        result = previousEntry.lub(result)
      }
    } else {
      // Default (fast) case. Do not lub with previous result. Only widen at loop heads.
      if (cfg.entryEdges(index).size > 1 && it > SystemParameters.wideningLimit) {
        val currentBlockStates = cfgState.statesOfBlock(index)
        val previousEntry = currentBlockStates.head
        result = previousEntry.widening(result)
      }
    }

    result
  }

  private def forwardExecuteBlock(entryState: S, currentBlockId: Int, cfgState: C) = {
    val resultingStates = ListBuffer(entryState)

    val blockStatements = cfgState.cfg.getBasicBlockStatements(currentBlockId)
    var previousState = resultingStates.head
    for ((statement, statementIndex) <- blockStatements.zipWithIndex)  {
      // Need to call this to make trace partitioning possible
      val tempState = previousState.before(ProgramPointUtils.identifyingPP(statement))
      val transformedState = statement.forwardSemantics(tempState)
      logger.trace(tempState.toString)
      logger.trace(statement.toString)
      logger.trace(transformedState.toString)
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

/** Forward interpreter that operates on `TrackingCFGState`s. */
case class TrackingForwardInterpreter[S <: State[S]](stateFactory: S) extends ForwardInterpreter[S] {
  type C = TrackingCFGState[S]
  val cfgStateFactory = TrackingCFGStateFactory[S](stateFactory)
}
