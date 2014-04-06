package ch.ethz.inf.pm.sample.backwardanalysis

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.SystemParameters
import scala.collection.mutable.ListBuffer
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, ProgramPointUtils, ControlFlowGraph}
import ch.ethz.inf.pm.sample.execution.{DefaultCFGStateFactory, DefaultCFGState, Interpreter}

/**
 * The abstract error state that is inserted at position `cfgPos` in the
 * control flow graph states, right before the statement with the error.
 */
case class InCFGInitialState[S <: State[S]](errorState: S, cfgPos: CFGPosition)

/**
 * Abstract interpreter (iteration engine) for a refining backward analysis
 */
trait RefiningBackwardInterpreter[S <: State[S]] extends Interpreter[S] {
  /**
   * Executes the refining backward interpretation  of a CFG.
   *
   * The abstract error under investigation does not occur in this method, but
   * the states that reach it are back-propagated through this method. Therefore,
   * the `exitState` parameter would be set non-bottom and the analysis proceeds
   * from the exit block (exactly dual to forward interpretation from entry block)
   *
   * @param cfg the CFG (method body) to interpret
   * @param forwardStates resulting states of the forward analysis
   * @param refinedBackwardStates current CFG states of backward analysis
   *                              (may be used to "generalize" summaries,
   *                              an all-bottom CFGState can be provided otherwise)
   * @param exitState the current exit state from which to analyze
   * @return CFGState with refined backward states
   */
  def executeFromExit(cfg: ControlFlowGraph, forwardStates: C,
                      refinedBackwardStates: C, exitState: S): C = {
    val backwStates = cfgStateFactory.makeFrom(cfg, refinedBackwardStates)
    val run = Run(cfg, backwStates, forwardStates, exitState, None)
    run.iterateCFG()
  }

  /**
   * Executes the refining backward interpretation  of a CFG.
   *
   * The abstract error under investigation occurs within the CFG,
   * so the abstract error state must be inserted there (just like an "initial" state)
   *
   * @param cfg the CFG (method body) to interpret
   * @param forwardStates resulting states of the forward analysis
   * @param refinedBackwardStates current CFG states of backward analysis
   *                              (may be used to "generalize" summaries,
   *                              an all-bottom CFGState can be provided otherwise)
   * @param exitState current exit state
   * @param inCFGError abstract error state and its position within the CFG
   * @return CFGState with refined backward states
   */
  def executeWithInCFGError(cfg: ControlFlowGraph, forwardStates: C,
                            refinedBackwardStates: C, exitState: S,
                            inCFGError: InCFGInitialState[S]): C = {
    val backwStates = cfgStateFactory.makeFrom(cfg, refinedBackwardStates)
    val run = Run(cfg, backwStates, forwardStates, exitState, Some(inCFGError))
    run.iterateCFG()
  }

  private case class Run(cfg: ControlFlowGraph, refinedStates: C, forwardStates: C, exitState: S,
                         fixedCFGState: Option[InCFGInitialState[S]]) {
    def iterateCFG(): C = {
      var blocksToProcessIds = iterationStartBlocks
      var iterationCountAtBlock = Map.empty[Int, Int]

      while (!blocksToProcessIds.isEmpty) {
        val currentBlockId = blocksToProcessIds.max
        blocksToProcessIds = blocksToProcessIds - currentBlockId
        val itNumber = iterationCountAtBlock.getOrElse(currentBlockId, 0)
        val currentExitState = computeBasicBlockExitState(currentBlockId, itNumber)
        val blockStates = refinedStates.statesOfBlock(currentBlockId)
        val oldExitState = blockStates.last
        val isFirstIter = itNumber == 0
        if (!currentExitState.lessEqual(oldExitState) || isFirstIter) {
          backwardExecuteBlock(currentExitState, currentBlockId)
          blocksToProcessIds = blocksToProcessIds ++ cfg.getDirectPredecessors(currentBlockId)
          iterationCountAtBlock = iterationCountAtBlock + (currentBlockId -> (itNumber + 1))
        }
      }
      refinedStates
    }

    def iterationStartBlocks: Set[Int] = {
      Set(cfg.exitBlockId) ++ fixedCFGState.map(_.cfgPos.blockIdx).toSet
    }

    def backwardExecuteBlock(exitState: S, currentBlockId: Int) = {
      val refinedPreStates = ListBuffer(exitState)

      val blockStmts = refinedStates.cfg.getBasicBlockStatements(currentBlockId)
      val blockForwardPreStates = forwardStates.statesOfBlock(currentBlockId)
      var postState = exitState
      for ((stmt, idx) <- blockStmts.zipWithIndex.reverse) {
        val oldPreState = blockForwardPreStates(idx)
        // Call "before" for trace partitioning (still needed? was in old code)
        val tempState = postState.before(ProgramPointUtils.identifyingPP(stmt))
        val transformedState = stmt.backwardSemantics(tempState, oldPreState)
        val backwardRefinedState = safeGlb(oldPreState, transformedState)
        val newPreState = joinWithInCFGInitialState(backwardRefinedState, CFGPosition(currentBlockId, idx))
        postState = newPreState
        refinedPreStates prepend newPreState
      }

      refinedStates.setStatesOfBlock(currentBlockId, refinedPreStates.toList)
    }

    /** Inserts (joins) the abstract error state if error occurs at current iteration positioin */
    def joinWithInCFGInitialState(refinedPre: S, cfgPos: CFGPosition): S = {
      fixedCFGState match {
        case Some(InCFGInitialState(init, initPos)) if initPos == cfgPos =>
          refinedPre.lub(init)
        case _ =>
          refinedPre
      }
    }

    /** Compute an updated block exit state from the entry states of successors */
    def computeBasicBlockExitState(blockIndex: Int, it: Int): S = {


      var result: S = refinedStates.stateFactory.bottom()
      val forwardPreStates = forwardStates.statesOfBlock(blockIndex)

      if (it == 0 && blockIndex == cfg.exitBlockId) {
        result = exitState
      } else {
        // Join with starting states of successor blocks
        for ((from, to, weight) <- refinedStates.cfg.exitEdges(blockIndex)) {
          val successorBlock = refinedStates.statesOfBlock(to)
          val filteredState =
            weight match {
              case Some(cond) =>
                if (cond) {
                  successorBlock.head.setExpression(forwardPreStates.last.expr).testTrue()
                } else {
                  successorBlock.head.setExpression(forwardPreStates.last.expr).testFalse()
                }
              case None =>
                successorBlock.head
            }

          result = result.lub(filteredState)
        }
      }

      // Apply  widening if necessary
      val currentBlockStates = refinedStates.statesOfBlock(blockIndex)
      val previousEntry = currentBlockStates.last
      if (it > SystemParameters.wideningLimit) {
        result = previousEntry.widening(result)
      }

      result = safeGlb(forwardPreStates.last, result)
      result
    }

    /**
     * A "safe" version of the glb that avoids problems with states spuriously becoming bottom
      * (in case they have different current expressions set)
      */
    private def safeGlb(pre: S, current: S): S = current.setExpression(pre.expr).strictGlb(pre)
  }
}

case class DefaultRefiningBackwardInterpreter[S <: State[S]](
    stateFactory: S) extends RefiningBackwardInterpreter[S] {
  type C = DefaultCFGState[S]

  val cfgStateFactory = DefaultCFGStateFactory[S](stateFactory)
}
