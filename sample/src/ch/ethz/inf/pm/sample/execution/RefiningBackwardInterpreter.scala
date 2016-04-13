/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.SystemParameters
import scala.collection.mutable.ListBuffer
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPointUtils, ControlFlowGraph}

trait RefiningBackwardInterpreter[S <: State[S]] extends Interpreter[S] {
  def execute(cfg: ControlFlowGraph, forwardStates: C, errorInfo: AbstractErrorInfo[S]): C = {
    val refinedStates = cfgStateFactory.allBottom(cfg)
    val run = Run(refinedStates, forwardStates, errorInfo)

    val backwardStartBlock = errorInfo.cfgPosition.blockIdx
    var blocksToProcessIds = Set(backwardStartBlock)
    var iterationCountAtBlock = Map.empty[Int, Int]

    while (blocksToProcessIds.nonEmpty) {
      val currentBlockId = blocksToProcessIds.max
      blocksToProcessIds = blocksToProcessIds - currentBlockId
      val itNumber = iterationCountAtBlock.getOrElse(currentBlockId, 0)
      val currentExitState = run.computeBasicBlockExitState(currentBlockId, itNumber)
      val blockStates = refinedStates.statesOfBlock(currentBlockId)
      val oldExitState = blockStates.last
      val isFirstIter = itNumber == 0
      if (!currentExitState.lessEqual(oldExitState) || isFirstIter) {
        run.backwardExecuteBlock(currentExitState, currentBlockId)
        blocksToProcessIds = blocksToProcessIds ++ cfg.getDirectPredecessors(currentBlockId)
        iterationCountAtBlock = iterationCountAtBlock + (currentBlockId -> (itNumber + 1))
      }
    }
    refinedStates
  }

  private case class Run(refinedStates: C, forwardStates: C, errorInfo: AbstractErrorInfo[S]) {
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
        val newPreState =
          if (errorInfo.cfgPosition.blockIdx == currentBlockId && errorInfo.cfgPosition.stmtIdx == idx) {
            backwardRefinedState.lub(errorInfo.errorState)
          } else backwardRefinedState
        postState = newPreState
        refinedPreStates prepend newPreState
      }

      refinedStates.setStatesOfBlock(currentBlockId, refinedPreStates.toList)
    }

    def computeBasicBlockExitState(blockIndex: Int, it: Int): S = {
      var result: S = refinedStates.stateFactory.bottom()
      val forwardPreStates = forwardStates.statesOfBlock(blockIndex)

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

      result

      // Apply  widening if necessary
      val currentBlockStates = refinedStates.statesOfBlock(blockIndex)
      val previousEntry = currentBlockStates.last
      if (it > SystemParameters.wideningLimit) {
        result = previousEntry.widening(result)
      }

      result = safeGlb(forwardPreStates.last, result)
      result
    }

    private def safeGlb(pre: S, current: S): S = current.setExpression(pre.expr).strictGlb(pre)
  }
}

case class DefaultRefiningBackwardInterpreter[S <: State[S]](
    stateFactory: S) extends RefiningBackwardInterpreter[S] {
  type C = DefaultCFGState[S]

  val cfgStateFactory = DefaultCFGStateFactory[S](stateFactory)
}
