/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleBlock
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation._
import com.typesafe.scalalogging.LazyLogging
import viper.silver.cfg.{ConditionalEdge, Kind, LoopHeadBlock, StatementBlock}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Severin Münger
  *         Added on 29.11.16.
  */
final class QPInterpreter extends SilverInterpreter[QuantifiedPermissionsState] with LazyLogging {

  var blocksLastInLoop: Set[SampleBlock] = Set()
  var nestingLevels: Map[SampleBlock, Int] = Map()
  var sequenceNumbers: Map[SampleBlock, Int] = Map()
  var currentSequenceNumbers: Map[Int, Int] = Map()

  def determineBlockTypes(cfg: SampleCfg, block: SampleBlock, nestingLevel: Int = 0, visited: Set[SampleBlock] = Set()): Boolean = {
    if (visited.contains(block)) {
      blocksLastInLoop += block
      true
    } else {
      nestingLevels += block -> nestingLevel
      val sequenceNumber = currentSequenceNumbers.getOrElse(nestingLevel, -1) + 1
      sequenceNumbers += block -> sequenceNumber
      currentSequenceNumbers += nestingLevel -> sequenceNumber
      val exitEdges = cfg.outEdges(block)
      exitEdges.size match {
        case 0 =>
          false
        case 1 =>
          val successor = cfg.successors(block).head
          determineBlockTypes(cfg, successor, if (nestingLevel > 0 && cfg.inEdges(successor).size > 1) nestingLevel - 1 else nestingLevel, visited + block)
        case 2 =>
          val (trueEdge, falseEdge) = exitEdges.head.kind match {
            case Kind.In => (exitEdges.head, exitEdges.last)
            case _ => (exitEdges.last, exitEdges.head)
          }
          val (toTrue, toFalse) = (trueEdge.target, falseEdge.target)
          determineBlockTypes(cfg, toTrue, nestingLevel + 1, visited + block)
          if (block.isInstanceOf[LoopHeadBlock[_, _]]) determineBlockTypes(cfg, toFalse, nestingLevel, visited + block)
          else determineBlockTypes(cfg, toFalse, nestingLevel + 1, visited + block)
      }
    }
  }

  override protected def initializeResult(cfg: SampleCfg, state: QuantifiedPermissionsState): CfgResult[QuantifiedPermissionsState] = {
    val cfgResult = FinalCfgResult[QuantifiedPermissionsState](cfg)
    cfgResult.initialize(state)
    cfgResult
  }

  override def execute(cfg: SampleCfg, initial: QuantifiedPermissionsState): CfgResult[QuantifiedPermissionsState] = backwardExecute(cfg, initial)

  def backwardExecute(cfg: SampleCfg, finalState: QuantifiedPermissionsState): CfgResult[QuantifiedPermissionsState] = {
    determineBlockTypes(cfg, cfg.entry)
    val bottom = finalState.bottom()
    val cfgResult: CfgResult[QuantifiedPermissionsState] = initializeResult(cfg, bottom)
    val ordering: Ordering[SampleBlock] = Ordering.by(block => (-nestingLevels(block), -sequenceNumbers(block)))
    var blocksToProcessIds: mutable.SortedSet[SampleBlock] = mutable.SortedSet[SampleBlock](cfg.exit)(ordering)
    var iterationAtBlock = Map[SampleBlock, Int]()
    while (blocksToProcessIds.nonEmpty) {
      val currentBlock: SampleBlock = blocksToProcessIds.head; blocksToProcessIds.remove(currentBlock)
      val currentCount: Int = iterationAtBlock.getOrElse(currentBlock, 0)
      val exitEdges = cfg.outEdges(currentBlock)
      val currentState: QuantifiedPermissionsState =
        if (cfg.exit == currentBlock) finalState
        else exitEdges.size match {
          case 1 => cfgResult.getStates(cfg.outEdges(currentBlock).head.target).head
          case 2 =>
            val (edge1, edge2) = (exitEdges.head.asInstanceOf[ConditionalEdge[Statement, Statement]], exitEdges.last.asInstanceOf[ConditionalEdge[Statement, Statement]])
            val (state1: QuantifiedPermissionsState, state2: QuantifiedPermissionsState) = (cfgResult.getStates(edge1.target).head, cfgResult.getStates(edge2.target).head)
            val cond = edge1.condition.forwardSemantics(state1.lub(state2)).expr
            state1.lub(state2, cond)
          case _ => throw new IllegalStateException("A non-leaf node must have at least one and at most two exit edges.")
        }
      val blockStates = cfgResult.getStates(currentBlock)
      val oldState: QuantifiedPermissionsState = if (blockStates.isEmpty) finalState else blockStates.last
      if (!currentState.lessEqual(oldState)) {
        backwardExecuteBlock(currentState, currentBlock, currentCount, cfgResult)
        blocksToProcessIds ++= cfg.predecessors(currentBlock)
        iterationAtBlock += currentBlock -> (currentCount + 1)
      }
    }
    cfgResult
  }

  private def backwardExecuteBlock(exitState: QuantifiedPermissionsState, block: SampleBlock, count: Int, cfgState: CfgResult[QuantifiedPermissionsState]): Unit = {
    var newStates = ListBuffer[QuantifiedPermissionsState]()
    val stmts: Seq[Statement] = block match {
      case StatementBlock(statements) => statements
      case LoopHeadBlock(invariants, statements) => invariants ++ statements
      case _ => Seq()
    }
    var nextState: QuantifiedPermissionsState = exitState
    for ((stmt: Statement, _: Int) <- stmts.zipWithIndex.reverse) {
      newStates = nextState +: newStates
      val pp = ProgramPointUtils.identifyingPP(stmt)
      val prevState: QuantifiedPermissionsState = stmt.specialBackwardSemantics(nextState.before(pp)).after(pp)
//      logger.info(nextState.toString)
//      logger.info(stmt.toString)
//      logger.info(prevState.toString)
      nextState = prevState
    }
    if (cfgState.cfg.outEdges(block).size > 1 && count > SystemParameters.wideningLimit) {
      val blockStates: Seq[QuantifiedPermissionsState] = cfgState.getStates(block)
      nextState = blockStates.head widening nextState
      newStates = nextState +: newStates
    } else {
      newStates = nextState +: newStates
    }
    cfgState.setStates(block, newStates.toList)
  }

}