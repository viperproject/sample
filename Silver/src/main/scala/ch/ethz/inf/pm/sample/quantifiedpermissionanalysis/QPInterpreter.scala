/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleBlock
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation._
import com.typesafe.scalalogging.LazyLogging
import viper.silver.cfg._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Severin Münger
  *         Added on 29.11.16.
  */
sealed trait QPInterpreter extends SilverInterpreter[QuantifiedPermissionsState] with LazyLogging {

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
}


final class QPBackwardInterpreter extends QPInterpreter {

  override def execute(cfg: SampleCfg, finalState: QuantifiedPermissionsState): CfgResult[QuantifiedPermissionsState] = {
    determineBlockTypes(cfg, cfg.entry)
    val bottom = finalState.bottom()
    val cfgResult: CfgResult[QuantifiedPermissionsState] = initializeResult(cfg, bottom)
    val ordering: Ordering[SampleBlock] = Ordering.by(block => (-nestingLevels(block), -sequenceNumbers(block)))
    var worklist: mutable.SortedSet[SampleBlock] = mutable.SortedSet[SampleBlock](cfg.exit)(ordering)
    var iterations = Map[SampleBlock, Int]()
    while (worklist.nonEmpty) {
      val currentBlock: SampleBlock = worklist.head
      worklist.remove(currentBlock)
      val currentCount: Int = iterations.getOrElse(currentBlock, 0)
      val exitEdges = cfg.outEdges(currentBlock)
      val currentState: QuantifiedPermissionsState =
        if (cfg.exit == currentBlock) finalState
        else exitEdges.size match {
          case 1 => cfgResult.getStates(cfg.outEdges(currentBlock).head.target).head
          case 2 =>
            // TODO: With the new CFG this became very hacky, can this be solved more properly?
            val (edge1, edge2) =  (exitEdges.head.asInstanceOf[ConditionalEdge[Statement, Statement]], exitEdges.last.asInstanceOf[ConditionalEdge[Statement, Statement]])
            val (state1: QuantifiedPermissionsState, state2: QuantifiedPermissionsState) = (cfgResult.getStates(edge1.target).head, cfgResult.getStates(edge2.target).head)
            val cond1 = edge1.condition.specialBackwardSemantics(state1.lub(state2)).expr.getSingle.get
            val cond2 = edge2.condition.specialBackwardSemantics(state1.lub(state2)).expr.getSingle.get
            val cond1Conjuncts = Utils.toCNFConjuncts(cond1)
            val cond2Conjuncts = Utils.toCNFConjuncts(cond2)
            val pp = edge1.condition.getPC()
            val (joined, condConjuncts) =
              if (cond1Conjuncts.size > cond2Conjuncts.size) (state1.lub(state2, cond1).before(pp), cond1Conjuncts)
              else (state2.lub(state1, cond2).before(pp), cond2Conjuncts)
            condConjuncts.foldRight(joined)((conjunct, filtered) => filtered.assume(conjunct)).after(pp)
          case _ => throw new IllegalStateException("A non-leaf node must have at least one and at most two exit edges.")
        }
      val blockStates = cfgResult.getStates(currentBlock)
      val oldState: QuantifiedPermissionsState = if (blockStates.isEmpty) finalState else blockStates.last
      if (!currentState.lessEqual(oldState)) {
        backwardExecuteBlock(currentState, currentBlock, currentCount, cfgResult)
        worklist ++= cfg.predecessors(currentBlock)
        iterations += currentBlock -> (currentCount + 1)
      }
    }
    if (QuantifiedPermissionsParameters.useSetSimplifications)
      simplifySets(cfgResult)
    if (QuantifiedPermissionsParameters.useExpressionsSimplifications)
      cfg.blocks.foreach(block => cfgResult.setStates(block, cfgResult.getStates(block).map(state => state.copy(permissions = state.permissions.simplify, refSets = state.refSets.mapValues(_.simplify)))))
    cfgResult
  }

  private def simplifySets(cfgResult: CfgResult[QuantifiedPermissionsState]) = {
    val cfg = cfgResult.cfg
    type Key = (ProgramPoint, Expression)

    var differentSets: Map[SampleBlock, Map[Key, Set[Key]]] = Map()
    val relevantBlocks = cfg.blocks.filter {
      case PreconditionBlock(_) | LoopHeadBlock(_, _) => true
      case _ => false
    }
    relevantBlocks.foreach { block =>
      val refSets = cfgResult.getStates(block).head.refSets
      differentSets += block -> refSets.foldLeft[Map[Key, Set[Key]]](Map()) {
        case (map, (key, refSet)) => map + (key -> refSets.keys.filterNot(refSets(_).isEquivalentDescription(refSet)).toSet)
      }
    }
    val allKeys = differentSets.values.toSet.flatMap((map: Map[Key, Set[Key]]) => map.keys.toSet)
    val mergeMap: Map[Key, Set[Key]] = differentSets.foldLeft(Map[Key, Set[Key]]()) {
      case (map, (_, differentSet)) => map ++ differentSet.transform {
        case (key, set) => if (map.contains(key)) set ++ map(key) else set
      }
    }.transform((_, set) => allKeys -- set)
    val setKeys: Map[Set[Key], Key] = mergeMap.foldLeft(Map[Set[Key], Key]()) {
      case (map, (key, set)) => if (!map.contains(set)) map + (set -> key) else map
    }
    val replacements: Map[Key, Key] = mergeMap.transform((_, set) => setKeys(set))
    relevantBlocks.foreach(block => cfgResult.setStates(block, cfgResult.getStates(block).map(state => state.copy(refSets = state.refSets.transform { case (key, refSet: ReferenceSetDescription.Inner) => refSet.copy(key = replacements(key)) }))))
  }

  private def backwardExecuteBlock(exitState: QuantifiedPermissionsState, block: SampleBlock, count: Int, cfgResult: CfgResult[QuantifiedPermissionsState]): Unit = {
    val newStates = ListBuffer[QuantifiedPermissionsState]()
    val stmts: Seq[Statement] = block match {
      case StatementBlock(statements) => statements
      case LoopHeadBlock(invariants, statements) => invariants ++ statements
      case PostconditionBlock(posts) => posts
      case PreconditionBlock(pres) => pres
    }
    var postState: QuantifiedPermissionsState = exitState
    for ((stmt: Statement, _: Int) <- stmts.zipWithIndex.reverse) {
      newStates.prepend(postState)
      val pp = ProgramPointUtils.identifyingPP(stmt)
      val preState: QuantifiedPermissionsState = stmt.specialBackwardSemantics(postState.before(pp)).after(pp)
      logger.info(postState.toString)
      logger.info(stmt.toString)
      logger.info(preState.toString)
      postState = preState
    }
    if (cfgResult.cfg.outEdges(block).size > 1 && count > SystemParameters.wideningLimit) {
      val blockStates: Seq[QuantifiedPermissionsState] = cfgResult.getStates(block)
      postState = blockStates.head widening postState
    }
    newStates.prepend(postState)
    cfgResult.setStates(block, newStates.toList)
  }

}

final class QPForwardInterpreter extends QPInterpreter {

  override def execute(cfg: SampleCfg, initial: QuantifiedPermissionsState): CfgResult[QuantifiedPermissionsState] = {
    null
  }
}