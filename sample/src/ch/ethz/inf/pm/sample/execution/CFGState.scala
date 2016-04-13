/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, ControlFlowGraph}

/**
 * Holds all the states associated with a ControlFlowGraph, that is the
 * pre- and post states of statements.
 *
 * The idea is to eventually get rid of ControlFlowGraphExecution (especially its mutability)
 * and separate the iteration logic from the states at program labels. Once this legacy is cleaned up,
 * we can enrich this trait with nicer methods and e.g. model program labels explicitly instead of juggling
 * around with lists of states.
 *
 * @tparam S the underlying state type
 */
trait CFGState[S <: State[S]] {
  def cfg: ControlFlowGraph

  def stateFactory: S

  def exitState(): S

  def statesOfBlock(idx: Int): List[S]

  // Once `CFGState`s are immutable, having the following method in this trait
  // should not be a problem anymore. It makes things simpler than having
  // a separate immutable and mutable `CFGState` type hierarchy.
  def setStatesOfBlock(blockIdx: Int, states: List[S])
}

/** Constructs `CFGState` objects from `ControlFlowGraph`s.
  * @todo should just have a method `make` and add methods `top` and `bottom`
  *       to `CFGState` instead. Requires immutability of `CFGState` first.
  */
trait CFGStateFactory[S <: State[S], C <: CFGState[S]] {
  def stateFactory: S

  def allBottom(cfg: ControlFlowGraph): C

  def allTop(cfg: ControlFlowGraph): C

  def makeFrom(cfg: ControlFlowGraph, cfgState: C): C
}

/** Holds methods that are independent of how states are actually stored
  * in the `CFGState`, i.e., by only using methods of the `CFGState` trait.
  *
  * @tparam S the underlying state type
  */
abstract class AbstractCFGState[S <: State[S]] extends CFGState[S] {
  def preStateAt(pos: CFGPosition): S = {
    val states = statesOfBlock(pos.blockIdx)
    states(pos.stmtIdx)
  }

  def postStateAt(pos: CFGPosition): S = {
    val states = statesOfBlock(pos.blockIdx)
    states(pos.stmtIdx+1)
  }

  def exitState(): S = {
    var result: S = stateFactory.bottom()
    for (blockId <- cfg.nodes.indices) {
      val states = statesOfBlock(blockId)
      var isExitPoint: Boolean = true
      for ((from, to, weight) <- cfg.edges) {
        if (from equals blockId)
          isExitPoint = false
      }
      if (isExitPoint) states match {
        case Nil =>
        case x => result = result.lub(states.last)

      }
    }
    result
  }

  def entryState(): S = statesOfBlock(0).head

  override def toString: String = {
    var result: String = ""
    for (blockId <- cfg.nodes.indices) {
      val blockStates = statesOfBlock(blockId)
      result = result +
        "Node n." + blockId + "\n-----------------\n" +
        "Preds: " + cfg.entryNodesToString(blockId) + "\n" +
        "Succs: " + cfg.exitNodesToString(blockId) + "\n"
      for ((state, idx) <- blockStates.zipWithIndex) {
        result = result + state.toString + "\n"
        if (idx < blockStates.size - 1)
          result = result + "| " + cfg.statementAt(blockId, idx).toSingleLineString + "\nV\n"
      }

      if (cfg.hasExitCondition(blockId)) {
        result += "exit cond: " + cfg.statementAt(blockId, blockStates.size-2) + "\n"
      }
    }
    result
  }
}
