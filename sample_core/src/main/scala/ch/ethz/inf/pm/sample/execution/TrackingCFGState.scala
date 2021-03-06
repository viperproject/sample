/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph

/** Constructs `TrackingCFGState` objects from `ControlFlowGraph`s.
  * @todo share code with `DefaultCFGState`
  */
case class TrackingCFGStateFactory[S <: State[S]](stateFactory: S)
  extends CFGStateFactory[S, TrackingCFGState[S]] {

  def allBottom(cfg: ControlFlowGraph): TrackingCFGState[S] = {
    val result = TrackingCFGState(cfg, stateFactory)
    result.initializeStates(stateFactory.bottom())
    result
  }

  def allTop(cfg: ControlFlowGraph): TrackingCFGState[S] = {
    val result = TrackingCFGState(cfg, stateFactory)
    result.initializeStates(stateFactory.top())
    result
  }

  def makeFrom(cfg: ControlFlowGraph, cfgState: TrackingCFGState[S]): TrackingCFGState[S] = {
    val result = new TrackingCFGState[S](cfg, stateFactory)
    for (idx <- cfg.nodes.indices) {
      result.setTrackedStatesOfBlock(idx, cfgState.trackedStatesOfBlock(idx))
    }
    result
  }
}

/** Keeps track of all states that occurred throughout the interpretation.
  * That is, for each block, it holds a list of list of states.
  * The last inner list holds the most recent states (or the fixpoint if
  * the interpretation has already terminated).
  *
  * @todo refactor and share as much code as possible with `DefaultCFGState`
  */
case class TrackingCFGState[S <: State[S]](cfg: ControlFlowGraph, stateFactory: S) extends AbstractCFGState[S] {
  var blockStates: Map[Int, List[List[S]]] = Map.empty

  def lub(other:TrackingCFGState[S]):TrackingCFGState[S] = {
    val res = this.copy()
    res.blockStates =
      (for (i <- this.blockStates.keys ++ other.blockStates.keys) yield {
        val a = this.blockStates.getOrElse(i,List(List(stateFactory.bottom())))
        val b = other.blockStates.getOrElse(i,List(List(stateFactory.bottom())))
        val zipped = a.zipAll(b,a.last,b.last)
        val joined = zipped.map(x => x._1.zipAll(x._2,stateFactory.bottom(),stateFactory.bottom()).map(y => y._1 lub y._2))
        i -> joined
      }).toMap
    res
  }

  def initializeStates(state: S) {
    for ((stmts, blockIdx) <- cfg.nodes.zipWithIndex) {
      val states = List.fill(stmts.length + 1)(state)
      blockStates = blockStates + (blockIdx -> List(states))
    }
  }

  /** Returns just the most recent states in a block. */
  def statesOfBlock(idx: Int): List[S] = {
    blockStates.get(idx) match {
      case Some(s) => s.last
      case None => if (idx >= cfg.nodes.size) {
        throw new IllegalArgumentException("CFG block with index " + idx + " does not exist.")
      } else {
        throw new IllegalStateException("States for CFG block " + idx + " have not been initialized")
      }
    }
  }

  def trackedStatesOfBlock(idx: Int): List[List[S]] = {
    blockStates(idx)
  }


  def setTrackedStatesOfBlock(blockIdx: Int, states: List[List[S]]) {
    blockStates += blockIdx -> states
  }

  def setStatesOfBlock(blockIdx: Int, states: List[S]) {
    val stmts = cfg.getBasicBlockStatements(blockIdx)
    val expectedLength = stmts.length + 1
    if (SystemParameters.DEBUG) assert (expectedLength == states.length)
    blockStates += blockIdx -> (blockStates(blockIdx) :+ states)
  }
}
