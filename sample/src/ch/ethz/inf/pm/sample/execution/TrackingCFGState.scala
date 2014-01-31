package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph

/** Constructs `TrackingCFGState` objects from `ControlFlowGraph`s.
  * @todo share code with `DefaultCFGState`
  */
case class TrackingCFGStateFactory[S <: State[S]](stateFactory: S)
  extends CFGStateFactory[S, TrackingCFGState[S]] {

  def allBottom(cfg: ControlFlowGraph): TrackingCFGState[S] = {
    val result = new TrackingCFGState(cfg, stateFactory)
    result.initializeStates(stateFactory.bottom())
    result
  }

  def allTop(cfg: ControlFlowGraph): TrackingCFGState[S] = {
    val result = new TrackingCFGState(cfg, stateFactory)
    result.initializeStates(stateFactory.top())
    result
  }

  def makeFrom(cfg: ControlFlowGraph, cfgState: CFGState[S]): TrackingCFGState[S] = {
    ???
  }
}

/** Keeps track of all states that occurred throughout the interpretation.
  * That is, for each block, it holds a list of list of states.
  * The last inner list holds the most recent states (or the fixpoint if
  * the interpretation has already terminated).
  *
  * @todo refactor and share as much code as possible with `DefaultCFGState`
  */
class TrackingCFGState[S <: State[S]](val cfg: ControlFlowGraph, val stateFactory: S) extends AbstractCFGState[S] {
  var blockStates: Map[Int, List[List[S]]] = Map.empty

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

  def setStatesOfBlock(blockIdx: Int, states: List[S]) {
    val stmts = cfg.getBasicBlockStatements(blockIdx)
    val expectedLength = stmts.length + 1
    assert (expectedLength == states.length)
    blockStates += blockIdx -> (blockStates(blockIdx) :+ states)
  }
}
