package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, ControlFlowGraph}

/** Constructs `DefaultCFGState` objects from `ControlFlowGraph`s. */
case class DefaultCFGStateFactory[S <: State[S]](stateFactory: S)
  extends CFGStateFactory[S, DefaultCFGState[S]] {

  def allBottom(cfg: ControlFlowGraph): DefaultCFGState[S] = {
    val result = new DefaultCFGState(cfg, stateFactory)
    result.initializeStates(stateFactory.bottom())
    result
  }

  def allTop(cfg: ControlFlowGraph): DefaultCFGState[S] = {
    val result = new DefaultCFGState(cfg, stateFactory)
    result.initializeStates(stateFactory.top())
    result
  }

  def makeFrom(cfg: ControlFlowGraph, cfgState: CFGState[S]): DefaultCFGState[S] = {
    val result = new DefaultCFGState(cfg, stateFactory)
    for (idx <- 0 until cfg.nodes.size) {
      result.setStatesOfBlock(idx, cfgState.statesOfBlock(idx))
    }
    result
  }
}

/** Default implementation of `CFGState` that just keeps the most recent
  * pre- and post-state of each statement in the associated `ControlFlowGraph`.
  *
  * @param cfg the control flow graph for which to store the states for
  * @tparam S the underlying state type
  * @todo make it immutable
  */
class DefaultCFGState[S <: State[S]](val cfg: ControlFlowGraph, val stateFactory: S) extends AbstractCFGState[S] {
   var blockStates: Map[Int, List[S]] = Map.empty

   def initializeStates(state: S) {
     for ((stmts, blockIdx) <- cfg.nodes.zipWithIndex) {
       val states = List.fill(stmts.length + 1)(state)
       blockStates = blockStates + (blockIdx -> states)
     }
   }

   def statesOfBlock(idx: Int): List[S] = {
     blockStates.get(idx) match {
       case Some(s) => s
       case None => if (idx >= cfg.nodes.size) {
         throw new IllegalArgumentException("CFG block with index " + idx + " does not exist.")
       } else {
         throw new IllegalStateException("States for CFG block " + idx + " have not been initialized")
       }
     }
   }

   def setStatesOfBlock(blockIdx: Int, states: List[S]) {
     val stmts = cfg.getBasicBlockStatements(blockIdx)
     val expectedLength = stmts.length + 1
     assert (expectedLength == states.length)
     blockStates += blockIdx -> states
   }
 }