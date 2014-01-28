package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, ControlFlowGraph}

/** Constructs `CFGState` objects from `ControlFlowGraph`s. */
trait CFGStateFactory[S <: State[S], C <: CFGState[S]] {
  def stateFactory: S

  def allBottom(cfg: ControlFlowGraph): C

  def allTop(cfg: ControlFlowGraph): C
}

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
    for (blockId <- 0 until cfg.nodes.size) {
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
    for (blockId <- 0 until cfg.nodes.size) {
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