package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, ControlFlowGraph}

object MapBasedCFGState {
   def allBottom[S <: State[S]](cfg: ControlFlowGraph, stateFactory: S): MapBasedCFGState[S] = {
     val result = new MapBasedCFGState(cfg, stateFactory)
     result.initializeStates(stateFactory.bottom())
     result
   }

   def allTop[S <: State[S]](cfg: ControlFlowGraph, stateFactory: S): MapBasedCFGState[S] = {
     val result = new MapBasedCFGState(cfg, stateFactory)
     result.initializeStates(stateFactory.top())
     result
   }

   def copyOf[S <: State[S]](cfgState: MapBasedCFGState[S]): MapBasedCFGState[S] = {
     val result = new MapBasedCFGState(cfgState.cfg, cfgState.factoryState)
     result.blockStates = cfgState.blockStates
     result
   }
 }

class MapBasedCFGState[S <: State[S]](val cfg: ControlFlowGraph, val factoryState: S) extends CFGState[S] {
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

   def preStateAt(pos: CFGPosition): S = {
     val states = statesOfBlock(pos.blockIdx)
     states(pos.stmtIdx)
   }

   def postStateAt(pos: CFGPosition): S = {
     val states = statesOfBlock(pos.blockIdx)
     states(pos.stmtIdx+1)
   }

   def setStatesOfBlock(blockIdx: Int, states: List[S]) {
     val stmts = cfg.getBasicBlockStatements(blockIdx)
     val expectedLength = stmts.length + 1
     assert (expectedLength == states.length)
     blockStates += blockIdx -> states
   }

   def exitState(): S = {
     var result: S = factoryState.bottom()
     for ((blockId, states) <- blockStates) {
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