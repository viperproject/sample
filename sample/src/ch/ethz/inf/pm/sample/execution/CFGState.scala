package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph

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

  def factoryState: S

  def exitState(): S

  def statesOfBlock(idx: Int): List[S]
}
