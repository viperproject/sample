/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.SampleCfg.{SampleBlock, SampleEdge}
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.ProgramPoint
import viper.silver.cfg._

/**
  * Denotes a position of an element (i.e., a statement or a condition) in a
  * control flow graph.
  *
  * @author Jerome Dohrau
  * @author Caterina Urban
  *
  */
trait CfgPosition

/**
  * Denotes a position of a statement in a control flow graph that is located in
  * a basic block.
  *
  * @param block The basic block containing the statement.
  * @param index The index of the statement.
  * @author Jerome Dohrau
  */
case class BlockPosition(block: SampleBlock, index: Int)
  extends CfgPosition

/**
  * Denotes a position of a condition in a control flow graph that is located
  * on an edge.
  *
  * @param edge The edge containing the condition.
  * @author Jerome Dohrau
  */
case class EdgePosition(edge: SampleEdge)
  extends CfgPosition

/**
  * A result of an analysis that holds the pre- and post states of statements
  * within a control flow graph.
  *
  * @tparam S The type of the states.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
trait CfgResult[S <: State[S]] {
  /**
    * Returns the control flow graph.
    *
    * @return The control flow graph.
    */
  def cfg: SampleCfg

  /**
    * Returns the top state.
    *
    * @return The top state
    */
  def top: S = entryState().top()

  /**
    * Returns the bottom state.
    *
    * @return The bottom state.
    */
  def bottom: S = entryState().bottom()

  /**
    * Returns the state before the given position.
    *
    * @param position The position.
    * @return The state before the given position.
    */
  def preStateAt(position: CfgPosition): S = position match {
    case BlockPosition(block, index) => getStates(block)(index)
    case EdgePosition(edge) => getStates(edge.source).last
  }

  /**
    * Returns the state before the given program point.
    *
    * @param pp The program point.
    * @return The state before the given program point.
    */
  def preStateAt(pp: ProgramPoint): S = preStateAt(cfg.getPosition(pp))

  /**
    * Returns the state after the given position.
    *
    * @param position The position.
    * @return The state after the given position.
    */
  def postStateAt(position: CfgPosition): S = position match {
    case BlockPosition(block, index) => getStates(block)(index + 1)
    case EdgePosition(edge) => getStates(edge.target).head
  }

  /**
    * Returns the state after the given program point.
    *
    * @param pp The program point.
    * @return The state after the given program point.
    */
  def postStateAt(pp: ProgramPoint): S = postStateAt(cfg.getPosition(pp))

  /**
    * Returns the entry state of the given block. If no block is specified, the
    * entry state of the control flow graph is returned.
    *
    * @param block The block.
    * @return The entry state.
    */
  def entryState(block: SampleBlock = cfg.entry): S =
    getStates(block).head

  /**
    * Returns the exit state of the control flow graph.
    *
    * @return The exit state.
    */
  def exitState(): S =
    cfg.exit.map(exitState).getOrElse(bottom)

  /**
    * Returns the exit state of the given block.
    *
    * @param block The block.
    * @return The exit state.
    */
  def exitState(block: SampleBlock): S =
    getStates(block).last

  /**
    * Returns the states corresponding to the given basic block.
    *
    * @param block The basic block.
    * @return The states corresponding to the given basic block.
    */
  def getStates(block: SampleBlock): Seq[S]

  /**
    * Sets the states corresponding to the given basic block.
    *
    * @param block  The basic block.
    * @param states The states corresponding to the given basic block.
    */
  def setStates(block: SampleBlock, states: Seq[S]): Unit

  def initialize(state: S)

  /**
    * Prints the result to the standard output.
    */
  def print(): Unit = {
    def header(title: String): Unit = {
      println("\n----------------------------------------")
      println(title)
      println("----------------------------------------\n")
    }

    // print entry state
    header("Entry State")
    println(entryState())

    for (block <- cfg.blocks) {
      header(block.toString)
      // print entry state of block
      println(preStateAt(BlockPosition(block, 0)))
      for ((element, index) <- block.elements.map(_.merge).zipWithIndex) {
        // print statement or expression ...
        println()
        println(element)
        println()
        // ... and the state after it
        println(postStateAt(BlockPosition(block, index)))
      }
    }

    // print exit state
    header("Exit State")
    println(exitState())
  }
}

/**
  * Holds the final pre- and post state of statements within a control flow graph.
  *
  * @param cfg The control flow graph.
  * @tparam S The type of the states.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
class FinalCfgResult[S <: State[S]](val cfg: SampleCfg)
  extends CfgResult[S] {
  var statesMap: Map[Int, Seq[S]] = Map.empty

  override def getStates(block: SampleBlock): Seq[S] = statesMap.get(block.id) match {
    case Some(s) => s
    case None => throw new IllegalArgumentException(s"Block with id ${block.id} could not be found.")
  }

  override def setStates(block: SampleBlock, states: Seq[S]): Unit = {
    if (SystemParameters.DEBUG) {
      assert(states.length == size(block))
    }
    statesMap = statesMap + (block.id -> states)
  }

  override def initialize(state: S): Unit = {
    for (block <- cfg.blocks) {
      val states = Seq.fill(size(block))(state)
      statesMap = statesMap + (block.id -> states)
    }
  }

  /**
    * Returns the number of states associated with the given basic block.
    *
    * @param block The given basic block.
    * @return The number of states associated with the given basic block.
    */
  private def size(block: SampleBlock): Int = block match {
    case StatementBlock(stmts) => stmts.length + 1
    case PreconditionBlock(pres) => pres.length + 1
    case PostconditionBlock(posts) => posts.length + 1
    case LoopHeadBlock(invs, stmts) => invs.length + stmts.length + 1
    case ConstrainingBlock(_, _) => 2
  }
}

object FinalCfgResult {
  def apply[S <: State[S]](cfg: SampleCfg): FinalCfgResult[S] =
    new FinalCfgResult[S](cfg)
}