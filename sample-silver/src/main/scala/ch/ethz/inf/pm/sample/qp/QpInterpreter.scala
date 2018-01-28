/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.SampleCfg.{SampleBlock, SampleEdge}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.permissionanalysis._
import viper.silver.cfg._

import scala.collection.mutable

case class QpInterpreter[D <: QpDomain[D]](cfg: SampleCfg, initial: QpState[D])
  extends SilverInterpreter[QpState[D]] {

  override def cfg(current: WorklistElement): SampleCfg = cfg

  override def initial(cfg: SampleCfg): QpState[D] = initial

  override def initializeResult(cfg: SampleCfg, state: QpState[D]): CfgResult[QpState[D]] = {
    val result = FinalCfgResult[QpState[D]](cfg)
    result.initialize(state)
    result
  }

  override def execute(): CfgResult[QpState[D]] = {
    // initialize
    implicit val result = initializeResult(cfg, initial.bottom())
    implicit val visited = mutable.Set[SampleBlock]()
    implicit val worklist = mutable.Stack[SampleBlock]()

    val exit = cfg.exit.get
    updateExitState(exit, initial)
    worklist.push(exit)

    while (worklist.nonEmpty) {
      // get current block
      val current = worklist.pop()
      visited.add(current)

      val state = result.exitState(current)
      val states = mutable.ListBuffer(state)
      current match {
        case StatementBlock(statements) =>
          // execute statements
          var successor = state
          statements.reverse.foreach(statement => {
            val predecessor = executeStatement(statement, successor)
            states.append(predecessor)
            successor = predecessor
          })
        case PreconditionBlock(preconditions) =>
          // process preconditions
          preconditions.foldRight(state) { (precondition, successor) =>
            val predecessor = executeCommand(PreconditionCommand, precondition, successor)
            states.append(predecessor)
            predecessor
          }
        case PostconditionBlock(postconditions) =>
          // process postconditions
          postconditions.foldRight(state) { (postcondition, successor) =>
            val predecessor = executeCommand(PostconditionCommand, postcondition, successor)
            states.append(predecessor)
            predecessor
          }
        case LoopHeadBlock(invariants, statements) =>
          // execute statements
          var successor = state
          statements.reverse.foreach(statement => {
            val predecessor = executeStatement(statement, successor)
            states.append(predecessor)
            successor = predecessor
          })
          // process invariants
          successor = invariants.foldRight(successor) { (invariant, successor) =>
            val predecessor = executeCommand(InvariantCommand, invariant, successor)
            states.append(predecessor)
            predecessor
          }
      }

      val reversed = states.reverse
      val first = reversed.head
      result.setStates(current, reversed.toList)

      val edges = current match {
        case LoopHeadBlock(_, _) =>
          // only process the predecessors corresponding to the in edges because
          // the predecessors corresponding to the body of the loop have already
          // been processed.
          cfg.inEdges(current).filter(_.isIn)
        case _ =>
          cfg.inEdges(current)
      }

      edges.foreach { edge =>
        // update exit state of predecessor block
        val predecessor = edge.source
        val processed = processEdge(edge, first)
        updateExitState(predecessor, processed)

        // check whether all dependencies have been processed
        if (cfg.successors(predecessor).forall(visited.contains)) {
          // enqueue predecessor block
          if (!worklist.contains(predecessor)) worklist.push(predecessor)
        } else predecessor match {
          case LoopHeadBlock(_, _) =>
            // check if all blocks after the loop have been processed
            val outEdges = cfg.outEdges(predecessor)
            val afterBlocks = outEdges.filter(_.isOut).map(_.target)
            if (afterBlocks.forall(visited.contains)) {
              // enqueue inner blocks
              val innerState = processed.reset()
              val innerEdges = cfg.inEdges(predecessor).filterNot(_.isIn)
              innerEdges.foreach { innerEdge =>
                val innerBlock = innerEdge.source
                updateExitState(innerBlock, innerState)
                if (!worklist.contains(innerBlock)) worklist.push(innerBlock)
              }
            }
          case _ => // do nothing
        }
      }
    }

    result
  }

  /**
    * Returns the state obtained from processing the given edge in the given
    * state.
    *
    * @param edge  The edge to process.
    * @param state The state.
    * @return The state after processing the edge.
    */
  protected def processEdge(edge: SampleEdge, state: QpState[D]): QpState[D] = {
    // enter loop if it is an in-edge
    val entered = edge.kind match {
      case Kind.In =>
        val changing = cfg.changingVariables(edge.target)
        val position = BlockPosition(edge.target, 0)
        state.enterLoop(changing, position)
      case _ => state
    }
    // filter state if there is a condition
    val filtered = edge match {
      case ConditionalEdge(condition, _, _, _) => assumeCondition(condition, entered)
      case UnconditionalEdge(_, _, _) => entered
    }
    // exit loop if it is an out-edge
    edge.kind match {
      case Kind.Out => filtered.command(LeaveLoopCommand())
      case _ => filtered
    }
  }

  /**
    * Returns the state obtained from executing the given statement in the given
    * state.
    *
    * @param statement The statement to execute.
    * @param state     The state.
    * @return The state after executing the statement.
    */
  protected def executeStatement(statement: Statement, state: QpState[D]): QpState[D] = {
    val successor = state.before(ProgramPointUtils.identifyingPP(statement))
    val predecessor = statement.backwardSemantics(successor)
    predecessor
  }

  /**
    * Returns the state obtained from executing the given command in the given
    * state.
    *
    * @param command  The command.
    * @param argument The argument to the command.
    * @param state    The state.
    * @return The state after executing the command.
    */
  protected def executeCommand(command: (ExpressionSet) => Command, argument: Statement, state: QpState[D]): QpState[D] = {
    val successor = state.before(ProgramPointUtils.identifyingPP(argument))
    val evaluated = argument.backwardSemantics(successor)
    val expression = evaluated.expr
    val predecessor = evaluated.command(command(expression))
    predecessor
  }

  /**
    * Returns the state obtained from assuming the given condition in the given
    * state.
    *
    * @param condition The condition.
    * @param state     The state.
    * @return The state after assuming the condition.
    */
  protected def assumeCondition(condition: Statement, state: QpState[D]): QpState[D] = {
    val predecessor = state.before(ProgramPointUtils.identifyingPP(condition))
    condition.forwardSemantics(predecessor).testTrue()
  }

  /**
    * Updates the exit state of the given block by computing the least upper
    * bound of the existing state and the given state.
    *
    * @param block  The block.
    * @param state  The state.
    * @param result The implicitly passed result.
    */
  private def updateExitState(block: SampleBlock, state: QpState[D])(implicit result: CfgResult[QpState[D]]): Unit = {
    val states = result.getStates(block)
    val updated = states.init :+ (states.last lub state)
    result.setStates(block, updated)
  }
}