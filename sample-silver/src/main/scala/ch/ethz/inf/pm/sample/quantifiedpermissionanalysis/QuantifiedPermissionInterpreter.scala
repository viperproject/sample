/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Command, ExpressionSet, State}
import ch.ethz.inf.pm.sample.execution.SampleCfg.{SampleBlock, SampleEdge}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPointUtils, Statement}
import ch.ethz.inf.pm.sample.permissionanalysis._
import viper.silver.cfg._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * An interpreter used for the quantified permission inference. It visits every
  * block only once and traverses the control flow graph from the exit block to
  * the entry block while prioritizing all blocks from inner loops over the
  * blocks of the outer loops.
  *
  * @param cfg     The control flow graph.
  * @param initial The initial state.
  * @author Jerome Dohrau
  * @author Severin Münger
  */
case class QuantifiedPermissionInterpreter(cfg: SampleCfg, initial: QuantifiedPermissionState)
  extends SilverInterpreter[QuantifiedPermissionState] {

  type S = QuantifiedPermissionState

  override def cfg(current: WorklistElement): SampleCfg = cfg

  override def initial(cfg: SampleCfg): S = initial

  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val result = FinalCfgResult[S](cfg)
    result.initialize(state)
    result
  }

  override def execute(): CfgResult[S] = {
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
      val states = ListBuffer(state)
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
          // TODO: Maybe we do not want to introduce a new state?
          val projected = successor.project
          states.append(projected)
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
          worklist.push(predecessor)
        } else predecessor match {
          case LoopHeadBlock(_, _) =>
            // check if all blocks after the loop have been processed
            val outEdges = cfg.outEdges(predecessor)
            val afterBlocks = outEdges.filter(_.isOut).map(_.target)
            if (afterBlocks.forall(visited.contains)) {
              // enqueue inner blocks
              val innerEdges = cfg.inEdges(predecessor).filterNot(_.isIn)
              innerEdges.foreach { innerEdge =>
                val innerBlock = innerEdge.source
                updateExitState(innerBlock, initial)
                worklist.push(innerBlock)
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
  protected def processEdge(edge: SampleEdge, state: S): S = {
    // filter state if there is a condition
    val filtered = edge match {
      case ConditionalEdge(condition, _, _, _) => assumeCondition(condition, state)
      case UnconditionalEdge(_, _, _) => state
    }
    // adapt state according to the kind of the edge
    edge.kind match {
      case Kind.In => filtered.command(EnterLoopCommand())
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
  protected def executeStatement(statement: Statement, state: S): S = {
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
  protected def executeCommand(command: (ExpressionSet) => Command, argument: Statement, state: S): S = {
    val successor = state.before(ProgramPointUtils.identifyingPP(argument))
    val evaluated: S = argument.backwardSemantics(successor)
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
  protected def assumeCondition(condition: Statement, state: S): S = {
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
  private def updateExitState(block: SampleBlock, state: S)(implicit result: CfgResult[S]): Unit = {
    val states = result.getStates(block)
    val updated = states.init :+ (states.last lub state)
    result.setStates(block, updated)
  }
}
