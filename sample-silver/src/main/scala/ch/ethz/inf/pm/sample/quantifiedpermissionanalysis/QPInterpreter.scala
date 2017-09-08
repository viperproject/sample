/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Command, ExpressionSet}
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleBlock
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPointUtils, Statement}
import ch.ethz.inf.pm.sample.permissionanalysis._
import com.typesafe.scalalogging.LazyLogging
import viper.silver.cfg._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * An interpreter used for the quantified permission inference. It visits every
  * block only once and traverses the CFG from be exit to the entry block while
  * prioritizing all blocks from inner loops over the blocks of the outer loops.
  *
  * @author Jerome Dohrau
  * @author Severin Münger
  */
final class QPInterpreter(val cfg: SampleCfg, val initial: QuantifiedPermissionsState)
  extends SilverInterpreter[QuantifiedPermissionsState] with LazyLogging {
  /**
    * Look up the CFG for the current worklist element. Intraprocedural interpreters can always return the same
    * CFG (there is only one cfg to analyze). Interprocedural interpreters on the other hand need to look up the
    * method/cfg depending on the actual element in the worklist.
    *
    * @param current The element in the worklist
    * @return The SampleCfg containing this block
    */
  override def cfg(current: WorklistElement): SampleCfg = cfg

  /**
    * Create or look up the initial state for a given cfg. Intraprocedural interpreters can always return the same
    * initial state. Interprocedural interpreters on the other hand need to create an initial state depending on the
    * given CFG.
    *
    * @param cfg The control flow graph
    * @return The initial state for a given cfg.
    */
  override def initial(cfg: SampleCfg): QuantifiedPermissionsState = initial

  /**
    * Executes the control flow graph.
    *
    * @return The result of the execution.
    */
  override def execute(): CfgResult[QuantifiedPermissionsState] = {
    val bottom = initial.bottom()
    val cfgResult = initializeResult(cfg, bottom)

    implicit val visited = mutable.Set[SampleBlock]()
    implicit val worklist = mutable.Stack[SampleBlock]()

    // push exit block
    worklist.push(cfg.exit.get)

    while (worklist.nonEmpty) {
      val current = worklist.pop()
      visited.add(current)

      val exit = if (cfg.exit.get == current) {
        initial(cfg)
      } else {
        var state = bottom
        // join outgoing states
        val edges = cfg.outEdges(current)
        for (edge <- edges) {
          val successor = cfgResult.getStates(edge.target).head
          // handle in and out edges
          val adapted = edge.kind match {
            case Kind.In => successor.command(EnterLoopCommand())
            case Kind.Out => successor.command(LeaveLoopCommand())
            case _ => successor
          }
          // filter state if there is a condition
          val filtered = edge match {
            case ConditionalEdge(condition, _, _, _) => assumeCondition(condition, adapted)
            case UnconditionalEdge(_, _, _) => adapted
          }
          state = state lub filtered
        }
        state
      }

      val states = ListBuffer(exit)
      current match {
        case StatementBlock(statements) =>
          // execute statements
          var successor = exit
          statements.reverse.foreach(statement => {
            val predecessor = executeStatement(statement, successor)
            states.append(predecessor)
            successor = predecessor
          })
        case PreconditionBlock(preconditions) =>
          // process preconditions
          preconditions.foldRight(exit) { (precondition, successor) =>
            val predecessor = executeCommand(PreconditionCommand, precondition, successor)
            states.append(predecessor)
            predecessor
          }
        case PostconditionBlock(postconditions) =>
          // process postconditions
          postconditions.foldRight(exit) { (postcondition, successor) =>
            val predecessor = executeCommand(PostconditionCommand, postcondition, successor)
            states.append(predecessor)
            predecessor
          }
        case LoopHeadBlock(invariants, statements) =>
          // execute statements
          var successor = exit
          statements.reverse.foreach(statement => {
            val predecessor = executeStatement(statement, successor)
            states.append(predecessor)
            successor = predecessor
          })
          val intermediate = successor
          // process invariants
          invariants.foldRight(intermediate) { (invariant, successor) =>
            val predecessor = executeCommand(InvariantCommand, invariant, successor)
            states.append(predecessor)
            predecessor
          }
      }

      cfgResult.setStates(current, states.reverse.toList)

      current match {
        case LoopHeadBlock(_, _) =>
          // only push the predecessors corresponding to the in edges because
          // the predecessors corresponding to the body of the loop have already
          // been processed.
          val edges = cfg.inEdges(current)
          edges.filter(_.isIn).map(_.source).foreach(push)
        case _ => cfg.predecessors(current).foreach(push)
      }
    }
    cfgResult
  }

  /**
    * Pushes the given block onto the worklist given that all dependencies have
    * been processed but the block itself has not, otherwise nothing happens.
    *
    * @param block    The block.
    * @param worklist The implicitly passed worklist.
    * @param visited  The implicitly passed set of visited blocks.
    */
  private def push(block: SampleBlock)(implicit worklist: mutable.Stack[SampleBlock], visited: mutable.Set[SampleBlock]): Unit =
    if (cfg.successors(block).forall(visited.contains)) worklist.push(block)
    else block match {
      case LoopHeadBlock(_, _) =>
        val outEdges = cfg.outEdges(block)
        val afterBlocks = outEdges.filter(_.isOut).map(_.target)
        if (afterBlocks.forall(visited.contains)) {
          val inEdges = cfg.inEdges(block)
          val innerBlocks = inEdges.filterNot(_.isIn).map(_.source)
          worklist.pushAll(innerBlocks)
          worklist.push(block)
        }
      case _ => // do nothing
    }

  protected def executeStatement(statement: Statement, state: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    val successor = state.before(ProgramPointUtils.identifyingPP(statement))
    val predecessor = statement.backwardSemantics(successor)
    predecessor
  }

  private def executeCommand(command: (ExpressionSet) => Command, argument: Statement, state: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    val successor = state.before(ProgramPointUtils.identifyingPP(argument))
    val evaluated: QuantifiedPermissionsState = argument.backwardSemantics(successor)
    val expression = evaluated.expr
    val predecessor = evaluated.command(command(expression))
    predecessor
  }

  private def assumeCondition(condition: Statement, state: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    val predecessor = state.before(ProgramPointUtils.identifyingPP(condition))
    condition.forwardSemantics(predecessor).testTrue()
  }

  /**
    * Initializes the result of the execution.
    *
    * @param cfg   The control flow graph to execute.
    * @param state A state.
    * @return The initialized result.
    */
  override protected def initializeResult(cfg: SampleCfg, state: QuantifiedPermissionsState): CfgResult[QuantifiedPermissionsState] = {
    val cfgResult = FinalCfgResult[QuantifiedPermissionsState](cfg)
    cfgResult.initialize(state)
    cfgResult
  }
}
