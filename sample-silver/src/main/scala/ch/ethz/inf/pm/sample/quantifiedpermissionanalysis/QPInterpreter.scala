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
    val cfgResult: CfgResult[QuantifiedPermissionsState] = initializeResult(cfg, bottom)
    val worklist: mutable.Stack[SampleBlock] = mutable.Stack()
    worklist.push(cfg.exit.get)
    while (worklist.nonEmpty) {
      val current = worklist.pop()
      val exit = if (cfg.exit.get == current) {
        initial(cfg)
      } else {
        var state: QuantifiedPermissionsState = bottom
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

      val states: ListBuffer[QuantifiedPermissionsState] = ListBuffer(exit)
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
        case ConstrainingBlock(_, _) =>
          // execute constraining block
          // TODO: We might want to not support constraining blocks in Sample.
          ???
      }

      cfgResult.setStates(current, states.reverse.toList)

      current match {
        case loopHead@LoopHeadBlock(_, _) =>
          cfg.inEdges(loopHead) match {
            // First push the node corresponding to in edge, then the other one.
            // Reasoning behind this is that we first want to execute inner-most blocks during our analysis.
            case one :: two :: Nil => (one.kind, two.kind) match {
              case (Kind.In, Kind.Normal) =>
                worklist.push(one.source)
                worklist.push(two.source)
              case (Kind.Normal, Kind.In) =>
                worklist.push(two.source)
                worklist.push(one.source)
            }
          }
        case _ => worklist.pushAll(cfg.predecessors(current))
      }
    }
    cfgResult
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
    condition.forwardSemantics(predecessor).asInstanceOf[QuantifiedPermissionsState].testTrue()
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
