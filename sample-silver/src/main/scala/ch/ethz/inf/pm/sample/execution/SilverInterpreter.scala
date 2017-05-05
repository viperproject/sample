/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.SampleCfg.{SampleBlock, SampleEdge}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.permissionanalysis._
import com.typesafe.scalalogging.LazyLogging
import viper.silver.cfg._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * TODO: Separate the generic parts of the interpreter with the silver specific parts.
  */

/**
  * Performs a forward or backward interpretation of a control flow graph.
  *
  * @tparam S The type of the states.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
trait SilverInterpreter[S <: State[S]] {
  /**
    * Executes the given control flow graph.
    *
    * @param cfg     The control flow graph to execute.
    * @param initial The initial state.
    * @return The result of the execution.
    */
  def execute(cfg: SampleCfg, initial: S): CfgResult[S]

  /**
    * Initializes the result of the execution.
    *
    * @param cfg   The control flow graph to execute.
    * @param state A state.
    * @return The initialized result.
    */
  protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S]
}

/**
  * Performs a forward interpretation of a control flow graph.
  *
  * @tparam S The type of the states.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
trait SilverForwardInterpreter[S <: State[S]]
  extends SilverInterpreter[S]
    with LazyLogging {
  var _cfg: SampleCfg = null

  protected def cfg(blockPosition: BlockPosition): SampleCfg = _cfg

  protected def onExitBlockExecuted(current: BlockPosition, worklist: mutable.Queue[BlockPosition]) = {}

  protected def initializeProgramResult(cfg: SampleCfg, state: S): Map[SampleCfg, CfgResult[S]] = {
    Map(cfg -> initializeResult(cfg, state))
  }

  override def execute(startCfg: SampleCfg, initial: S): CfgResult[S] = {
    // initialize cfg result
    this._cfg = startCfg //TODO @flurin ugly
    val startPosition = BlockPosition(startCfg.entry, 0)
    val bottom = initial.bottom()
    val cfgResult = initializeProgramResult(startCfg, bottom)

    // prepare data structures
    val worklist = mutable.Queue[BlockPosition](BlockPosition(cfg(startPosition).entry, 0))
    val iterations = mutable.Map[BlockPosition, Int]()

    while (worklist.nonEmpty) {
      val current = worklist.dequeue()
      val iteration = iterations.getOrElse(current, 0)

      // compute entry state state of current block
      val entry = if (current.block == startCfg.entry) {
        initial
      } else {
        var state = bottom
        // join incoming states.
        val edges = inEdges(current, cfgResult)
        for (edge <- edges) {
          val predecessor = getPredecessorState(cfgResult(cfg(current)), current, edge)
          // filter state if there is a condition
          val filtered = edge match {
            case Left(ConditionalEdge(condition, _, _, _)) => assumeCondition(condition, predecessor)
            case Left(UnconditionalEdge(_, _, _)) => predecessor
            case Right(_) => predecessor
          }
          // handle in and out edges
          val adapted = if (edge.isLeft) edge.left.get.kind match {
            case Kind.In => filtered.command(EnterLoopCommand())
            case Kind.Out => filtered.command(LeaveLoopCommand())
            case _ => filtered
          } else filtered

          state = state lub adapted
        }
        // widening
        if (edges.size > 1 && iteration > SystemParameters.wideningLimit) {
          cfgResult(cfg(current)).preStateAt(current) widening state
        } else {
          state
        }
      }

      // check for termination and execute block
      val oldStates = cfgResult(cfg(current)).getStates(current.block)
      val numToSkip = current.index
      val oldEntry = if (oldStates.isEmpty) bottom else cfgResult(cfg(current)).preStateAt(current)
      if (!(entry lessEqual oldEntry)) {
        // execute block
        val states = ListBuffer(oldStates.take(numToSkip): _*)
        states.append(entry)
        var blockFullyExecuted = true
        current.block match {
          case StatementBlock(statements) =>
            // execute statements
            var predecessor = entry
            statements.drop(numToSkip).foreach(st => {
              if (blockFullyExecuted) {
                val (executed, successor) = executeStatement(st, predecessor, worklist)
                states.append(successor)
                predecessor = successor
                blockFullyExecuted = executed
              }
            })
          case PreconditionBlock(preconditions) =>
            // process preconditions
            preconditions.drop(numToSkip).foldLeft(entry) { (predecessor, precondition) =>
              val successor = executeCommand(PreconditionCommand, precondition, predecessor)
              states.append(successor)
              successor
            }
          case PostconditionBlock(postconditions) =>
            // process postconditions
            postconditions.drop(numToSkip).foldLeft(entry) { (predecessor, postcondition) =>
              val successor = executeCommand(PostconditionCommand, postcondition, predecessor)
              states.append(successor)
              successor
            }
          case LoopHeadBlock(invariants, statements) =>
            // process invariants
            val intermediate = invariants.drop(numToSkip).foldLeft(entry) { (predecessor, invariant) =>
              val successor = executeCommand(InvariantCommand, invariant, predecessor)
              states.append(successor)
              successor
            }
            // execute statements
            var predecessor = intermediate
            statements.drop(numToSkip - invariants.size).foreach(st => {
              if (blockFullyExecuted) {
                val (executed, successor) = executeStatement(st, predecessor, worklist)
                states.append(successor)
                predecessor = successor
                blockFullyExecuted = executed
              }
            })
          case ConstrainingBlock(variables, body) =>
            // execute constraining block
            // TODO: We might want to not support constraining blocks in Sample.
            ???
        }
        cfgResult(cfg(current)).setStates(current.block, states.toList)
        // update worklist and iteration count
        if (blockFullyExecuted)
          worklist.enqueue(cfg(current).successors(current.block).map(b => BlockPosition(b, 0)): _*)
        iterations.put(current, iteration + 1)
        //notify (subclasses) about processed exit blocks
        val exitBlock = cfg(current).exit
        if (exitBlock.isDefined && exitBlock.get == current.block) {
          onExitBlockExecuted(current, worklist)
        }
      }
    }

    // return result
    cfgResult(_cfg)
  }

  protected def inEdges(current: BlockPosition, cfgResult: Map[SampleCfg, CfgResult[S]]): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
    current match {
      case BlockPosition(_, 0) => cfg(current).inEdges(current.block).map(Left(_))
      case _ => Seq(Right(DummyEdge(current))) // just let the interpreter know that we jump to current for any reason
    }
  }

  private def assumeCondition(condition: Statement, state: S): S = {
    val predecessor = state.before(ProgramPointUtils.identifyingPP(condition))
    val successor = condition.forwardSemantics(predecessor).testTrue()
    logger.trace(predecessor.toString)
    logger.trace(condition.toString)
    logger.trace(successor.toString)
    successor
  }

  protected def getPredecessorState(cfgResult: CfgResult[S], current: BlockPosition, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    case Left(e) if (current.index == 0) => cfgResult.getStates(e.source).last
    case Left(e) => cfgResult.preStateAt(current)
    case Right(b: DummyEdge[BlockPosition]) => cfgResult.preStateAt(current)
  }

  protected def executeStatement(statement: Statement, state: S, worklist: mutable.Queue[BlockPosition]): (Boolean, S) = {
    val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
    val successor = statement.forwardSemantics(predecessor)
    logger.trace(predecessor.toString)
    logger.trace(statement.toString)
    logger.trace(successor.toString)
    (true, successor)
  }

  private def executeCommand(command: (ExpressionSet) => Command, argument: Statement, state: S): S = {
    val predecessor = state.before(ProgramPointUtils.identifyingPP(argument))
    val evaluated = argument.forwardSemantics(predecessor)
    val expression = evaluated.expr
    val successor = evaluated.command(command(expression))
    logger.trace(predecessor.toString)
    logger.trace(argument.toString)
    logger.trace(successor.toString)
    successor
  }
}

/**
  * Performs a backward interpretation of a control flwo graph.
  *
  * @tparam S The type of the states.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
trait SilverBackwardInterpreter[S <: State[S]]
  extends SilverInterpreter[S]
    with LazyLogging {
  override def execute(cfg: SampleCfg, initial: S): CfgResult[S] = {
    // initialize cfg result
    val bottom = initial.bottom()
    val cfgResult = initializeResult(cfg, bottom)

    // TODO: Compute the list of starting points.
    val starts = cfg.exit.toList

    // prepare data structures
    val worklist = mutable.Queue(starts: _*)
    val iterations = mutable.Map[SampleBlock, Int]()

    while (worklist.nonEmpty) {
      val current = worklist.dequeue()
      val iteration = iterations.getOrElse(current, 0)

      // compute exit state of current block
      val exit = if (starts contains current) {
        initial
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
        // widening
        if (edges.size > 1 && iteration > SystemParameters.wideningLimit) {
          cfgResult.getStates(current).last widening state
        } else {
          state
        }
      }

      // check for termination and execute block
      val oldStates = cfgResult.getStates(current)
      val oldExit = if (oldStates.isEmpty) bottom else oldStates.last
      if (!(exit lessEqual oldExit)) {
        // execute block
        val states = ListBuffer(exit)
        current match {
          case StatementBlock(statements) =>
            // execute statements
            statements.foldRight(exit) { (statement, successor) =>
              val predecessor = executeStatement(statement, successor)
              states.append(predecessor)
              predecessor
            }
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
            val intermediate = statements.foldRight(exit) { (statement, successor) =>
              val predecessor = executeStatement(statement, successor)
              states.append(predecessor)
              predecessor
            }
            // process invariants
            invariants.foldRight(intermediate) { (invariant, successor) =>
              val predecessor = executeCommand(InvariantCommand, invariant, successor)
              states.append(predecessor)
              predecessor
            }
          case ConstrainingBlock(variables, body) =>
            // execute constraining block
            // TODO: We might want to not support constraining blocks in Sample.
            ???
        }
        cfgResult.setStates(current, states.reverse.toList)

        // update worklist and iteration count
        worklist.enqueue(cfg.predecessors(current): _*)
        iterations.put(current, iteration + 1)
      }
    }

    // return result
    cfgResult
  }

  private def assumeCondition(condition: Statement, state: S): S = {
    val successor = state.before(ProgramPointUtils.identifyingPP(condition))
    val predecessor = condition.backwardSemantics(successor).testTrue()
    logger.trace(successor.toString)
    logger.trace(condition.toString)
    logger.trace(predecessor.toString)
    predecessor
  }

  private def executeStatement(statement: Statement, state: S): S = {
    val successor = state.before(ProgramPointUtils.identifyingPP(statement))
    val predecessor = statement.backwardSemantics(successor)
    logger.trace(successor.toString)
    logger.trace(statement.toString)
    logger.trace(predecessor.toString)
    predecessor
  }

  private def executeCommand(command: (ExpressionSet) => Command, argument: Statement, state: S): S = {
    val successor = state.before(ProgramPointUtils.identifyingPP(argument))
    val evaluated = argument.backwardSemantics(successor)
    val expression = evaluated.expr
    val predecessor = evaluated.command(command(expression))
    logger.trace(successor.toString)
    logger.trace(argument.toString)
    logger.trace(predecessor.toString)
    predecessor
  }
}

/**
  * Performs a forward interpretation of a control flow graph that computes the
  * final pre- and post states of statements within a control flow graph.
  *
  * @tparam S The type of the states.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
case class FinalResultForwardInterpreter[S <: State[S]]()
  extends SilverForwardInterpreter[S] {
  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val cfgResult = FinalCfgResult[S](cfg)
    cfgResult.initialize(state)
    cfgResult
  }
}

/**
  * Performs a backward interpretation of a control flow graph that computes the
  * final pre- and post states of statements within a control flow graph.
  *
  * @tparam S The type of the states.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
case class FinalResultBackwardInterpreter[S <: State[S]]()
  extends SilverBackwardInterpreter[S] {
  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val cfgResult = FinalCfgResult[S](cfg)
    cfgResult.initialize(state)
    cfgResult
  }
}
