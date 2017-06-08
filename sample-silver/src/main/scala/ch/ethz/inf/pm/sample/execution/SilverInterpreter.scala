/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.SampleCfg.{SampleBlock, SampleEdge}
import ch.ethz.inf.pm.sample.execution.SilverInterpreter.{CfgResultMapType, InterpreterWorklist}
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
    * Look up the CFG containing the given blockPosition. Intraprocedural interpreters can always return the same
    * CFG (there is only one cfg to analyze). Interprocedural interpreters on the other hand need to look up the
    * method/cfg depending on the blockPosition.
    *
    * @param blockPosition The BlockPosition
    * @return The SampleCfg containing this block
    */
  def cfg(blockPosition: BlockPosition): SampleCfg

  /**
    * Create or look up the initial state for a given cfg. Intraprocedural interpreters can always return the same
    * initial state. Interprocedural interpreters on the other hand need to create an initial state depending on the
    * given CFG.
    *
    * @param cfg The control flow graph
    * @return The initial state for a given cfg.
    */
  def initial(cfg: SampleCfg): S

  /**
    * Executes the control flow graph.
    *
    * @return The result of the execution.
    */
  def execute(): CfgResult[S]

  /**
    * Initializes the result of the execution.
    *
    * @param cfg   The control flow graph to execute.
    * @param state A state.
    * @return The initialized result.
    */
  protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S]
}

object SilverInterpreter {

  /**
    * the worklist of the (forward) interpreter consists of a blockposition and a boolean flag "forceReinterpretStmt"
    * using BlockPosition any index within a block can be enqueued
    * forceReinterpretStmt=true can be used to force re-interpretation of the enqueued position even though the
    * successor state did not change. This is useful for example to merge the effect of a method call after the callee
    * has been analyzed.
    */
  type InterpreterWorklist = mutable.Queue[(BlockPosition, Boolean)]

  /**
    * The interpreter may have to store CfgResults for multiple Cfgs. During interpretation a map is used
    * and later depending on the use case this can be converted into either a CfgResult or a ProgramResult
    *
    * @tparam S The type of the states
    */
  type CfgResultMapType[S <: State[S]] = Map[SampleCfg, CfgResult[S]]
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

  /**
    * Is called everytime the exit block of a CFG was executed
    *
    * @param current  The Block that was interpreted last
    * @param worklist The interpreters worklist
    */
  protected def onExitBlockExecuted(current: BlockPosition, worklist: InterpreterWorklist): Unit = {}

  /**
    * Create and initialize all CfgResults for the given cfgs
    *
    * @param cfgs the cfgs for which CfgResults should be created
    * @return a map of all initialized CfgResults
    */
  protected def initializeProgramResult(cfgs: Seq[SampleCfg]): CfgResultMapType[S] = {
    (for (cfg <- cfgs) yield {
      cfg -> initializeResult(cfg, bottom(cfg))
    }).toMap
  }

  def execute(cfgs: Seq[SampleCfg]): CfgResultMapType[S] = {
    // initialize cfg result
    val cfgResults = initializeProgramResult(cfgs)

    // prepare data structures
    val worklist: InterpreterWorklist = mutable.Queue[(BlockPosition, Boolean)]()
    cfgs.foreach(c => worklist.enqueue((BlockPosition(c.entry, 0), false)))
    val iterations = mutable.Map[BlockPosition, Int]()

    while (worklist.nonEmpty) {
      val (current, forceReinterpretStmt) = worklist.dequeue()
      val currentCfg = cfg(current)
      val iteration = iterations.getOrElse(current, 0)

      // compute entry state state of current block
      val edges = inEdges(current, cfgResults)
      val entry = if (current.block == currentCfg.entry && edges.isEmpty) {
        initial(currentCfg)
      } else {
        var state = bottom(currentCfg)
        // join incoming states.
        for (edge <- edges) {
          val predecessor = getPredecessorState(cfgResults(currentCfg), current, edge)
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
          cfgResults(currentCfg).preStateAt(current) widening state
        } else {
          state
        }
      }

      // check for termination and execute block
      val oldStates = cfgResults(currentCfg).getStates(current.block)
      val numToSkip = current.index
      val oldEntry = if (oldStates.isEmpty) bottom(currentCfg) else cfgResults(currentCfg).preStateAt(current)
      if (!(entry lessEqual oldEntry) || forceReinterpretStmt) {
        // execute block
        val states = ListBuffer(oldStates.take(numToSkip): _*)
        states.append(entry)
        current.block match {
          case StatementBlock(statements) =>
            // execute statements
            var predecessor = entry
            statements.drop(numToSkip).foreach(st => {
              val successor = executeStatement(st, predecessor, worklist, cfgResults)
              states.append(successor)
              predecessor = successor
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
              val successor = executeStatement(st, predecessor, worklist, cfgResults)
              states.append(successor)
              predecessor = successor
            })
          case ConstrainingBlock(variables, body) =>
            // execute constraining block
            // TODO: We might want to not support constraining blocks in Sample.
            ???
        }
        cfgResults(currentCfg).setStates(current.block, states.toList)
        // update worklist and iteration count
        worklist.enqueue(currentCfg.successors(current.block).map(b => (BlockPosition(b, 0), false)): _*)
        iterations.put(current, iteration + 1)
        //notify (subclasses) about processed exit blocks
        val exitBlock = currentCfg.exit
        if (exitBlock.isDefined && exitBlock.get == current.block) {
          onExitBlockExecuted(current, worklist)
        }
      }
    }

    // return result
    cfgResults
  }

  /**
    * Look up the bottom state for a cfg.
    *
    * @param cfg The control flow graph
    * @return The bottom state for a given cfg.
    */
  def bottom(cfg: SampleCfg): S = initial(cfg).bottom()

  protected def inEdges(current: BlockPosition, cfgResult: CfgResultMapType[S]): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
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
    case Left(e) if current.index == 0 => cfgResult.getStates(e.source).last
    case _ => cfgResult.preStateAt(current)
  }

  protected def executeStatement(statement: Statement, state: S, worklist: InterpreterWorklist, programResult: CfgResultMapType[S]): S = {
    val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
    val successor = statement.forwardSemantics(predecessor)
    logger.trace(predecessor.toString)
    logger.trace(statement.toString)
    logger.trace(successor.toString)
    successor
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
  * Performs a backward interpretation of a control flow graph.
  *
  * @tparam S The type of the states.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
trait SilverBackwardInterpreter[S <: State[S]]
  extends SilverInterpreter[S]
    with LazyLogging {

  /**
    * Look up the bottom state for a cfg.
    *
    * @param cfg The control flow graph
    * @return The bottom state for a given cfg.
    */
  def bottom(cfg: SampleCfg): S = initial(cfg).bottom()

  //BEGIN TODO @flurin these helper methods are to later memoize calls to elements.size because those numbers never change. or do they?
  def lastIndex(current: BlockPosition): Int = lastIndex(current.block)

  def lastIndex(block: SampleBlock): Int = numElementsInBlock(block) - 1

  def numElementsInBlock(block: SampleBlock): Int = block.elements.size

  //END TODO @flurin

  /**
    * Is called everytime the entry block of a CFG was executed
    *
    * @param current  The Block that was interpreted last
    * @param worklist The interpreters worklist
    */
  protected def onEntryBlockExecuted(current: BlockPosition, worklist: InterpreterWorklist): Unit = {}

  def getSuccessorState(cfgResult: CfgResult[S], current: BlockPosition, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    case Left(e: SampleEdge) if current.index == lastIndex(current) => cfgResult.getStates(e.target).head
    case _ => cfgResult.postStateAt(current)
  }

  def execute(cfgs: Seq[SampleCfg]): CfgResultMapType[S] = {
    // initialize cfg result
    val cfgResults: CfgResultMapType[S] = initializeProgramResult(cfgs)

    // TODO: Compute the list of starting points.
    val starts = cfgs.flatMap(_.exit).toList

    // prepare data structures
    val worklist: InterpreterWorklist = mutable.Queue()
    cfgs.foreach(c => if (c.exit.isDefined) worklist.enqueue((BlockPosition(c.exit.get, lastIndex(c.exit.get)), false)))
    val iterations = mutable.Map[BlockPosition, Int]()

    while (worklist.nonEmpty) {
      val (current, forceReinterpretStmt) = worklist.dequeue()
      val currentCfg = cfg(current)
      val iteration = iterations.getOrElse(current, 0)

      // compute exit state of current block
      val exit = if (starts contains current.block) { //TODO @flurin
        initial(currentCfg)
      } else {
        var state = bottom(currentCfg)
        // join outgoing states
        val edges = outEdges(current, cfgResults)
        for (edge <- edges) {
          val successor = getSuccessorState(cfgResults(currentCfg), current, edge)
          // handle in and out edges
          val adapted = if (edge.isLeft) edge.left.get.kind match {
            case Kind.In => successor.command(EnterLoopCommand())
            case Kind.Out => successor.command(LeaveLoopCommand())
            case _ => successor
          } else successor
          // filter state if there is a condition
          val filtered = edge match {
            case Left(ConditionalEdge(condition, _, _, _)) => assumeCondition(condition, adapted)
            case Left(UnconditionalEdge(_, _, _)) => adapted
            case Right(_) => adapted
          }

          state = state lub filtered
        }
        // widening
        if (edges.size > 1 && iteration > SystemParameters.wideningLimit) {
          cfgResults(currentCfg).postStateAt(current) widening state
        } else {
          state
        }
      }

      // check for termination and execute block
      val oldStates = cfgResults(currentCfg).getStates(current.block)
      // index is zero-based. When interpreting backwards from the Kth index we have to interpret k+1 statements in the block
      val elemsToTake = current.index + 1
      val oldExit = if (oldStates.isEmpty) bottom(currentCfg) else oldStates.last
      if (!(exit lessEqual oldExit) || forceReinterpretStmt) {
        // execute block
        val states = ListBuffer(oldStates.reverse.take(numElementsInBlock(current.block) - elemsToTake): _*)
        states.append(exit)
        current.block match {
          case StatementBlock(statements) =>
            // execute statements
            statements.take(elemsToTake).foldRight(exit) { (statement, successor) =>
              val predecessor = executeStatement(statement, successor, worklist, cfgResults)
              states.append(predecessor)
              predecessor
            }
          case PreconditionBlock(preconditions) =>
            // process preconditions
            preconditions.take(elemsToTake).foldRight(exit) { (precondition, successor) =>
              val predecessor = executeCommand(PreconditionCommand, precondition, successor)
              states.append(predecessor)
              predecessor
            }
          case PostconditionBlock(postconditions) =>
            // process postconditions
            postconditions.take(elemsToTake).foldRight(exit) { (postcondition, successor) =>
              val predecessor = executeCommand(PostconditionCommand, postcondition, successor)
              states.append(predecessor)
              predecessor
            }
          case LoopHeadBlock(invariants, statements) =>
            // The elements in the LoopHeadBlock are stored as invariants ++ statements
            // we need to figure out wheter current.index points to a location in the statements or in the
            // invariants. numStatementsToTake may be negative but that's ok because Seq(....).take(-1) == Nil
            val numStatementsToTake = elemsToTake - invariants.size
            val numInvariantsToTake = if (invariants.size > elemsToTake) elemsToTake else invariants.size
            // execute statements
            val intermediate = statements.take(numStatementsToTake).foldRight(exit) { (statement, successor) =>
              val predecessor = executeStatement(statement, successor, worklist, cfgResults)
              states.append(predecessor)
              predecessor
            }
            // process invariants
            invariants.take(numInvariantsToTake).foldRight(intermediate) { (invariant, successor) =>
              val predecessor = executeCommand(InvariantCommand, invariant, successor)
              states.append(predecessor)
              predecessor
            }
          case ConstrainingBlock(variables, body) =>
            // execute constraining block
            // TODO: We might want to not support constraining blocks in Sample.
            ???
        }
        cfgResults(currentCfg).setStates(current.block, states.reverse.toList)

        // update worklist and iteration count
        worklist.enqueue(cfg(current).predecessors(current.block).map(b => (BlockPosition(b, lastIndex(b)), false)): _*)
        iterations.put(current, iteration + 1)
        //notify (subclasses) about processed entry blocks
        if (currentCfg.entry == current.block) {
          onEntryBlockExecuted(current, worklist)
        }
      }
    }

    // return result
    cfgResults
  }

  /**
    * Create and initialize all CfgResults for the given cfgs
    *
    * @param cfgs the cfgs for which CfgResults should be created
    * @return a map of all initialized CfgResults
    */
  protected def initializeProgramResult(cfgs: Seq[SampleCfg]): CfgResultMapType[S] = {
    (for (cfg <- cfgs) yield {
      cfg -> initializeResult(cfg, bottom(cfg))
    }).toMap
  }

  private def assumeCondition(condition: Statement, state: S): S = {
    val successor = state.before(ProgramPointUtils.identifyingPP(condition))
    val predecessor = condition.backwardSemantics(successor).testTrue()
    logger.trace(successor.toString)
    logger.trace(condition.toString)
    logger.trace(predecessor.toString)
    predecessor
  }

  protected def executeStatement(statement: Statement, state: S, worklist: InterpreterWorklist, programResult: CfgResultMapType[S]): S = {
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

  protected def outEdges(current: BlockPosition, cfgResult: CfgResultMapType[S]): Seq[Either[SampleEdge, AuxiliaryEdge]] = current match {
    case BlockPosition(_, i) if i == lastIndex(current) => cfg(current).outEdges(current.block).map(Left(_))
    case _ => Seq(Right(DummyEdge(current)))
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
case class FinalResultForwardInterpreter[S <: State[S]](cfg: SampleCfg, initial: S)
  extends SilverForwardInterpreter[S] {

  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val cfgResult = FinalCfgResult[S](cfg)
    cfgResult.initialize(state)
    cfgResult
  }

  override def initial(cfg: SampleCfg): S = initial

  override def cfg(blockPosition: BlockPosition): SampleCfg = cfg

  override def execute(): CfgResult[S] = {
    val result = execute(Seq(cfg))
    result(cfg)
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
case class FinalResultBackwardInterpreter[S <: State[S]](cfg: SampleCfg, initial: S)
  extends SilverBackwardInterpreter[S] {
  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val cfgResult = FinalCfgResult[S](cfg)
    cfgResult.initialize(state)
    cfgResult
  }

  override def initial(cfg: SampleCfg): S = initial

  override def cfg(blockPosition: BlockPosition): SampleCfg = cfg

  override def execute(): CfgResult[S] = {
    val result = execute(Seq(cfg))
    result(cfg)
  }
}
