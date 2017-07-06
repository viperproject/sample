/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.SampleCfg.{SampleBlock, SampleEdge}
import ch.ethz.inf.pm.sample.execution.SilverInterpreter.{CfgResultsType, InterpreterWorklist}
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
    * Look up the CFG for the current worklist element. Intraprocedural interpreters can always return the same
    * CFG (there is only one cfg to analyze). Interprocedural interpreters on the other hand need to look up the
    * method/cfg depending on the actual element in the worklist.
    *
    * @param current The element in the worklist
    * @return The SampleCfg containing this block
    */
  def cfg(current: WorklistElement): SampleCfg

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

/**
  * the worklist of the interpreter consists of a BlockPosition and a boolean flag "forceReinterpretStmt"
  * using BlockPosition any index within a block can be enqueued
  * forceReinterpretStmt=true can be used to force re-interpretation of the enqueued position even though the
  * successor state did not change. This is useful for example to merge the effect of a method call after the callee
  * has been analyzed.
  */
trait WorklistElement {
  val pos: BlockPosition
  val forceReinterpretStmt: Boolean

  /**
    * Creates a new WorklistElement. This method can be used to introduce a relationship between the elements in the
    * worklist and the then-current element that added them to the wl.
    * A useful example could be when we need to tag all worklist elements (e.g. call-strings) and want to preserve
    * the tag throughout the whole analysis.
    *
    * @param newPos                  The BlockPosition that should be enqueued
    * @param newForceReinterpretStmt Flag to force the interpreter to (re-) interpret the statement at newPos even though the entryState of the block may not have changed.
    * @return A new worklist element that can be added to the worklist
    */
  def createSuccessorForEnqueue(newPos: BlockPosition, newForceReinterpretStmt: Boolean): WorklistElement
}

/**
  * A default implementation for @see WorklistElement
  *
  * @param pos                  enqueued BlockPosition
  * @param forceReinterpretStmt helper flag to skip checking if inputState "goes up" in the lattice
  */
case class SimpleWorklistElement private(pos: BlockPosition, forceReinterpretStmt: Boolean) extends WorklistElement {
  override def createSuccessorForEnqueue(newPos: BlockPosition, newForceReinterpretStmt: Boolean): WorklistElement = {
    copy(pos = newPos, forceReinterpretStmt = newForceReinterpretStmt)
  }
}

object SilverInterpreter {

  type InterpreterWorklist = mutable.Queue[WorklistElement]

  /**
    * The interpreter may have to store CfgResults for multiple Cfgs. During interpretation a map/lookup functions is used
    * and later depending on the use case this can be converted into either a CfgResult or a ProgramResult. For some
    * (interprocedural) analyses the CfgResult depends on the current worklist element (e.g. call-string).
    * That's why the result type is a Function from (WorklistElement, SampleCfg) to CfgResult
    *
    * @tparam S The type of the states
    */
  type CfgResultsType[S <: State[S]] = (WorklistElement, SampleCfg) => CfgResult[S]
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
    * Is called every time the exit block of a CFG was executed
    *
    * @param current  The Block that was interpreted last
    * @param worklist The interpreters worklist
    */
  protected def onExitBlockExecuted(current: WorklistElement, worklist: InterpreterWorklist): Unit = {}

  /**
    * Create and initialize all CfgResults for the given cfgs
    *
    * @param cfgs the cfgs for which CfgResults should be created
    * @return a map from cfg to CfgResult. The "worklist" argument is not necessary in the intraprocedural case
    */
  protected def initializeProgramResult(cfgs: Seq[SampleCfg]): CfgResultsType[S] = {
    def lookup(res: Map[SampleCfg, CfgResult[S]])(current: WorklistElement, cfg: SampleCfg) = res(cfg)

    lookup((for (cfg <- cfgs) yield {
      cfg -> initializeResult(cfg, bottom(cfg))
    }).toMap)
  }

  def execute(cfgs: Seq[SampleCfg]): CfgResultsType[S] = {
    // initialize cfg result
    val cfgResults = initializeProgramResult(cfgs)
    val starts = cfgs.map(_.entry).toSet

    // prepare data structures
    val worklist: InterpreterWorklist = mutable.Queue()
    cfgs.foreach(c => worklist.enqueue(SimpleWorklistElement(BlockPosition(c.entry, 0), forceReinterpretStmt = false)))
    val iterations = mutable.Map[WorklistElement, Int]() //TODO @flurin
    while (worklist.nonEmpty) {
      val current = worklist.dequeue()
      val currentCfg = cfg(current)
      val iteration = iterations.getOrElse(current, 0)

      // compute entry state state of current block
      val edges = inEdges(current, cfgResults)
      val entry = if ((starts contains current.pos.block) && edges.isEmpty) {
        initial(currentCfg)
      } else {
        var state = bottom(currentCfg)
        // join incoming states.
        for (edge <- edges) {
          val predecessor = getPredecessorState(cfgResults(current, currentCfg), current, edge)
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
          cfgResults(current, currentCfg).preStateAt(current.pos) widening state
        } else {
          state
        }
      }

      // check for termination and execute block
      val oldStates = cfgResults(current, currentCfg).getStates(current.pos.block)
      val numToSkip = current.pos.index
      val oldEntry = if (oldStates.isEmpty) bottom(currentCfg) else cfgResults(current, currentCfg).preStateAt(current.pos)
      var canContinueBlock = true
      if (!(entry lessEqual oldEntry) || current.forceReinterpretStmt) {
        // execute block
        val states = ListBuffer(oldStates.take(numToSkip): _*)
        states.append(entry)
        current.pos.block match {
          case StatementBlock(statements) =>
            // execute statements
            var predecessor = entry
            statements.drop(numToSkip).foreach(st => if (canContinueBlock) {
              val (successor, continue) = executeStatement(current, st, predecessor, worklist, cfgResults)
              states.append(successor)
              predecessor = successor
              canContinueBlock = continue
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
            statements.drop(numToSkip - invariants.size).foreach(st => if (canContinueBlock) {
              val (successor, continue) = executeStatement(current, st, predecessor, worklist, cfgResults)
              states.append(successor)
              predecessor = successor
              canContinueBlock = continue
            })
          case ConstrainingBlock(variables, body) =>
            // execute constraining block
            // TODO: We might want to not support constraining blocks in Sample.
            ???
        }
        // If we aborted a StatementBlock early we'll have less states than we should have.
        // Fill the gap with bottom. These bottom states will be replaced by the actual state when
        // the interpreter continues at this BlockPosition.
        val gapFiller = if (!canContinueBlock) {
          // some statements have been skipped!
          val btm = bottom(currentCfg)
          Seq.fill(oldStates.size - states.size)(btm)
        } else {
          Nil
        }
        cfgResults(current, currentCfg).setStates(current.pos.block, states.toList ++ gapFiller)
        // update worklist and iteration count if the whole block has been executed
        if (canContinueBlock) { // Only enqueue sueccessors if we interpreter the whole block
          worklist.enqueue(currentCfg.successors(current.pos.block).map(b => current.createSuccessorForEnqueue(BlockPosition(b, 0), newForceReinterpretStmt = false)): _*)
          iterations.put(current, iteration + 1)
        }
        //notify (subclasses) about processed exit blocks
        val exitBlock = currentCfg.exit
        if (exitBlock.isDefined && exitBlock.get == current.pos.block) {
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

  protected def inEdges(current: WorklistElement, cfgResult: CfgResultsType[S]): Seq[Either[SampleEdge, AuxiliaryEdge]] = current.pos match {
    case BlockPosition(_, 0) => cfg(current).inEdges(current.pos.block).map(Left(_))
    case _ => Seq(Right(DummyEdge(current))) // just let the interpreter know that we jump to current for any reason
  }

  private def assumeCondition(condition: Statement, state: S): S = {
    val predecessor = state.before(ProgramPointUtils.identifyingPP(condition))
    val successor = condition.forwardSemantics(predecessor).testTrue()
    logger.trace(predecessor.toString)
    logger.trace(condition.toString)
    logger.trace(successor.toString)
    successor
  }

  protected def getPredecessorState(cfgResult: CfgResult[S], current: WorklistElement, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    case Left(e) if current.pos.index == 0 => cfgResult.getStates(e.source).last
    case Right(DummyEdge(st: S)) => st
    case _ => cfgResult.preStateAt(current.pos)
  }

  /**
    * Execute the statement. Can tell the caller whether the interpreter should continue executing statements or whether
    * it should abort. For method calls it is for example not useful to continue with the next statement unless
    * the effect of the method is actually available.
    *
    * @param current       The currently processed element of the worklist
    * @param statement     The statement to execute
    * @param state         The state to execute the statement on
    * @param worklist      The interpreter's worklist
    * @param programResult The programResult for this analysis
    * @return The new state with the effect of the statement and a boolean telling the caller whether execution of the
    *         successor statement can continue or not.
    */
  protected def executeStatement(current: WorklistElement, statement: Statement, state: S, worklist: InterpreterWorklist, programResult: CfgResultsType[S]): (S, Boolean) = {
    val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
    val successor = statement.forwardSemantics(predecessor)
    logger.trace(predecessor.toString)
    logger.trace(statement.toString)
    logger.trace(successor.toString)
    (successor, true) // true = always continue executing the statements in the block
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

  /**
    * Helper that computes the last position in a block that can be enqueued to the worklist
    *
    * @param current A position anywhere in a block
    * @return The index of the last existing position in the block
    */
  protected def lastIndex(current: BlockPosition): Int = lastIndex(current.block)

  /**
    * Helper that computes the last position in a block that can be enqueued to the worklist
    *
    * @param block The block to look at
    * @return The index of the last existing position in the block
    */
  protected def lastIndex(block: SampleBlock): Int = numElementsInBlock(block) - 1

  // used to memoize the number of elements in a block.
  private val blockElementMemo = mutable.Map[SampleBlock, Int]()

  /**
    * Returns the number of elements for a given block. Since block.elements can be quite costly
    * this method uses memoization to store the number of elements in a block.
    *
    * @param block The block to look at
    * @return Number of elemtns in this block
    */
  protected def numElementsInBlock(block: SampleBlock): Int = {
    if (!(blockElementMemo contains block))
      blockElementMemo += (block -> block.elements.size)
    blockElementMemo(block)
  }

  /**
    * Is called every time the entry block of a CFG was executed
    *
    * @param current  The Block that was interpreted last
    * @param worklist The interpreters worklist
    */
  protected def onEntryBlockExecuted(current: WorklistElement, worklist: InterpreterWorklist): Unit = {}

  def getSuccessorState(cfgResult: CfgResult[S], current: WorklistElement, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    case Left(e: SampleEdge) if current.pos.index == lastIndex(current.pos) => cfgResult.getStates(e.target).head
    case Right(DummyEdge(st: S)) => st
    case _ => cfgResult.postStateAt(current.pos)
  }

  def execute(cfgs: Seq[SampleCfg]): CfgResultsType[S] = {
    // initialize cfg result
    val cfgResults: CfgResultsType[S] = initializeProgramResult(cfgs)

    // TODO: Compute the list of starting points.
    val starts = cfgs.flatMap(_.exit).toSet

    // prepare data structures
    val worklist: InterpreterWorklist = mutable.Queue()
    cfgs.foreach(c => if (c.exit.isDefined) worklist.enqueue(SimpleWorklistElement(BlockPosition(c.exit.get, lastIndex(c.exit.get)), forceReinterpretStmt = false)))
    val iterations = mutable.Map[WorklistElement, Int]() //TODO @flurin

    while (worklist.nonEmpty) {
      val current = worklist.dequeue()
      val currentCfg = cfg(current)
      val iteration = iterations.getOrElse(current, 0)

      // compute exit state of current block
      val exit = if (starts contains current.pos.block) {
        initial(currentCfg)
      } else {
        var state = bottom(currentCfg)
        // join outgoing states
        val edges = outEdges(current, cfgResults)
        for (edge <- edges) {
          val successor = getSuccessorState(cfgResults(current, currentCfg), current, edge)
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
          cfgResults(current, currentCfg).postStateAt(current.pos) widening state
        } else {
          state
        }
      }

      // check for termination and execute block
      val oldStates = cfgResults(current, currentCfg).getStates(current.pos.block)
      // index is zero-based. When interpreting backwards from the Kth index we have to interpret k+1 statements in the block
      val elemsToTake = current.pos.index + 1
      val oldExit = if (oldStates.isEmpty) bottom(currentCfg) else oldStates.last
      var canContinueBlock = true
      if (!(exit lessEqual oldExit) || current.forceReinterpretStmt) {
        // execute block
        val states = ListBuffer(oldStates.reverse.take(numElementsInBlock(current.pos.block) - elemsToTake): _*)
        states.append(exit)
        current.pos.block match {
          case StatementBlock(statements) =>
            // execute statements
            var successor = exit
            statements.take(elemsToTake).reverse.foreach(statement => if (canContinueBlock) {
              val (predecessor, continue) = executeStatement(current, statement, successor, worklist, cfgResults)
              states.append(predecessor)
              successor = predecessor
              canContinueBlock = continue
            })
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
            // we need to figure out whether current.index points to a location in the statements or in the
            // invariants. numStatementsToTake may be negative but that's ok because Seq(....).take(-1) == Nil
            val numInvariantsToTake = if (invariants.size > elemsToTake) elemsToTake else invariants.size
            // execute statements
            var successor = exit
            statements.take(elemsToTake).reverse.foreach(statement => if (canContinueBlock) {
              val (predecessor, continue) = executeStatement(current, statement, successor, worklist, cfgResults)
              states.append(predecessor)
              successor = predecessor
              canContinueBlock = continue
            })
            val intermediate = successor
            // process invariants
            if (canContinueBlock) {
              invariants.take(numInvariantsToTake).foldRight(intermediate) { (invariant, successor) =>
                val predecessor = executeCommand(InvariantCommand, invariant, successor)
                states.append(predecessor)
                predecessor
              }
            }
          case ConstrainingBlock(variables, body) =>
            // execute constraining block
            // TODO: We might want to not support constraining blocks in Sample.
            ???
        }
        // If we aborted a StatementBlock early we'll have less states than we should have.
        // Fill the gap with bottom. These bottom states will be replaced by the actual state when
        // the interpreter continues at this BlockPosition.
        val gapFiller = if (!canContinueBlock) {
          // some statements have been skipped!
          val btm = bottom(currentCfg)
          Seq.fill(oldStates.size - states.size)(btm)
        } else {
          Nil
        }
        cfgResults(current, currentCfg).setStates(current.pos.block, gapFiller ++ states.reverse.toList)

        // update worklist and iteration count
        if (canContinueBlock) { // Only enqueue successors if we interpreter the whole block
          worklist.enqueue(cfg(current).predecessors(current.pos.block).map(b => current.createSuccessorForEnqueue(BlockPosition(b, lastIndex(b)), newForceReinterpretStmt = false)): _*)
          iterations.put(current, iteration + 1)
        }
        //notify (subclasses) about processed entry blocks
        if (currentCfg.entry == current.pos.block) {
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
    * @return a map from cfg to CfgResult. The "worklist" argument is not necessary in the intraprocedural case
    */
  protected def initializeProgramResult(cfgs: Seq[SampleCfg]): CfgResultsType[S] = {
    def lookup(res: Map[SampleCfg, CfgResult[S]])(current: WorklistElement, cfg: SampleCfg) = res(cfg)

    lookup((for (cfg <- cfgs) yield {
      cfg -> initializeResult(cfg, bottom(cfg))
    }).toMap)
  }

  private def assumeCondition(condition: Statement, state: S): S = {
    val successor = state.before(ProgramPointUtils.identifyingPP(condition))
    val predecessor = condition.backwardSemantics(successor).testTrue()
    logger.trace(successor.toString)
    logger.trace(condition.toString)
    logger.trace(predecessor.toString)
    predecessor
  }

  /**
    * Execute the statement. Can tell the caller whether the interpreter should continue executing statements or whether
    * it should abort. For method calls it is for example not useful to continue with the next statement unless
    * the effect of the method is actually available.
    *
    * @param current       The currently processed element of the worklist
    * @param statement     The statement to execute
    * @param state         The state to execute the statement on
    * @param worklist      The interpreter's worklist
    * @param programResult The programResult for this analysis
    * @return The new state with the effect of the statement and a boolean telling the caller whether execution of the
    *         predecessor statement can continue or not.
    */
  protected def executeStatement(current: WorklistElement, statement: Statement, state: S, worklist: InterpreterWorklist, programResult: CfgResultsType[S]): (S, Boolean) = {
    val successor = state.before(ProgramPointUtils.identifyingPP(statement))
    val predecessor = statement.backwardSemantics(successor)
    logger.trace(successor.toString)
    logger.trace(statement.toString)
    logger.trace(predecessor.toString)
    (predecessor, true) // true = always continue executing the statements in the block
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

  protected def outEdges(current: WorklistElement, cfgResult: CfgResultsType[S]): Seq[Either[SampleEdge, AuxiliaryEdge]] = current.pos match {
    case BlockPosition(_, i) if i == lastIndex(current.pos) => cfg(current).outEdges(current.pos.block).map(Left(_))
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

  override def cfg(blockPosition: WorklistElement): SampleCfg = cfg

  override def execute(): CfgResult[S] = {
    val result = execute(Seq(cfg))
    result(null, cfg) //TODO @flurin "null"
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

  override def cfg(blockPosition: WorklistElement): SampleCfg = cfg

  override def execute(): CfgResult[S] = {
    val result = execute(Seq(cfg))
    result(null, cfg) //TODO @flurin "null"
  }
}
