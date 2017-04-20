/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleBlock
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverProgramDeclaration
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
  override def execute(cfg: SampleCfg, initial: S): CfgResult[S] = {
    // initialize cfg result
    val bottom = initial.bottom()
    val cfgResult = initializeResult(cfg, bottom)

    // prepare data structures
    val worklist = mutable.Queue[BlockPosition](BlockPosition(cfg.entry, 0))
    val iterations = mutable.Map[BlockPosition, Int]()

    while (worklist.nonEmpty) {
      val current = worklist.dequeue()
      val iteration = iterations.getOrElse(current, 0)

      // compute entry state state of current block
      val entry = if (current.block == cfg.entry) {
        initial
      } else {
        var state = bottom
        // join incoming states
        val edges = cfg.inEdges (current.block)
        for (edge <- edges) {
          val predecessor = getPredecessorState(cfgResult, current, edge)
          // filter state if there is a condition
          val filtered = edge match {
            case ConditionalEdge(condition, _, _, _) => condition.forwardSemantics(predecessor).testTrue()
            case UnconditionalEdge(_, _, _) => predecessor
          }
          // handle in and out edges
          val adapted = edge.kind match {
            case Kind.In => filtered.command(EnterLoopCommand())
            case Kind.Out => filtered.command(LeaveLoopCommand())
            case _ => filtered
          }

          state = state lub adapted
        }
        // widening
        if (edges.size > 1 && iteration > SystemParameters.wideningLimit) {
          cfgResult.preStateAt(current) widening  state
        } else {
          state
        }
      }

      // check for termination and execute block
      val  oldStates = cfgResult.getStates(current.block)
      val numToSkip = current.index
      val oldEntry = if (oldStates.isEmpty) bottom else cfgResult.preStateAt(current)
      if (!(entry lessEqual oldEntry)) {
        // execute block
        val states = ListBuffer(oldStates.take(numToSkip): _*)
        states.append(entry)
        current.block match {
          case StatementBlock(statements) =>
            // execute statements
            statements.drop(numToSkip).foldLeft(entry) { (predecessor, statement) =>
                val successor = executeStatement(statement, predecessor, worklist)
                states.append(successor)
                successor
            }
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
            statements.drop(numToSkip - invariants.size).foldLeft(intermediate) { (predecessor, statement) =>
              val successor = executeStatement(statement, predecessor, worklist)
              states.append(successor)
              successor
            }
          case ConstrainingBlock(variables, body) =>
            // execute constraining block
            // TODO: We might want to not support constraining blocks in Sample.
            ???
        }
        cfgResult.setStates(current.block, states.toList)

        // update worklist and iteration count
        worklist.enqueue(cfg.successors(current.block).map(b => BlockPosition(b, 0)): _*)
        iterations.put(current, iteration + 1)
      }
    }

    // return result
    cfgResult
  }

  protected def getPredecessorState(cfgResult: CfgResult[S], current: BlockPosition, edge: Edge[Statement, Statement]): S = {
    current.index match {
      case 0 => cfgResult.getStates(edge.source).last
      // For offsets do not use the incoming edge but the state of the previous instruction of the CURRENT block
      case _ => cfgResult.preStateAt(current)
    }
  }

  protected def executeStatement(statement: Statement, state: S, worklist: mutable.Queue[BlockPosition]): S = {
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
  * Performs a forward interpretation of a control flow graph with special handling for method and function calls.
  *
  * @tparam S The type of the states.
  */
trait InterproceduralSilverForwardInterpreter[S <: State[S]]
  extends SilverForwardInterpreter[S]
    with LazyLogging {
  val program: SilverProgramDeclaration
  val builder: SilverEntryStateBuilder[S]
  val methodEntryStates : mutable.Map[String, mutable.Map[ProgramPoint, S]] = mutable.Map().withDefault(_ => mutable.Map())
  val methodExitStates : mutable.Map[String, S] = mutable.Map()
  val callsInProgram : Map[String, Set[BlockPosition]]// = mutable.Map().withDefault(_ => Set())

  override def getPredecessorState(cfgResult: CfgResult[S], current: BlockPosition, edge: Edge[Statement, Statement]): S = {
    //
    // idea: if previous instruction was a method-call and we hava a BlockPosition.index > 0 then it was a back-jump from
    // a method-call. Use the entrystate and add the infos we got from analyzing the called method.
    //
    //TODO @flurin
    current.index match {
      case 0 => super.getPredecessorState(cfgResult, current, edge)
      case _ => {
        val tmp = cfgResult.preStateAt(current) //TODO @flurin check again // getStates(edge.target)(current.index -1)
        current.block match {
          case StatementBlock(statements) => statements(current.index -1) match {
            case MethodCall(_, v: Variable, _, _, _, targets: List[Statement]) => {
              val methodDeclaration = program.methods.find(m => m.name.name == v.getName).get
              val methodCall = statements(current.index -1).asInstanceOf[MethodCall]
              tmp.command(LeaveMethodCommand(methodDeclaration, methodCall, tmp, methodExitStates))
            }
            case _ => tmp
          }
          case LoopHeadBlock(invs, statements) => statements(current.index - invs.size - 1) match {
            case MethodCall(_, v: Variable, _, _, _, targets: List[Statement]) =>{
              val methodDeclaration = program.methods.find(m => m.name.name == v.getName).get
              val methodCall = statements(current.index -1).asInstanceOf[MethodCall]
              tmp.command(LeaveMethodCommand(methodDeclaration, methodCall, tmp, methodExitStates))
            }
            case _ => tmp
          }
          case _ => tmp
        }
      }
    }
  }

  override protected def executeStatement(statement: Statement, state: S, worklist: mutable.Queue[BlockPosition]): S = {
    statement match {
      case MethodCall(_, f: FieldAccess, _, _, _, _) => return super.executeStatement(statement, state, worklist)
      case call: MethodCall => {
        val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
        val name: String = call.method match {
          case v: Variable => v.getName
          case _ => throw new RuntimeException("Should not happen") //TODO
        }
        var currentState = predecessor
        val targetExpressions = for(target <- call.targets) yield {
          val (exp, st) = UtilitiesOnStates.forwardExecuteStatement(currentState, target)
          currentState = st
          exp
        }
        val parameterExpressions = for (parameter <- call.parameters) yield {
          currentState = parameter.forwardSemantics[S](currentState)
          currentState.expr
        }
        val methodDeclaration = program.methods.find(m => m.name.name == name).get

        // create arg_# variables and assign the value to them. then remove all non arg_# variables
        var tmpVariableState = currentState
        val tmpArguments = for((param, index) <- parameterExpressions.zipWithIndex) yield {
          val exp = ExpressionSet(VariableIdentifier("arg_#" + index )(param.typ))
          tmpVariableState = tmpVariableState.createVariable(exp, param.typ, DummyProgramPoint)
          tmpVariableState = tmpVariableState.assignVariable(exp, param)
          exp
        }
        tmpVariableState = tmpVariableState.ids.toSetOrFail // let's remove them
          .filter(id => ! id.getName.startsWith("arg_#"))
          .foldLeft(tmpVariableState)((st, ident)=> st.removeVariable(ExpressionSet(ident)))

        //context insensitive analysis: analyze the called method with the join of all calling states
        // this implementation could analyze the method several times
        methodEntryStates(name) = methodEntryStates(name) + (statement.getPC() -> tmpVariableState)
        tmpVariableState = methodEntryStates(name).values.foldLeft(tmpVariableState)((st1, st2) => st1 lub st2)

        // create input state for intraprocedural analysis
        var inputState = builder.build(program, methodDeclaration) lub tmpVariableState
        // assign (temporary) arguments to parameters and remove the temp args
        inputState = methodDeclaration.parameters.zip(tmpArguments).foldLeft(inputState)((st, tuple) => st.assignVariable(ExpressionSet(tuple._1.variable.id), tuple._2))
        inputState = tmpArguments.foldLeft(inputState)((st, tmpArg) => st.removeVariable(tmpArg))
        // (intraprocedural) analysis of method
        var result = FinalResultForwardInterpreter[S]().execute(methodDeclaration.body, inputState)
        // create return variables ret_# and assign the values to them
        var exitState = result.exitState()

        //
        val resultState = currentState.command(LeaveMethodCommand(methodDeclaration, call, exitState, methodExitStates))

        logger.trace(predecessor.toString)
        logger.trace(statement.toString)
        logger.trace(resultState.toString)

        //enqueue all statements directly after each calls to the method
        //if the method-call was the last statement of the block we do not enqueue here. the interpreter will enqueue all
        //blocks for us. Calls from a block that has not been analyzed before are also not enqueued.
        callsInProgram(name)
          .filter(b => methodEntryStates(name)
            // block must have been analyzed before and methodcall mustn't be the last statement
            .contains(b.block.elements(b.index).merge.getPC()) && b.index < b.block.elements.size -1
          )
          .foreach(b => worklist.enqueue(BlockPosition(b.block, b.index + 1)))
        resultState
      }
      case _ => return super.executeStatement(statement, state, worklist)
    }
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
            case ConditionalEdge(condition, _, _, _) => condition.backwardSemantics(adapted).testTrue()
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

case class FinalResultInterproceduralForwardInterpreter[S <: State[S]](override val program: SilverProgramDeclaration, override val builder: SilverEntryStateBuilder[S], override val callsInProgram : Map[String, Set[BlockPosition]])
  extends InterproceduralSilverForwardInterpreter[S] {
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