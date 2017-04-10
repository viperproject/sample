/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleBlock
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverProgramDeclaration
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
  override def execute(cfg: SampleCfg, initial: S): CfgResult[S] = {
    // initialize cfg result
    val bottom = initial.bottom()
    val cfgResult = initializeResult(cfg, bottom)

    // prepare data structures
    val worklist = mutable.Queue(cfg.entry)
    val iterations = mutable.Map[SampleBlock, Int]()

    while (worklist.nonEmpty) {
      val current = worklist.dequeue()
      val iteration = iterations.getOrElse(current, 0)

      // compute entry state state of current block
      val entry = if (current == cfg.entry) {
        initial
      } else {
        var state = bottom
        // join incoming states
        val edges = cfg.inEdges(current)
        for (edge <- edges) {
          val predecessor = cfgResult.getStates(edge.source).last
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
          cfgResult.getStates(current).head widening state
        } else {
          state
        }
      }

      // check for termination and execute block
      val oldStates = cfgResult.getStates(current)
      val oldEntry = if (oldStates.isEmpty) bottom else oldStates.head
      if (!(entry lessEqual oldEntry)) {
        // execute block
        val states = ListBuffer(entry)
        current match {
          case StatementBlock(statements) =>
            // execute statements
            statements.foldLeft(entry) { (predecessor, statement) =>
              val successor = executeStatement(statement, predecessor, worklist)
              states.append(successor)
              successor
            }
          case PreconditionBlock(preconditions) =>
            // process preconditions
            preconditions.foldLeft(entry) { (predecessor, precondition) =>
              val successor = executeCommand(PreconditionCommand, precondition, predecessor)
              states.append(successor)
              successor
            }
          case PostconditionBlock(postconditions) =>
            // process postconditions
            postconditions.foldLeft(entry) { (predecessor, postcondition) =>
              val successor = executeCommand(PostconditionCommand, postcondition, predecessor)
              states.append(successor)
              successor
            }
          case LoopHeadBlock(invariants, statements) =>
            // process invariants
            val intermediate = invariants.foldLeft(entry) { (predecessor, invariant) =>
              val successor = executeCommand(InvariantCommand, invariant, predecessor)
              states.append(successor)
              successor
            }
            // execute statements
            statements.foldLeft(intermediate) { (predecessor, statement) =>
              val successor = executeStatement(statement, predecessor, worklist)
              states.append(successor)
              successor
            }
          case ConstrainingBlock(variables, body) =>
            // execute constraining block
            // TODO: We might want to not support constraining blocks in Sample.
            ???
        }
        cfgResult.setStates(current, states.toList)

        // update worklist and iteration count
        worklist.enqueue(cfg.successors(current): _*)
        iterations.put(current, iteration + 1)
      }
    }

    // return result
    cfgResult
  }

  protected def executeStatement(statement: Statement, state: S, worklist: mutable.Queue[SampleBlock]): S = {
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
  var methodEntryStates : mutable.Map[String, mutable.Map[ProgramPoint, S]] = mutable.Map().withDefault(_ => mutable.Map())
  var callsInProgram : mutable.Map[String, Set[SampleBlock]] = mutable.Map().withDefault(_ => Set())

  override def execute(cfg: SampleCfg, initial: S): CfgResult[S] = {
    //find every call to a method
    //this will also include assert() etc.
    //TODO: cleanup code
    for(method <- program.methods; block <- method.body.blocks) {
      block match {
      case StatementBlock(statements) =>
        statements.foreach(s => s match {
          case MethodCall(_, v: Variable, _, _, _, _) => callsInProgram(v.getName) = callsInProgram(v.getName) + block
          case _ =>
        })
      case LoopHeadBlock(_, statements) =>
        statements.foreach(s => s match {
          case MethodCall(_, v: Variable, _, _, _, _) => callsInProgram(v.getName) = callsInProgram(v.getName) + block
          case _ =>
        })
      case _ =>
      }
    }
    super.execute(cfg, initial)
  }

  override protected def executeStatement(statement: Statement, state: S, worklist: mutable.Queue[SampleBlock]): S = {
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
        //TODO can we use State.ids like this?
        tmpVariableState = tmpVariableState.ids.toSetOrFail // let's remove them
          .filter(id => ! id.getName.startsWith("arg_#"))
          .foldLeft(tmpVariableState)((st, ident)=> st.removeVariable(ExpressionSet(ident)))

        //context insensitive analysis: analyze the called method with the join of all calling states
        // this implementation could analyze the method several times
        //TODO is this a problem?
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
        var index = 0
        val returnVariableMapping = for(tuple <- methodDeclaration.parameters.reverse.zip(targetExpressions.reverse).reverse) yield {
          // methodDeclaration.parameters = [param1, param2... paramN, return1, return2... returnN] .reverse calls above are to
          // only work with return parameters and to preserve their order
          // tuple._1 = the variable declared in returns(...) of the method
          // tuple._2 = the target-expression which we'll assign to later
          val exp = ExpressionSet(VariableIdentifier("ret_#" + index )(tuple._1.typ))
          index += 1
          exitState = exitState.createVariable(exp, tuple._1.typ, DummyProgramPoint).assignVariable(exp, ExpressionSet(tuple._1.variable.id))
          (tuple._2, exp)
        }
        //TODO can we use State.ids like this?
        exitState = exitState.ids.toSetOrFail // let's all non ret_# variables
          .filter(id => ! id.getName.startsWith("ret_#"))
          .foldLeft(exitState)((st, ident)=> st.removeVariable(ExpressionSet(ident)))
        // map return values to temp variables and remove all temporary ret_# variables
        val joinedState = returnVariableMapping.foldLeft(currentState lub exitState)((st, tuple) => (st.assignVariable _).tupled(tuple))
        val resultState = returnVariableMapping.foldLeft(joinedState)((st, tupple) => st.removeVariable(tupple._2))
        logger.trace(predecessor.toString)
        logger.trace(statement.toString)
        logger.trace(resultState.toString)

        //contex insensitive analysis: we need to reanalyze all blocks containing a call to this method
        //TODO figure out how to handle enqueued blocks in other method
        worklist.enqueue(callsInProgram(name).toSeq : _*)

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

case class FinalResultInterproceduralForwardInterpreter[S <: State[S]](override val program: SilverProgramDeclaration, override val builder: SilverEntryStateBuilder[S])
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