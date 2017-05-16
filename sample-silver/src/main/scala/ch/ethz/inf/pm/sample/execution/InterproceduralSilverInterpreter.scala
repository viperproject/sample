/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, UtilitiesOnStates, VariableIdentifier}
import ch.ethz.inf.pm.sample.execution.InterproceduralSilverInterpreter.{CallGraphMap, MethodEntryStatesMap, MethodExitStatesMap}
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleEdge
import ch.ethz.inf.pm.sample.execution.SilverInterpreter.{CfgResultMapType, InterpreterWorklistType}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverIdentifier, SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.permissionanalysis.ReturnFromMethodCommand
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

package object InterproceduralSilverInterpreter {
  /**
    * The MethodEntryStatesMap keeps track of all the incoming states to a method.
    * For each method call in the program the ProgramPoint and the state will be saved in this map.
    *
    * @tparam S the type of the state
    */
  type MethodEntryStatesMap[S] = mutable.Map[SilverIdentifier, mutable.Map[ProgramPoint, S]]

  /**
    * A datastructure used to store the exit states of an analzyed method. Context insensitive analyses
    * can use this to lookup the effect of a called method.
    *
    * @tparam S the type of the state
    */
  type MethodExitStatesMap[S] = mutable.Map[SilverIdentifier, S]

  /**
    * The CallGraphMap maps each method(SilverIdentifiers) to a set of it's callees.
    * "foo" -> Set(BlockPosition(block, index))
    * Would mean that the statement "index" of block "block" is a method call. The statement "index+1" would be
    * the next instruction after the method call.
    */
  type CallGraphMap = Map[SilverIdentifier, Set[BlockPosition]]
}

/**
  * The interprocedural interpreter adds auxiliary edges to the CFG.
  * This trait represents such an edge.
  */
sealed trait AuxiliaryEdge

/**
  * A dummy edge used to annotate the CFG with additional information.
  *
  * @param info can be used to provide more info about this edge
  * @tparam T type of the info attribute
  */
case class DummyEdge[T](info: T) extends AuxiliaryEdge

/**
  * Represents a method call. Blocks with in-edges of type MethodCallEdge are called from somewhere in the
  * analysed program. The inputState is a state where the parameters have been initialised using the arguments
  * from the calling context.
  *
  * @param inputState a method declaration of the called method.
  */
case class MethodCallEdge[S](inputState: S) extends AuxiliaryEdge

/**
  * Performs a forward interpretation of a control flow graph with special handling for methods.
  *
  * @tparam S The type of the states.
  * @author Flurin Rindisbacher
  */
trait InterproceduralSilverForwardInterpreter[S <: State[S]]
  extends SilverForwardInterpreter[S]
    with LazyLogging {
  val program: SilverProgramDeclaration
  val builder: SilverEntryStateBuilder[S]
  val methodEntryStates: MethodEntryStatesMap[S] = mutable.Map().withDefault(_ => mutable.Map())
  val methodExitStates: MethodExitStatesMap[S] = mutable.Map()
  val callsInProgram: CallGraphMap
  val programResult: ProgramResult[S] = DefaultProgramResult(program)
  /*
   * Seq of cfg that will be analyzed.
   * cfgsInAnalysisOrder.head is the first that is enqueued into the worklist
   */
  val cfgsInAnalysisOrder: Seq[SampleCfg]

  def executeInterprocedural(): ProgramResult[S] = {
    super.execute(cfgsInAnalysisOrder)
    programResult
  }

  override protected def inEdges(current: BlockPosition, cfgResult: Map[SampleCfg, CfgResult[S]]): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
    /**
      * If the current block is an entrypoint of the cfg and this block (method) is called throughout the program
      * we'll add a MethodCallEdge. The number of edges should equal the number of calls. But it's possible that
      * methodEntryStates does not yet contain calling-contexts of all call locations. If the number of known entry states
      * is less than the number of calls, we'll add additional edges with bottom as calling context.
      *
      * @return
      */
    def createMethodCallEdges(): Seq[Either[SampleEdge, MethodCallEdge[S]]] = {
      if (cfg(current).entry != current.block)
        return Nil
      val method = findMethod(current)
      if (callsInProgram.contains(method.name)) {
        val numInEdgesShould = callsInProgram(method.name).size
        val numInEdgesIs = methodEntryStates(method.name).size
        val bottom = initial(cfg(current)).bottom()
        (for (entryState <- methodEntryStates(method.name).values) yield {
          Right(MethodCallEdge(entryState))
        }).toSeq ++ Seq.fill(numInEdgesShould - numInEdgesIs)(Right(MethodCallEdge(bottom)))
      } else {
        Nil
      }
    }

    current match {
      case BlockPosition(_, 0) => super.inEdges(current, cfgResult) ++ createMethodCallEdges()
      case _ => super.inEdges(current, cfgResult)
    }
  }

  override protected def onExitBlockExecuted(current: BlockPosition, worklist: InterpreterWorklistType): Unit = {
    /**
      * In the context insensitive analysis everytime a method has been analysed we enqueue all call locations
      * of this method. This way the new analysis results will be merged into the caller state.
      */
    val method = findMethod(current)
    callsInProgram(method.name)
      .filter(b => methodEntryStates(method.name)
        // only enqueue blocks that have been analysed before
        .contains(b.block.elements(b.index).merge.getPC())
      )
      .foreach(b => worklist.enqueue((BlockPosition(b.block, b.index), true)))
  }

  override protected def initial(cfg: SampleCfg): S = {
    builder.build(program, findMethod(BlockPosition(cfg.entry, 0)))
  }

  override protected def cfg(blockPosition: BlockPosition): SampleCfg = {
    program.methods.filter(_.body.blocks.contains(blockPosition.block)).head.body
  }

  private def findMethod(blockPosition: BlockPosition): SilverMethodDeclaration = {
    program.methods.filter(_.body.blocks.contains(blockPosition.block)).head
  }

  private def findMethod(name: SilverIdentifier): SilverMethodDeclaration = {
    findMethod(name.name)
  }

  private def findMethod(name: String): SilverMethodDeclaration = {
    program.methods.find(_.name.name == name).get
  }

  override def getPredecessorState(cfgResult: CfgResult[S], current: BlockPosition, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    // For MethodCallEdges use an empty state with the arguments from the call
    case Right(MethodCallEdge(callingContext: S)) => {
      val methodDeclaration = findMethod(current)
      val name = methodDeclaration.name.name
      val tmpArguments = for ((param, index) <- methodDeclaration.arguments.zipWithIndex) yield {
        ExpressionSet(VariableIdentifier("arg_#" + index)(param.typ))
      }
      var inputState = initial(methodDeclaration.body) lub callingContext
      // assign (temporary) arguments to parameters and remove the temp args
      inputState = methodDeclaration.arguments.zip(tmpArguments).foldLeft(inputState)((st, tuple) => st.assignVariable(ExpressionSet(tuple._1.variable.id), tuple._2))
      tmpArguments.foldLeft(inputState)((st, tmpArg) => st.removeVariable(tmpArg))
    }
    case _ => super.getPredecessorState(cfgResult, current, edge)
  }

  override protected def executeStatement(statement: Statement, state: S, worklist: InterpreterWorklistType, programResult: CfgResultMapType[S]): S = statement match {
    case call@MethodCall(_, v: Variable, _, _, _, _) => {
      //
      // prepare calling context (evaluate method targets and parameters)
      //
      val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
      val methodIdentifier = SilverIdentifier(v.getName)
      var currentState = predecessor
      val targetExpressions = for (target <- call.targets) yield {
        val (exp, st) = UtilitiesOnStates.forwardExecuteStatement(currentState, target)
        currentState = st
        exp
      }
      val parameterExpressions = for (parameter <- call.parameters) yield {
        currentState = parameter.forwardSemantics[S](currentState)
        currentState.expr
      }
      //
      // transfer arguments to methodEntryState
      //
      val methodDeclaration = findMethod(methodIdentifier)
      // create arg_# variables and assign the value to them. then remove all non arg_# variables
      var tmpVariableState = currentState
      val tmpArguments = for ((param, index) <- parameterExpressions.zipWithIndex) yield {
        val exp = ExpressionSet(VariableIdentifier("arg_#" + index)(param.typ))
        tmpVariableState = tmpVariableState.createVariable(exp, param.typ, DummyProgramPoint)
        tmpVariableState = tmpVariableState.assignVariable(exp, param)
        exp
      }
      tmpVariableState = tmpVariableState.ids.toSetOrFail // let's remove them
        .filter(id => !id.getName.startsWith("arg_#"))
        .foldLeft(tmpVariableState)((st, ident) => st.removeVariable(ExpressionSet(ident)))

      //context insensitive analysis: analyse the called method with the join of all calling states
      methodEntryStates(methodIdentifier) = methodEntryStates(methodIdentifier) + (statement.getPC() -> tmpVariableState)
      worklist.enqueue((BlockPosition(methodDeclaration.body.entry, 0), false))

      //
      // if callee has been analyzed, merge results back into our state
      // (otherwise currentState.command() will return bottom (which is valid until the called method is analyzed))
      //
      val exitState = programResult(methodDeclaration.body).exitState()
      val resultState = currentState.command(ReturnFromMethodCommand(methodDeclaration, call, exitState))
      logger.trace(predecessor.toString)
      logger.trace(statement.toString)
      logger.trace(resultState.toString)
      resultState
    }
    case _ => return super.executeStatement(statement, state, worklist, programResult)
  }
}

/**
  * Forward interpreter that handles method calls using a context insensitive approach.
  *
  * @param program             The program that is analysed
  * @param cfgsInAnalysisOrder A sequence of cfgs that should be analysed in this order
  * @param builder             A builder to create initial states for each cfg to analyse
  * @param callsInProgram      The call graph of the program
  * @tparam S The type of the states.
  */
case class FinalResultInterproceduralForwardInterpreter[S <: State[S]](
                                                                        override val program: SilverProgramDeclaration,
                                                                        override val cfgsInAnalysisOrder: Seq[SampleCfg],
                                                                        override val builder: SilverEntryStateBuilder[S],
                                                                        override val callsInProgram: CallGraphMap)
  extends InterproceduralSilverForwardInterpreter[S] {

  //
  // Store all CfgResults inside the ProgramResult and return a CfgResultMapType to let the intraprocedural
  // interpreter do its work
  //
  override protected def initializeProgramResult(cfgs: Seq[SampleCfg], states: Seq[S]): CfgResultMapType[S] = {
    // initialize each CfgResult with its bottom state. Our initializer does not need the 2nd parameter to initialize()
    // states.head is just passed in to make the compiler happy
    programResult.initialize(c => {
      val stForCfg = states(cfgs.indexOf(c))
      initializeResult(c, stForCfg)
    })
    (for (method <- program.methods) yield {
      (method.body -> programResult.getResult(method.name))
    }) toMap
  }

  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val cfgResult = FinalCfgResult[S](cfg)
    cfgResult.initialize(state)
    cfgResult
  }

  /*
   *  initial() and cfg() only make sense in the intraprocedural case
   */
  override def initial: S = ???

  override def cfg: SampleCfg = ???
}