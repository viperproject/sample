/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, UtilitiesOnStates, VariableIdentifier}
import ch.ethz.inf.pm.sample.execution.InterproceduralSilverInterpreter.{CallGraphMap, CallString, MethodTransferStatesMap}
import ch.ethz.inf.pm.sample.execution.SampleCfg.{SampleBlock, SampleEdge}
import ch.ethz.inf.pm.sample.execution.SilverInterpreter.{CfgResultsType, InterpreterWorklist}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverIdentifier, SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.permissionanalysis.{CallMethodBackwardsCommand, ReturnFromMethodCommand}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

object InterproceduralSilverInterpreter {
  /**
    * The MethodTransferStatesMap keeps track of all the incoming states to a method.
    * For each method call in the program the ProgramPoint and the state will be saved in this map. In the forward analysis
    * a state in here represents the arguments to a method. In the backward analysis these states represent the state
    * of the Method's returns.
    *
    * @tparam S the type of the state
    */
  type MethodTransferStatesMap[S] = mutable.Map[(CallString, SilverIdentifier), S]

  /**
    * The CallGraphMap maps each method(SilverIdentifiers) to a set of it's callees.
    * "foo" -> Set(BlockPosition(block, index))
    * Would mean that the statement "index" of block "block" is a method call. The statement "index+1" would be
    * the next instruction after the method call.
    */
  type CallGraphMap = Map[SilverIdentifier, Set[BlockPosition]]

  /**
    * Type to represent a call-string for interprocedural analysis
    * TODO @flurin should we append to the left or right for growing the call-string?
    */
  type CallString = Seq[ProgramPoint]

  /**
    * Prefix to be used for temporary method arguments.
    */
  val ArgumentPrefix = "arg_#"

  /**
    * Prefix to bused for temporary method returns.
    */
  val ReturnPrefix = "ret_#"
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
  * @param inputState The state of the method's arguments in the calling context.
  * @tparam S The type of the states.
  */
case class MethodCallEdge[S](inputState: S) extends AuxiliaryEdge

/**
  * Represents the return edge of a method call. A MethodReturnEdge is for the backward analysis, what a MethodCallEdge
  * is for the forward analysis. The exitState represents the state of the called method's returns in the calling context.
  *
  * @param exitState The state of the called method's returns in the calling context.
  * @tparam S The type of the states.
  */
case class MethodReturnEdge[S](exitState: S) extends AuxiliaryEdge


case class TaggedWorklistElement(val callString: CallString,
                                 override val pos: BlockPosition,
                                 override val forceReinterpretStmt: Boolean) extends WorklistElement {

  override def createSuccessorForEnqeueue(newPos: BlockPosition, newForceReinterpretStmt: Boolean): WorklistElement = {
    copy(pos = newPos, forceReinterpretStmt = newForceReinterpretStmt)
  }

}

trait InterprocHelpers[S <: State[S]] {
  val program: SilverProgramDeclaration
  val builder: SilverEntryStateBuilder[S]
  /** this map is used to store entry (forward) or exit (backward) states for method calls
    * executeStatement() stores the states here and in-/outEdges then uses them to create MethodCall / MethodReturn edges
    */
  val methodTransferStates: MethodTransferStatesMap[S] = mutable.Map()
  val callsInProgram: CallGraphMap
  val programResult: ProgramResult[S] = DefaultProgramResult(program)
  /*
   * A set of methods that will be treated as main-methods.
   * A main method is analyzed using "initial" as the entry state.
   */
  val mainMethods: Set[SilverIdentifier]

  private lazy val methods: Map[Either[String, SampleBlock], SilverMethodDeclaration] = {
    var res: Map[Either[String, SampleBlock], SilverMethodDeclaration] = Map()
    for (m <- program.methods) {
      res += (Left(m.name.name) -> m)
      for (b <- m.body.blocks)
        res += (Right(b) -> m)
    }
    res
  }

  protected def findMethod(current: WorklistElement): SilverMethodDeclaration = findMethod(current.pos)

  protected def findMethod(blockPosition: BlockPosition): SilverMethodDeclaration = {
    methods(Right(blockPosition.block))
  }

  protected def findMethod(name: SilverIdentifier): SilverMethodDeclaration = {
    findMethod(name.name)
  }

  protected def findMethod(name: String): SilverMethodDeclaration = {
    methods(Left(name))
  }

}

/**
  * Performs a forward interpretation of a whole program starting with a set of main methods.
  *
  * @tparam S The type of the states.
  * @author Flurin Rindisbacher
  */
trait InterproceduralSilverForwardInterpreter[S <: State[S]]
  extends SilverForwardInterpreter[S]
    with LazyLogging with InterprocHelpers[S] {

  import InterproceduralSilverInterpreter.ArgumentPrefix

  def executeInterprocedural(): ProgramResult[S] = {
    // execute the interpreter starting with all "main"-methods
    super.execute(program.methods.filter(m => mainMethods.contains(m.name)).map(_.body))
    programResult
  }

  override protected def inEdges(current: WorklistElement, cfgResult: CfgResultsType[S]): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
    /**
      * If the current block is an entrypoint of the cfg and this block (method) is called throughout the program
      * we'll add a MethodCallEdge. The number of edges should equal the number of calls. But it's possible that
      * methodEntryStates does not yet contain calling-contexts of all call locations. If the number of known entry states
      * is less than the number of calls, we'll add additional edges with bottom as calling context.
      *
      * @return
      */
    def createMethodCallEdges(): Seq[Either[SampleEdge, MethodCallEdge[S]]] = {
      lazy val method = findMethod(current)
      if (cfg(current).entry == current.pos.block) { // only entry-blocks can have incoming MethodCall edges
        current match {
          case TaggedWorklistElement(callString, _, _) =>
            val initialState = initial(cfg(current))
            val bottom = initialState.bottom()
            if (methodTransferStates.contains((callString, method.name)))
              Seq(Right(MethodCallEdge(methodTransferStates((callString, method.name)))))
            else
              Seq(Right(MethodCallEdge(bottom)))
        }
      } else {
        Nil
      }
    }

    current.pos match {
      case BlockPosition(_, 0) => super.inEdges(current, cfgResult) ++ createMethodCallEdges()
      case _ => super.inEdges(current, cfgResult)
    }
  }

  override protected def onExitBlockExecuted(current: WorklistElement, worklist: InterpreterWorklist): Unit = {
    /**
      * In the context insensitive analysis everytime a method has been analysed we enqueue all call locations
      * of this method. This way the new analysis results will be merged into the caller state.
      */
    val method = findMethod(current)
    callsInProgram(method.name)
      //.filter(b => methodTransferStates((current.asInstanceOf[TaggedWorklistElement].callString, method.name)) //TODO @flurin
      // only enqueue blocks that have been analysed before
      //  .contains(b.block.elements(b.index).merge.getPC())
      //)
      .foreach(b => {
      current match {
        case TaggedWorklistElement(callString, _, _) if callString.length > 0 && b.block.elements(b.index).merge.getPC() == callString.reverse.head => // TODO @flurin fix this!
          worklist.enqueue(TaggedWorklistElement(callString.reverse.drop(1).reverse, b, true))
        case _ =>
      }
    })
  }

  override def initial(cfg: SampleCfg): S = {
    builder.build(program, findMethod(BlockPosition(cfg.entry, 0)))
  }

  override def cfg(blockPosition: WorklistElement): SampleCfg = {
    findMethod(blockPosition).body
  }

  override def getPredecessorState(cfgResult: CfgResult[S], current: WorklistElement, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    // For MethodCallEdges use an empty state with the arguments from the call
    case Right(edge: MethodCallEdge[S]) =>
      val callingContext = edge.inputState
      val methodDeclaration = findMethod(current)
      val tmpArguments = for ((param, index) <- methodDeclaration.arguments.zipWithIndex) yield {
        ExpressionSet(VariableIdentifier(ArgumentPrefix + index)(param.typ))
      }
      var inputState = initial(methodDeclaration.body) lub callingContext
      // assign (temporary) arguments to parameters and remove the temp args
      inputState = methodDeclaration.arguments.zip(tmpArguments).foldLeft(inputState)((st, tuple) => st.assignVariable(ExpressionSet(tuple._1.variable.id), tuple._2))
      tmpArguments.foldLeft(inputState)((st, tmpArg) => st.removeVariable(tmpArg))
    case _ => super.getPredecessorState(cfgResult, current, edge)
  }

  override protected def executeStatement(current: WorklistElement, statement: Statement, state: S, worklist: InterpreterWorklist, programResult: CfgResultsType[S]): S = statement match {
    case call@MethodCall(_, v: Variable, _, _, _, _) =>
      //
      // prepare calling context (evaluate method targets and parameters)
      //
      val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
      val methodIdentifier = SilverIdentifier(v.getName)
      var currentState = predecessor
      val parameterExpressions = for (parameter <- call.parameters) yield {
        currentState = parameter.forwardSemantics[S](currentState)
        currentState.expr
      }
      val targetExpressions = for (target <- call.targets) yield {
        val (exp, st) = UtilitiesOnStates.forwardExecuteStatement(currentState, target)
        currentState = st
        exp
      }
      //
      // transfer arguments to methodEntryState
      //
      val methodDeclaration = findMethod(methodIdentifier)
      // create temp argument variables and assign the value to them. then remove all non temp-arg-variables
      var tmpVariableState = currentState
      for ((param, index) <- parameterExpressions.zipWithIndex) {
        val exp = ExpressionSet(VariableIdentifier(ArgumentPrefix + index)(param.typ))
        tmpVariableState = tmpVariableState.createVariable(exp, param.typ, DummyProgramPoint)
        tmpVariableState = tmpVariableState.assignVariable(exp, param)
      }
      tmpVariableState = tmpVariableState.ids.toSetOrFail // let's remove them
        .filter(id => !id.getName.startsWith(ArgumentPrefix))
        .foldLeft(tmpVariableState)((st, ident) => st.removeVariable(ExpressionSet(ident)))

      //Enqueue the called method for analysis and grow the callstring
      val callString = current match {
        case tagged: TaggedWorklistElement => tagged.callString :+ statement.getPC()
        case _ => Seq(statement.getPC())
      }
      //TODO @flurin tag the transfer state also with the callstring
      methodTransferStates((callString, methodIdentifier)) = tmpVariableState
      worklist.enqueue(TaggedWorklistElement(callString, BlockPosition(methodDeclaration.body.entry, 0), false))

      //
      // if callee has been analyzed, merge results back into our state
      // (otherwise currentState.command() will return bottom (which is valid until the called method is analyzed))
      //
      val analyzed = TaggedWorklistElement(callString, null, false) //TODO @flurin that's ugly
    val exitState = programResult(analyzed, methodDeclaration.body).exitState()
      val resultState = currentState.command(ReturnFromMethodCommand(methodDeclaration, call, targetExpressions, exitState))
      logger.trace(predecessor.toString)
      logger.trace(statement.toString)
      logger.trace(resultState.toString)
      resultState
    case _ => super.executeStatement(current, statement, state, worklist, programResult)
  }
}

/**
  * Forward interpreter that handles method calls using a context insensitive approach.
  *
  * @param program        The program that is analysed
  * @param mainMethods    A set of methods that should be treated as main-methos (i.e. use initial as entry state)
  * @param builder        A builder to create initial states for each cfg to analyse
  * @param callsInProgram The call graph of the program
  * @tparam S The type of the states.
  */
case class FinalResultInterproceduralForwardInterpreter[S <: State[S]](
                                                                        override val program: SilverProgramDeclaration,
                                                                        override val mainMethods: Set[SilverIdentifier],
                                                                        override val builder: SilverEntryStateBuilder[S],
                                                                        override val callsInProgram: CallGraphMap)
  extends InterproceduralSilverForwardInterpreter[S] {

  //
  // Store all CfgResults inside the ProgramResult and return a CfgResultMapType to let the intraprocedural
  // interpreter do its work. Note: the interprocedural interpreter initializes ALL methods and not only those passed in
  // using "cfgs". (This is needed to initialize all callees too)
  //
  override protected def initializeProgramResult(cfgs: Seq[SampleCfg]): CfgResultsType[S] = {
    // initialize each CfgResult with its bottom state.
    programResult.initialize(c => {
      val stForCfg = bottom(c)
      initializeResult(c, stForCfg)
    })

    def lookup(res: mutable.Map[(CallString, SampleCfg), CfgResult[S]])(current: WorklistElement, cfg: SampleCfg) = current match {
      case TaggedWorklistElement(callString, _, _) =>
        if (!(res contains(callString, cfg)))
          res += ((callString, cfg) -> initializeResult(cfg, bottom(cfg))) //TODO @flurin should clone the result here?
        res((callString, cfg))
      case _ =>
        res((Nil, cfg))
    }

    lookup(mutable.Map((for (method <- program.methods) yield {
      (Nil, method.body) -> programResult.getResult(method.name)
    }): _*))
  }

  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val cfgResult = FinalCfgResult[S](cfg)
    cfgResult.initialize(state)
    cfgResult
  }

  // The entrypoint for an interprocedural analysis is executeInterprocedural()
  // For programs with multiple methods calling execute() cannot work as expected because it's not clear
  // which CfgResult should be returned.
  override def execute(): CfgResult[S] = {
    if (program.methods.size == 1) {
      val result = executeInterprocedural()
      result.getResult(result.identifiers.head)
    } else {
      throw new RuntimeException("Cannot return one CfgResult for multiple methods. Use executeInterprocedural().")
    }
  }
}

trait InterproceduralSilverBackwardInterpreter[S <: State[S]]
  extends SilverBackwardInterpreter[S]
    with LazyLogging with InterprocHelpers[S] {

  import InterproceduralSilverInterpreter.ReturnPrefix

  def executeInterprocedural(): ProgramResult[S] = {
    // execute the interpreter starting with all "main"-methods
    super.execute(program.methods.filter(m => mainMethods.contains(m.name)).map(_.body))
    programResult
  }

  override def initial(cfg: SampleCfg): S = {
    builder.build(program, findMethod(BlockPosition(cfg.exit.get, 0)))
  }

  override def cfg(blockPosition: WorklistElement): SampleCfg = {
    findMethod(blockPosition).body
  }

  override protected def outEdges(current: WorklistElement, cfgResult: CfgResultsType[S]): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
    def createMethodReturnEdges(): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
      lazy val method = findMethod(current)
      val currentCfg = cfg(current)
      if (currentCfg.exit.isDefined && currentCfg.exit.get == current.pos.block // only add edges for exit-blocks
        && callsInProgram.contains(method.name)) {
        current match {
          case TaggedWorklistElement(callString, _, _) =>
            val initialState = initial(currentCfg)
            val bottom = initialState.bottom()
            if (methodTransferStates contains(callString, method.name))
              Seq(Right(MethodCallEdge(methodTransferStates((callString, method.name)))))
            else
              Seq(Right(MethodCallEdge(bottom)))
        }
      } else {
        Nil
      }
    }

    current.pos match {
      case BlockPosition(_, i) if i == lastIndex(current.pos) => super.outEdges(current, cfgResult) ++ createMethodReturnEdges()
      case _ => super.outEdges(current, cfgResult)
    }
  }

  override protected def executeStatement(current: WorklistElement, statement: Statement, state: S, worklist: InterpreterWorklist, programResult: CfgResultsType[S]): S = statement match {
    case call@MethodCall(_, v: Variable, _, _, _, _) =>
      //
      // prepare calling context (evaluate method targets and parameters)
      //
      val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
      val methodIdentifier = SilverIdentifier(v.getName)
      var currentState = predecessor
      val targetExpressions = for (target <- call.targets) yield {
        val (exp, st) = UtilitiesOnStates.backwardExecuteStatement(currentState, target)
        currentState = st
        exp
      }
      //
      // transfer arguments to method exit state
      //
      val methodDeclaration = findMethod(methodIdentifier)
      // create arg_# variables and assign the value to them. then remove all non arg_# variables
      for ((param, index) <- targetExpressions.zipWithIndex) {
        val exp = ExpressionSet(VariableIdentifier(ReturnPrefix + index)(param.typ))
        currentState = currentState.createVariable(exp, param.typ, DummyProgramPoint)
        currentState = currentState.assignVariable(param, exp)
      }
      val tmpVariableState = currentState.ids.toSetOrFail // let's remove them
        .filter(id => !id.getName.startsWith(ReturnPrefix))
        .foldLeft(currentState)((st, ident) => st.removeVariable(ExpressionSet(ident)))

      //Enqueue the called method for analysis and grow the callstring
      val callString = current match {
        case tagged: TaggedWorklistElement => tagged.callString :+ statement.getPC()
        case _ => Seq(statement.getPC())
      }
      //TODO @flurin tag the transfer state also with the callstring
      methodTransferStates((callString, methodIdentifier)) = tmpVariableState
      worklist.enqueue(TaggedWorklistElement(callString, BlockPosition(methodDeclaration.body.exit.get, lastIndex(methodDeclaration.body.exit.get)), false))

      //
      // if callee has been analyzed, merge results back into our state
      // (otherwise currentState.command() will return bottom (which is valid until the called method is analyzed))
      //
      val entryState = programResult(current, methodDeclaration.body).entryState()

      // current state now holds the previous state with evaluated assignments of the targets
      // we can safely remove arg_ now
      currentState = currentState.ids.toSetOrFail
        .filter(_.getName.startsWith(ReturnPrefix))
        .foldLeft(currentState)((st, ident) => st.removeVariable(ExpressionSet(ident)))

      var st = currentState
      val parameterExpressions = for (parameter <- call.parameters) yield {
        st = parameter.backwardSemantics[S](st)
        st.expr
      }
      val resultState = currentState.command(CallMethodBackwardsCommand(methodDeclaration, call, parameterExpressions, entryState))
      logger.trace(predecessor.toString)
      logger.trace(statement.toString)
      logger.trace(resultState.toString)
      resultState
    case _ => super.executeStatement(current, statement, state, worklist, programResult)
  }


  /**
    * Is called everytime the entry block of a CFG was executed
    *
    * @param current  The Block that was interpreted last
    * @param worklist The interpreters worklist
    */
  override protected def onEntryBlockExecuted(current: WorklistElement, worklist: InterpreterWorklist): Unit = {
    /**
      * In the context insensitive analysis everytime a method has been analysed we enqueue all call locations
      * of this method. This way the new analysis results will be merged into the caller state.
      */
    val method = findMethod(current)
    callsInProgram(method.name)
      //.filter(b => methodTransferStates((current.asInstanceOf[TaggedWorklistElement].callString, method.name)) //TODO @flurin
      // only enqueue blocks that have been analysed before
      //  .contains(b.block.elements(b.index).merge.getPC())
      //)
      .foreach(b => {
      current match {
        case TaggedWorklistElement(callString, _, _) if callString.length > 0 => // TODO @flurin do not enqueue for empty callstrings. right?
          worklist.enqueue(TaggedWorklistElement(callString.reverse.drop(1).reverse, b, true))
        case _ =>
      }
    })
  }

  override def getSuccessorState(cfgResult: CfgResult[S], current: WorklistElement, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    case Right(edge: MethodReturnEdge[S]) =>
      val exitContext = edge.exitState
      val methodDeclaration = findMethod(current)
      val tmpReturns = for ((retVar, index) <- methodDeclaration.returns.zipWithIndex) yield {
        ExpressionSet(VariableIdentifier(ReturnPrefix + index)(retVar.typ))
      }
      var inputState = bottom(methodDeclaration.body) lub exitContext
      // assign the methods actual returns to the temporary returns and remove the temp variables
      inputState = tmpReturns.zip(methodDeclaration.returns).foldLeft(inputState)((st, tuple) => st.assignVariable(tuple._1, ExpressionSet(tuple._2.variable.id)))
      tmpReturns.foldLeft(inputState)((st, tmpRet) => st.removeVariable(tmpRet))
    case _ => super.getSuccessorState(cfgResult, current, edge)
  }
}

case class FinalResultInterproceduralBackwardInterpreter[S <: State[S]](
                                                                         override val program: SilverProgramDeclaration,
                                                                         override val mainMethods: Set[SilverIdentifier],
                                                                         override val builder: SilverEntryStateBuilder[S],
                                                                         override val callsInProgram: CallGraphMap)
  extends InterproceduralSilverBackwardInterpreter[S] {

  //
  // Store all CfgResults inside the ProgramResult and return a CfgResultMapType to let the intraprocedural
  // interpreter do its work. Note: the interprocedural interpreter initializes ALL methods and not only those passed in
  // using "cfgs". (This is needed to initialize all callees too)
  //
  override protected def initializeProgramResult(cfgs: Seq[SampleCfg]): CfgResultsType[S] = {
    // initialize each CfgResult with its bottom state.
    programResult.initialize(c => {
      val stForCfg = bottom(c)
      initializeResult(c, stForCfg)
    })

    def lookup(res: Map[SampleCfg, CfgResult[S]])(current: WorklistElement, cfg: SampleCfg) = res(cfg)

    lookup((for (method <- program.methods) yield {
      method.body -> programResult.getResult(method.name)
    }).toMap)
  }

  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val cfgResult = FinalCfgResult[S](cfg)
    cfgResult.initialize(state)
    cfgResult
  }

  // The entrypoint for an interprocedural analysis is executeInterprocedural()
  // For programs with multiple methods calling execute() cannot work as expected because it's not clear
  // which CfgResult should be returned.
  override def execute(): CfgResult[S] = {
    if (program.methods.size == 1) {
      val result = executeInterprocedural()
      result.getResult(result.identifiers.head)
    } else {
      throw new RuntimeException("Cannot return one CfgResult for multiple methods. Use executeInterprocedural().")
    }
  }
}