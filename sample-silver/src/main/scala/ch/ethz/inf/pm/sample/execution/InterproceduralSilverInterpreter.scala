/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.InterproceduralSilverInterpreter.{CallGraphMap, MethodTransferStatesMap}
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
  type MethodTransferStatesMap[S] = mutable.Map[CallString, S]

  /**
    * The CallGraphMap maps each method(SilverIdentifiers) to a set of it's callees.
    * "foo" -> Set(BlockPosition(block, index))
    * Would mean that the statement "index" of block "block" is a method call. The statement "index+1" would be
    * the next instruction after the method call.
    */
  type CallGraphMap = Map[SilverIdentifier, Set[BlockPosition]]

  /**
    * Prefix to be used for temporary method arguments.
    */
  val ArgumentPrefix = "arg_#"

  /**
    * Prefix to be used for temporary method returns.
    */
  val ReturnPrefix = "ret_#"
}

/**
  * A CallString represents the stack of method calls during the interprocedural analysis.
  *
  * Entering a callee will push() on the call-string.
  * Leaving a callee will pop() on the call-string.
  *
  * Using the suffix() method it is possible to shorten a call-string and therefore approximate the analysis result.
  * see CallString.FullPrecision, CallString.ContextInsensitive and CallString.approximate
  */
trait CallString {
  /**
    * Represents the position of the last method call
    *
    * @return
    */
  def lastCaller: ProgramPoint

  /**
    * Returns the ProgramPoint of the current method that this call-string calls into
    * There is no currentMethod for CallString.Empty
    *
    * @return
    */
  def currentMethod: ProgramPoint

  /**
    * Shrink the call-string. This represents a return from a callee.
    *
    * @return
    */
  def pop: CallString

  /**
    * Grow the call-string according to the given methodCall
    *
    * @param callee   The called Method
    * @param callStmt The MethodCall statement that causes the call-string to grow
    * @return a new call-string that represents this stack of calls
    */
  def push(callee: SilverMethodDeclaration, callStmt: MethodCall): CallString

  /**
    * Denotes that we're inside a called method
    *
    * @return True if call-string relates to a callee
    */
  def inCallee: Boolean

  /**
    * Returns a call-string shortened to the last k calls.
    *
    * @param k Max length of the shortened call-string
    * @return The shortened call-string consisting of (at most) k calls.
    */
  def suffix(k: Option[Int]): CallString
}

object CallString {

  def apply(callee: SilverMethodDeclaration, callStmt: MethodCall): CallString = SimpleCallString(List(callee.programPoint, callStmt.getPC()))

  /**
    * Represents an empty call-string
    */
  val Empty: CallString = SimpleCallString(Nil)

  /**
    * FullPrecision will analyse the whole call-string length
    */
  val FullPrecision: Option[Int] = None

  /**
    * ContextInsensitive sets the call-string length to 0
    */
  val ContextInsensitive: Option[Int] = Some(0)

  /**
    * The default (approximate) length controlled by the global system parameter
    */
  val DefaultLength: Option[Int] = Some(SystemParameters.callStringLength)

  /**
    * Returns a CallStringLength of length k
    *
    * @param k The call-string length to be used
    * @return
    */
  def approximate(k: Int): Option[Int] = Some(k)

  /**
    * The default implementation of a call-string
    * For every method call the call-string grows by two elements. The method that was called, and the location
    * that called the method. Due to this, the actual implementation works with a call-string list double the size
    * of CallStringLength. Calling suffix(k=2) can internally work with a callStack of size <= 4.
    *
    * @param callStack The stack of method calls in the call-string
    */
  private case class SimpleCallString(callStack: List[ProgramPoint] = Nil) extends CallString {

    def lastCaller: ProgramPoint = callStack.tail.head

    def currentMethod: ProgramPoint = callStack.head

    def pop: CallString = copy(callStack = callStack.tail.tail)

    def push(callee: SilverMethodDeclaration, callStmt: MethodCall): CallString = copy(callStack = callee.programPoint :: callStmt.getPC() :: callStack)

    def inCallee: Boolean = callStack.nonEmpty

    def suffix(k: Option[Int]): CallString = k match {
      case Some(length) => copy(callStack = callStack.take(2 * length))
      case _ => this
    }
  }

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


case class TaggedWorklistElement(callString: CallString,
                                 override val pos: BlockPosition,
                                 override val forceReinterpretStmt: Boolean) extends WorklistElement {

  override def createSuccessorForEnqueue(newPos: BlockPosition, newForceReinterpretStmt: Boolean): WorklistElement = {
    copy(pos = newPos, forceReinterpretStmt = newForceReinterpretStmt)
  }

}

trait InterprocHelpers[S <: State[S]] {
  //
  // "global" values passed to the interpreter from the analysis runner
  //
  val program: SilverProgramDeclaration
  val builder: SilverEntryStateBuilder[S]
  val callsInProgram: CallGraphMap
  val CallStringLength: Option[Int]
  // A main method is analyzed using "initial" as the entry state.
  val mainMethods: Set[SilverIdentifier]

  //
  // variables that track the state of the analysis. E.g. values transferred into callees or CfgResults
  //

  /** this map is used to store entry (forward) or exit (backward) states for method calls
    * executeStatement() stores the states here and in-/outEdges then uses them to create MethodCall / MethodReturn edges
    */
  val methodTransferStates: MethodTransferStatesMap[S] = mutable.Map()
  val programResult: ProgramResult[S] = DefaultProgramResult(program)
  // tracks whether the result of a CallString/Cfg combination is already available. They are if
  // the exit- (forward) / entry-block (backward) has been interpreted
  val analysisResultReady: mutable.Set[(CallString, SampleCfg)] = mutable.Set()

  //
  // helper methods to lookup Methods, BlockPositions etc.
  //
  private lazy val methods: Map[Either[String, SampleBlock], SilverMethodDeclaration] = {
    var res: Map[Either[String, SampleBlock], SilverMethodDeclaration] = Map()
    for (m <- program.methods) {
      res += (Left(m.name.name) -> m)
      for (b <- m.body.blocks)
        res += (Right(b) -> m)
    }
    res
  }

  def findMethod(current: WorklistElement): SilverMethodDeclaration = findMethod(current.pos)

  def findMethod(blockPosition: BlockPosition): SilverMethodDeclaration = {
    methods(Right(blockPosition.block))
  }

  def findMethod(name: SilverIdentifier): SilverMethodDeclaration = {
    findMethod(name.name)
  }

  def findMethod(name: String): SilverMethodDeclaration = {
    methods(Left(name))
  }

  /**
    * A map from ProgramPoint to BlockPosition
    * This allows us the enqueue a callee to the interpreter by knowing the ProgramPoint of the MethodCall.
    */
  protected lazy val calleePositions: Map[ProgramPoint, BlockPosition] = {
    (for (call <- callsInProgram.values.flatten) yield {
      call.block.elements(call.index).merge.getPC() -> call
    }).toMap
  }

  //
  // various methods shared between the forward and backward interpreters
  //

  /**
    * Return the initial/default state for the given cfg under the assumption that
    * the method is called from another method.
    *
    * @param cfg The cfg of the called method
    * @return The initial state
    */
  def initialForCallee(cfg: SampleCfg): S

  /**
    * Unifies two states
    *
    * @param state The state we merge into
    * @param other The state that is merged
    * @return a unified state of state and other
    */
  def unify(state: S, other: S): S = state.command(UnifyCommand(other))

  /**
    * Enqueue callers to the worklist.
    * A caller is enqueued if the suffix of the call-string matches the current call-string.
    *
    * @param current  the current worklist element
    * @param worklist the worklist to enqueue to
    * @param cfg      the cfg belonging to the current worklist element
    */
  def enqueueCallers(current: WorklistElement, worklist: InterpreterWorklist, cfg: SampleCfg): Unit = current match {
    /**
      * Enqueue the callee(s) of the method. To find them we look at all existing call-strings.
      * For full-length call-strings we can simply enqueue the last caller. For approximate solutions (call-string-length bounded)
      * we enqueue all callers that have the same suffix.
      */
    case TaggedWorklistElement(callString, _, _) =>
      val method = findMethod(current)
      val shortenedCallString = callString.suffix(CallStringLength)
      methodTransferStates.keys.filter(_.currentMethod == method.programPoint)
        .filter(_.suffix(CallStringLength) == shortenedCallString)
        .foreach(caller => {
          val position = calleePositions(caller.lastCaller)
          worklist.enqueue(TaggedWorklistElement(caller.pop, position, forceReinterpretStmt = true))
        })
      // mark the result as available
      analysisResultReady += ((callString, cfg))
    case _ =>
  }

  /**
    * Renames the given variables into variables named $newNamePrefix + id where id starts at zero.
    *
    * @param state         The state that should be renamed
    * @param variables     The Sequence of variables to be renamed (usually methoddeclaration.arguments or returns)
    * @param newNamePrefix The prefix for the temporary variables (see ArgumentPrefix ReturnPrefix)
    * @return The renamed state
    */
  def renameToTemporaryVariable(state: S, variables: Seq[VariableDeclaration], newNamePrefix: String): S = {
    var id = 0
    variables.foldLeft(state)((st, v) => {
      val newName = ExpressionSet(VariableIdentifier(newNamePrefix + id)(v.typ))
      id += 1
      val old = ExpressionSet(v.variable.id)
      st.createVariable(newName, v.typ, DummyProgramPoint)
        .assignVariable(newName, old)
        .removeVariable(old)
    })
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
      * If the current block is an entry-point of the cfg and this block (method) is called throughout the program
      * we'll add a MethodCallEdge. The number of edges should equal the number of calls. But it's possible that
      * methodEntryStates does not yet contain calling-contexts of all call locations. If the number of known entry states
      * is less than the number of calls, we'll add additional edges with bottom as calling context.
      *
      * @return
      */
    def createMethodCallEdges(): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
      if (cfg(current).entry == current.pos.block) { // only entry-blocks can have incoming MethodCall edges
        current match {
          case TaggedWorklistElement(callString, _, _) =>
            lazy val method = findMethod(current)
            lazy val currentCallStringShortened = callString.suffix(CallStringLength)
            // create an edge for each call string with the same k-length suffix
            val inEdgesBySuffix = methodTransferStates.keys.filter(_.currentMethod == method.programPoint)
              .filter(_.suffix(CallStringLength) == currentCallStringShortened)
              .map(st => Right(MethodCallEdge(methodTransferStates(st))))
              .toSeq
            inEdgesBySuffix
          case _ => Nil
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

  override protected def onExitBlockExecuted(current: WorklistElement): Unit = enqueueCallers(current, worklist, cfg(current))

  override def initial(cfg: SampleCfg): S = {
    builder.build(program, findMethod(BlockPosition(cfg.entry, 0)))
  }

  override def initialForCallee(cfg: SampleCfg): S = {
    builder.buildForMethodCallEntry(program, findMethod(BlockPosition(cfg.entry, 0)))
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
      var inputState = unify(initialForCallee(methodDeclaration.body), callingContext)
      // assign (temporary) arguments to parameters
      inputState = methodDeclaration.arguments.zip(tmpArguments).foldLeft(inputState)((st, tuple) => st.assignVariable(ExpressionSet(tuple._1.variable.id), tuple._2))
      inputState.ids.toSetOrFail.filter(_.getName.startsWith(ArgumentPrefix)).foldLeft(inputState)((st, ident) => st.removeVariable(ExpressionSet(ident)))
    case _ => super.getPredecessorState(cfgResult, current, edge)
  }

  override protected def executeStatement(current: WorklistElement, statement: Statement, state: S, programResult: CfgResultsType[S]): (S, Boolean) = statement match {
    case call@MethodCall(_, v: Variable, _, _, _, _) =>
      val methodIdentifier = SilverIdentifier(v.getName)
      val methodDeclaration = findMethod(methodIdentifier)
      //
      // prepare calling context (evaluate method targets and parameters)
      //
      val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
      var methodCallStateInCaller = predecessor
      val parameterExpressions = for (parameter <- call.parameters) yield {
        methodCallStateInCaller = parameter.forwardSemantics[S](methodCallStateInCaller)
        methodCallStateInCaller.expr
      }
      val targetExpressions = for (target <- call.targets) yield {
        val (exp, st) = UtilitiesOnStates.forwardExecuteStatement(methodCallStateInCaller, target)
        methodCallStateInCaller = st
        exp
      }

      //
      // transfer arguments to methodEntryState
      //

      // create temp argument variables and assign the arguments to them
      for ((param, index) <- parameterExpressions.zipWithIndex) {
        val exp = ExpressionSet(VariableIdentifier(ArgumentPrefix + index)(param.typ))
        methodCallStateInCaller = methodCallStateInCaller.createVariable(exp, param.typ, DummyProgramPoint)
        methodCallStateInCaller = methodCallStateInCaller.assignVariable(exp, param)
      }
      val calleeEntryState = methodCallStateInCaller.ids.toSetOrFail // The entry state should only contain argument_# variables
        .filter(id => !id.getName.startsWith(ArgumentPrefix))
        .foldLeft(methodCallStateInCaller)((st, ident) => st.removeVariable(ExpressionSet(ident)))

      //Enqueue the called method for analysis and grow the call-string
      val callString = current match {
        case tagged: TaggedWorklistElement => tagged.callString.push(methodDeclaration, call)
        case _ => CallString(methodDeclaration, call)
      }
      val old = if (methodTransferStates contains callString) methodTransferStates(callString) else calleeEntryState.bottom()
      methodTransferStates(callString) = calleeEntryState
      val callStringInCallee = callString.suffix(CallStringLength)
      if (!(calleeEntryState lessEqual old)) {
        worklist.enqueue(TaggedWorklistElement(callStringInCallee, BlockPosition(methodDeclaration.body.entry, 0), forceReinterpretStmt = false))
      }

      //
      // if callee has been analyzed, merge results back into our state
      // otherwise return bottom since the analysis cannot continue without the result of the callee (yet)
      //
      val analyzed = TaggedWorklistElement(callStringInCallee, null, forceReinterpretStmt = false)
      val exitState = programResult(analyzed, methodDeclaration.body).exitState()
      val canContinue = analysisResultReady.contains((callStringInCallee, methodDeclaration.body))

      val resultState = if (canContinue) {
        // rename the methods arguments to temporary variables to relate method call state to exit state of the calle
        val renamed = renameToTemporaryVariable(exitState, methodDeclaration.arguments, ArgumentPrefix)
        // merge the renamed exit state into the state of the caller and remove all temporary variables
        methodCallStateInCaller.command(ReturnFromMethodCommand(methodDeclaration, call, targetExpressions, renamed))
      } else {
        methodCallStateInCaller.bottom()
      }

      logger.trace(predecessor.toString)
      logger.trace(statement.toString)
      logger.trace(resultState.toString)

      (resultState, canContinue)
    case _ => super.executeStatement(current, statement, state, programResult)
  }

}

/**
  * Forward interpreter that handles method calls using a call-string approach.
  *
  * @param program          The program that is analysed
  * @param mainMethods      A set of methods that should be treated as main-methos (i.e. use initial as entry state)
  * @param builder          A builder to create initial states for each cfg to analyse
  * @param callsInProgram   The call graph of the program
  * @param CallStringLength an optional upper bound for the call-string length
  * @tparam S The type of the states.
  */
case class FinalResultInterproceduralForwardInterpreter[S <: State[S]](
                                                                        override val program: SilverProgramDeclaration,
                                                                        override val mainMethods: Set[SilverIdentifier],
                                                                        override val builder: SilverEntryStateBuilder[S],
                                                                        override val callsInProgram: CallGraphMap,
                                                                        override val CallStringLength: Option[Int] = CallString.DefaultLength)
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
          res += ((callString, cfg) -> initializeResult(cfg, bottom(cfg)))
        res((callString, cfg))
      case _ =>
        res((CallString.Empty, cfg))
    }

    lookup(mutable.Map((for (method <- program.methods) yield {
      (CallString.Empty, method.body) -> programResult.getResult(method.name)
    }): _*))
  }

  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = {
    val cfgResult = FinalCfgResult[S](cfg)
    cfgResult.initialize(state)
    cfgResult
  }

  // The entry-point for an interprocedural analysis is executeInterprocedural()
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

  override def initialForCallee(cfg: SampleCfg): S = {
    builder.buildForMethodCallEntry(program, findMethod(BlockPosition(cfg.exit.get, 0)))
  }

  override def cfg(blockPosition: WorklistElement): SampleCfg = {
    findMethod(blockPosition).body
  }

  override protected def outEdges(current: WorklistElement, cfgResult: CfgResultsType[S]): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
    def createMethodReturnEdges(): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
      val currentCfg = cfg(current)
      if (currentCfg.exit.isDefined && currentCfg.exit.get == current.pos.block) {
        // only add edges for exit-blocks
        current match {
          case TaggedWorklistElement(callString, _, _) =>
            lazy val method = findMethod(current)
            lazy val currentCallStringShortened = callString.suffix(CallStringLength)
            // create an edge for each call string with the same k-length suffix
            val outEdgesBySuffix = methodTransferStates.keys.filter(_.currentMethod == method.programPoint)
              .filter(_.suffix(CallStringLength) == currentCallStringShortened)
              .map(st => Right(MethodReturnEdge(methodTransferStates(st))))
              .toSeq
            outEdgesBySuffix
          case _ => Nil
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

  override protected def executeStatement(current: WorklistElement, statement: Statement, state: S, programResult: CfgResultsType[S]): (S, Boolean) = statement match {
    case call@MethodCall(_, v: Variable, _, _, _, _) =>
      val methodIdentifier = SilverIdentifier(v.getName)
      val methodDeclaration = findMethod(methodIdentifier)
      //
      // prepare calling context (evaluate method targets and parameters)
      //
      val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
      var stateInCaller = predecessor
      val targetExpressions = for (target <- call.targets) yield {
        val (exp, st) = UtilitiesOnStates.backwardExecuteStatement(stateInCaller, target)
        stateInCaller = st
        exp
      }
      //
      // transfer arguments to method exit state
      //
      // create ret_# variables and assign the value to them.
      for ((param, index) <- targetExpressions.zipWithIndex) {
        val exp = ExpressionSet(VariableIdentifier(ReturnPrefix + index)(param.typ))
        stateInCaller = stateInCaller.createVariable(exp, param.typ, DummyProgramPoint)
        stateInCaller = stateInCaller.assignVariable(param, exp)
        exp
      }
      // only transfer temporary ret_# variables into the callee. remove everything else
      val calleeExitState = stateInCaller.ids.toSetOrFail
        .filter(id => !id.getName.startsWith(ReturnPrefix))
        .foldLeft(stateInCaller)((st, ident) => st.removeVariable(ExpressionSet(ident)))

      //Enqueue the called method for analysis and grow the call-string
      val callString = current match {
        case tagged: TaggedWorklistElement => tagged.callString.push(methodDeclaration, call)
        case _ => CallString(methodDeclaration, call)
      }
      val old = if (methodTransferStates contains callString) methodTransferStates(callString) else calleeExitState.bottom()
      methodTransferStates(callString) = calleeExitState
      val callStringInCallee = callString.suffix(CallStringLength)
      if (!(calleeExitState lessEqual old)) {
        worklist.enqueue(TaggedWorklistElement(callStringInCallee, BlockPosition(methodDeclaration.body.exit.get, lastIndex(methodDeclaration.body.exit.get)), forceReinterpretStmt = false))
      }

      //
      // if callee has been analyzed, merge results back into our state
      // otherwise return bottom since the analysis cannot continue without the result of the callee (yet)
      //
      val analyzed = TaggedWorklistElement(callStringInCallee, null, forceReinterpretStmt = false)
      val entryState = programResult(analyzed, methodDeclaration.body).entryState()
      val canContinue = analysisResultReady.contains((callStringInCallee, methodDeclaration.body))

      var st = stateInCaller
      val parameterExpressions = for (parameter <- call.parameters) yield {
        st = parameter.backwardSemantics[S](st)
        st.expr
      }
      val resultState = if (canContinue) {
        // rename the state such that the callee's formal return variables are named return_#1,2,3,4 etc.
        val renamed = renameToTemporaryVariable(entryState, methodDeclaration.returns, ReturnPrefix)
        // merge the (renamed) callee entry state into our caller state and then remove all temporary ret_ or arg_ variables
        // the renaming is necessary for relational domains to keep the relationship between callee arguments and returns
        stateInCaller.command(CallMethodBackwardsCommand(methodDeclaration, call, parameterExpressions, renamed))
      } else {
        stateInCaller.bottom()
      }
      logger.trace(predecessor.toString)
      logger.trace(statement.toString)
      logger.trace(resultState.toString)
      (resultState, canContinue)
    case _ => super.executeStatement(current, statement, state, programResult)
  }


  /**
    * Is called every time the entry block of a CFG was executed
    *
    * @param current  The Block that was interpreted last
    */
  override protected def onEntryBlockExecuted(current: WorklistElement): Unit = enqueueCallers(current, worklist, cfg(current))

  override def getSuccessorState(cfgResult: CfgResult[S], current: WorklistElement, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    case Right(edge: MethodReturnEdge[S]) =>
      val exitContext = edge.exitState
      val methodDeclaration = findMethod(current)
      val tmpReturns = for ((retVar, index) <- methodDeclaration.returns.zipWithIndex) yield {
        ExpressionSet(VariableIdentifier(ReturnPrefix + index)(retVar.typ))
      }
      var inputState = unify(initialForCallee(methodDeclaration.body), exitContext)
      // assign the methods actual returns to the temporary returns and then remove the temporary variables
      inputState = tmpReturns.zip(methodDeclaration.returns).foldLeft(inputState)((st, tuple) => st.assignVariable(tuple._1, ExpressionSet(tuple._2.variable.id)))
      inputState.ids.toSetOrFail.filter(_.getName.startsWith(ReturnPrefix)).foldLeft(inputState)((st, ident) => st.removeVariable(ExpressionSet(ident)))
    case _ => super.getSuccessorState(cfgResult, current, edge)
  }
}

/**
  * Backward interpreter that handles method calls using a call-string approach.
  *
  * @param program          The program that is analysed
  * @param mainMethods      A set of methods that should be treated as main-methods (i.e. use initial as entry state)
  * @param builder          A builder to create initial states for each cfg to analyse
  * @param callsInProgram   The call graph of the program
  * @param CallStringLength an optional upper bound for the call-string length
  * @tparam S The type of the states.
  */
case class FinalResultInterproceduralBackwardInterpreter[S <: State[S]](
                                                                         override val program: SilverProgramDeclaration,
                                                                         override val mainMethods: Set[SilverIdentifier],
                                                                         override val builder: SilverEntryStateBuilder[S],
                                                                         override val callsInProgram: CallGraphMap,
                                                                         override val CallStringLength: Option[Int] = CallString.DefaultLength)
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

  // The entry-point for an interprocedural analysis is executeInterprocedural()
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