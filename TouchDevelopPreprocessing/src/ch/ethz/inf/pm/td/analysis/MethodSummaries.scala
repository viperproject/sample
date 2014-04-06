package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.TUnknown
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.td.domain.MultiValExpression
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.sample.AnalysisUnitContext
import ch.ethz.inf.pm.sample.execution.DefaultCFGStateFactory
import ch.ethz.inf.pm.td.compiler.TouchMethodIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.ProgramPointScopeIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.backwardanalysis.{DefaultRefiningBackwardInterpreter, InCFGInitialState}

/** Summarizes abstract semantics of an analyzed method */
case class MethodSummary[S <: State[S]](pp: ProgramPoint, method: MethodDeclaration,
                                        cfgState: DefaultCFGState[S])

/** Used to keep track of the current CFG location during forward interpretation */
case class ForwardInterpreterEventHandler[S <: State[S]]() extends InterpreterEventHandler[S] {
  var currentPos: Option[CFGPosition] = None

  override def beforeCFGStatement(stmt: Statement, cfgPos: CFGPosition, preState: S): S = {
    currentPos = Some(cfgPos)
    preState
  }
}

/**
 * Stores summaries of methods. This is not thread-safe.
 */
object MethodSummaries {

  /**
   * Stores the summaries of called methods, program point specific
   */
  private var summaries: Map[ProgramPoint, MethodSummary[_]] = Map.empty

  /**
   * Stores the entry states of a method that is currently on the stack (for recursive calls)
   */
  private var entriesOnStack:Map[ProgramPoint,_] = Map.empty

  val forwardInterpreterEventHandler = ForwardInterpreterEventHandler()

  def currentForwardPos: Option[CFGPosition] = forwardInterpreterEventHandler.currentPos

  /**
   * Stores the entry state of a closure method
   */
  private var closureEntries:Map[String,State[_]] = Map.empty

  /**
   * Stores exit states at positions which end the script prematurely
   */
  private var abnormalExits:Option[State[_]] = None

  /**
   * Stores the summaries with refined backward states
   */
  private var backwardSummaries: Map[ProgramPoint, MethodSummary[_]] = Map.empty

  /**
   * Store exit states of methods currently on the stack (detection
   * of recursive calls, backward version)
   */
  private var exitsOnStack:Map[ProgramPoint,_] = Map.empty

  /** Casting helper for summaries. Remove once MethodSummaries is not a singleton  anymore... */
  def summaryFor[S <: State[S]](pp: ProgramPoint): MethodSummary[S] = summaries(pp).asInstanceOf[MethodSummary[S]]

  /** Casting helper for backward summaries. Remove once MethodSummaries is not a singleton  anymore... */
  def backwardSummaryFor[S <: State[S]](pp: ProgramPoint): MethodSummary[S] = backwardSummaries(pp).asInstanceOf[MethodSummary[S]]

  /**
   *
   * This updates the summary of a method. If our current summary is not general enough,
   * this will reanalyze the method.
   *
   * @param callPoint The program point at which the method is called
   * @param callTarget The declaration of the method that is called
   * @param entryState The state when entering the method
   * @tparam S Our current abstract domain
   * @return The exit state of the method
   */
  def collect[S <: State[S]](callPoint: ProgramPoint, callTarget: MethodDeclaration, entryState: S,
                             parameters: List[ExpressionSet], localHandlerScope: Option[S] = None): S = {
    val identifyingPP =
      if(TouchAnalysisParameters.contextSensitiveInterproceduralAnalysis) callPoint
      else callTarget.programpoint

    var enteredState = enterFunction(callPoint,callTarget,entryState,parameters)

    /**
     * If this is a closure, we may get local variable from the local scope of closure creation
     */
    enteredState = localHandlerScope match {
      case Some(x) =>
        enteredState.lub(x)
      case None => enteredState
    }

    val result = entriesOnStack.get(identifyingPP) match {
      case Some(oldEntryState) =>

        // This is a recursive call (non top level).
        // Join the entry state and continue with previously recorded
        // exit + entryState (updates inside recursive calls are weak)
        val newEntryState = oldEntryState.asInstanceOf[S].widening(enteredState)
        entriesOnStack += ((identifyingPP,newEntryState))
        summaries.get(identifyingPP) match {
          case Some(s) =>

            // Get the result of the previous recursion depth and join it with the local state
            val summary = s.asInstanceOf[MethodSummary[S]]
            val prevExitState = summary.cfgState.exitState()
            val exitedState = exitFunction(callPoint,callTarget,prevExitState,parameters)
            val localState = pruneGlobalState(entryState)
            exitedState.lub(localState)

          case None =>

            // We do not have a result for our recursion yet. Bottom.
            entryState.bottom()

        }

      case None =>

        // This is a top-level call (recursive or non-recursive)
        // Record the effect of one iteration

        entriesOnStack += ((identifyingPP,enteredState))

        var currentSummary = summaries.get(identifyingPP) match {
          case Some(prevSummary) =>
            val prev = prevSummary.asInstanceOf[MethodSummary[S]]
            executeMethod(enteredState, prev)
          case None =>
            val factoryState = enteredState.factory()
            val cfgState = DefaultCFGStateFactory(factoryState).allBottom(callTarget.body)
            val prevSummary = new MethodSummary(identifyingPP, callTarget, cfgState)
            executeMethod(enteredState, prevSummary)
        }

        summaries += ((identifyingPP, currentSummary))

        // Are there more possible depths?
        while (!entriesOnStack.get(identifyingPP).get.asInstanceOf[S].removeExpression().lessEqual(enteredState.removeExpression())) {
          enteredState = entriesOnStack.get(identifyingPP).get.asInstanceOf[S]
          currentSummary = executeMethod(enteredState, currentSummary)
          summaries += ((identifyingPP, currentSummary))
        }

        entriesOnStack = entriesOnStack - identifyingPP

        val exitState = exitFunction(callPoint,callTarget,currentSummary.cfgState.exitState(),parameters)
        val localState = pruneGlobalState(entryState)

        localState.lub(exitState)
    }

    result
  }

  /**
   * Collect the exit state of a stopped script somewhere in the middle of the script, due to
   */
  def collectExit[S <: State[S]](exitState:S):S = {

    var curState = exitState.removeExpression()

    curState = curState.pruneVariables({
      case id:VariableIdentifier =>
        !id.typ.asInstanceOf[TouchType].isSingleton &&
          !CFGGenerator.isGlobalReferenceIdent(id.toString)
      case _ => false
    })
    curState = curState.pruneUnreachableHeap()

    abnormalExits = abnormalExits match {
      case Some(x) => Some(x.asInstanceOf[S].widening(curState))
      case None => Some(curState)
    }

    curState.bottom()

  }


  def collectClosureEntry[S <: State[S]](handlerName:String,entryState:S) = {

    closureEntries += (handlerName ->
      (closureEntries.get(handlerName) match {
        case None => entryState
        case Some(x) => entryState.lub(x.asInstanceOf[S])
      })
      )

  }

  def getClosureEntry[S <: State[S]](handlerName:String):Option[S] = {
    closureEntries.get(handlerName) match {
      case None => None
      case Some(x) => Some(x.asInstanceOf[S])
    }
  }

  def joinAbnormalExits[S <: State[S]](s:S):S = {

    abnormalExits match {
      case None => s
      case Some(x) => s.lub(x.asInstanceOf[S])
    }

  }

  def resetBackward[S <: State[S]]() {
    backwardSummaries = Map.empty
    exitsOnStack = Map.empty[ProgramPoint,S]
  }


  def reset[S <: State[S]]() {
    summaries = Map.empty[ProgramPoint,MethodSummary[S]]
    entriesOnStack = Map.empty[ProgramPoint,S]
    closureEntries = Map.empty
    abnormalExits = None
    resetBackward()
  }

  def getSummaries[S <: State[S]] = summaries.asInstanceOf[Map[ProgramPoint,MethodSummary[S]]]

  def executeMethod[S <: State[S]](entryState:S, currentSummary: MethodSummary[S]): MethodSummary[S] = {
    val methodDecl = currentSummary.method
    val newState: DefaultCFGState[S] =
      SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(methodDecl)) {
        val forwardInterpreter = new DefaultForwardInterpreter(entryState.factory(), forwardInterpreterEventHandler.asInstanceOf[ForwardInterpreterEventHandler[S]])
        forwardInterpreter.forwardExecuteWithCFGState(methodDecl.body, currentSummary.cfgState, entryState)
      }
    currentSummary.copy(cfgState = newState)
  }

  def executeMethodBackward[S <: State[S]](exitState: S, currentBackwardSummary: MethodSummary[S],
                                          forwardSummary: MethodSummary[S]): MethodSummary[S] = {
    val methodToExecute = currentBackwardSummary.method

    val newCfgState: DefaultCFGState[S] =
      SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(methodToExecute)) {
        val interpreter = new DefaultRefiningBackwardInterpreter[S](exitState.factory())
        val backwardContext = SystemParameters.backwardContext
        backwardContext.currentError match {
          case Some(err) if err.method == methodToExecute =>
            val inCFGErrorState = InCFGInitialState[S](err.errorState, err.cfgPosition)
            interpreter.executeWithInCFGError(methodToExecute.body, forwardSummary.cfgState,
              refinedBackwardStates = currentBackwardSummary.cfgState, exitState, inCFGErrorState)
          case _ =>
            interpreter.executeFromExit(methodToExecute.body, forwardSummary.cfgState,
              refinedBackwardStates = currentBackwardSummary.cfgState, exitState)
        }
      }

    currentBackwardSummary.copy(cfgState = newCfgState)
  }

  private def pruneGlobalState[S <: State[S]](entryState:S):S = {
    var curState = entryState
    curState = curState.pruneVariables({
      case id:VariableIdentifier =>
        id.typ.asInstanceOf[TouchType].isSingleton ||
          CFGGenerator.isGlobalReferenceIdent(id.toString) ||
          CFGGenerator.isParamIdent(id.toString) ||
          CFGGenerator.isReturnIdent(id.toString)
      case _ => false
    })

    curState = curState.pruneUnreachableHeap()
    curState = curState.setExpression(curState.expr.bottom())

    curState
  }

  def enterFunction[S <: State[S]](callPoint:ProgramPoint, callTarget: MethodDeclaration, entryState: S,
                                   parameters: List[ExpressionSet]): S = {
    var curState = entryState

    if (parameters.length == callTarget.arguments.apply(0).length) {

      val inParameters = callTarget.arguments(0)

      // Initialize in-parameters to temporary variables
      val tempVars = for ((decl,value) <- inParameters.zip(parameters)) yield {
        val tempVar = VariableIdentifier(CFGGenerator.paramIdent(decl.variable.id.toString), ProgramPointScopeIdentifier(callTarget.programpoint))(decl.typ, callPoint)
        val expr = ExpressionSet(tempVar)
        curState = curState.createVariable(expr, tempVar.typ, callTarget.programpoint)
        curState = curState.assignVariable(expr,value)
        tempVar
      }

      // Prune non-parameters and non-globals (reach. based localization)
      if (TouchAnalysisParameters.localizeStateOnMethodCall) {
        curState = curState.pruneVariables({
          case id:VariableIdentifier =>
            !id.typ.asInstanceOf[TouchType].isSingleton &&
            !CFGGenerator.isGlobalReferenceIdent(id.toString) &&
            !CFGGenerator.isParamIdent(id.toString)
          case _ => false
        })
      }

      // Initialize in-parameters to temp vars
      for ((decl,value) <- inParameters.zip(tempVars)) {
        val variable = decl.variable.id
        val expr = ExpressionSet(variable)
        curState = curState.createVariable(expr, variable.typ, callTarget.programpoint)
        curState = curState.assignVariable(expr, ExpressionSet(value))
      }

      // Prune temporary variables
      curState = curState.pruneVariables({
        case id:VariableIdentifier => CFGGenerator.isParamIdent(id.toString)
        case _ => false
      })

      // Prune unreachable heap locations
      curState = curState.pruneUnreachableHeap()

    } else {

      // Prune local state
      curState = curState.pruneVariables({
        case id:VariableIdentifier =>
          !id.typ.asInstanceOf[TouchType].isSingleton &&
          !CFGGenerator.isGlobalReferenceIdent(id.toString)
        case _ => false
      })
      curState = curState.pruneUnreachableHeap()

      // Initialize in-parameters to top
      callTarget.arguments.apply(0).foreach({
        x:VariableDeclaration =>
          if(TouchAnalysisParameters.argumentsToPublicMethodsValid || callTarget.name.asInstanceOf[TouchMethodIdentifier].isEvent) {
            curState = Top[S](x.typ.asInstanceOf[TouchType])(curState,x.programpoint)
          } else {
            curState = TopWithInvalid[S](x.typ.asInstanceOf[TouchType])(curState,x.programpoint)
          }
          val right = curState.expr
          val expr = toExpressionSet(x.variable.id)
          curState = curState.createVariable(expr,expr.getType(),x.programpoint)
          curState = curState.assignVariable(expr,right)
      })

    }

    // Initialize out-parameters to invalid
    callTarget.arguments.apply(1).foreach({
      x:VariableDeclaration =>
        val expr = toExpressionSet(x.variable.id)
        curState = curState.createVariable(expr,expr.getType(),callTarget.programpoint)
        curState = curState.assignVariable(expr,Invalid(x.typ)(x.programpoint))
    })

    curState

  }

  private def exitFunctionLocalVarFilter(callTarget: MethodDeclaration): Identifier => Boolean = {
    case id:VariableIdentifier =>
      // Belongs to scope of call target
      id.scope == ProgramPointScopeIdentifier(callTarget.programpoint) &&
        // Is not a return value
        !CFGGenerator.isReturnIdent(id.toString)
    case _ => false
  }

  private def exitFunction[S <: State[S]](callPoint: ProgramPoint, callTarget: MethodDeclaration, entryState: S,
                                          parameters: List[ExpressionSet]): S = {

    val outParameters = callTarget.arguments(1)
    var curState = entryState

    // Store returns in temporary variables
    val tempVars = for (outParam <- outParameters) yield {
      val tempVar = VariableIdentifier(CFGGenerator.returnIdent(outParam.variable.getName), ProgramPointScopeIdentifier(callTarget.programpoint))(outParam.typ, callPoint)
      val tempVarExpr = ExpressionSet(tempVar)
      curState = curState.createVariable(tempVarExpr, tempVarExpr.getType(), callTarget.programpoint)
      curState = curState.assignVariable(tempVarExpr, ExpressionSet(outParam.variable.id))
      tempVar
    }

    def buildMultiVal(tempVars:List[VariableIdentifier]): Expression = tempVars match {
      case x :: Nil => x
      case x :: xs => MultiValExpression(x,buildMultiVal(xs),TUnknown.typ.top())
    }

    val returnExpr = if (tempVars.isEmpty) ExpressionFactory.unitExpr else ExpressionSet(buildMultiVal(tempVars))
    curState = curState.setExpression(returnExpr)

    // Prune local state (except return values)
    curState = curState.pruneVariables(exitFunctionLocalVarFilter(callTarget))
    curState = curState.pruneUnreachableHeap()

    curState

  }

  /**
   * Does backward analysis on called method, uses summaries when possible
   * and generalize them if deemed necessary.
   *
   * This is fairly symmetric to the forward collect operation.
   */
  def backwardCollect[S <: State[S]](callPoint: ProgramPoint, callTarget: MethodDeclaration, postState: S,
                                     parameters: List[ExpressionSet]): S = {
    val identifyingPP =
      if(TouchAnalysisParameters.contextSensitiveInterproceduralAnalysis) callPoint
      else callTarget.programpoint

    summaries.get(identifyingPP) match {
      case Some(forwardSummary: MethodSummary[S]) =>

        var beforeExitState = backwardExitFunction(callPoint, callTarget,
          forwardSummary.cfgState.exitState(), postState)

        val result = exitsOnStack.get(identifyingPP) match {
          case Some(oldExitState) =>
            // Recursive call

            val newExitState = oldExitState.asInstanceOf[S].widening(beforeExitState)
            exitsOnStack += ((identifyingPP,newExitState))

            backwardSummaries.get(identifyingPP) match {
              case Some(s) =>

                // Get the result of the previous recursion depth and join it with the local state
                val summary = s.asInstanceOf[MethodSummary[S]]
                val enteredState = summary.cfgState.entryState()
                val forwardAfterEntered = forwardSummary.cfgState.entryState()
                val beforeEnteredState = backwardEnterFunction(callPoint, callTarget, forwardAfterEntered, enteredState, parameters)
                val localState = pruneGlobalState(postState)
                beforeEnteredState lub localState

              case None =>
                postState.bottom()
            }

          case None =>
            // Top-level call

            exitsOnStack += ((identifyingPP, beforeExitState))

            var currentBackwardSummary: MethodSummary[S] =
              backwardSummaries.get(identifyingPP) match {
                case Some(backwardSummary: MethodSummary[S]) =>
                  executeMethodBackward[S](beforeExitState, backwardSummary, forwardSummary)

                case None =>
                  val factoryState = postState.factory()
                  val cfgState = DefaultCFGStateFactory(factoryState).allBottom(callTarget.body)
                  val backwardSummary = new MethodSummary(identifyingPP, callTarget, cfgState)
                  executeMethodBackward[S](beforeExitState, backwardSummary, forwardSummary)
              }

            backwardSummaries += ((identifyingPP, currentBackwardSummary))

            while (!exitsOnStack.get(identifyingPP).get.asInstanceOf[S].removeExpression().lessEqual(beforeExitState.removeExpression())) {
              beforeExitState = exitsOnStack.get(identifyingPP).get.asInstanceOf[S]
              currentBackwardSummary =  executeMethodBackward[S](beforeExitState, currentBackwardSummary, forwardSummary)
              backwardSummaries += ((identifyingPP, currentBackwardSummary))
            }

            val backwardAfterEntryState = currentBackwardSummary.cfgState.entryState()
            val forwardBeforeEntry = forwardSummary.cfgState.entryState()
            val backwardBeforeEntry = backwardEnterFunction(callPoint, callTarget, forwardBeforeEntry, backwardAfterEntryState, parameters)
            val localState = pruneGlobalState(postState)
            exitsOnStack = exitsOnStack - identifyingPP
            localState.lub(backwardBeforeEntry)

        }
        result

      case _ => sys.error("Cannot execute method backward without forward summary")

    }
  }

  /** Backward semantics for enterFunction */
  def backwardEnterFunction[S <: State[S]](callPoint: ProgramPoint, callTarget: MethodDeclaration,
                                           forwardBeforeEntry: S, backwardPostState: S, actualInParams: List[ExpressionSet]): S = {
    var curState = backwardPostState

    val formalInParams = callTarget.arguments(0)
    val outParameters = callTarget.arguments(1)

    
    if (actualInParams.length == formalInParams.length) {
      for ((decl, value)  <- formalInParams zip actualInParams) {
        val variable = decl.variable.id
        // TODO:
        // This should ideally be a backward assignment, but that sometimes
        // yields bottom for recursive methods (potential unsoundness or bug?)
        //curState = curState.backwardAssignVariable(curState.top(), ExpressionSet(variable), value)
        curState = curState.setVariableToTop(ExpressionSet(variable))
        curState = curState.removeVariable(ExpressionSet(variable))
      }
    } else {
      for (decl <- formalInParams) {
        val variable = decl.variable.id
        curState = curState.removeVariable(ExpressionSet(variable))
      }
    }


    for (decl <- outParameters) {
      val variable = decl.variable.id
      curState = curState.backwardAssignVariable(forwardBeforeEntry, ExpressionSet(variable), Invalid(variable.typ)(decl.programpoint))
    }

    curState
  }

  /** Backward semantics for exitFunction */
  def backwardExitFunction[S <: State[S]](callPoint:ProgramPoint, callTarget: MethodDeclaration,
                                          forwardBeforeExit: S, methodPostState: S): S = {
    val exitVarFilter = exitFunctionLocalVarFilter(callTarget)

    val prunedVarState = forwardBeforeExit.pruneVariables(exitVarFilter)
    val recreatedUnreachables = methodPostState.undoPruneUnreachableHeap(prunedVarState)
    val recreatedLocalVars = recreatedUnreachables.undoPruneVariables(forwardBeforeExit, exitVarFilter)
    var curState = recreatedLocalVars.removeExpression()

    // Backward assign out variables and remove "temporary" return ids
    val outParameters = callTarget.arguments(1)
    for (outParam <- outParameters) {
      val tempVar = VariableIdentifier(CFGGenerator.returnIdent(outParam.variable.getName),
        ProgramPointScopeIdentifier(callTarget.programpoint))(outParam.typ,callPoint)
      val tempVarExpr = ExpressionSet(tempVar)
      curState = curState.backwardAssignVariable(forwardBeforeExit, tempVarExpr, ExpressionSet(outParam.variable.id))
      //curState = curState.removeVariable(ExpressionSet(tempVar))
    }

    curState.setExpression(forwardBeforeExit.expr).glb(forwardBeforeExit)
  }


  /**
   * Takes a basic program point (for source of non-determinism)
   * and enhances it with calling context information, i.e. all the call points
   * of methods on the stack. Purpose is similar to DeepeningProgramPoint
   * for fields, but it depends on calling context.
   *
   * note: highly experimental feature turned off by default
   */
  def nonDetPP(pp: ProgramPoint): ProgramPoint = {
    if (TouchAnalysisParameters.contextSensitiveInterproceduralAnalysis) {
      val context = SystemParameters.backwardContext
      val entryPPs = (if(context.runningBackward) exitsOnStack else entriesOnStack).keys.map(_.toString).toList.sorted
      DeepeningProgramPoint(pp, entryPPs)
    } else pp
  }

}
