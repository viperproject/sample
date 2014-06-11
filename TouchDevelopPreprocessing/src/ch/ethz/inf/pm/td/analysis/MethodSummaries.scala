package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.sample.{AnalysisUnitContext, SystemParameters}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.td.compiler.TouchMethodIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.domain.MultiValExpression
import ch.ethz.inf.pm.td.semantics.{TNothing, TUnknown}
import ch.ethz.inf.pm.sample.execution.CFGState

case class MethodSummary[S <: State[S]](pp: ProgramPoint, method: MethodDeclaration,
                                        cfgState: CFGState[S])

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
  private var entriesOnStack: Map[ProgramPoint, _] = Map.empty

  /**
   * Stores the entry state of a closure method
   */
  private var closureEntries: Map[String, State[_]] = Map.empty

  /**
   * Stores exit states at positions which end the script prematurely
   */
  private var abnormalExits: Option[State[_]] = None

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
      if (TouchAnalysisParameters.contextSensitiveInterproceduralAnalysis) callPoint
      else callTarget.programpoint

    var enteredState = enterFunction(callPoint, callTarget, entryState, parameters)

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
        entriesOnStack += ((identifyingPP, newEntryState))
        summaries.get(identifyingPP) match {
          case Some(s) =>

            // Get the result of the previous recursion depth and join it with the local state
            val summary = s.asInstanceOf[MethodSummary[S]]
            val prevExitState = summary.cfgState.exitState()
            val exitedState = exitFunction(callPoint, callTarget, prevExitState, parameters)
            val localState = pruneGlobalState(entryState)
            exitedState.lub(localState)

          case None =>

            // We do not have a result for our recursion yet. Bottom.
            entryState.bottom()

        }

      case None =>

        // This is a top-level call (recursive or non-recursive)
        // Record the effect of one iteration

        entriesOnStack += ((identifyingPP, enteredState))

        var currentSummary = summaries.get(identifyingPP) match {
          case Some(prevSummary) =>
            val prev = prevSummary.asInstanceOf[MethodSummary[S]]
            executeMethod(enteredState, prev)
          case None =>
            val factoryState = enteredState.factory()
            val cfgState = new ControlFlowGraphExecution(callTarget.body, factoryState)
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

        val exitState = exitFunction(callPoint, callTarget, currentSummary.cfgState.exitState(), parameters)
        val localState = pruneGlobalState(entryState)

        localState.lub(exitState)
    }

    result
  }

  /**
   * Collect the exit state of a stopped script somewhere in the middle of the script, due to
   */
  def collectExit[S <: State[S]](exitState: S): S = {

    var curState = exitState.removeExpression()

    curState = curState.pruneVariables({
      case id: VariableIdentifier =>
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


  def collectClosureEntry[S <: State[S]](handlerName: String, entryState: S) = {

    closureEntries += (handlerName ->
      (closureEntries.get(handlerName) match {
        case None => entryState
        case Some(x) => entryState.lub(x.asInstanceOf[S])
      })
      )

  }

  def getClosureEntry[S <: State[S]](handlerName: String): Option[S] = {
    closureEntries.get(handlerName) match {
      case None => None
      case Some(x) => Some(x.asInstanceOf[S])
    }
  }

  /**
   * This joins the given states with all abnormal exit states that where collected during the analysis.
   *
   * @param s The (global!)
   * @tparam S state type
   * @return The widened abnormal exit
   */
  def joinAbnormalExits[S <: State[S]](s: S): S = {

    abnormalExits match {
      case None => s
      case Some(x) => s.lub(x.asInstanceOf[S])
    }

  }

  /**
   * Resets the method summaries to an empty state.
   *
   * YOU HAVE TO CALL THIS BEFORE ANALYZING THE NEXT SCRIPT
   *
   * @tparam S state type
   */
  def reset[S <: State[S]]() {
    summaries = Map.empty[ProgramPoint, MethodSummary[S]]
    entriesOnStack = Map.empty[ProgramPoint, S]
    closureEntries = Map.empty
    abnormalExits = None
  }

  /**
   * Returns the current method summaries, casted to the correct type
   *
   * @tparam S state type
   * @return the current method summaries
   */
  def getSummaries[S <: State[S]] = summaries.asInstanceOf[Map[ProgramPoint, MethodSummary[S]]]

  /**
   * (Re-)analyzes a method
   *
   * @param entryState the entry state
   * @param currentSummary the previously computed summary
   * @tparam S state type
   * @return a generalized summary
   */
  private def executeMethod[S <: State[S]](entryState: S, currentSummary: MethodSummary[S]): MethodSummary[S] = {
    val methodDecl = currentSummary.method
    val newState =
      SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(methodDecl)) {
        currentSummary.cfgState.asInstanceOf[ControlFlowGraphExecution[S]].forwardSemantics(entryState)
      }
    currentSummary.copy(cfgState = newState)
  }

  /**
   *
   * The abstract transformer along the local edge in the supergraph. Remove all global state to
   * allow destructive updates inside methods. This may cause a loss in precision, as some
   * relations between local variables and global state may be lost
   *
   * @param entryState The entry state of the local edge (the call state after argument evaluation)
   * @tparam S The type of the state.
   * @return The exit state of the local edge
   */
  private def pruneGlobalState[S <: State[S]](entryState: S): S = {

    var curState = entryState
    curState = curState.pruneVariables({
      case id: VariableIdentifier =>
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

  /**
   * Defines the abstract transformer along the return edge of a function.
   *
   * Maps given arguments to formal parameters. Also perform reachability based localization, that is, removes
   * all local variables from the calling context (which may reduce precision in the presence of relational domains!)
   *
   * Again, we perform abstract garbage collection to prune unreachable heap - but also, to generalize our summaries.
   *
   * @param callPoint The program point of the call
   * @param callTarget The called method
   * @param entryState The entry state of the call edge, that is, the entry state of the call node
   *                   after evaluation of arguments
   * @param parameters The original parameters to call the function
   * @tparam S The type of the state
   * @return The exit state of the call edge, that is, the entry state of the function
   */
  private def enterFunction[S <: State[S]](callPoint: ProgramPoint, callTarget: MethodDeclaration, entryState: S,
                                           parameters: List[ExpressionSet]): S = {
    var curState = entryState

    if (parameters.length == callTarget.arguments.apply(0).length) {

      val inParameters = callTarget.arguments(0)

      // Initialize in-parameters to temporary variables
      val tempVars = for ((decl, value) <- inParameters.zip(parameters)) yield {
        val tempVar = VariableIdentifier(CFGGenerator.paramIdent(decl.variable.id.toString), ProgramPointScopeIdentifier(callTarget.programpoint))(decl.typ, callPoint)
        val expr = ExpressionSet(tempVar)
        curState = curState.createVariable(expr, tempVar.typ, callTarget.programpoint)
        curState = curState.assignVariable(expr, value)
        tempVar
      }

      // Prune non-parameters and non-globals (reach. based localization)
      if (TouchAnalysisParameters.localizeStateOnMethodCall) {
        curState = curState.pruneVariables({
          case id: VariableIdentifier =>
            !id.typ.asInstanceOf[TouchType].isSingleton &&
              !CFGGenerator.isGlobalReferenceIdent(id.toString) &&
              !CFGGenerator.isParamIdent(id.toString)
          case _ => false
        })
      }

      // Initialize in-parameters to temp vars
      for ((decl, value) <- inParameters.zip(tempVars)) {
        val variable = decl.variable.id
        val expr = ExpressionSet(variable)
        curState = curState.createVariable(expr, variable.typ, callTarget.programpoint)
        curState = curState.assignVariable(expr, ExpressionSet(value))
      }

      // Prune temporary variables
      curState = curState.pruneVariables({
        case id: VariableIdentifier => CFGGenerator.isParamIdent(id.toString)
        case _ => false
      })

      // Prune unreachable heap locations
      curState = curState.pruneUnreachableHeap()

    } else {

      // For an unknown reason, sometimes actions are called with no arguments, without the TouchDevelop
      // environement recognizing this as an error, In these cases, as we don't know the intended behavior,
      // we initialize all in-parameters to top. (It seems these parameters are implicit in very restricted
      // settings, resulting from conversions of previous language features)

      // Prune local state
      curState = curState.pruneVariables({
        case id: VariableIdentifier =>
          !id.typ.asInstanceOf[TouchType].isSingleton &&
            !CFGGenerator.isGlobalReferenceIdent(id.toString)
        case _ => false
      })
      curState = curState.pruneUnreachableHeap()

      // Initialize in-parameters to top
      callTarget.arguments.apply(0).foreach({
        x: VariableDeclaration =>
          if (TouchAnalysisParameters.argumentsToPublicMethodsValid || callTarget.name.asInstanceOf[TouchMethodIdentifier].isEvent) {
            curState = Top[S](x.typ.asInstanceOf[TouchType])(curState, x.programpoint)
          } else {
            curState = TopWithInvalid[S](x.typ.asInstanceOf[TouchType])(curState, x.programpoint)
          }
          val right = curState.expr
          val expr = toExpressionSet(x.variable.id)
          curState = curState.createVariable(expr, expr.getType(), x.programpoint)
          curState = curState.assignVariable(expr, right)
      })

    }

    // Initialize out-parameters to invalid
    callTarget.arguments.apply(1).foreach({
      x: VariableDeclaration =>
        val expr = toExpressionSet(x.variable.id)
        curState = curState.createVariable(expr, expr.getType(), callTarget.programpoint)
        curState = curState.assignVariable(expr, Invalid(x.typ)(x.programpoint))
    })

    curState

  }

  /**
   *
   * Defines the abstract transformer along the return edge of a function.
   *
   * In particular, it stores the output parameters of the function in the expression of the state
   * ("puts the values on the stack") with special handling for multiple output parameters.
   *
   * Then, we remove all local variables that are not returned (e.g. those that become unreachable)
   * and perform abstract garbage collection.
   *
   * @param callPoint The program point of the call
   * @param callTarget The called method
   * @param entryState The entry state of the return edge, that is, the exit state of the method before return
   * @param parameters The original parameters to call the function
   * @tparam S The type of the state
   * @return The exit state of the return edge
   */
  private def exitFunction[S <: State[S]](callPoint: ProgramPoint, callTarget: MethodDeclaration, entryState: S,
                                          parameters: List[ExpressionSet]): S = {

    val outParameters = callTarget.arguments(1)
    var curState = entryState

    // Store returns in temporary variables
    val tempVars = for (outParam <- outParameters) yield {
      val tempVar = VariableIdentifier(CFGGenerator.returnIdent(outParam.variable.getName),
        ProgramPointScopeIdentifier(callTarget.programpoint))(outParam.typ, callPoint)
      val tempVarExpr = ExpressionSet(tempVar)
      curState = curState.createVariable(tempVarExpr, tempVarExpr.getType(), callTarget.programpoint)
      curState = curState.assignVariable(tempVarExpr, ExpressionSet(outParam.variable.id))
      tempVar
    }

    // Set the expression to the set of output parameters.
    def buildMultiVal(tempVars: List[VariableIdentifier]): Expression = tempVars match {
      case x :: Nil => x
      case x :: xs => MultiValExpression(x, buildMultiVal(xs), TUnknown.typ.top())
      case Nil => UnitExpression(TNothing.typ, callPoint)
    }
    val z = buildMultiVal(tempVars)
    curState = curState.setExpression(ExpressionSet(z))

    // Prune the local state from the called function
    curState = curState.pruneVariables({
      case id: VariableIdentifier =>
        // Belongs to scope of call target
        id.scope == ProgramPointScopeIdentifier(callTarget.programpoint) &&
          // Is not a return value
          !CFGGenerator.isReturnIdent(id.toString)
      case _ => false
    })
    curState = curState.pruneUnreachableHeap()
    curState

  }

}
