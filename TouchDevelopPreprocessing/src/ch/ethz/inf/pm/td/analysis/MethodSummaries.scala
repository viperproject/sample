package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.td.compiler.TouchMethodIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.domain.MultiValExpression
import ch.ethz.inf.pm.td.semantics.{TNothing, TUnknown}

/**
 * Stores summaries of methods. This is not thread-safe.
 */
object MethodSummaries {

  /**
   * Stores the summaries of called methods, program point specific
   */
  private var summaries:Map[ProgramPoint,(ClassDefinition,MethodDeclaration,ControlFlowGraphExecution[_])] = Map.empty

  /**
   * Stores the entry states of a method that is currently on the stack (for recursive calls)
   */
  private var entries:Map[ProgramPoint,_] = Map.empty

  /**
   * Stores the entry state of a closure method
   */
  private var closureEntries:Map[String,State[_]] = Map.empty

  /**
   * Stores exit states at positions which end the script prematurely
   */
  private var abnormalExits:Option[State[_]] = None

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
  def collect[S <: State[S]](callPoint:ProgramPoint,callType:ClassDefinition,callTarget:MethodDeclaration,entryState:S,parameters:List[ExpressionSet], localHandlerScope: Option[S] = None):S = {
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

    val result = entries.get(identifyingPP) match {
      case Some(oldEntryState) =>

        // This is a recursive call (non top level).
        // Join the entry state and continue with previously recorded
        // exit + entryState (updates inside recursive calls are weak)
        val newEntryState = enteredState.widening(oldEntryState.asInstanceOf[S])
        entries += ((identifyingPP,newEntryState))
        summaries.get(identifyingPP) match {
          case Some((_,_,prevExecution)) =>

            val prevExitState = prevExecution.asInstanceOf[ControlFlowGraphExecution[S]].exitState()
            val exitedState = exitFunction(callPoint,callTarget,prevExitState,parameters)
            val localState = pruneGlobalState(entryState)

            // We immediately widen the entrystate for now
            entryState.lub(localState)

          case None => entryState.bottom()
        }

      case None =>

        // This is a top-level call (recursive or non-recursive)
        // Record the effect of one iteration

        entries += ((identifyingPP,enteredState))

        var currentSummary = summaries.get(identifyingPP) match {
          case Some((_,_,prevSummary)) =>
            executeMethod(enteredState,callType,callTarget,prevSummary.asInstanceOf[ControlFlowGraphExecution[S]])
          case None =>
            executeMethod(enteredState,callType,callTarget,new ControlFlowGraphExecution[S](callTarget.body,enteredState))
        }

        summaries += ((identifyingPP,(callType,callTarget,currentSummary)))

        // Are there more possible depths?
        while (!entries.get(identifyingPP).get.asInstanceOf[S].removeExpression().lessEqual(enteredState.removeExpression())) {
          enteredState = entries.get(identifyingPP).get.asInstanceOf[S]
          currentSummary = executeMethod(enteredState,callType,callTarget,currentSummary)
          summaries += ((identifyingPP,(callType,callTarget,currentSummary)))
        }

        entries = entries - identifyingPP

        val exitState = exitFunction(callPoint,callTarget,currentSummary.exitState(),parameters)
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
        !id.getType.asInstanceOf[TouchType].isSingleton &&
        !CFGGenerator.isGlobalReferenceIdent(id.toString())
      case _ => false
    })
    curState = curState.pruneUnreachableHeap()

    abnormalExits = abnormalExits match {
      case Some(x) => Some(curState.widening(x.asInstanceOf[S]))
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

  def reset[S <: State[S]]() {
    summaries = Map.empty[ProgramPoint,(ClassDefinition,MethodDeclaration,ControlFlowGraphExecution[S])]
    entries = Map.empty[ProgramPoint,S]
    closureEntries = Map.empty
    abnormalExits = None
  }

  def getSummaries = summaries

  private def executeMethod[S <: State[S]](entryState:S, callType:ClassDefinition, callTarget:MethodDeclaration, cfgEx:ControlFlowGraphExecution[S]):ControlFlowGraphExecution[S] = {
    val callContext = (SystemParameters.currentMethod,SystemParameters.currentCFG,SystemParameters.currentClass,SystemParameters.typ)
    SystemParameters.typ = callType.typ
    SystemParameters.currentClass = callType.typ
    SystemParameters.currentCFG = callTarget.body
    SystemParameters.currentMethod = callTarget.name.toString
    //SystemParameters.progressOutput.begin("METHOD: "+callTarget.name)
    val newState = cfgEx.forwardSemantics(entryState)
    //SystemParameters.progressOutput.end()
    SystemParameters.typ = callContext._4
    SystemParameters.currentClass = callContext._3
    SystemParameters.currentCFG = callContext._2
    SystemParameters.currentMethod = callContext._1
    newState
  }

  private def pruneGlobalState[S <: State[S]](entryState:S):S = {
    var curState = entryState
    curState = curState.pruneVariables({
      case id:VariableIdentifier =>
        id.getType.asInstanceOf[TouchType].isSingleton ||
          CFGGenerator.isGlobalReferenceIdent(id.toString()) ||
          CFGGenerator.isParamIdent(id.toString()) ||
          CFGGenerator.isReturnIdent(id.toString())
      case _ => false
    })

    curState = curState.pruneUnreachableHeap()
    curState = curState.setExpression(curState.getExpression.bottom())

    curState
  }

  private def enterFunction[S <: State[S]](callPoint:ProgramPoint,callTarget:MethodDeclaration,entryState:S,
                                           parameters:List[ExpressionSet]):S = {

    var curState = entryState

    if (parameters.length == callTarget.arguments.apply(0).length) {

      val inParameters = callTarget.arguments(0)

      // Initialize in-parameters to temporary variables
      val tempVars = for ((decl,value) <- inParameters.zip(parameters)) yield {
        val tempVar = VariableIdentifier(CFGGenerator.paramIdent(decl.variable.id.toString),decl.typ,callPoint,ProgramPointScopeIdentifier(callTarget.programpoint))
        val expr = new ExpressionSet(tempVar.getType).add(tempVar)
        curState = curState.createVariable(expr, tempVar.getType, callTarget.programpoint)
        curState = curState.assignVariable(expr,value)
        tempVar
      }

      // Prune non-parameters and non-globals (reach. based localization)
      if (TouchAnalysisParameters.localizeStateOnMethodCall) {
        curState = curState.pruneVariables({
          case id:VariableIdentifier =>
            !id.getType.asInstanceOf[TouchType].isSingleton &&
            !CFGGenerator.isGlobalReferenceIdent(id.toString()) &&
            !CFGGenerator.isParamIdent(id.toString())
          case _ => false
        })
      }

      // Initialize in-parameters to temp vars
      for ((decl,value) <- inParameters.zip(tempVars)) {
        val variable = decl.variable.id
        val expr = new ExpressionSet(variable.getType).add(variable)
        curState = curState.createVariable(expr,variable.getType,callTarget.programpoint)
        curState = curState.assignVariable(expr,new ExpressionSet(value.getType).add(value))
      }

      // Prune temporary variables
      curState = curState.pruneVariables({
        case id:VariableIdentifier => CFGGenerator.isParamIdent(id.toString())
        case _ => false
      })

      // Prune unreachable heap locations
      curState = curState.pruneUnreachableHeap()

    } else {

      // Prune local state
      curState = curState.pruneVariables({
        case id:VariableIdentifier =>
          !id.getType.asInstanceOf[TouchType].isSingleton &&
          !CFGGenerator.isGlobalReferenceIdent(id.toString())
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
          val right = curState.getExpression
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

  private def exitFunction[S <: State[S]](callPoint:ProgramPoint,callTarget:MethodDeclaration,entryState:S,
                                          parameters:List[ExpressionSet]):S = {

    val outParameters = callTarget.arguments(1)
    var curState = entryState

    // Store returns in temporary variables
    val tempVars = for (outParam <- outParameters) yield {
      val tempVar = VariableIdentifier(CFGGenerator.returnIdent(outParam.variable.getName),outParam.typ,callPoint,ProgramPointScopeIdentifier(callTarget.programpoint))
      val tempVarExpr = new ExpressionSet(tempVar.getType).add(tempVar)
      curState = curState.createVariable(tempVarExpr,tempVarExpr.getType(),callTarget.programpoint)
      curState = curState.assignVariable(tempVarExpr,new ExpressionSet(outParam.typ).add(outParam.variable.id))
      tempVar
    }

    def buildMultiVal(tempVars:List[VariableIdentifier]): Expression = tempVars match {
      case x :: Nil => x
      case x :: xs => MultiValExpression(x,buildMultiVal(xs),TUnknown.typ.top())
      case Nil => UnitExpression(TNothing.typ,callPoint)
    }

    val z = buildMultiVal(tempVars)
    curState = curState.setExpression(new ExpressionSet(z.getType).add(z))

    // Prune local state (except return values)
    curState = curState.pruneVariables({
      case id:VariableIdentifier =>
        // Belongs to scope of call target
        id.scope == ProgramPointScopeIdentifier(callTarget.programpoint) &&
        // Is not a return value
        !CFGGenerator.isReturnIdent(id.toString())
      case _ => false
    })
    curState = curState.pruneUnreachableHeap()

    curState

  }

}
