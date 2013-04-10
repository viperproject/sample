package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.td.compiler.{TouchMethodIdentifier, TouchType, TouchTuple}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import scala.Some
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.td.compiler.TouchMethodIdentifier

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
  def collect[S <: State[S]](callPoint:ProgramPoint,callType:ClassDefinition,callTarget:MethodDeclaration,entryState:S,parameters:List[ExpressionSet]):S = {
    val identifyingPP =
      if(TouchAnalysisParameters.contextSensitiveInterproceduralAnalysis) callPoint
      else callTarget.programpoint

    var enteredState = enterFunction(callPoint,callTarget,entryState,parameters)
    val result = entries.get(identifyingPP) match {
      case Some(oldEntryState) =>

        // This is a recursive call (non top level).
        // Join the entry state and continue with previously recorded
        // exit + entryState (updates inside recursive calls are weak)
        val newEntryState = enteredState.lub(enteredState,oldEntryState.asInstanceOf[S])
        entries += ((identifyingPP,newEntryState))
        summaries.get(identifyingPP) match {
          case Some((_,_,prevExecution)) =>

            val prevExitState = prevExecution.asInstanceOf[ControlFlowGraphExecution[S]].exitState()
            val exitedState = exitFunction(callPoint,callTarget,prevExitState,parameters)
            val localState = pruneGlobalState(entryState)

            // We immediately widen the entrystate for now
            entryState.widening(localState,exitedState)

          case None => entryState
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

        entryState.lub(localState,exitState)
    }

    result
  }
  def reset[S <: State[S]]() {
    summaries = Map.empty[ProgramPoint,(ClassDefinition,MethodDeclaration,ControlFlowGraphExecution[S])]
    entries = Map.empty[ProgramPoint,S]
  }

  def getSummaries = summaries

  private def executeMethod[S <: State[S]](entryState:S, callType:ClassDefinition, callTarget:MethodDeclaration, cfgEx:ControlFlowGraphExecution[S]):ControlFlowGraphExecution[S] = {
    val callContext = (SystemParameters.currentMethod,SystemParameters.currentCFG,SystemParameters.typ)
    SystemParameters.typ = callType.typ
    SystemParameters.currentCFG = callTarget.body
    SystemParameters.currentMethod = callTarget.name.toString
    //SystemParameters.progressOutput.begin("METHOD: "+callTarget.name)
    val newState = cfgEx.forwardSemantics(entryState)
    //SystemParameters.progressOutput.end()
    SystemParameters.typ = callContext._3
    SystemParameters.currentCFG = callContext._2
    SystemParameters.currentMethod = callContext._1
    newState
  }

  private def pruneGlobalState[S <: State[S]](entryState:S):S = {
    var curState = entryState
    curState = curState.pruneVariables({
      id:VariableIdentifier =>
        id.getType().asInstanceOf[TouchType].isSingleton ||
          id.toString.startsWith("__data_") ||
          id.toString.startsWith("__param_") ||
          id.toString.startsWith("__last_return")
    })

    curState = curState.pruneUnreachableHeap()
    curState = curState.setExpression(curState.getExpression().bottom())

    curState
  }

  private def enterFunction[S <: State[S]](callPoint:ProgramPoint,callTarget:MethodDeclaration,entryState:S,
                                           parameters:List[ExpressionSet]):S = {

    var curState = entryState

    if (parameters.length == callTarget.arguments.apply(0).length) {

      val inParameters = callTarget.arguments(0)

      // Initialize in-parameters to temporary variables
      val tempVars = for ((decl,value) <- inParameters.zip(parameters)) yield {
        val tempVar = VariableIdentifier("__param_"+decl.variable.id.toString,decl.typ,callPoint)
        curState = curState.assignVariable(new ExpressionSet(tempVar.getType()).add(tempVar),value)
        tempVar
      }

      // Prune non-parameters and non-globals (reach. based localization)
      curState = curState.pruneVariables({
        id:VariableIdentifier =>
          !id.getType().asInstanceOf[TouchType].isSingleton &&
          !id.toString.startsWith("__data_") &&
          !id.toString.startsWith("__param_")
      })

      // Initialize in-parameters to temp vars
      for ((decl,value) <- inParameters.zip(tempVars)) {
        val variable = decl.variable.id
        curState = curState.assignVariable(new ExpressionSet(variable.getType()).add(variable),new ExpressionSet(value.getType()).add(value))
      }

      // Prune temporary variables
      curState = curState.pruneVariables({ id:VariableIdentifier => id.toString.startsWith("__param_") })

      // Prune unreachable heap locations
      curState = curState.pruneUnreachableHeap()

    } else {

      // Prune local state
      curState = curState.pruneVariables({ id:VariableIdentifier =>
        !id.getType().asInstanceOf[TouchType].isSingleton &&
        !id.toString.startsWith("__data_")
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
          curState = curState.assignVariable(toExpressionSet(x.variable.id),curState.getExpression())
      })

    }

    // Initialize out-parameters to invalid
    callTarget.arguments.apply(1).foreach({
      x:VariableDeclaration =>
        curState = curState.assignVariable(toExpressionSet(x.variable.id),Invalid(x.typ)(x.programpoint))
    })

    curState

  }

  private def exitFunction[S <: State[S]](callPoint:ProgramPoint,callTarget:MethodDeclaration,entryState:S,
                                          parameters:List[ExpressionSet]):S = {

    val outParameters = callTarget.arguments(1)
    var curState = entryState

    if (outParameters.length > 0) {

      // Generate returned expression with extra handling for multiple return values
      val returnExpr =
        if (outParameters.length > 1) {

          // Create a tuple for storing the return values
          val tupleType = TouchTuple(outParameters map (_.typ.asInstanceOf[TouchType]))
          curState = curState.createObject(tupleType,callPoint)
          val tuple = curState.getExpression()

          // Assign fields of the tuple with given arguments
          for ((f,a) <- tupleType.getPossibleFields().zip(outParameters)) {
            curState = curState.assignField(List(tuple),f.getName(),new ExpressionSet(a.typ).add(a.variable.id))
          }

          // Set valid
          curState = curState.assignVariable(tuple,new ExpressionSet(tupleType).add(Constant("valid",tupleType,callPoint)))

          tuple

        } else {
          new ExpressionSet(outParameters.head.typ).add(outParameters.head.variable.id)
        }

      // Store in temporary variable
      val tempVar = VariableIdentifier("__last_return",returnExpr.getType(),callPoint)
      val tempVarExpr = new ExpressionSet(tempVar.getType()).add(tempVar)
      curState = curState.assignVariable(tempVarExpr,returnExpr)
      curState = curState.setExpression(tempVarExpr)

    }

    // Prune local state (except __last_return)
    curState = curState.pruneVariables({
      id:VariableIdentifier =>
        !id.getType().asInstanceOf[TouchType].isSingleton &&
        !id.toString.startsWith("__last_return") &&
        !id.toString.startsWith("__data_")
    })
    curState = curState.pruneUnreachableHeap()

    curState

  }

}
