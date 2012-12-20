package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, ControlFlowGraphExecution, ProgramPoint}
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, ExpressionSet, State}
import ch.ethz.inf.pm.td.compiler.{TouchType, TouchTuple}

/**
 * Stores summaries of methods. This is not thread-safe.
 */
object MethodSummaries {

  /**
   * Stores the summaries of called methods, program point specific
   */
  private var summaries:Map[ProgramPoint,ControlFlowGraphExecution[_]] = Map.empty

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
  def collect[S <: State[S]](callPoint:ProgramPoint,callTarget:MethodDeclaration,entryState:S,parameters:List[ExpressionSet]):S = {
    val enteredState = enterFunction(callPoint,callTarget,entryState,parameters)
    entries.get(callPoint) match {
      case Some(oldEntryState) =>

        // This is a recursive call (non top level).
        // Join the entry state and continue with previously recorded
        // exit + entryState (updates inside recursive calls are weak)
        val newEntryState = enteredState.lub(enteredState,oldEntryState.asInstanceOf[S])
        entries += ((callPoint,newEntryState))
        summaries.get(callPoint) match {
          case Some(prevExecution) =>
            val prevExitState = prevExecution.asInstanceOf[ControlFlowGraphExecution[S]].exitState()
            val exitedState = exitFunction(callPoint,callTarget,prevExitState,parameters)
            exitedState
          case None => entryState
        }

      case None =>

        // This is a top-level call (recursive or non-recursive)
        // Record the effect of one iteration

        entries += ((callPoint,enteredState))

        var currentSummary = summaries.get(callPoint) match {
          case Some(prevSummary) =>
            val prevSummaryCFG = prevSummary.asInstanceOf[ControlFlowGraphExecution[S]]
            val newSummaryCFG = callTarget.forwardSemantics(enteredState)
            newSummaryCFG.widening(prevSummaryCFG)
          case None => callTarget.forwardSemantics(enteredState)
        }

        summaries += ((callPoint,currentSummary))

        // Are there more possible depths?
        while (!entries.get(callPoint).get.asInstanceOf[S].removeExpression().lessEqual(currentSummary.entryState().removeExpression())) {
          currentSummary = currentSummary.widening(callTarget.forwardSemantics(enteredState))
          summaries += ((callPoint,currentSummary))
        }

        entries = entries - callPoint

        exitFunction(callPoint,callTarget,currentSummary.exitState(),parameters)

    }
  }


  def reset[S <: State[S]]() { summaries = Map.empty[ProgramPoint,ControlFlowGraphExecution[S]] }

  def apply[S <: State[S]](pp:ProgramPoint):Option[ControlFlowGraphExecution[S]] =
    summaries.get(pp).asInstanceOf[Option[ControlFlowGraphExecution[S]]]

  def getSummaries = summaries

  private def enterFunction[S <: State[S]](callPoint:ProgramPoint,callTarget:MethodDeclaration,entryState:S,
                                           parameters:List[ExpressionSet]):S = {

    var curState = entryState
    val inParameters = callTarget.arguments(0)
    for ((decl,value) <- inParameters.zip(parameters)) {
      curState = decl.forwardSemantics(curState)
      val variable = decl.variable.id
      curState = curState.assignVariable(new ExpressionSet(variable.getType()).add(variable),value)
    }
    curState

  }

  private def exitFunction[S <: State[S]](callPoint:ProgramPoint,callTarget:MethodDeclaration,entryState:S,
                                          parameters:List[ExpressionSet]):S = {

    val outParameters = callTarget.arguments(1)
    var curState = entryState

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

      curState.setExpression(tuple)

    } else if (outParameters.length == 1)  {
      curState.setExpression(new ExpressionSet(outParameters.head.typ).add(outParameters.head.variable.id))
    } else {
      curState
    }

  }

}
