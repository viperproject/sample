package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, ControlFlowGraphExecution, ProgramPoint}
import ch.ethz.inf.pm.sample.abstractdomain.State

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
   * TODO: Parameter rewriting should be done here.
   *
   * @param callPoint The program point at which the method is called
   * @param callTarget The declaration of the method that is called
   * @param entryState The state when entering the method
   * @tparam S Our current abstract domain
   * @return The exit state of the method
   */
  def collect[S <: State[S]](callPoint:ProgramPoint,callTarget:MethodDeclaration,entryState:S):S = {
    entries.get(callPoint) match {
      case Some(oldEntryState) =>

        // This is a recursive call (non top level).
        // Join the entry state and continue with previously recorded
        // exit + entryState (updates inside recursive calls are weak)
        entries += ((callPoint,entryState.lub(entryState,oldEntryState.asInstanceOf[S])))
        summaries.get(callPoint) match {
          case Some(prevExecution) => entryState.lub(prevExecution.asInstanceOf[ControlFlowGraphExecution[S]].exitState(),entryState)
          case None => entryState
        }

      case None =>

        // This is a top-level call (recursive or non-recursive)
        // Record the effect of one iteration

        entries += ((callPoint,entryState))

        var currentSummary = summaries.get(callPoint) match {
          case Some(prevSummary) => callTarget.forwardSemantics(entryState).widening(prevSummary.asInstanceOf[ControlFlowGraphExecution[S]])
          case None => callTarget.forwardSemantics(entryState)

        }

        summaries += ((callPoint,currentSummary))

        // Are there more possible depths?
        while (!entries.get(callPoint).get.asInstanceOf[S].removeExpression().lessEqual(currentSummary.entryState().removeExpression())) {
          currentSummary = currentSummary.widening(callTarget.forwardSemantics(entryState))
          summaries += ((callPoint,currentSummary))
        }

        entries = entries - callPoint

        currentSummary.exitState()

    }
  }


  def reset[S <: State[S]]() { summaries = Map.empty[ProgramPoint,ControlFlowGraphExecution[S]] }

  def apply[S <: State[S]](pp:ProgramPoint):Option[ControlFlowGraphExecution[S]] =
    summaries.get(pp).asInstanceOf[Option[ControlFlowGraphExecution[S]]]

  def getSummaries = summaries

}
