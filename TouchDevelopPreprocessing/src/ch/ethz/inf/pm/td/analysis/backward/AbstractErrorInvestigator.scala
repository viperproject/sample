package ch.ethz.inf.pm.td.analysis.backward

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, MethodDeclaration}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.td.compiler.{TouchProgramPoint, TouchCompiler}
import ch.ethz.inf.pm.td.analysis.interpreter._
import ch.ethz.inf.pm.td.analysis.{MethodSummaries, TouchAnalysisParameters}
import ch.ethz.inf.pm.sample.reporting.Reporter
import scala.Some
import ch.ethz.inf.pm.td.analysis.interpreter.FailedExecution
import ch.ethz.inf.pm.sample.execution.AbstractErrorInfo
import ch.ethz.inf.pm.sample.backwardanalysis.BackwardAnalysisContext


/** The result of our error investigation process */
sealed trait InvestigationResult

case class FalseAlarm[S <: State[S]](
    abstractError: AbstractErrorInfo[S])
  extends InvestigationResult

case class ActualError[S <: State[S]](
    abstractError: AbstractErrorInfo[S],
    input: InterpreterTestInput)
  extends InvestigationResult

case object Undecided extends InvestigationResult


/** A refined entry state that the backward analysis inferred for an abstract error */
case class RefinedEntry[S <: State[S]](

    /** Method this entry refers to */
    method: MethodDeclaration,

    /**
     * Entry state immediately  _before_ the method was entered
     *
     */
    preEnterState: S,

    /**
     * Entry state immediately  _after_ the method was entered
     * (parameters etc. already set up in state)
     */
    postEnterState: S)


/**
 * Core class to investigate the significance of all abstract errors.
 *
 * Responsible for detecting alse alarms and finding counterexamples.
 *
 */
class AbstractErrorInvestigator[S <: State[S]](
    backwardContext: BackwardAnalysisContext[S],
    factoryState: S,
    compiler: TouchCompiler) {

  /** Number of times concrete testing is tried */
  val NumberOfCETries = 3

  /** Investigate all the abstract errors registered during the forward analysis */
  def investigateAll() {

    def isSuccess(result: InvestigationResult): Boolean = result match {
      case Undecided => false
      case _ => true
    }

    backwardContext.runningBackward = true

    /*
      Because we record errors on the fly during forward analysis, multiple
      errors may be registered for the same program location. Later errors
      are "more general" since they were generated in a "larger" state,
      so we simply use the last one.
     */
    val allAbstractErrors = backwardContext.errors.values.map(_.last).toList
    val orderedAbstractErrors = orderErrors(allAbstractErrors)
    // lazily investigate errors until we find a false alarm or actual error
    val results = orderedAbstractErrors.view.map(investigateError)
    results.find(isSuccess)

    backwardContext.runningBackward = false
  }

  /**
   * Investigate a single abstract error
   *
   * @param err the abstract error to analyze
   * @return investigation result, i.e. whether false alarm, counter example found or undecided
   */
  def investigateError(err: AbstractErrorInfo[S]): InvestigationResult = {
    log("Investigating " + err)
    backwardContext.currentError = Some(err)
    MethodSummaries.resetBackward()
    if (TouchAnalysisParameters.backwardInterprocAnalysis) {
      investigateErrorInterprocedural(err)
    } else {
      investigateErrorLocal(err)
    }
  }

  /**
   * Investigate an abstract error by going back to the entry
   * of the method containing the error. The analysis is strictly local.
   *
   * @param err the abstract error to analyze
   * @return investigation result, i.e. whether false alarm, counter example found or undecided
   */
  def investigateErrorLocal(err: AbstractErrorInfo[S]): InvestigationResult = {
    val refinedEntry = computeTopLevelRefinedEntry(err.method)
    val errorEntryState = refinedEntry.postEnterState

    // detect false alarm
    if (errorEntryState.isBottom) {
      logFalseAlarm(err, refinedEntry)
      return FalseAlarm(err)
    }

    // try to find counterexamples
    findCounterExample(refinedEntry, err)
  }

  /**
   * Investigate an abstract error by going back to the entry
   * of "top-level" methods that eventually call the method containing the abstract error.
   *
   * NOTES
   * - This error investigation strategy is highly experimental.
   * - By "top-level" we mean the public entry actions of scripts and events (i.e. there is no other
   *   method "on the call stack"). E.g., if  main calls privateAction1 which calls privateAction2 where an error is found,
   *   the analysis tries to find counterexample starting at the entry of main.
   *
   * - An additional feature for the special case of detecting a false alarm in an event:
   *    Compute fixed point of all events, going back all the way to program entry.
   *    This considers all possible event sequences, and we can report a false alarm
   *    if we get bottom
   *
   * @param err the abstract error to analyze
   * @return investigation result, i.e. whether false alarm, counter example found or undecided
   */
  def investigateErrorInterprocedural(err: AbstractErrorInfo[S]): InvestigationResult = {
    val errorMethod = err.method
    val callGraphGen = new CallGraphGenerator(compiler)
    val callGraph = callGraphGen.generateForScript(compiler.main)
    val possibleToplevelMethods = callGraph.callRoots(errorMethod)

    var allPathsBottom = true

    for (entryMethod <- possibleToplevelMethods) {
      val refinedEntry = computeTopLevelRefinedEntry(entryMethod)

      // Try to detect false alarms by going all the way back to the program start

      def refineAllEvents(exitState: S): S = {
        val refinedEventEntries = for (ev <- compiler.events) yield {
          val re = computeTopLevelRefinedEntry(ev, exitState)
          re.preEnterState
        }
        Lattice.bigLub(refinedEventEntries, exitState.bottom())
      }

      def refineAllPublicActions(exitState: S): S = {
        val refinedActionEntries = for (action <- compiler.getPublicMethods) yield {
          val re = computeTopLevelRefinedEntry(action, exitState)
          re.preEnterState
        }
        Lattice.bigLub(refinedActionEntries, exitState.bottom())
      }

      // Assumption on execution model:
      // (initial state) [all public entry actions, e.g. main]  => (before events state) [all events]* => (refinedEntry.beforeEntryState) [error event]
      val beforeAllEventsState =
        Lattice.lfp(refinedEntry.preEnterState, refineAllEvents, SystemParameters.wideningLimit)

      val beforeAllActionsState = refineAllPublicActions(beforeAllEventsState)
      if (!beforeAllActionsState.isBottom) {
        allPathsBottom = false

        findCounterExample(refinedEntry, err) match {
          case a: ActualError[S] => return a
          case _ => // continue investigation with other top-level methods
        }
      }
    }

    if (allPathsBottom) {
      // We detected a false alarm!
      // Use programpoint of "main" as the point where the test annotation should be placed
      val pp = compiler.mainAction.programpoint
      logFalseAlarmAtProgramEntry(err, pp)
      FalseAlarm(err)

    } else {
      Undecided
    }
  }

  /**
   * Try to synthesize a counterexample by concretizing and testing
   * states from the refined entry state at the given method
   *
   * @param refinedEntryState refined entry state and the method to which it belongs
   * @param err the corresponding abstract error that is being investigated
   * @return ActualError/Undecided, depending on testing results
   */
  private def findCounterExample(refinedEntryState: RefinedEntry[S], err: AbstractErrorInfo[S]): InvestigationResult = {
    val mdecl = refinedEntryState.method
    val ceGen = new PotentialCounterExampleGenerator(refinedEntryState , err, compiler)

    for (n <- 1 to NumberOfCETries) {
      val ceGenSolution = ceGen.findNext()
      ceGenSolution match {
        case Some(potentialCE) =>
          log(s"Trying potential counterexample #$n")
          if (TouchAnalysisParameters.printPotentialCounterExample) {
            log(CounterExamplePrettyPrinter.print(potentialCE))
          }
          val interpreter = new ConcreteInterpreter(compiler, potentialCE)
          val result = interpreter.run()
          result match {
            case SuccessfulExecution =>
              log("Potential counterexample did not trigger error. Not sure if true alarm.")
              Undecided
            case FailedExecution(sampleErr) =>
              Reporter.reportInfo(s"Found counterexample for: ${sampleErr.message}", mdecl.programpoint, "backward.counterexample.found")
              if (TouchAnalysisParameters.printDefiniteCounterExample) {
                log(CounterExamplePrettyPrinter.print(potentialCE))
              }
              return ActualError(err, potentialCE)
          }
        case None =>
          log("No more solutions (potential counterexamples) could be generated by the solver.")
          return Undecided
      }
    }

    Undecided
  }

  /** Starts the computation of a refined entry at entryMethod */
  private def computeTopLevelRefinedEntry(entryMethod: MethodDeclaration): RefinedEntry[S] = {
    computeTopLevelRefinedEntry(entryMethod, factoryState.bottom())
  }

  /** Starts the computation of the refined entry at entryMethod */
  private def computeTopLevelRefinedEntry(entryMethod: MethodDeclaration, exitState: S): RefinedEntry[S] = {
    val callingPP = entryMethod.programpoint
    val beforeEntryState = MethodSummaries.backwardCollect(callingPP, entryMethod, exitState, Nil)
    val backwSummary = MethodSummaries.backwardSummaryFor[S](callingPP)
    val afterEntryState = backwSummary.cfgState.entryState()
    RefinedEntry(entryMethod, beforeEntryState, afterEntryState)
  }

  def log(str: String): Unit = {
    SystemParameters.progressOutput.put(str)
  }


  private def logFalseAlarm(err: AbstractErrorInfo[S], refinedEntry: RefinedEntry[S]): Unit = {
    val mdecl = refinedEntry.method
    val msg = s"FALSE alarm: Bottom at entry ${mdecl.name} for $err"
    Reporter.reportInfo(msg, mdecl.programpoint, "backward.bottom")
  }

  private def logFalseAlarmAtProgramEntry(err: AbstractErrorInfo[S], pp: ProgramPoint): Unit = {
    val msg = s"FALSE alarm: Bottom at program entry for $err"
    Reporter.reportInfo(msg, pp, "backward.bottom")
  }

  /**
   * Heuristically orders errors to be explored. This is mainly needed for testing.
   * Ideally, the user would be able to interactively select an error for which
   * the error investigation is started.
   *
   * Roughly:
   *  - earlier in original program text => first
   *  - unrolled loop program point => first
   *
   *  Unfortunately this is a bit hacky because our custom program points
   *  are strings (e.g. suffixes for unrolled programs)
   **/
  private def orderErrors(errors: List[AbstractErrorInfo[S]]): List[AbstractErrorInfo[S]] = {
    def tpp(err: AbstractErrorInfo[S]) = err.pp.asInstanceOf[TouchProgramPoint]

    def sortByLine(errors: List[AbstractErrorInfo[S]]): List[AbstractErrorInfo[S]] = {
      errors.sortWith { case (a, b) =>
        val (pp1, pp2) = (tpp(a), tpp(b))
        (pp1.lineColumnPosition, pp2.lineColumnPosition) match {
          case (Some(pos1), Some(pos2)) =>
            pos1 < pos2
          case _ =>
            pp1.fullPosString < pp2.fullPosString
        }
      }
    }

    val (firstLoopIterErrs, laterErrs) = errors.partition { err =>
      val pp = tpp(err)
      val unrolled = pp
        .customPositionElements
        .lastOption
        .getOrElse("")
        .startsWith("it_")
      !unrolled || pp.customPositionElements.last == "it1"
    }

    sortByLine(firstLoopIterErrs) ++ sortByLine(laterErrs)
  }

}
