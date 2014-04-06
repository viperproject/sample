package ch.ethz.inf.pm.sample.backwardanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{State, ExpressionSet}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.execution.AbstractErrorInfo


/**
 * Global state (singleton, see SystemParameters) used for backward analysis.
 *
 * TODO: Would be nice if Sample/TouchBoost architecture could be improved
 *       so that this global state is not necessary in this form
 */
class BackwardAnalysisContext[S <: State[S]] {
  /** All collected abstract errors */
  var errors: Map[(ProgramPoint, ExpressionSet),List[AbstractErrorInfo[S]]] = Map.empty

  /** Collected sources of non-determinism */
  var nonDeterminismSources: Set[NonDetSource] = Set.empty

  /**
   * Hacky flag to globally signal whether a backward analysis is running.
   * E.g. we do not want to report alarms again during backward analysis!
   */
  var runningBackward = false

  /** Current abstract error under investigation */
  var currentError: Option[AbstractErrorInfo[S]] = None

  def hasErrors: Boolean = !errors.isEmpty

  /** Registers and abstract error */
  def registerError(err: AbstractErrorInfo[_]): Unit = {
    val foundError = err.asInstanceOf[AbstractErrorInfo[S]]
    val errorId = (foundError.pp, foundError.errorExpr)
    val ppErrors = errors.getOrElse(errorId, Nil)
    errors += errorId -> (ppErrors :+ foundError)
  }
}

/**
 * Describes a source of non-determinism in the program. Used before an actual identifier is created,
 * e.g. for the results of the NonDeterminismAnalysis
 */
case class NonDetSource(pp: ProgramPoint, typ: Type, summary: Boolean = false, valid: Boolean = true) {
  override def toString: String = s"NonDetSource($pp, $typ, summary: $summary)"
}