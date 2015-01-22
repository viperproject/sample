package ch.ethz.inf.pm.td.analysis

/**
 * TODO: This class is annoyingly overengineered
 */
object TouchAnalysisParameters {

  val numberOfVersions = 2

  val numberOfUnrollings = 0

  /**
   * Lets the analysis timeout after a number of seconds
   */
  var timeout: Option[Int] = None

  private var currentParams: TouchAnalysisParameters = TouchAnalysisParameters()

  def get: TouchAnalysisParameters = currentParams

  /**
   * Sets the TouchAnalysisParameters to be used during the analysis. This method should ONLY be called once
   * by test suites when setting up the analysis with non-standard parameters
   */
  def set(params: TouchAnalysisParameters) = {
    currentParams = params
  }

  // Accessors

  def argumentsToPublicMethodsValid = currentParams.execution.argumentsToPublicMethodsValid

  def contextSensitiveInterproceduralAnalysis = currentParams.execution.contextSensitiveInterproceduralAnalysis

  def libraryFieldPruning = currentParams.libraryFieldPruning

  def generalPersistentState = currentParams.execution.generalPersistentState

  def singleExecution = currentParams.execution.singleExecution

  def fullAliasingInGenericInput = currentParams.execution.fullAliasingInGenericInput

  def prematureAbortion = currentParams.execution.prematureAbortion

  def resetEnv = currentParams.execution.resetEnv

  def singleEventOccurrence = currentParams.execution.singleEventOccurrence

  def treatPrivateMethodLikePublicMethods = currentParams.execution.treatPrivateMethodLikePublicMethods

  def stringRepresentationBound = currentParams.domains.stringRepresentationBound

  def localizeStateOnMethodCall = currentParams.execution.localizeStateOnMethodCall

  def reportNoncriticalParameterBoundViolations = currentParams.reporting.reportNoncriticalParameterBoundViolations

  def reportDummyImplementations = currentParams.reporting.reportDummyImplementations

  def reportNumericalErrors = currentParams.reporting.reportNumericalErrors

  def reportPrematurelyOnInternetAccess = currentParams.reporting.reportPrematurelyOnInternetAccess

  def silent = currentParams.reporting.silent

  def enableCollectionMustAnalysis = currentParams.domains.enableCollectionMustAnalysis

  def enableCollectionSummaryAnalysis = currentParams.domains.enableCollectionSummaryAnalysis

  def printValuesInWarnings = currentParams.reporting.printValuesInWarnings

  def reportOnlyAlarmsInMainScript = currentParams.reporting.reportOnlyAlarmsInMainScript

  // TODO: Move to muted fields
  def topFields = currentParams.topFields


}

/**
 * Parameters for the TouchBoost Analysis
 *
 * They are kept in case classes for several reasons:
 * 1) Individual settings are immutable and type-safe like vals in an object.
 * 2) Concise construction: Change a few options, keep all other parameters.
 * Example: TouchAnalysisParameters(reporting = ReportingParams(reportNumericalErrors = true)) to enable checking
 * for numerical errors.
 *
 * Individual parameters are naturally grouped for better overview, and because case classes only support
 * up to 22 arguments :)
 */
case class TouchAnalysisParameters(
                                    execution: ExecutionModelParams = ExecutionModelParams(),
                                    domains: DomainParams = DomainParams(),
                                    reporting: ReportingParams = ReportingParams(),

                                    /**
                                     * If this is enabled, only relevant fields (i.e. that are read in the program) of
                                     * objects from the library will be represented.
                                     */
                                    libraryFieldPruning: Boolean = true,

                                    // Fields that are always TOP
                                    topFields: Set[String] = Set("x", "y", "z", "z index", "speed x", "speed y",
                                      "speed z", "width", "height", "acceleration x", "acceleration y", "angle", "radius",
                                      "angular speed", "leaderboard score", "opacity", "duration", "font size", "text", "item", "speed")
                                    )

case class ExecutionModelParams(
                                 /**
                                  * When a public method (which can be executed by the user) has parameters, they are
                                  * almost always expected to be non-invalid. Technically this is a bug, but something
                                  * that nobody will fix. This parameter turns on the assumption that parameters to
                                  * public methods are valid (if run by the user or when a library is called).
                                  *
                                  * Is that function called from a different function which might pass an invalid value,
                                  * this error is still reported.
                                  */
                                 argumentsToPublicMethodsValid: Boolean = true,

                                 /**
                                  * Context-sensitivity in the interprocedural analysis.
                                  */
                                 contextSensitiveInterproceduralAnalysis: Boolean = true,

                                 /**
                                  * The default behavior of TouchBoost is to initialize the global state to invalid
                                  * ("first run") and then compute the fixpoint over an arbitrary number of
                                  * script executions.
                                  *
                                  * When this option is set to true, we will initialize the global state to Top instead
                                  * ("any run") and just analyze a single execution of the script. This is faster,
                                  * but less precise.
                                  *
                                  * EXPERIMENTAL
                                  */
                                 generalPersistentState: Boolean = false,

                                 /**
                                  * Analyze a single execution
                                  */
                                 singleExecution: Boolean = false,

                                 fullAliasingInGenericInput: Boolean = false,

                                 /**
                                  * Take into account premature abortion. This means, the persistent data at any
                                  * program point will be included in the entry state.
                                  */
                                 prematureAbortion: Boolean = true,

                                 /**
                                  * Reset assumptions about environment between events / public methods
                                  */
                                 resetEnv: Boolean = false,

                                 /**
                                  * The default behavior of TouchBoost is to compute the fixpoint over an arbitrary
                                  * number of event occurrences for each execution.
                                  *
                                  * When this option is set to true, we will analyze each event once with the
                                  * top global state.
                                  *
                                  * EXPERIMENTAL
                                  */
                                 singleEventOccurrence: Boolean = false,

                                 /**
                                  *
                                  * Treat private methods just like public methods
                                  *
                                  * That means, assume that they can be run by the user - analyze them separately
                                  * with top entryState
                                  *
                                  */
                                 treatPrivateMethodLikePublicMethods: Boolean = false,


                                 /**
                                  * If this is set to true, we remove all local variables from the calling context when
                                  * entering a function. This improves performance, but may reduce precision (since
                                  * relations between passed values and removed values may be lost).
                                  *
                                  * TL;DR:
                                  * true = less precision, more speed.
                                  * false = more precision, less speed.
                                  */
                                 localizeStateOnMethodCall: Boolean = false

                                 )

case class DomainParams(enableCollectionMustAnalysis: Boolean = true,
                        enableCollectionSummaryAnalysis: Boolean = false,

                        /**
                         * Maximum number of possible string values represented for a single variable
                         */
                        stringRepresentationBound: Int = 3,

                        /**
                         * The numerical domain to be used.
                         *
                         * IMPORTANT: This parameter is ONLY respected when using
                         * TouchApronRun/TestRunner (important for test suites), but not the GUI.
                         */
                        numericalDomain: NumericDomainChoice.Value = NumericDomainChoice.Pentagons
                         )

object NumericDomainChoice extends Enumeration {
  type NumericDomainChoice = Value

  val Intervals = Value
  val Octagons = Value
  val Polyhedra = Value
  val StrictPolyhedra = Value
  val Pentagons = Value
}

case class ReportingParams(reportNoncriticalParameterBoundViolations: Boolean = false,
                           reportDummyImplementations: Boolean = false,
                           reportNumericalErrors: Boolean = false,
                           reportPrematurelyOnInternetAccess: Boolean = false,

                           /**
                            * do not report errors in libraries
                            */
                           reportOnlyAlarmsInMainScript: Boolean = false,

                           /**
                            * If this is true, the analysis will print something like
                            * "parameter X ("somevalue",invalid) may be invalid"
                            * This is helpful, but may confuse users and duplicate warnings
                            */
                           printValuesInWarnings: Boolean = false,

                           /** If true, suppress as much output as possible  (useful for testing) */
                           silent: Boolean = false
                            )