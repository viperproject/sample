package ch.ethz.inf.pm.td.analysis

/**
 * User: lucas
 * Date: 3/4/13
 * Time: 6:25 PM
 */
object TouchAnalysisParameters {

  /**
   * When a public method (which can be executed by the user) has parameters, they are almost always expected to be
   * non-invalid. Technically this is a bug, but something that nobody will fix. This parameter turns on the
   * assumption that parameters to public methods are valid (if run by the user or when a library is called).
   *
   * Is that function called from a different function which might pass an invalid value, this error is still reported.
   */
  val argumentsToPublicMethodsValid = true

  /**
   * Context-sensitivity in the interprocedural analysis.
   */
  val contextSensitiveInterproceduralAnalysis = false

  /**
   * Enable HTML output
   */
  val exportAsHtml = true
  val exportAsTSV = true

  /**
   * Print the resulting json records representing error information
   */
  var printJsonErrorRecords = false

  /**
   * If this is enabled, only relevant fields (i.e. that are read in the program) of objects from the library will be
   * represented.
   */
  val libraryFieldPruning = true

  /**
   * The default behavior of TouchBoost is to initialize the global state to invalid ("first run")
   * and then compute the fixpoint over an arbitrary number of script executions.
   *
   * When this option is set to true, we will initialize the global state to Top instead ("any run")
   * and just analyze a single execution of the script. This is faster, but less precise.
   *
   * EXPERIMENTAL
   */
  val singleExecution = false

  val fullAliasingInGenericInput = false

  /** take into account premature abortion.
    * This means, the persistent data at any program point will be included in the entry state. */
  val prematureAbortion = true

  /**
   * Reset assumptions about environment between events / public methods
   */
  val resetEnv = true

  /**
   * The default behavior of TouchBoost is to compute the fixpoint over an arbitrary number of event occurrences
   * for each execution.
   *
   * When this option is set to true, we will analyze each event once with the top global state.
   *
   * EXPERIMENTAL
   */
  val singleEventOccurrence = false

  /**
   *
   * Treat private methods just like public methods
   *
   * That means, assume that they can be run by the user - analyze them separately with top entryState
   *
   */
  val treatPrivateMethodLikePublicMethods = false

  /**
   * Maximum number of possible string values represented for a single variable
   */
  val stringRepresentationBound = 3

  /**
   * If this is set to true, we remove all local variables from the calling context when entering a function.
   * This improves performance, but may reduce precision (since relations between passed values and removed
   * values may be lost).
   *
   * TL;DR:
   *  true = less precision, more speed.
   *  false = more precision, less speed.
   */
  val localizeStateOnMethodCall = false

  val reportNoncriticalParameterBoundViolations = false

  val reportDummyImplementations = false

  val enableCollectionMustAnalysis = false

  val enableCollectionSummaryAnalysis = false

  val warnPrematurelyOnInternetAccess = false
  /**
   * If this is true, the analysis will print something like "parameter X ("somevalue",invalid) may be invalid"
   * This is helpful, but may confuse users and duplicate warnings
   */
  val printValuesInWarnings = false

  /** do not report errors in libraries */
  val reportOnlyAlarmsInMainScript = true

  // Fields that are always TOP
  val topFields = Set("x","y","z","z index","speed x","speed y","speed z","width","height","acceleration x",
    "acceleration y","angle","angular speed","leaderboard score")

}
