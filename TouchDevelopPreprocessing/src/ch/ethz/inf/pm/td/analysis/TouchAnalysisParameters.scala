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
  val argumentsToPublicMethodsValid = false

  /**
   * Context-sensitivity in the interprocedural analysis.
   */
  val contextSensitiveInterproceduralAnalysis = false

  /**
   * Enable HTML output
   */
  val exportAsHtml = true

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
  val singleExecution = true

  /**
   * The default behavior of TouchBoost is to compute the fixpoint over an arbitrary number of event occurrences
   * for each execution.
   *
   * When this option is set to true, we will analyze each event once with the top global state.
   *
   * EXPERIMENTAL
   */
  val singleEventOccurrence = true


}
