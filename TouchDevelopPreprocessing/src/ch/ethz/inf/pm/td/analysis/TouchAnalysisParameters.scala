package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.td.analysis.LibraryErrorReportingMode.LibraryErrorReportingMode

object TouchAnalysisParameters {

  // == SET AND GET CURRENT PARAMS ==

  private var cur: TouchAnalysisParameters = TouchAnalysisParameters()

  def get: TouchAnalysisParameters = cur

  /**
   * Sets the TouchAnalysisParameters to be used during the analysis. This method should ONLY be called once
   * by test suites when setting up the analysis with non-standard parameters
   */
  def set(params: TouchAnalysisParameters) = {
    cur = params
  }

}

/**
 * Parameters for the TouchGuru Analysis
 *
 * They are kept in case classes for several reasons:
 * 1) Individual settings are immutable and type-safe like vals in an object.
 * 2) Concise construction: Change a few options, keep all other parameters.
 * Example: TouchAnalysisParameters(reporting = ReportingParams(reportNumericalErrors = true)) to enable checking
 * for numerical errors.
 */
case class TouchAnalysisParameters(

                                    /**
                                     * If this is enabled, only relevant fields (i.e. that are read in the program) of
                                     * objects from the library will be represented.
                                     */
                                    libraryFieldPruning: Boolean = true,

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
                                     * The default behavior of TouchGuru is to initialize the global state to invalid
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
                                    prematureAbort: Boolean = true,

                                    /**
                                     * Reset assumptions about environment between events / public methods
                                     */
                                    resetEnv: Boolean = false,

                                    /**
                                     * The default behavior of TouchGuru is to compute the fixpoint over an arbitrary
                                     * number of event occurrences for each
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
                                    reachabilityBasedLocalization: Boolean = true,

                                    /**
                                     * If this is set to true, we use the results of a preanalysis to determine what
                                     * objects are read in a method call.
                                     *
                                     * See VMCAI'11 Oh, Brutschy, Yi: Access-analysis based tight localization of abstract memories
                                     */
                                    accessBasedLocalization: Boolean = true,

                                    /**
                                     * In numerical relational analysis, only relate those packs of variables that may belong together
                                     * according to a preanalysis (currently, all sets of variables that may appear in the same
                                     * assume/assign.
                                     */
                                    variablePacking: Boolean = true,

                                    /**
                                     * If the semantics of an API is not defined, default to an unsound solution
                                     * which excludes invalid values
                                     */
                                    defaultToUnsound: Boolean = true,


                                    /**
                                     * use hand-written library contracts
                                     */
                                    useLibraryContracts: Boolean = false,

                                    /**
                                     * Unsoundly assume that we do not have to create a copy of the collection
                                     * when entering a foreach loop
                                     */
                                    assumeCollectionsNotModifiedDuringIteration: Boolean = true,

                                    /**
                                     * Unrolls loops n times (0 means no unrolling) before analysis
                                     */
                                    numberOfUnrollings: Int = 0,

                                    /**
                                     * Lets the analysis timeout after a number of seconds
                                     */
                                    timeout: Option[Int] = None,
                                    collectionsSummarizeLinearElements: Boolean = true,
                                    collectionsSummarizeElements: Boolean = true,

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
                                    numericalDomain: NumericDomainChoice.Value = NumericDomainChoice.OptOctagons,

                                    /**
                                     * The number of heap nodes to store for each program point.
                                     *
                                     * 2 means, we have 1 node, 2 nodes or 1 node and 1 summary node.
                                     */
                                    numberOfVersions: Int = 2,

                                    reportNoncriticalParameterBoundViolations: Boolean = false,
                                    reportDummyImplementations: Boolean = true,
                                    reportNumericalErrors: Boolean = false,
                                    reportPrematurelyOnInternetAccess: Boolean = false,

                                    /**
                                     * Do not report errors in libraries.
                                     */
                                    libraryErrorReportingMode: LibraryErrorReportingMode = LibraryErrorReportingMode.ReportAtBoundary,

                                    /**
                                     * If this is true, the analysis will print something like
                                     * "parameter X ("somevalue",invalid) may be invalid"
                                     * This is helpful, but may confuse users and duplicate warnings
                                     */
                                    printValuesInWarnings: Boolean = false,

                                    /** If true, suppress as much output as possible  (useful for testing) */
                                    silent: Boolean = false,

                                    /**
                                     * Report whenever functions of the API are not defined in our tool
                                     */
                                    reportUnanalyzedFunctions: Boolean = true,

                                    /**
                                     * Includes library stable component in program point
                                     * For potential integration with editor
                                     * May be incompatible with HTML reporting
                                     */
                                    includeLibraryStableComponent: Boolean = false

                                    )

object LibraryErrorReportingMode extends Enumeration {

  type LibraryErrorReportingMode = Value

  val Suppress = Value
  val ReportAtBoundary = Value
  val Report = Value

}

object NumericDomainChoice extends Enumeration {
  type NumericDomainChoice = Value

  val Intervals = Value
  val Octagons = Value
  val OptOctagons = Value
  val Polyhedra = Value
  val StrictPolyhedra = Value
  val Pentagons = Value
}