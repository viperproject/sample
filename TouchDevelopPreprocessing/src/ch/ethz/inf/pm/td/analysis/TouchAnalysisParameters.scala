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
   * Context-sensitivity in the interprocedural analysis
   */
  val contextSensitiveInterproceduralAnalysis = false

  /**
   * Enable HTML output
   */
  val exportAsHtml = true

}
