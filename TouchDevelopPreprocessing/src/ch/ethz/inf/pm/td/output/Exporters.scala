package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.td.compiler.TouchCompiler

/**
 * Exports results into various formats
 *
 * @author Lucas Brutschy
 */
object Exporters {

  var jobID: String = System.nanoTime().toString

  /**
   * Enable HTML output
   */
  var exportAsHTML = true

  /**
   * Export analysis result in a tab-separated value format
   */
  var exportAsTSV = false

  /**
   * Export the resulting json records representing error information
   */
  var exportAsJSON = false

  /**
   * Export to MongoDB
   */
  var exportToMongo = false

  def apply(compiler: TouchCompiler) {
    if (exportAsHTML) new HTMLExporter().exportWarnings(compiler)
    if (exportAsTSV) new TSVExporter().exportWarnings(compiler)
    if (exportAsJSON) new JSONExporter().exportWarnings(compiler)
    if (exportToMongo) new MongoExporter().exportWarnings(compiler)
  }

  def setStatus(s: String) = {
    if (exportToMongo) new MongoExporter().setStatus(s)
  }

  def setDebugInformation(s: String) = {
    if (exportToMongo) new MongoExporter().setDebugInformation(s)
  }

}