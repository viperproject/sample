package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.td.compiler.TouchCompiler

/**
 * Exports the analysis result to a string of some format (HTML, TSV, JSON etc.)
 *
 * @author Lucas Brutschy
 */
trait ErrorExporter {

  /**
   * Exports all analysis results
   * @param compiler the compiler listing all analyzed scripts
   * @return analysis results in the desired format
   */
  def apply(compiler:TouchCompiler):String

  /**
   * Exports analysis results for a specific script
   * @param compiler the compiler listing all analyzed scripts
   * @param id identifier of the script, e.g. public id or filename
   * @return analysis results in the desired format
   */
  def apply(compiler:TouchCompiler,id:String):String

  def getExtension:String

}

/**
 * An exception that occurs during exporting
 *
 * @param msg Some message.)
 *
 * @author Lucas Brutschy
 */
case class ExporterException(msg:String) extends Exception(msg)