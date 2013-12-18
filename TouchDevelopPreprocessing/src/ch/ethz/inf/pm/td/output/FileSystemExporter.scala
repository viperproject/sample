package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.td.compiler.TouchCompiler
import java.io.{PrintWriter, FileWriter, File}

/**
 * Exports results into various formats
 *
 * @author Lucas Brutschy
 */
object FileSystemExporter {

  /**
   * Export to the given path
   */
  var exportPath = "/tmp/"+System.nanoTime()

  /**
   * Enable HTML output
   */
  var exportAsHTML = true

  /**
   * Export analysis result in a tab-separated value format
   */
  var exportAsTSV = true

  /**
   * Export the resulting json records representing error information
   */
  var exportAsJSON = true


  def apply(compiler:TouchCompiler) {
    if (exportAsHTML) exportToFile(new HTMLExporter(),compiler)
    if (exportAsTSV)  exportToFile(new TSVExporter(),compiler)
    if (exportAsJSON) exportToFile(new JSONExporter(),compiler)
  }

  def exportToFile(exporter:ErrorExporter, compiler:TouchCompiler) {

    val dir = new File(exportPath)

    if (dir.isDirectory || dir.mkdir()) {

      for (pubID <- compiler.parsedNames) {

        val file = new File(exportPath+File.separator+pubID+"."+exporter.getExtension)
        var fw:FileWriter = null
        var pw:PrintWriter = null

        try {

          fw = new FileWriter(file, true)
          pw = new PrintWriter(fw)

          pw.println(exporter(compiler,pubID))

          println("Exported errors for id "+pubID+" in "+exporter.getExtension+" format to "+file.toString)

        } catch {

          case e:Exception =>
            throw new ExporterException("Failed to write id "+pubID+" in "+exporter.getExtension+": "+e.toString)

        } finally {

          if(pw != null) pw.close()
          if(fw != null) fw.close()

        }

      }

    } else {
      throw new ExporterException("Failed to create output directory")
    }
  }

}