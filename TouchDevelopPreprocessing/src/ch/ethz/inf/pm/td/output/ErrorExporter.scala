/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import java.io.{File, FileWriter, PrintWriter}

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.execution.AnalysisResult
import ch.ethz.inf.pm.td.compiler.TouchCompiler


object FileSystemExporter {

  /**
    * Export to the given path
    */
  var exportPath: String = "/tmp/" + Exporters.jobID

  def export(fileName: String, contents: String): String = {

    val dir = new File(FileSystemExporter.exportPath)

    if (dir.isDirectory || dir.mkdir()) {

      val file = new File(FileSystemExporter.exportPath + File.separator + fileName)
      var fw: FileWriter = null
      var pw: PrintWriter = null

      try {

        fw = new FileWriter(file, false)
        pw = new PrintWriter(fw)

        pw.println(contents)

      } finally {

        if (pw != null) pw.close()
        if (fw != null) fw.close()

      }

      file.getAbsolutePath

    } else {
      throw ExporterException("Failed to create output directory")
    }

  }
}

trait FileSystemResultExporter extends ResultExporter {

  override def exportResults(compiler: TouchCompiler, results: List[AnalysisResult]) {
    for (pubID <- compiler.parsedTouchScripts.keys) {
      val file = FileSystemExporter.export(pubID + "." + getExtension, warningsToString(compiler, pubID))
      SystemParameters.progressOutput.put("Exported errors for id " + pubID + " in " + getExtension + " format to " + file.toString)
    }
  }
  /**
    * Exports analysis results for a specific script
    *
    * @param compiler the compiler listing all analyzed scripts
    * @param id       identifier of the script, e.g. public id or filename
    * @return analysis results in the desired format
    */
  def warningsToString(compiler: TouchCompiler, id: String): String

  def getExtension: String

}


/**
  * An exception that occurs during exporting
  *
  * @param msg Some message.)
  * @author Lucas Brutschy
  */
case class ExporterException(msg: String) extends Exception(msg)