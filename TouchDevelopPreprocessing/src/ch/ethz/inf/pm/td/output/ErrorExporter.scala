/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import java.io.{File, FileWriter, PrintWriter}

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.compiler.TouchCompiler

/**
 * Exports the analysis result to a string of some format (HTML, TSV, JSON etc.)
 *
 * @author Lucas Brutschy
 */
trait ErrorExporter {

  def setDebugInformation(s: String)

  def setStatus(s: String)

  def exportWarnings(compiler: TouchCompiler)

}

object FileSystemExporter {

  /**
   * Export to the given path
   */
  var exportPath = "/tmp/" + Exporters.jobID

}

trait FileSystemExporter {

  def setDebugInformation(s: String) {

    val dir = new File(FileSystemExporter.exportPath)

    if (dir.isDirectory || dir.mkdir()) {

      val file = new File(FileSystemExporter.exportPath + File.separator + "debug")
      var fw: FileWriter = null
      var pw: PrintWriter = null

      try {

        fw = new FileWriter(file, false)
        pw = new PrintWriter(fw)

        pw.println(s)

      } finally {

        if (pw != null) pw.close()
        if (fw != null) fw.close()

      }

    } else {
      throw ExporterException("Failed to create output directory")
    }
  }

  def setStatus(s: String) {

    val dir = new File(FileSystemExporter.exportPath)

    if (dir.isDirectory || dir.mkdir()) {

      val file = new File(FileSystemExporter.exportPath + File.separator + "status")
      var fw: FileWriter = null
      var pw: PrintWriter = null

      try {

        fw = new FileWriter(file, false)
        pw = new PrintWriter(fw)

        pw.println(s)

      } finally {

        if (pw != null) pw.close()
        if (fw != null) fw.close()

      }

    } else {
      throw ExporterException("Failed to create output directory")
    }
  }

  def exportWarnings(compiler: TouchCompiler) {

    val dir = new File(FileSystemExporter.exportPath)

    if (dir.isDirectory || dir.mkdir()) {

      for (pubID <- compiler.parsedTouchScripts.keys) {

        val file = new File(FileSystemExporter.exportPath + File.separator + pubID + "." + getExtension)
        var fw: FileWriter = null
        var pw: PrintWriter = null

        try {

          fw = new FileWriter(file, true)
          pw = new PrintWriter(fw)

          pw.println(warningsToString(compiler, pubID))

          SystemParameters.progressOutput.put("Exported errors for id " + pubID + " in " + getExtension + " format to " + file.toString)

        } finally {

          if (pw != null) pw.close()
          if (fw != null) fw.close()

        }

      }

    } else {
      throw ExporterException("Failed to create output directory")
    }
  }

  /**
   * Exports all analysis results
   * @param compiler the compiler listing all analyzed scripts
   * @return analysis results in the desired format
   */
  def warningsToString(compiler: TouchCompiler): String

  /**
   * Exports analysis results for a specific script
   * @param compiler the compiler listing all analyzed scripts
   * @param id identifier of the script, e.g. public id or filename
   * @return analysis results in the desired format
   */
  def warningsToString(compiler: TouchCompiler, id: String): String

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