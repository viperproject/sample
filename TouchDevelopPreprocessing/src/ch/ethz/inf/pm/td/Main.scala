package ch.ethz.inf.pm.td

import ch.ethz.inf.pm.td.output.FileSystemExporter
import ch.ethz.inf.pm.td.analysis.TouchApronRun

object Main {

  def main(args: Array[String]) {

    val ExportPath = "-exportPath=(.*)".r

    val nonOptions = (for (arg <- args) yield {
      arg match {
        case ExportPath(x) => FileSystemExporter.exportPath = x; None
        case "-json" => FileSystemExporter.exportAsJSON = true; None
        case "-njson" => FileSystemExporter.exportAsJSON = false; None
        case "-html" => FileSystemExporter.exportAsHTML = true; None
        case "-nhtml" => FileSystemExporter.exportAsHTML = false; None
        case "-tsv" => FileSystemExporter.exportAsTSV = true; None
        case "-ntsv" => FileSystemExporter.exportAsTSV = false; None
        case _ => Some(arg)
      }
    }).flatten

    TouchApronRun.main(nonOptions)

  }

}