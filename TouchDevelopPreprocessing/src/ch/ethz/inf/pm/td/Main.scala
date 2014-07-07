package ch.ethz.inf.pm.td

import ch.ethz.inf.pm.td.analysis.TouchApronRun
import ch.ethz.inf.pm.td.output.{Exporters, FileSystemExporter}

object Main {

  def main(args: Array[String]) {

    var watchMode = false
    val ExportPath = "-exportPath=(.*)".r
    val JobID = "-jobID=(.*)".r

    val nonOptions = (for (arg <- args) yield {
      arg match {
        case ExportPath(x) => FileSystemExporter.exportPath = x; None
        case "-json" => Exporters.exportAsJSON = true; None
        case "-njson" => Exporters.exportAsJSON = false; None
        case "-html" => Exporters.exportAsHTML = true; None
        case "-nhtml" => Exporters.exportAsHTML = false; None
        case "-tsv" => Exporters.exportAsTSV = true; None
        case "-ntsv" => Exporters.exportAsTSV = false; None
        case "-mongo" => Exporters.exportToMongo = true; None
        case "-nmongo" => Exporters.exportToMongo = false; None
        case JobID(x) => Exporters.jobID = x; None
        case "-watchMode" => watchMode = true; None
        case _ => Some(arg)
      }
    }).flatten

    if (!watchMode) {
      TouchApronRun.main(nonOptions)
    } else {
      runWatchMode()
    }

  }

  /**
   *
   * Watches the mongo database for incoming jobs
   *
   */
  def runWatchMode() {

    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", 27017)
    val collection = mongoClient("tb")("analysisJobs")

    while (true) {

      collection.findAndModify(MongoDBObject("status" -> "todo"), $set("status" -> "running")) match {
        case Some(x) =>
          Exporters.jobID = x.get("jobID").toString
          TouchApronRun.main(List(x.get("url").toString).toArray)
        case _ =>
          Thread.sleep(1000)
      }

    }

  }

}