package ch.ethz.inf.pm.td

import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.output.{Exporters, FileSystemExporter}

object Main {

  def main(args: Array[String]) {

    var watchMode = false
    val ExportPath = "-exportPath=(.*)".r
    val JobID = "-jobID=(.*)".r
    val Timeout = "-timeout=(.*)".r

    val nonOptions = (for (arg <- args) yield {
      arg match {
        case ExportPath(x) => FileSystemExporter.exportPath = x; None
        case "-json" => Exporters.exportAsJSON = true; None
        case "-no-json" => Exporters.exportAsJSON = false; None
        case "-html" => Exporters.exportAsHTML = true; None
        case "-no-html" => Exporters.exportAsHTML = false; None
        case "-tsv" => Exporters.exportAsTSV = true; None
        case "-no-tsv" => Exporters.exportAsTSV = false; None
        case "-mongo" => Exporters.exportToMongo = true; None
        case "-no-mongo" => Exporters.exportToMongo = false; None
        case "-fast" => setFastMode(); None
        case "-no-fast" => unsetFastMode(); None
        case Timeout(x) => TouchAnalysisParameters.timeout = Some(x.toInt); None
        case "-no-timeout" => TouchAnalysisParameters.timeout = None; None
        case JobID(x) => Exporters.jobID = x; None
        case "-watchMode" => watchMode = true; None
        case _ => Some(arg)
      }
    }).flatten

    if (!watchMode) {
      TouchRun.main(nonOptions)
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

      collection.findAndModify(MongoDBObject("status" -> "Waiting"), $set("status" -> "Initializing")) match {
        case Some(x) =>
          TouchAnalysisParameters.timeout = x.getAs[Int]("timeout")
          if(x.getAsOrElse[Boolean]("fast", false)) setFastMode() else unsetFastMode()
          Exporters.jobID = x.getAsOrElse[String]("jobID", System.currentTimeMillis().toString)
          TouchRun.main(x.getAs[String]("url").toArray)
        case _ =>
          Thread.sleep(1000)
      }

    }

  }

  def setFastMode() {

    TouchAnalysisParameters.numberOfVersions = 1
    TouchAnalysisParameters.set(
      TouchAnalysisParameters(
        execution = ExecutionModelParams(
          //localizeStateOnMethodCall = true, BUGGY
          singleExecution = true,
          prematureAbortion = false,
          //singleEventOccurrence = true, BUGGY
          contextSensitiveInterproceduralAnalysis = false
        ),
        domains = DomainParams(
          collectionsSummarizeElements = true,
          collectionsSummarizeLinearElements = true,
          numericalDomain = NumericDomainChoice.Intervals)
      )
    )

  }

  def unsetFastMode() {

    TouchAnalysisParameters.set(
      TouchAnalysisParameters()
    )

  }

}