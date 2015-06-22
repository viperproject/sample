package ch.ethz.inf.pm.td

import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.output.{Exporters, FileSystemExporter}
import ch.ethz.inf.pm.td.webapi.NewScripts

/**
 * Defines the commandline interface for TouchGuru
 */
object Main {

  /**
   * Defines the modes of the commandline interfac
   *
   * - Default:   Reads a list of files or script ids from the command-line and analyses them one by one
   * - WatchMode: Watches a the mongodb for incoming analysis jobs (nonterminating)
   * - Help:      Prints a list of options and exits
   * - Feeder:    Feeds analysis jobs into the database
   */
  object Mode extends Enumeration {
    type WeekDay = Value
    val Default, WatchMode, Help, FeedMode, Statistics = Value
  }

  def main(args: Array[String]) {

    var mode:Mode.Value = Mode.Default
    val ExportPath = "-exportPath=(.*)".r
    val JobID = "-jobID=(.*)".r
    val Timeout = "-timeout=(.*)".r

    val nonOptions = args.filter{
      case ExportPath(x) => FileSystemExporter.exportPath = x; false
      case "-json" => Exporters.exportAsJSON = true; false
      case "-no-json" => Exporters.exportAsJSON = false; false
      case "-html" => Exporters.exportAsHTML = true; false
      case "-no-html" => Exporters.exportAsHTML = false; false
      case "-tsv" => Exporters.exportAsTSV = true; false
      case "-no-tsv" => Exporters.exportAsTSV = false; false
      case "-mongo" => Exporters.exportToMongo = true; false
      case "-no-mongo" => Exporters.exportToMongo = false; false
      case "-fast" => setFastMode(); false
      case "-no-fast" => unsetFastMode(); false
      case Timeout(x) => TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(timeout = Some(x.toInt))); false
      case "-no-timeout" => TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(timeout = None)); false
      case JobID(x) => Exporters.jobID = x; false
      case "-watchMode" => mode = Mode.WatchMode; false
      case "-help" => mode = Mode.Help; false
      case "-feedMode" => mode = Mode.FeedMode; false
      case "-statistics" => mode = Mode.Statistics; false
      case _ => true
    }

    mode match {
      case Mode.Default =>
        TouchRun.main(nonOptions)
      case Mode.FeedMode =>
        runFeedMode(nonOptions)
      case Mode.Help =>
        printHelp(nonOptions)
      case Mode.WatchMode =>
        runWatchMode(nonOptions)
      case Mode.Statistics =>
        printStatistics(nonOptions)
    }

  }

  /**
   *
   * Prints the help screen
   *
   */
  def printHelp(args: Array[String]) {

    println(
      """
        | TouchGuru commandline interface.
        |
        | Possible Modes:
        |  (1) Default
        |    Reads a list of files or script ids from the command-line and analyses them one by one
        |
        |  (2) WatchMode, enabled by -watchMode
        |    Watches a the mongodb for incoming analysis jobs (nonterminating)
        |    Allows options as above
        |
        |  (3) FeedMode, enabled by -feedMode
        |    Constantly feeds new analysis jobs into the database
        |    either
        |      -reanalyzeFailing  Reanalyzes scripts that failed before
        |    or it adds new scripts, which may take
        |      -skipPrevious      Skips scripts that exist in the database (otherwise, they are reanalyzed)
        |    By default, at most 2 new scripts are added. Change with
        |      -parallelTasks=[NUM]
        |
        |  (4) Help, enabled by -help
        |    Prints this
        |
        |  (5) Statistics, enabled by -statistics
        |    Prints statistics about the database
        |
      """.stripMargin)

  }

  /**
   *
   * Prints statistics about the analysis results in the local mongo database
   *
   */
  def printStatistics(args: Array[String]) {

    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", 27017)
    val collection = mongoClient("tb")("analysisJobs")

    println("Successful scripts: " + collection.count(MongoDBObject("status" -> "Done")))
    println("Failed scripts:     " + collection.count(MongoDBObject("status" -> "Failed")))
    println("Waiting scripts:    " + collection.count(MongoDBObject("status" -> "Waiting")))
    var map = Map.empty[String,Set[String]]
    for (x <- collection.find(MongoDBObject("status" -> "Failed"))) {
      val error =  x.getAsOrElse[String]("debug","").split("\n").head
      val script = x.getAsOrElse[String]("url","").split("\n").head
      map = map + (error -> (map.getOrElse(error,Set.empty) + script))
    }
    println(map.toList.sortBy{ _._2.size }.map{x => x._2.size+"\t||"+x._1+"\t||"+x._2.mkString(",")}.mkString("\n"))

  }

  /**
   *
   * Constantly inserts new jobs into the database
   *
   */
  def runFeedMode(args: Array[String]) {

    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", 27017)
    val collection = mongoClient("tb")("analysisJobs")

    val query = new NewScripts

    var reanalyzeFailing = false
    var skipPrevious = false
    var parallelTasks = 2
    val ParallelTasksOption = "-parallelTasks=([\\d]+)".r
    args filter {

      case ParallelTasksOption(x) => parallelTasks = x.toInt; false
      case "-skipPrevious" =>        skipPrevious = true;     false
      case "-reanalyzeFailing" =>    reanalyzeFailing = true;          false
      case _ => true

    }

    if (reanalyzeFailing) {

      println("Reanalyzing failing scripts")
      for (x <- collection.find(MongoDBObject("status" -> "Failed"))) {
        println("Reanalyzing "+x.getAsOrElse[String]("url","(no url??)"))
        collection.findAndModify(x,$set("status" -> "Waiting", "debug" -> "", "result" -> MongoDBObject(), "html" -> ""))
      }

    } else {

      println("Adding fresh scripts")

      for (script <- query) {

        if (collection.count(MongoDBObject("status" -> "Waiting")) < parallelTasks) {
          collection.findOne(MongoDBObject("url" -> ("td://"+script.id))) match {
            case Some(x) =>
              if (!skipPrevious) {
                println("Enabled an existing record for "+script.id)
                collection.findAndModify(x,$set("status" -> "Waiting", "debug" -> "", "result" -> MongoDBObject(), "html" -> ""))
              } else {
                println("Skipping an existing record for "+script.id)
              }
            case None =>
              println("Creating new record for "+script.id)
              collection.insert(MongoDBObject(
                "url" -> ("td://"+script.id),
                "status" -> "Waiting",
                "jobID" -> scala.util.Random.nextString(10),
                "fast" -> false,
                "timeout" -> 40
              ))
          }
        } else {
          println("Enough tasks for now")
          Thread.sleep(1000)
        }
      }

    }
  }

  /**
   *
   * Watches the mongo database for incoming jobs
   *
   */
  def runWatchMode(args: Array[String]) {

    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", 27017)
    val collection = mongoClient("tb")("analysisJobs")

    while (true) {

      collection.findAndModify(MongoDBObject("status" -> "Waiting"), $set("status" -> "Initializing")) match {
        case Some(x) =>
          TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(timeout = x.getAs[Int]("timeout")))
          if(x.getAsOrElse[Boolean]("fast", false)) setFastMode() else unsetFastMode()
          Exporters.jobID = x.getAsOrElse[String]("jobID", System.currentTimeMillis().toString)
          TouchRun.main(x.getAs[String]("url").toArray)
        case _ =>
          Thread.sleep(200)
      }

    }

  }

  def setFastMode() {

    TouchAnalysisParameters.set(
      TouchAnalysisParameters(
          //localizeStateOnMethodCall = true, BUGGY
          //singleExecution = true,
          prematureAbortion = false,
          //singleEventOccurrence = true, BUGGY
          contextSensitiveInterproceduralAnalysis = false,
          collectionsSummarizeElements = true,
          collectionsSummarizeLinearElements = true,
//          numericalDomain = NumericDomainChoice.Intervals
          numberOfVersions = 1
        )
    )

  }

  def unsetFastMode() {

    TouchAnalysisParameters.set(
      TouchAnalysisParameters()
    )

  }

}