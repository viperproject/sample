package ch.ethz.inf.pm.td

import java.io.IOException

import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.output.{Exporters, FileSystemExporter}
import ch.ethz.inf.pm.td.tools.{AnalyzeRecords, Instrumentation, FindCloud, FindConstruct}
import ch.ethz.inf.pm.td.webapi._
import com.mongodb.MongoException
import net.liftweb.json.MappingException



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
    type Mode = Value
    val Default, WatchMode, Help, FeedMode, FetchMode, Statistics, FindCloud, Instrument, AnalyzeRecords = Value
  }

  def main(args: Array[String]) {

    var mode:Mode.Value = Mode.Default
    val ExportPath = "-exportPath=(.*)".r
    val JobID = "-jobID=(.*)".r
    val Timeout = "-timeout=(.*)".r

    // MongoSettings
    val MongoServer = "-mongoServer=(.*)".r
    val MongoPort = "-mongoPort=(.*)".r
    val MongoDatabase = "-mongoDatabase=(.*)".r

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
      case "-fetchMode" => mode = Mode.FetchMode; false
      case "-instrument" => mode = Mode.Instrument; false

      case MongoServer(x) => TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(mongoServer = x)); false
      case MongoPort(x) => TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(mongoPort = x.toInt)); false
      case MongoDatabase(x) => TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(mongoDatabase = x)); false

        // Undocumented tools
      case "-findCloud" => mode = Mode.FindCloud; false
      case "-analyzeRecords" => mode = Mode.AnalyzeRecords; false

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
      case Mode.FetchMode =>
        runFetchMode(nonOptions)
      case Mode.Statistics =>
        printStatistics(nonOptions)
      case Mode.FindCloud =>
        FindCloud.main(nonOptions)
      case Mode.AnalyzeRecords =>
        AnalyzeRecords.main(nonOptions)
      case Mode.Instrument =>
        Instrumentation.main(nonOptions)
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
        |    -waitTime=[int] how often to poll, in ms.
        |
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
        |    -dummies   lists all the dummies produced by the analysis
        |    -bottoms   lists all the bottoms produced by the analysis
        |    -debug     lists all exceptions thrown by the analysis
        |    -libs      lists all libs used by applications
        |    -alarms    lists all alarms
        |    -timings   lists the average of all timings
        |
        |  (6) Fetch Mode, enabled by -fetchMode
        |    Downloads all scripts, puts them into the database, and scans for a specific feature
        |    -redownload    redownload existing scripts in database
        |
        |  (6) Instrument Mode, enabled by -instrument
        |    Inserts runtime instrumentation into the script
        |    -print    prints results instead of storing them in the database
        |
      """.stripMargin)

  }

  /**
   *
   * Prints statistics about the analysis results in the local mongo database
   *
   */
  def printStatistics(args: Array[String]) {


    object StatisticsMode extends Enumeration {
      type StatisticsMode = Value
      val Default, Timings, Alarms, Dummies, Bottoms, Debug, Libraries = Value
    }

    var mode = StatisticsMode.Default
    args filter {

      case "-dummies" => mode = StatisticsMode.Dummies;     false
      case "-bottoms" => mode = StatisticsMode.Bottoms;     false
      case "-debug"   => mode = StatisticsMode.Debug;       false
      case "-libs"    => mode = StatisticsMode.Libraries;   false
      case "-alarms"  => mode = StatisticsMode.Alarms;      false
      case "-timings" => mode = StatisticsMode.Timings;     false
      case _ => true

    }

    val settings = TouchAnalysisParameters.get
    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient(settings.mongoServer, settings.mongoPort)
    val collection = mongoClient(settings.mongoDatabase)("analysisJobs")

    mode match {

      case StatisticsMode.Bottoms =>

        var map = scala.collection.mutable.HashMap.empty[String,Set[String]]
        for (x <- collection.find(MongoDBObject("status" -> "Done"))) {
          val script = x.getAsOrElse[String]("url","").split("\n").head
          for (y <- x.getAsOrElse[MongoDBList]("bottoms",MongoDBList.empty)) {
            val mess =  y.asInstanceOf[DBObject].getAsOrElse[String]("message","").split("\n").head
            map += (mess -> (map.getOrElse(mess,Set.empty) + script))
          }
        }
        println(map.toList.sortBy{ _._2.size }.map{x => x._2.size+"\t||"+x._1+"\t||"+x._2.mkString(",")}.mkString("\n"))

      case StatisticsMode.Debug =>

        var map = scala.collection.mutable.HashMap.empty[String,Set[String]]
        for (x <- collection.find(MongoDBObject("status" -> "Failed"))) {
          val error =  x.getAsOrElse[String]("debug","").split("\n").head
          val script = x.getAsOrElse[String]("url","").split("\n").head
          map += (error -> (map.getOrElse(error,Set.empty) + script))
        }
        println(map.toList.sortBy{ _._2.size }.map{x => x._2.size+"\t||"+x._1+"\t||"+x._2.mkString(",")}.mkString("\n"))

      case StatisticsMode.Default =>

        println("Successful scripts: " + collection.count(MongoDBObject("status" -> "Done")))
        println("Failed scripts:     " + collection.count(MongoDBObject("status" -> "Failed")))
        println("Waiting scripts:    " + collection.count(MongoDBObject("status" -> "Waiting")))

      case StatisticsMode.Libraries =>

        var map = scala.collection.mutable.HashMap.empty[String,Set[String]]
        for (x <- collection.find(MongoDBObject("status" -> "Done"))) {
          val script = x.getAsOrElse[String]("url","").split("\n").head
          for (y <- x.getAsOrElse[MongoDBList]("libs",MongoDBList.empty)) {
            val mess =  y.asInstanceOf[DBObject].getAsOrElse[String]("id","").split("\n").head
            map += (mess -> (map.getOrElse(mess,Set.empty) + script))
          }
        }
        println(map.toList.sortBy{ _._2.size }.map{x => x._2.size+"\t||"+x._1+"\t||"+x._2.mkString(",")}.mkString("\n"))

      case StatisticsMode.Timings =>

        val mapAvg = scala.collection.mutable.HashMap.empty[String,Double]
        val mapNumber = scala.collection.mutable.HashMap.empty[String,Int]
        for (x <- collection.find(MongoDBObject("status" -> "Done"))) {
          for (y <- x.getAsOrElse[MongoDBList]("timings",MongoDBList.empty)) {
            for (z <- List("TouchAnalysisMainAnalysis", "TouchAnalysisHeapPreanalysis", "TouchAnalysisLibraryFieldAnalysis")) {
              val time =  y.asInstanceOf[DBObject].getAsOrElse[Long](z,0)
              val num = mapNumber.getOrElse(z,0) + 1
              mapNumber += (z -> num)
              mapAvg += (z -> (mapAvg.getOrElse(z,0.0)+time.toDouble/num))
            }
          }
        }
        println(mapAvg.map{x => x._1+"\t||"+x._2}.mkString("\n"))

      case StatisticsMode.Dummies =>

        var map = scala.collection.mutable.HashMap.empty[String,Set[String]]
        for (x <- collection.find(MongoDBObject("status" -> "Done"))) {
          val script = x.getAsOrElse[String]("url","").split("\n").head
          for (y <- x.getAsOrElse[MongoDBList]("dummies",MongoDBList.empty)) {
            val mess =  y.asInstanceOf[DBObject].getAsOrElse[String]("message","").split("\n").head
            map += (mess -> (map.getOrElse(mess,Set.empty) + script))
          }
        }
        println(map.toList.sortBy{ _._2.size }.map{x => x._2.size+"\t||"+x._1+"\t||"+x._2.mkString(",")}.mkString("\n"))

      case StatisticsMode.Alarms =>

        var map = scala.collection.mutable.HashMap.empty[String,Set[String]]
        for (x <- collection.find(MongoDBObject("status" -> "Done"))) {
          val script = x.getAsOrElse[String]("url","").split("\n").head
          for (y <- x.getAsOrElse[MongoDBList]("result",MongoDBList.empty)) {
            val mess =  y.asInstanceOf[DBObject].getAsOrElse[String]("message","").split("\n").head
            map += (mess -> (map.getOrElse(mess,Set.empty) + script))
          }
        }
        println(map.toList.sortBy{ _._2.size }.map{x => x._2.size+"\t||"+x._1+"\t||"+x._2.mkString(",")}.mkString("\n"))

    }
  }

  /**
   *
   * Constantly inserts new jobs into the database
   *
   */
  def runFeedMode(args: Array[String]) {

    val settings = TouchAnalysisParameters.get
    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient(settings.mongoServer, settings.mongoPort)
    val collection = mongoClient(settings.mongoDatabase)("analysisJobs")

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
                collection.findAndModify(x,$set("status" -> "Waiting", "debug" -> "", "result" -> MongoDBObject(), "html" -> "", "bottoms" -> MongoDBObject(),
                    "dummies" -> MongoDBObject(), "libs" -> MongoDBObject()))
              } else {
                println("Skipping an existing record for "+script.id)
              }
            case None =>
              println("Creating new record for "+script.id)
              collection.insert(MongoDBObject(
                "url" -> ("td://"+script.id),
                "status" -> "Waiting",
                "jobID" -> randomString(10),
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
  def runFetchMode(args: Array[String]) {

    var query:ScriptQuery = new Scripts

    var redownload = false
    val Search = """-search=([^\s]+)""".r
    args filter {

      case "-redownload" => redownload = true; false
      case "-new" => query = new NewScripts; false
      case "-top" => query = new TopScripts; false
      case "-featured" => query = new FeaturedScripts; false
      case Search(q) => query = new ScriptSearch(q); false

      case _ => true

    }

    println("Downloading fresh scripts, adding them to database")

    val settings = TouchAnalysisParameters.get
    import com.mongodb.casbah.Imports._
    import com.novus.salat._
    import com.novus.salat.global._
    val mongoClient = MongoClient(settings.mongoServer, settings.mongoPort)
    val programs = mongoClient(settings.mongoDatabase)("programs")
    for (script <- query) {

      try {

        if (redownload || programs.findOne(MongoDBObject("programID" -> script.id)).isEmpty) {

          if (redownload) {
            programs.findAndRemove(MongoDBObject("programID" -> script.id))
          }

          val url = ScriptQuery.webastURLfromPubID(script.id)
          println("Trying to download " + url)
          var scriptText = ""
          var webAST: Option[JApp] = None
          try {
            scriptText = URLFetcher.fetchFile(url)
            webAST = WebASTImporter.parseAST(scriptText)
          } catch {
            case x: MappingException =>
              println("Failed to parse: " + x.getMessage)
              println(scriptText)
            case x: IOException =>
              println("Failed to download: " + x.getMessage)
          }

          if (scriptText.nonEmpty) {
            println("Creating new record for " + script.id)
            programs.insert(MongoDBObject(
              "programID" -> script.id,
              "script" -> grater[ScriptRecord].asDBObject(script),
              "ast" -> (webAST match {
                case None => None
                case Some(x) => Some(grater[JApp].asDBObject(x))
              }),
              "fetchedAt" -> System.currentTimeMillis / 1000
            ))
          } else {
            println("Seems like I failed to download")
            println(scriptText)
          }
        } else {
          println("skipping existing " + script.id)
        }
      } catch {

        case x: MongoException =>
          println("Some mongo operation failed. Continuing with next one." + x.getLocalizedMessage)

      }

    }

  }


  /**
   *
   * Watches the mongo database for incoming jobs
   *
   */
  def runWatchMode(args: Array[String]) {

    var waitTime = 200
    val WaitTimeOption = "-waitTime=([\\d]+)".r
    val otherArgs = args filter {

      case WaitTimeOption(x) => waitTime = x.toInt; false
      case _ => true

    }
    if (otherArgs.nonEmpty) { println("Invalid argument"); sys.exit(1) }

    val settings = TouchAnalysisParameters.get
    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient(settings.mongoServer, settings.mongoPort)
    val collection = mongoClient(settings.mongoDatabase)("analysisJobs")

    while (true) {

      collection.findAndModify(MongoDBObject("status" -> "Waiting"), $set("status" -> "Initializing")) match {
        case Some(x) =>
          TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(timeout = x.getAs[Int]("timeout")))
          Exporters.jobID = x.getAsOrElse[String]("jobID", System.currentTimeMillis().toString)
          // Objective may be analysis or instrumentation
          if (x.getAsOrElse[String]("objective","analysis") == "instrumentation") {
            Instrumentation.main(x.getAs[String]("url").toArray)
          } else {
            if(x.getAsOrElse[Boolean]("fast", false)) setFastMode() else unsetFastMode()
            TouchRun.main(x.getAs[String]("url").toArray)
          }
        case _ =>
          Thread.sleep(waitTime)
      }

    }

  }

  def setFastMode() {

    TouchAnalysisParameters.set(
      TouchAnalysisParameters(
          //localizeStateOnMethodCall = true, BUGGY
          //singleExecution = true,
          prematureAbort = false,
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

  def randomString(len: Int): String = {
    val rand = new scala.util.Random(System.nanoTime)
    val sb = new StringBuilder(len)
    val ab = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    for (i <- 0 until len) {
      sb.append(ab(rand.nextInt(ab.length)))
    }
    sb.toString()
  }

}