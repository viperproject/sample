package ch.ethz.inf.pm.td.compiler

import java.io.{IOException, File, PrintWriter}
import java.util.NoSuchElementException

import ch.ethz.inf.pm.td.parser.{ScriptParser, Script}
import ch.ethz.inf.pm.td.webapi.{ScriptQuery, URLFetcher, WebASTImporter}
import com.mongodb.casbah.Imports._
import com.typesafe.scalalogging.LazyLogging

import scala.io.Source

object ScriptRetriever extends LazyLogging {

  val CACHE_DIR = "/tmp/td_cache"

  /**
   * This takes one of the following arguments:
   *
   * http://www.touchdevelop.com/api/[pubID]/... Some URL to a script
   * https://www.touchdevelop.com/api/[pubID]/... Some URL to a script
   * td://[pubID] Some PubID in uri form
   * Some path to a local file with extension .td for source code.
   * Some path to a local file with extension .json for a cached json representation
   *
   * It uses either the WebAST importer or the script parser to get the corresponding
   * TouchDevelop AST. If a URL or a pubID is provided, we may use the local cache
   *
   */
  def getPath(path: String): (Script, String) = {
    try {
      if (path.startsWith("http://"))
        (ScriptRetriever.getLocally(ScriptQuery.pubIDfromURL(path)).get, ScriptQuery.pubIDfromURL(path))
      else if (path.startsWith("https://"))
        (ScriptRetriever.getLocally(ScriptQuery.pubIDfromURL(path)).get, ScriptQuery.pubIDfromURL(path))
      else if (path.startsWith("td://"))
        (ScriptRetriever.getLocally(path.substring(5)).get, path.substring(5))
      else if (path.startsWith("mongo://"))
        (MongoImporter.get(path.substring(8)).get, path.substring(8))
      else if (path.toLowerCase.endsWith(".td"))
        (ScriptParser(Source.fromFile(path).getLines().mkString("\n")), ScriptQuery.pubIDfromFilename(path))
      else if (path.toLowerCase.endsWith(".json"))
        (WebASTImporter.convertFromString(Source.fromFile(path, "utf-8").getLines().mkString("\n")).get, ScriptQuery.pubIDfromFilename(path))
      else throw TouchException("Unrecognized path " + path)
    } catch {
      case x:NoSuchElementException =>
        throw TouchException("Failed to open " + path)
    }
  }

  def getLocally(pubID:String):Option[Script] = {
    val cache = new File(CACHE_DIR)
    if (cache.isDirectory || cache.mkdir()) {
      val file = new File(CACHE_DIR + File.separator + pubID + ".json")
      if (!file.exists()) {
        val results = URLFetcher.fetchFile(ScriptQuery.webastURLfromPubID(pubID))
        if (!results.isEmpty) {
          try {
            val out = new PrintWriter(file)
            out.println(results)
            out.close()
          } catch {
            case x:IOException =>
              logger.debug("Cache could not be written")
          }
          WebASTImporter.convertFromString(results)
        } else {
          None
        }
      } else {
        WebASTImporter.convertFromString(Source.fromFile(file, "utf-8").getLines().mkString("\n"))
      }
    } else {
      // Can not use read or write cache
      logger.debug("Cache can not be read or written")
      WebASTImporter.queryAndConvert(pubID)
    }
  }

}

object MongoImporter {

  lazy val client = {
    MongoClient()("tb")("programs")
  }

  def get(programID: String): Option[Script] = {

    client.findOne(MongoDBObject("programID" -> programID)) match {

      case Some(x) =>

        x.get("program") match {
          case x: String => WebASTImporter.convertFromString(x)
          case _ => throw TouchException("Unexpected contents in database")
        }

      case None =>

        throw TouchException("Could not find program in database")

    }

  }


}