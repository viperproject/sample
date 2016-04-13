/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.compiler

import java.io.{IOException, File, PrintWriter}
import java.util.NoSuchElementException

import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.parser.{ScriptParser, Script}
import ch.ethz.inf.pm.td.webapi.{JApp, ScriptQuery, URLFetcher, WebASTImporter}
import com.novus.salat.util.MissingTypeHint
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
  def getPath(path: String): ((Script,Option[JApp]), String) = {
    try {
      if (path.startsWith("http://"))
        (ScriptRetriever.getLocally(ScriptQuery.pubIDfromURL(path)).get, ScriptQuery.pubIDfromURL(path))
      else if (path.startsWith("https://"))
        (ScriptRetriever.getLocally(ScriptQuery.pubIDfromURL(path)).get, ScriptQuery.pubIDfromURL(path))
      else if (path.startsWith("td://"))
        (ScriptRetriever.getMongo(path.substring(5)).get, path.substring(5))
      else if (path.toLowerCase.endsWith(".td"))
        ((ScriptParser(Source.fromFile(path).getLines().mkString("\n")),None), ScriptQuery.pubIDfromFilename(path))
      else if (path.toLowerCase.endsWith(".json"))
        (WebASTImporter.convertFromStringBoth(Source.fromFile(path, "utf-8").getLines().mkString("\n")).get, ScriptQuery.pubIDfromFilename(path))
      else throw TouchException("Unrecognized path " + path)
    } catch {
      case x:NoSuchElementException =>
        throw TouchException("Failed to open " + path)
    }
  }

  /**
   * Tries to fetch the file from a local database
   */
  def getMongo(pubID:String):Option[(Script,Option[JApp])] = {
    import com.mongodb.casbah.Imports._

    try {
      import com.novus.salat._
      import com.novus.salat.global._
      val settings = TouchAnalysisParameters.get
      val client =  MongoClient(settings.mongoServer,settings.mongoPort)(settings.mongoDatabase)("programs")
      client.findOne(MongoDBObject("programID" -> pubID)) match {

        case Some(x) =>

          val res = x.get("ast")
          try {
            val japp = grater[JApp].asObject(res.asInstanceOf[BasicDBObject])
            Some(WebASTImporter.convert(japp),Some(japp))
          } catch {
            case x:MissingTypeHint =>
              // Seems like the web interface added this. Here is my ugly trick: parse the JSON
              WebASTImporter.convertFromStringBoth(res.asInstanceOf[BasicDBObject].toString)
          }

        case None =>

          getLocally(pubID)

      }

    } catch {

      case x:MongoException =>
        logger.info("got mongo exception" + x.getMessage)
        getLocally(pubID)
    }

  }


  def getLocally(pubID:String):Option[(Script,Option[JApp])] = {
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
          WebASTImporter.convertFromStringBoth(results)
        } else {
          None
        }
      } else {
        WebASTImporter.convertFromStringBoth(Source.fromFile(file, "utf-8").getLines().mkString("\n"))
      }
    } else {
      // Can not use read or write cache
      logger.debug("Cache can not be read or written")
      WebASTImporter.queryAndConvertBoth(pubID)
    }
  }

}