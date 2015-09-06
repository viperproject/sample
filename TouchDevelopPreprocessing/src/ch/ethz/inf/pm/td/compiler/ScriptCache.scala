package ch.ethz.inf.pm.td.compiler

import java.io.{IOException, File, PrintWriter}

import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.webapi.{ScriptQuery, URLFetcher, WebASTImporter}
import com.mongodb.casbah.Imports._
import com.typesafe.scalalogging.LazyLogging

import scala.io.Source

object ScriptCache extends LazyLogging {

  val CACHE_DIR = "/tmp/td_cache"

  def get(pubID:String):Option[Script] = {
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