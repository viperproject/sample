package ch.ethz.inf.pm.td.compiler

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.webapi.{ScriptQuery, URLFetcher, WebASTImporter}
import com.mongodb.casbah.Imports._

import scala.io.Source

object ScriptCache {

  private val CACHE_DIR = "Test" + File.separator + "test" + File.separator + "TouchDevelop" + File.separator + "cache"

  def get(pubID:String):Script = {
    val cache = new File(CACHE_DIR)
    if (cache.isDirectory || cache.mkdir()) {
      val file = new File(CACHE_DIR + File.separator + pubID + ".json")
      if (!file.exists()) {
        val out = new PrintWriter(file)
        out.println(URLFetcher.fetchFile(ScriptQuery.webastURLfromPubID(pubID)))
        out.close()
      }
      WebASTImporter.convertFromString(Source.fromFile(file, "utf-8").getLines().mkString("\n"))
    } else {
      // Can not use read or write cache
      println("Cache can not be read or written")
      WebASTImporter.queryAndConvert(pubID)
    }
  }

}

object MongoImporter {

  lazy val client = {
    MongoClient()("tb")("programs")
  }

  def get(programID: String): Script = {

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