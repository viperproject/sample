package ch.ethz.inf.pm.td.compiler

import java.io.{PrintWriter, File}
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.webapi.{URLFetcher, Scripts, WebASTImporter}
import scala.io.Source

object ScriptCache {

  private val CACHE_DIR = "TouchDevelopPreprocessing" + File.separator + "testfiles" + File.separator + "cache"

  def get(pubID:String):Script = {
    val cache = new File(CACHE_DIR)
    if (cache.isDirectory || cache.mkdir()) {
      val file = new File(CACHE_DIR + File.separator + pubID + ".json")
      if (!file.exists()) {
        val out = new PrintWriter(file)
        out.println(URLFetcher.fetchFile(Scripts.webastURLfromPubID(pubID)))
        out.close()
      }
      WebASTImporter.convertFromString(Source.fromFile(file).getLines().mkString("\n"))
    } else {
      // Can not use read or write cache
      println("Cache can not be read or written")
      WebASTImporter.queryAndConvert(pubID)
    }
  }

}
