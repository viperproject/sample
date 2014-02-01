package ch.ethz.inf.pm.td.webapi

import java.io.FileNotFoundException
import java.net.URL
import scala.io.Source

object URLFetcher {

  val connectionTimeout = 60000

  /**
   * Scala's Source.fromURL does not have a timeout. This implements the same thing, just with a timeout so that
   * we don't get stuck if the server is broken.
   */
  def fromURLWithTimeout(url:String):Source = {
    val conn = new URL(url).openConnection()
    conn.setConnectTimeout(connectionTimeout)
    conn.setReadTimeout(connectionTimeout)
    val inputStream = conn.getInputStream
    Source.fromInputStream(inputStream)(io.Codec("UTF-8"))
  }


  def fetchFile(url: String) = {
    try {
      val codeSource = fromURLWithTimeout(url)
      codeSource.getLines().mkString("\n")
    } catch {
      case ex: FileNotFoundException => {
        println("Could not fetch url " + url)
        ""
      }
    }
  }

}
