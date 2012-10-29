package ch.ethz.inf.pm.td.webapi

import io.Source
import java.io.FileNotFoundException

object URLFetcher {

  def fetchFile(url: String) = {
    try {
      val codeSource = Source.fromURL(url)
      codeSource.getLines().mkString("\n")
    } catch {
      case ex: FileNotFoundException => {
        println("Could not fetch url " + url)
        ""
      }
    }
  }

}
