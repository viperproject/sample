/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.webapi

import java.net.URL
import scala.io.Source

/**
 * Implements fetching of URLs with timeouts
 */
object URLFetcher {

  /**
   * The connection timeout in milliseconds
   */
  val connectionTimeout = 60000

  /**
   * Scala's Source.fromURL does not have a timeout. This implements the same thing, just with a timeout so that
   * we don't get stuck if the server is broken.
   */
  def fromURLWithTimeout(url: String): Source = {
    val conn = new URL(url).openConnection()
    conn.setConnectTimeout(connectionTimeout)
    conn.setReadTimeout(connectionTimeout)
    val inputStream = conn.getInputStream
    Source.fromInputStream(inputStream)(scala.io.Codec("UTF-8"))
  }

  /**
   * Download the given URL and return the contents as a string.
   *
   * @param url The URL to download
   * @return A string containing the payload
   */
  def fetchFile(url: String): String = {
    val codeSource = fromURLWithTimeout(url)
    codeSource.getLines().mkString("\n")
  }

  /**
   * Download the given File and return the contents as a string.
   *
   * @param path the File to load
   * @return A string containing the payload
   */
  def fetchLocalFile(path: String): String = {
    Source.fromFile(path).getLines().mkString("\n")
  }

}
