/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import ch.ethz.inf.pm.td.webapi.{URLFetcher, ScriptQuery}

object CountScripts {

  def main(args:Array[String]) {

    var totalCount = 0
    var libraryCount = 0
    var rootCount = 0
    var hiddenCount = 0

    println("Counting scripts:")

    for (s <- new ScriptQuery) {
      if (s.rootid == s.id) rootCount += 1
      if (s.islibrary) libraryCount += 1
      if (s.ishidden) hiddenCount += 1
      totalCount += 1
      if (totalCount % 100 == 0) println("Currently at... " + totalCount)
    }

    println("totalCount " + totalCount)
    println("libraryCount " + libraryCount)
    println("rootCount " + rootCount)
    println("hiddenCount " + hiddenCount)

    println("Official report:")
    println(URLFetcher.fetchFile("http://www.touchdevelop.com/api/stats"))

  }

}