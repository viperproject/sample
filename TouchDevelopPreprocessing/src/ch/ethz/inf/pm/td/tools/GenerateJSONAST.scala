/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.tools

import ch.ethz.inf.pm.td.webapi.URLFetcher._


object GenerateJSONAST {

  def main(args: Array[String]) {
    val url = "https://www.touchdevelop.com/api/language/webast"
    val description = fetchFile(url)

    var current = description

    current = current.replaceAll( """([^\w])type([^\w])""", "$1`type`$2")
    current = current.replaceAll( """bool([^e])""", "Boolean$1")
    current = current.replaceAll( """boolean([^\w])""", "Boolean$1")
    current = current.replaceAll( """string([^\w])""", "String$1")
    current = current.replaceAll( """number""", "Int")
    current = current.replaceAll( """(\w+)\[\]""", "Array[$1]")
    current = current.replaceAll( """\?: ([\w\[\]]+)""", ": Option[$1]")
    current = current.replaceAll( """\?:([\w\[\]]+)""", ": Option[$1]")
    current = current.replaceAll( """;""", ",")
    current = current.replaceAll( """(?m)(?s)/\*abstract\*/ export interface (\w+)(\s+)\{([^\}]*)\}""", "trait $1 {$3}")
    current = current.replaceAll( """(?m)(?s)/\*abstract\*/ export interface (\w+) extends (\w+)(\s+)\{([^\}]*)\}""", "trait $1 extends $2 {$4}")
    current = current.replaceAll( """(?m)(?s)export interface (\w+) extends (\w+)(\s+)\{([^\}]*)\}""", "case class $1($4) extends $2")
    current = current.replaceAll( """(?m)(?s)export interface (\w+)(\s+)\{([^\}]*)\}""", "case class $1($3)")
    current = current.replaceAll( """(?m)(?s),(\s*)\)""", "$1)")
    current = current.replaceAll( """Array""", "List")

    println(current)
    println("List(" + (for (m <- """case class (\w+)""".r.findAllIn(current).matchData) yield {
      "classOf[" + m.group(1) + "]"
    }).mkString(", ") + ")")
  }
}

