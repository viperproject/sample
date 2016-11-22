/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.tools

import java.io.{File, FileFilter}

import ch.ethz.inf.pm.td.compiler.{ScriptRetriever, TouchException}
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.transform.{LoopRewriter, Matcher}
import ch.ethz.inf.pm.td.typecheck.Typer
import ch.ethz.inf.pm.td.webapi.WebASTImporter
import net.liftweb.json.JsonParser.ParseException

/**
  * Created by lucas on 6/19/14.
  *
  * Scans the cache for any use of a certain set of functions
  *
  */

object FindMethodsInCache {

  def main(args: Array[String]) {

    val CACHE_DIR = ScriptRetriever.CACHE_DIR

    val files = new File(CACHE_DIR).listFiles(
      new FileFilter {
        def accept(p1: File): Boolean = {
          p1.getName.matches(".*\\.td$") || p1.getName.matches(".*\\.json$")
        }
      }
    ).map(CACHE_DIR + _.getName).sortWith((a, b) => a.compare(b) < 0)

    for (file <- files) {
      val source = scala.io.Source.fromFile(file)
      val lines = source.getLines mkString "\n"
      source.close()
      if (!lines.trim.isEmpty) {
        try {
          val script = WebASTImporter.convertFromString(lines).get
          val rewrittenScript = LoopRewriter(script)
          Typer.processScript(rewrittenScript)

          var matches = false
          Matcher(rewrittenScript)(
            { x: Declaration => () }, { x: Statement => () }, { case Access(e, Identifier("heading"), _) if e.typeName.ident == "Senses" => matches = true; case _ => () }
          )
          if (matches) {
            println(" ===================== " + file + " ===================== ")
            println(PrettyPrinter(script))
          }
        } catch {
          case x: ParseException =>
            println("Could not parse " + file + ": " + x.toString)
          case x: TouchException =>
            println("Could not typecheck " + file + ": " + x.toString)
        }
      }

    }
  }
}