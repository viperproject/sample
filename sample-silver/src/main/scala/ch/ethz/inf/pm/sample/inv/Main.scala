/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.inv

import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverCompiler

object Main {
  def main(arguments: Array[String]): Unit = {
    require(arguments.nonEmpty, "No file specified")
    val filename = arguments(0)

    val program = SilverCompiler.compile(filename)
    val extended = SimpleInference.extend(program)

    extended match {
      case Some(existing) =>
        println("--- EXTENDED PROGRAM ---")
        println(existing)
      case None => println("--- UNABLE TO EXTEND PROGRAM ---")
    }
  }

}
