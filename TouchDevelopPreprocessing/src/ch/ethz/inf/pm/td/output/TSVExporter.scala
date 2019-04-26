/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleError}
import ch.ethz.inf.pm.td.compiler.{SpaceSavingProgramPoint, TouchCompiler, TouchProgramPoint, TouchProgramPointRegistry}

object TSVExporter extends FileSystemResultExporter {

  def getExtension = "tsv"

  def warningsToString(compiler: TouchCompiler, id: String): String = {
    apply(id)
  }

  def apply(id: String): String = {

    var res = ""

    for (SampleError(_, message, pp, causes) <- Reporter.assertionViolations) {
      pp match {
        case touchPP: SpaceSavingProgramPoint =>
          val tpp = TouchProgramPointRegistry.reg(touchPP.id)
          if (id == tpp.scriptID) "Error\t" + message + "\t" + pp else ""
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Error\t" + message + "\t" + pp else ""
        case _ => ""
      }
    }

    for (m <- Reporter.unreachableCode) {
      m.pp match {
        case touchPP: SpaceSavingProgramPoint =>
          val tpp = TouchProgramPointRegistry.reg(touchPP.id)
          if (id == tpp.scriptID) "Error\t" + m.message + "\t" + m.pp else ""
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Bottom\t" + m.message + "\t" + m.pp else ""
        case _ => ""
      }
    }

    for (m <- Reporter.impreciseSemantics) {
      m.pp match {
        case touchPP: SpaceSavingProgramPoint =>
          val tpp = TouchProgramPointRegistry.reg(touchPP.id)
          if (id == tpp.scriptID) "Error\t" + m.message + "\t" + m.pp else ""
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Imprecision\t" + m.message + "\t" + m.pp else ""
        case _ => ""
      }
    }

    res

  }

}
