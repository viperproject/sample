/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.execution.{AnalysisResult, LabeledGraphAnalysisResult}
import ch.ethz.inf.pm.td.compiler.TouchCompiler

object LabeledGraphExporter extends ResultExporter {

  def exportResults(compiler: TouchCompiler, results: List[AnalysisResult]) {
    results.foreach {
      case x: LabeledGraphAnalysisResult[_, _] =>

        val file = FileSystemExporter.export(compiler.mainID + "." + x.shortName + ".html", makeGraph(x))
        SystemParameters.progressOutput.put("Exported graph " + x.name + " for id " + compiler.mainID + " to " + file.toString)

      case _ => ()
    }
  }

  def makeGraph(result: LabeledGraphAnalysisResult[_, _]): String = {

    DumpGraph.getString(
      result.graph
    )

  }

}
