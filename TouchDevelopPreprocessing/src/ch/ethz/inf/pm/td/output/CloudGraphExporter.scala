/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.execution.{AnalysisResult, WeightedGraphAnalysisResult}
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.output.DumpGraph.SimpleGraphRenderer

object CloudGraphExporter extends ResultExporter {

  def exportResults(compiler: TouchCompiler, results: List[AnalysisResult]) {
    results.foreach {
      case x: WeightedGraphAnalysisResult[_, _] =>

        val file = FileSystemExporter.export(x.name + ".html", makeGraph(x))
        SystemParameters.progressOutput.put("Exported graph " + x.name + " for id " + compiler.mainID + " to " + file.toString)

      case _ => ()
    }
  }

  def makeGraph(result: WeightedGraphAnalysisResult[_, _]): String = {

    DumpGraph.getString(
      result.graph,
      SimpleGraphRenderer
    )

  }

}
