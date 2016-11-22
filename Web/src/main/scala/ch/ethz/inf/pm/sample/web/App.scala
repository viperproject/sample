/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.web

import ch.ethz.inf.pm.sample.abstractdomain.State
import org.scalatra._
import ch.ethz.inf.pm.sample.oorepresentation.sil._
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.server.Server
import ch.ethz.inf.pm.sample.execution.{AnalysisResult, AnalysisRunner, MethodAnalysisResult, WeightedGraphAnalysisResult}
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, WeightedGraph}

import scala.language.existentials

/** Web application that lets users analyze programs and explore the result.
  *
  * The user may analyze any files that are resources on the classpath.
  * That is, these provided test files can actually be embedded in a JAR file.
  *
  * The user can explore the resulting `TrackingCFGState`,
  * i.e., the CFG itself, blocks, pre- and post-states of statements
  * in that blocks at any point in the iteration. It also offers convenient
  * navigation between all of these views.
  *
  * @todo add support for `DefaultCFGState`
  * @todo add support for states other than `ValueDrivenHeapState`
  */
abstract class App extends ScalatraServlet {

  /** Provides all test files that the user can choose to analyze. */
  def fileProvider: TestFileProvider

  /** Provides all online test files that the user can choose to analyze. */
  def identifierProvider: Option[IdentifierProvider]

  /** List of pre-defined analysis runners. */
  def availableAnalysisRunners: Seq[AnalysisRunner[_ <: State[_]]]

  /** URL prefix */
  def prefix: String

  /** The currently active runner using which analyses are performed.
    * Can be changed from the web interface and defaults to the first one.
    */
  var analysisRunnerOption: Option[AnalysisRunner[_ <: State[_]]] = None

  /** The currently active analysis results that the user can inspect. */
  var resultsOption: Option[List[AnalysisResult]] = None

  /** Renders the list of test files that can be analyzed. */
  get("/") {
    html.Home()(this)
  }

  /** Analyzes the test file passed as a parameter. */
  get("/analyze/") {
    val testFileString = params("file")
    val results = fileProvider.testFiles.find(_.toString == testFileString).map(x => analysisRunner.run(Compilable.Path(x.path)))
      .orElse(identifierProvider.flatMap(_.identifiers.find(_.id == testFileString)).map(x => analysisRunner.run(Compilable.Identifier(x.id))))

    results match {
      case Some(r) =>
        resultsOption = Some(r)

        // If there is only a single result, redirect to it
        // Otherwise, let the user choose
        if (r.size == 1)
          redirect("/"+prefix+"/results/0/?")
        else
          redirect("/"+prefix+"/results/?")
      case None =>
        // TODO: Should probably output an error message
        redirect("/"+prefix+"/")
    }
  }

  /** Sets a new analysis runner and purges the current analysis result. */
  get("/runner/") {
    val analysisRunner = availableAnalysisRunners(params("index").toInt)
    analysisRunnerOption = Some[AnalysisRunner[_ <: State[_]]](analysisRunner)
    resultsOption = None
    redirect("/"+prefix+"/")
  }

  /** Renders the list of analysis results that the user can inspect. */
  get("/results/") {
    resultsOption match {
      case Some(result) => html.AnalysisResults()(this)
      case None => redirect("/"+prefix+"/")
    }
  }

  /** Renders the CFG of the current result. */
  get("/results/:result/") {
    resultOption match {
      case Some(result) =>
        result match {
          case m:MethodAnalysisResult[_] =>
            html.CFGState(m)(this)
          case w:WeightedGraphAnalysisResult[_,_] =>
            html.WeightedGraphResult(w)(this)
          case _ => redirect("/"+prefix+"/")
        }
      case None => redirect("/"+prefix+"/")
    }
  }

  /** Renders a single CFG block of the current result. */
  get("/results/:result/:block/") {
    resultOption match {
      case Some(result) =>
        result match {
          case m:MethodAnalysisResult[_] =>
            val blockIndex = params("block").toInt
            html.CFGBlockState(m, blockIndex, iter(blockIndex,m))(this)
          case _ => redirect("/"+prefix+"/")
        }
      case None => redirect("/"+prefix+"/")
    }
  }

  /** Renders a single state in some CFG block of the current result. */
  get("/results/:result/:block/:state/") {
    resultOption match {
      case Some(result) =>
        result match {
          case m:MethodAnalysisResult[_] =>
            val blockIndex = params("block").toInt
            val stateIndex = params("state").toInt
            html.State(m, blockIndex, stateIndex, iter(blockIndex,m))(this)
          case _ => redirect("/"+prefix+"/")
        }
      case None => redirect("/"+prefix+"/")
    }
  }

  def analysisRunner: AnalysisRunner[_ <: State[_]] =
    analysisRunnerOption.getOrElse(availableAnalysisRunners.head)

  /** Returns the `AnalysisResult` to display according to the URL parameter. */
  private def resultOption[S <: State[S]]: Option[AnalysisResult] = {
    resultsOption match {
      case Some(results) =>
        val resultID = params("result")
        results.find(_.uniqueID == resultID)
      case None =>
        None
    }
  }

  /** Returns at which iteration to display the states of a CFG block.
    *
    * When no 'iter' parameter is present, just display the fixed point state,
    * that is, the state in the last iteration.
    */
  private def iter[S <: State[S]](blockIndex: Int, res:MethodAnalysisResult[S]): Int =
    if (params.contains("iter")) params("iter").toInt
    else res.cfgState.trackedStatesOfBlock(blockIndex).size - 1
}

/** Web app that detects SIL test programs and lets the user analyze them. */
class SilApp extends App {
  val fileProvider = ResourceTestFileProvider(namePattern = ".*\\.sil")
  val identifierProvider = None
  val availableAnalysisRunners = Seq(
    PredicateAnalysisRunner,
// TODO: Instantiates existential type with several types
    DefaultAnalysisRunner,
    PreciseAnalysisRunner
  )
  val prefix = "sil"
}

/** Launches the web server.
  *
  * Be sure to add Apron to the native library path in IntelliJ's run config.
  */
object App {
  def launch() = {
    val context = new WebAppContext()
    context setContextPath "/"
    context.setResourceBase("Web/src/main/webapp")
    context.addEventListener(new ScalatraListener)
    context.addServlet(classOf[DefaultServlet], "/")

    val server = new Server(8080)
    server.setHandler(context)
    server.start()
    server.join()
  }

  def main(args: Array[String]) {
    launch()
  }
}