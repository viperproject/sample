package ch.ethz.inf.pm.sample.web

import org.scalatra._
import scala.reflect.io.File
import ch.ethz.inf.pm.sample.oorepresentation.sil.{AnalysisResult, AnalysisRunner}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.server.Server

/** A web application that lets users explore a `TrackingCFGState`,
  * i.e., the CFG itself, blocks, pre- and post-states of statements
  * in that blocks at any point in the iteration. It also offers convenient
  * navigation between all of these views.
  *
  * Currently, it only supports the SIL compiler and `ValueDrivenHeapState`s,
  * but it could be extended quite easily.
  *
  * @todo add support for other compilers than SIL, especially Scala
  * @todo add support for `DefaultCFGState`
  * @todo add support for states other than `ValueDrivenHeapState`
  * @todo make it possible to select the file to be analyzed
  */
class App extends ScalatraServlet {
  // Hard-coded file to analyze
  var result: AnalysisResult[_] = analyze("heap/list/prepend.sil")

  private def analyze(resourceSilFile: String): AnalysisResult[_] =
    AnalysisRunner.run(File(s"SIL/src/test/resources/sil/$resourceSilFile")).head

  /** Renders the CFG of the current result. */
  get("/cfg") {
    html.CFGState(result)
  }

  /** Renders a single CFG block of the current result. */
  get("/cfg/:block") {
    val blockIndex = params("block").toInt
    html.CFGBlockState(result, blockIndex, iter(blockIndex))
  }

  /** Renders a single state in some CFG block of the current result. */
  get("/cfg/:block/:state") {
    val blockIndex = params("block").toInt
    val stateIndex = params("state").toInt
    html.ValueDrivenHeapState(result, blockIndex, stateIndex, iter(blockIndex))
  }

  /** Returns at which iteration to display the states of a CFG block.
    *
    * When no 'iter' parameter is present, just display the fixpoint state,
    * that is, the state in the last iteration.
    */
  private def iter(blockIndex: Int): Int =
    if (params.contains("iter")) params("iter").toInt
    else result.cfgState.trackedStatesOfBlock(blockIndex).size - 1

  get("/analyze/*.*") {
    result = analyze(multiParams("splat").mkString("."))
    redirect("/cfg")
  }
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

  def main(args: Array[String]) =
    launch()
}