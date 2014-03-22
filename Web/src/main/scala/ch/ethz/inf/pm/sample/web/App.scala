package ch.ethz.inf.pm.sample.web

import org.scalatra._
import ch.ethz.inf.pm.sample.oorepresentation.sil.{PreciseAnalysisRunner, AnalysisResult}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.server.Server

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
  * Currently, it only supports the SIL compiler and `ValueDrivenHeapState`s,
  * but it could be extended quite easily.
  *
  * @todo add support for other compilers than SIL, especially Scala
  * @todo add support for `DefaultCFGState`
  * @todo add support for states other than `ValueDrivenHeapState`
  */
class App extends ScalatraServlet {
  /** Provides all test files that the user can choose to analyze. */
  val fileProvider = ResourceTestFileProvider(namePattern = ".*\\.sil")

  /** The currently active analysis results that the user can inspect. */
  var resultsOption: Option[List[AnalysisResult[_]]] = None

  /** Renders the list of test files that can be analyzed. */
  get("/") {
    html.Home(fileProvider.testFiles)
  }

  /** Analyzes the test file passed as a parameter. */
  get("/analyze/") {
    val testFileString = params("file")
    fileProvider.testFiles.find(_.toString == testFileString) match {
      case Some(testFile) =>
        // TODO: Make it configurable
        val results = PreciseAnalysisRunner.run(testFile.path)
        resultsOption = Some(results)

        // If there is only a single result, redirect to it
        // Otherwise, let the user choose
        if (results.size == 1)
          redirect("/results/0/")
        else
          redirect("/results/?")
      case None =>
        // TODO: Should probably output an error message
        redirect("/")
    }
  }

  /** Renders the list of analysis results that the user can inspect. */
  get("/results/") {
    resultsOption match {
      case Some(result) => html.AnalysisResults(resultsOption.get)
      case None => redirect("/")
    }
  }

  /** Renders the CFG of the current result. */
  get("/results/:result/") {
    resultOption match {
      case Some(result) => html.CFGState(result)
      case None => redirect("/")
    }
  }

  /** Renders a single CFG block of the current result. */
  get("/results/:result/:block/") {
    resultOption match {
      case Some(result) =>
        val blockIndex = params("block").toInt
        html.CFGBlockState(result, blockIndex, iter(blockIndex))
      case None => redirect("/")
    }
  }

  /** Renders a single state in some CFG block of the current result. */
  get("/results/:result/:block/:state/") {
    resultOption match {
      case Some(result) =>
        val blockIndex = params("block").toInt
        val stateIndex = params("state").toInt
        html.ValueDrivenHeapState(result, blockIndex, stateIndex, iter(blockIndex))
      case None => redirect("/")
    }
  }

  /** Returns the `AnalysisResult` to display according to the URL parameter. */
  private def resultOption: Option[AnalysisResult[_]] = {
    resultsOption match {
      case Some(results) =>
        val resultIndex = params("result").toInt
        Some(results(resultIndex))
      case None =>
        None
    }
  }

  /** Returns at which iteration to display the states of a CFG block.
    *
    * When no 'iter' parameter is present, just display the fixpoint state,
    * that is, the state in the last iteration.
    */
  private def iter(blockIndex: Int): Int =
    if (params.contains("iter")) params("iter").toInt
    else resultOption.get.cfgState.trackedStatesOfBlock(blockIndex).size - 1
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