package ch.ethz.inf.pm.sample.web

import java.nio.file.Path

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.{StdOutOutput, StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain}
import ch.ethz.inf.pm.sample.execution.{TrackingCFGState, AnalysisResult, AnalysisRunner, SimpleAnalysis}
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler.TouchCompiler

/**
 * Implements web interface for TouchDevelop
 */
class TouchDevelopApp extends App {

  /** Provides all test files that the user can choose to analyze. */
  override def fileProvider: TestFileProvider = TouchDevelopFileProvider

  /** List of pre-defined analysis runners. */
  override def availableAnalysisRunners = Seq(new TouchDevelopMayMustAnalysisRunner())//,new TouchDevelopMayAnalysisRunner(),new TouchDevelopSummaryAnalysisRunner())

  val prefix = "td"
}

object TouchDevelopFileProvider extends ResourceTestFileProvider(namePattern = ".*\\.(td|json)") {


}

trait TouchDevelopAnalysisRunner[S <: State[S]] extends AnalysisRunner[S] {

  val touchParams = TouchAnalysisParameters.get

  override val compiler = new TouchCompiler

  override def prepareContext() = {
    super.prepareContext()

    SystemParameters.analysisOutput = if (touchParams.silent) new StringCollector() else new StdOutOutput()
    SystemParameters.progressOutput = if (touchParams.silent) new StringCollector() else new StdOutOutput()

  }

  override def run(path: Path): List[AnalysisResult[S]] = {
    prepareContext()
    compiler.generateTopType()
    val entryState = new TouchEntryStateBuilder(TouchAnalysisParameters.get).topState

    SystemParameters.compiler.compile(path.toString)
    SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

    val analyzer = new TouchAnalysis[Apron.OptOctagons, NonrelationalStringDomain[StringKSetDomain]]
    analyzer.analyze(Nil,entryState) map { x => AnalysisResult[S](x._2,x._3.asInstanceOf[TrackingCFGState[S]]) }
  }

}

class TouchDevelopMayMustAnalysisRunner extends TouchDevelopAnalysisRunner[TouchEntryStateBuilder.State] {
  override val analysis = new TouchDevelopMayMustAnalysis
}
class TouchDevelopMayMustAnalysis extends SimpleAnalysis[TouchEntryStateBuilder.State](new TouchEntryStateBuilder(TouchAnalysisParameters.get))



