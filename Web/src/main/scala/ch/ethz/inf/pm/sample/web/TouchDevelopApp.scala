package ch.ethz.inf.pm.sample.web

import java.nio.file.{Path, Paths}

import ch.ethz.inf.pm.sample.{StdOutOutput, StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{ApronInterface, NumericalDomain}
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain, StringDomain, StringValueDomain}
import ch.ethz.inf.pm.sample.execution.{TrackingCFGState, AnalysisResult, AnalysisRunner, SimpleAnalysis}
import ch.ethz.inf.pm.sample.property.SingleStatementProperty
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler.{TouchProgramPointRegistry, TouchCompiler}
import ch.ethz.inf.pm.td.output.Exporters

/**
 * Implements web interface for TouchDevelop
 */
class TouchDevelopApp extends App {

  /** Provides all test files that the user can choose to analyze. */
  override def fileProvider: TestFileProvider = TouchDevelopFileProvider

  /** List of pre-defined analysis runners. */
  override def availableAnalysisRunners = Seq(new TouchDevelopMayMustAnalysisRunner())//,new TouchDevelopMayAnalysisRunner(),new TouchDevelopSummaryAnalysisRunner())

}

object TouchDevelopFileProvider extends ResourceTestFileProvider(namePattern = ".*\\.json") {


}

trait TouchDevelopAnalysisRunner[S <: State[S]] extends AnalysisRunner[S] {

  val touchParams = TouchAnalysisParameters.get

  override val compiler = new TouchCompiler

  override def prepareContext() = {
    super.prepareContext()

    SystemParameters.analysisOutput = if (touchParams.reporting.silent) new StringCollector() else new StdOutOutput()
    SystemParameters.progressOutput = if (touchParams.reporting.silent) new StringCollector() else new StdOutOutput()

  }

  override def run(path: Path): List[AnalysisResult[S]] = {
    prepareContext()
    compiler.generateTopType()
    val entryState = new MayMustEntryStateBuilder(TouchAnalysisParameters.get).topState

    SystemParameters.compiler.compile(path.toString)
    SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

    val analyzer = new TouchAnalysis[ApronInterface.Default, NonrelationalStringDomain[StringKSetDomain]]
    analyzer.analyze(entryState) map { x => AnalysisResult[S](x._2,x._3.asInstanceOf[TrackingCFGState[S]]) }
  }

}

class TouchDevelopMayMustAnalysisRunner extends TouchDevelopAnalysisRunner[TouchDevelopEntryStateBuilder.AnalysisMayMustCollectionStateHeapType] {
  override val analysis = new TouchDevelopMayMustAnalysis
}
//class TouchDevelopMayAnalysisRunner extends TouchDevelopAnalysisRunner[TouchDevelopEntryStateBuilder.AnalysisMayCollectionStateType] {
//  override val analysis = new TouchDevelopMayAnalysis
//}
//class TouchDevelopSummaryAnalysisRunner extends TouchDevelopAnalysisRunner[TouchDevelopEntryStateBuilder.AnalysisSummaryCollectionHeapStateType] {
//  override val analysis = new TouchDevelopSummaryAnalysis
//}

class TouchDevelopMayMustAnalysis extends SimpleAnalysis[TouchDevelopEntryStateBuilder.AnalysisMayMustCollectionStateHeapType](new MayMustEntryStateBuilder(TouchAnalysisParameters.get))
//class TouchDevelopMayAnalysis extends SimpleAnalysis[TouchDevelopEntryStateBuilder.AnalysisMayCollectionStateType](new MayEntryStateBuilder(TouchAnalysisParameters.get))
//class TouchDevelopSummaryAnalysis extends SimpleAnalysis[TouchDevelopEntryStateBuilder.AnalysisSummaryCollectionHeapStateType](new SummaryEntryStateBuilder(TouchAnalysisParameters.get))



