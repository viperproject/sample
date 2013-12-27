package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, SimpleAnalyzer}
import ch.ethz.inf.pm.sample.property.OutputCollector
import graph.{ValueDrivenHeapAnalysis, ValueDrivenHeapState}
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 5/10/13
 * Time: 4:46 PM
 * To change this template use File | Settings | File Templates.
 */
object ValueDrivenHeapAnalysisRun {

  private val methods : List[String] = "createNext" :: Nil;

  def main(args : Array[String]) : Unit = {
    //Mandatory global settings
    SystemParameters.compiler = new ScalaCompiler;
    //SystemParameters.property = new ShowGraphProperty;

    SystemParameters.isValueDrivenHeapAnalysis = true

    SystemParameters.analysisOutput = new StdOutOutput();
    SystemParameters.progressOutput = new StdOutOutput();

    //Files paths
//    val file = "ValueDrivenHeapAnalysis/src/test/files/TestFiles.scala"
    val file = "ValueDrivenHeapAnalysis/src/test/files/Node.scala"

    SystemParameters.compiler.compile(file :: Nil);

    //EntryState
    val entryState = new ValueDrivenHeapState[ApronInterface](ExpressionSet()).top()
    //entryState.isBottom = true

    var analyzer = new ValueDrivenHeapAnalysis()

    val analysisResult = analyzer.analyze(methods, entryState, new OutputCollector)

    ShowGraph.Show[ValueDrivenHeapState[ApronInterface]](analysisResult)

  }
}
