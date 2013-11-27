package test.graph

import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{ApronInterface, ShowGraphProperty}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, SimpleAnalyzer}
import ch.ethz.inf.pm.sample.property.OutputCollector
import graph.{ValueDrivenHeapAnalysis, ValueDrivenHeapState}

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 5/10/13
 * Time: 4:46 PM
 * To change this template use File | Settings | File Templates.
 */
object ValueDrivenHeapAnalysisRun {

  //private val methods : List[String] = "emptyMethod" :: Nil;
//  private val methods : List[String] = "valueVariableAssignment" :: Nil;
  private val methods : List[String] = "createNext" :: Nil;

  def main(args : Array[String]) : Unit = {
    //Mandatory global settings
    SystemParameters.compiler = new ScalaCompiler;
    SystemParameters.property = new ShowGraphProperty;

    SystemParameters.analysisOutput = new StdOutOutput();
    SystemParameters.progressOutput = new StdOutOutput();

    //Files paths
//    val file = "/home/milos/SampleMyBranch/ValueDrivenHeapAnalysis/src/test/files/TestFiles.scala";
    val file = "/home/milos/SampleMyBranch/ValueDrivenHeapAnalysis/src/test/files/Node.scala";

    SystemParameters.compiler.compile(file :: Nil);

    //EntryState
    var entryState = new ValueDrivenHeapState[ApronInterface](new ExpressionSet(SystemParameters.getType.top)).top()
    //entryState.isBottom = true

    var analyzer = new ValueDrivenHeapAnalysis()

    analyzer.analyze(methods, entryState, new OutputCollector);

    System.out.println("Semantic time: " + SystemParameters.domainTimer.totalTime);

  }
}
