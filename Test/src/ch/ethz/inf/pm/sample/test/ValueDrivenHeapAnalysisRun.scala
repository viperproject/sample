package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.property.OutputCollector
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import ch.ethz.inf.pm.sample.abstractdomain.vdha._

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
    val file = "ValueDrivenHeapAnalysis/src/test/resources/Node.scala"

    SystemParameters.compiler.compile(file :: Nil);

    val analyzer = new ValueDrivenHeapAnalysis()
    val analysisResult = analyzer.analyze[DefaultValueDrivenHeapState[ApronInterface]](methods, analyzer.getInitialState(), new OutputCollector)

    ShowGraph.Show[DefaultValueDrivenHeapState[ApronInterface]](analysisResult)

  }
}
