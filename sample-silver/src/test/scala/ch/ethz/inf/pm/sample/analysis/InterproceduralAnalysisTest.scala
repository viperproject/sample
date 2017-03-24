package ch.ethz.inf.pm.sample.analysis

import ch.ethz.inf.pm.sample.abstractdomain.IntegerIntervalAnalysis
import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.test.SampleTest
import org.scalatest.FunSuite

/**
  * @author Flurin Rindisbacher
  */
class InterproceduralAnalysisTest extends FunSuite with SampleTest {

  test("trivial") {
    run(
      """
         method main() {
             var x: Int
             x := 0
             x := foo()
             assert x != 1
         }
         method foo() returns (k: Int) {
             k := 1
         }
      """.stripMargin
    )
    //TODO check for failed asserts
  }

  def run(s: String) = {
    val results = IntegerIntervalAnalysis.run(
      Compilable.Code("(no name)", s)
    )
    //TODO return some kind of error measurement. Asserts?
    //return

//    val cfgResults = results.map { case (id, cfgResult) => id.name -> cfgResult }
//    println("\n*******************\n* Analysis Result *\n*******************\n")
//    for ((method, cfgResult) <- cfgResults) {
//      println("******************* " + method + "\n")
//      cfgResult.print()
//    }
  }

}