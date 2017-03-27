package ch.ethz.inf.pm.sample.analysis

import ch.ethz.inf.pm.sample.abstractdomain.{IntegerIntervalAnalysis, IntegerIntervalAnalysisState, VariableIdentifier}
import ch.ethz.inf.pm.sample.execution.CfgResult
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, DummyIntegerType}
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.sample.test.SampleTest
import org.scalatest.FunSuite

/**
  * @author Flurin Rindisbacher
  */
class InterproceduralAnalysisTest extends FunSuite with SampleTest {

  test("trivial") {
    val (assertions, result) = run(
      """
         method main() {
             var x: Int
             x := 0
             x := foo()
             assert x != 1
         }
         method foo() returns (k: Int) {
             k := 1
             assert k == 1
         }
      """.stripMargin
    )
    val x = result("main").exitState().getVariableValue(VariableIdentifier("x")(DummyIntegerType))
    assert(x.domain.dom.isTop, "x is set to top at the end of main()")
    assert(assertions.size == 1, "only one assertion fails")
  }

  /**
    *
    * @param s silver program as string
    * @return Tuple with set of assertion errors and the result of the analysis in the integer interval domain
    */
  def run(s: String): (Set[SampleMessage], Map[String, CfgResult[IntegerIntervalAnalysisState]]) = {
    val results = IntegerIntervalAnalysis.run(
      Compilable.Code("(no name)", s)
    )
    val cfgResults = results.map { case (id, cfgResult) => id.name -> cfgResult }
    (Reporter.assertionViolations, cfgResults)
  }

}