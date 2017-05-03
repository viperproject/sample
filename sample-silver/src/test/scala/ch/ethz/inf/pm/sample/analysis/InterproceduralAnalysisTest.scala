/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.analysis

import ch.ethz.inf.pm.sample.abstractdomain.{IntegerIntervalAnalysis, IntegerIntervalAnalysisState, State, VariableIdentifier}
import ch.ethz.inf.pm.sample.execution.{CfgResult, SilverState}
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, DummyIntegerType}
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.sample.test.SampleTest
import org.scalatest.FunSuite

/**
  * @author Flurin Rindisbacher
  */
class InterproceduralAnalysisTest extends FunSuite with SampleTest {

  private def isTop(state: IntegerIntervalAnalysisState, name: String): Boolean = {
    state.domain.map(VariableIdentifier(name)(DummyIntegerType)).isTop
  }

  test("trivial") {
    val (assertions, result) = run(
      """
         method main() {
             var x: Int
             x := 0
             x := foo()
             assert x != 1 // should fail
         }
         method foo() returns (k: Int) {
             k := 1
             assert k == 1 // should succeed
         }
      """.stripMargin
    )
    //assert(x.isTop) //TODO shouln't this be true too?
    assert(isTop(result("main").exitState(), "x"), "x is set to top at the end of main()")
    assert(assertions.size == 1, "Only assert x != 1 is expected to fail")
  }

  test("multiple assignments") {
    val (assertions, result) = run(
      """
        method foo(this: Ref)
        {
            var i: Int := 0
            var z: Int := 0
            var y: Int := 1
            assert i == 0
            assert z == 0
            i, z := bar(i)
            assert y == 1 // should succeed
        }

        method bar(i: Int) returns (k: Int, l: Int)
        {
            k := i + 1
            l := i + 1
        }
      """.stripMargin
    )
    val variableValues = List("i", "z").map(v => isTop(result("foo").exitState(), v))
    assert(variableValues.forall(v => v), "i and z must be set to top")
    assert(assertions.isEmpty, "Method call must not change the value of y")
  }

  test("functions") {
    val (assertions, result) = run(
      """
        method foo(this: Ref)
        {
            var i: Int := 0
            var j: Int := 1
            i := funbar(i)
            assert j == 1 // should succeed
        }

        function funbar(i: Int) : Int
        {
            i + 1
        }
      """.stripMargin
    )
    assert(assertions.isEmpty, "method call must not change the value of j")
    assert(isTop(result("foo").exitState(), "i"), "Function call should set variable to top.")
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
    val cfgResults = results.identifiers.map(id => id.name -> results.getResult(id)).toMap
    (Reporter.assertionViolations, cfgResults)
  }

}