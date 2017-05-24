/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.IntegerInterval
import ch.ethz.inf.pm.sample.execution.{CfgResult, ProgramResult}
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, DummyIntegerType}
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.sample.test.SampleTest
import org.scalatest.FunSuite

trait InterproceduralAnalysisTest extends FunSuite with SampleTest

/**
  * Tests for the trivial interprocedural analysis (just go to Top)
  *
  * @author Flurin Rindisbacher
  */
class TrivialInterproceduralAnalysisTest extends InterproceduralAnalysisTest {

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

/**
  * Tests for the naive/context insensitive interprocedural analysis
  * Run an interprocedural interval analysis and checks the exit states of the methods
  *
  * @author Flurin Rindisbacher
  */
class ContextInsensitiveInterproceduralAnalysisTest extends InterproceduralAnalysisTest {

  private def checkVariableInExitState(programResult: ProgramResult[IntegerIntervalAnalysisState],
                                       method: String, variable: String, expected: IntegerInterval, reason: String): Unit = {
    val interval = programResult.getResult(SilverIdentifier(method)).exitState().domain.get(VariableIdentifier(variable)(DummyIntegerType))
    assert(expected.equivalent(interval), s"$variable is: $interval but: $reason")
  }

  test("nop") {
    val programResult = run(
      """
        method foo() {
          var z : Int := 1
          var i : Int := 2
          i := nop(i)
          i := i + 1
        }

        method nop(i: Int) returns (k: Int) {
          k := i
        }
      """.stripMargin
    )
    checkVariableInExitState(programResult, "foo", "z", IntegerInterval.Inner(1, 1), "z should not change")
    checkVariableInExitState(programResult, "foo", "i", IntegerInterval.Inner(3, 3), "i should be 3")
    checkVariableInExitState(programResult, "nop", "i", IntegerInterval.Inner(2, 2), "nop(2) should set i to 2")
    checkVariableInExitState(programResult, "nop", "k", IntegerInterval.Inner(2, 2), "nop(2) shoudl set k to 2")
  }

  test("recursion-without-widening") {
    val programResult = run(
      """
        // assuming a widening limit of 3 this program should be analyzed precisely.
        method foo()
        {
            var i: Int := 3
            i := bar(i)
        } // expected exitState: i -> [0,0]

        method bar(j: Int) returns (k: Int)
        {
            if(j > 0) {
                k := bar(j - 1)
            } else {
                k := j
            }
        } // expected exitState: j -> [0, 3], k -> [0, 0]
      """.stripMargin
    )
    checkVariableInExitState(programResult, "foo", "i", IntegerInterval.Inner(0, 0), "i should be exact")
    checkVariableInExitState(programResult, "bar", "j", IntegerInterval.Inner(0, 3), "j should collect all arguments (3, 2, 1, 0)")
    checkVariableInExitState(programResult, "bar", "k", IntegerInterval.Inner(0, 0), "k should be exact")
  }

  test("recursion-with-widening") {
    val programResult = run(
      """
        // assuming a widening limit of 3 widening will be used for this analysis
        method foo()
        {
            var i: Int := 10
            i := bar(i)
        } // expected exitState: i -> [-inf, 0]

        method bar(j: Int) returns (k: Int)
        {
            if(j > 0) {
                k := bar(j - 1)
            } else {
                k := j
            }
        } // expected exitState: j -> [-inf, 10], k -> [-inf, 0]
      """.stripMargin
    )
    val minusInf = Int.MinValue
    checkVariableInExitState(programResult, "foo", "i", IntegerInterval.Inner(minusInf, 0), "i should be [-inf, 0]")
    checkVariableInExitState(programResult, "bar", "j", IntegerInterval.Inner(minusInf, 10), "j should collect arguments [-inf, 10]")
    checkVariableInExitState(programResult, "bar", "k", IntegerInterval.Inner(minusInf, 0), "k should be [-inf, 0]")
  }

  test("multiple-callers") {
    val programResult = run(
      """
        //
        // bar() is called with i:=0 and i:=2 and therefore with an inputstate of [i -> [0,2]]
        // the exitstate in both foo and baz should be [i -> [1,3]]
        //
        method foo(this: Ref)
        {
            var i: Int := 0
            i := bar(this, i)
        } // NO STMT AFTER METHOD CALL

        method bar(this: Ref, i: Int) returns (k: Int)
        {
            k := i + 1
        }

        method test(this: Ref) {
          var i: Int := 2
          i := bar(this, i)
          i := i                 //SOME STMT AFTER METHOD CALL
        }
      """.stripMargin
    )
    checkVariableInExitState(programResult, "foo", "i", IntegerInterval.Inner(1, 3), "i should be [1,3]")
    checkVariableInExitState(programResult, "bar", "i", IntegerInterval.Inner(0, 2), "i should collect all arguments [0,2]")
    checkVariableInExitState(programResult, "bar", "k", IntegerInterval.Inner(1, 3), "k should be [1,3]")
    checkVariableInExitState(programResult, "test", "i", IntegerInterval.Inner(1, 3), "i should be [1,3]")
  }

  test("dummy-main-recursive") {
    // Test for:
    // https://bitbucket.org/viperproject/sample/pull-requests/12/context-insensitive-forward-analysis/activity#comment-37181754
    val programResult = run(
      """
        //
        // foo() has one in-edge (MethodCallEdge) that doesn't contribute anything to the incoming state (it's bottom at the beginning)
        // In that case we expect to analysis to start with the "initial" state instead of merging all the in-edges.
        // foo() is "promoted to be a main method"
        //
        method foo(a: Int) returns (x: Int) {
            if (a > 0) {
                x := foo(a - 1)
            } else {
                x := 0
            }
        }
      """.stripMargin
    )
    checkVariableInExitState(programResult, "foo", "x", IntegerInterval.Inner(0, 0), "x should be [0, 0]")
  }

  test("dummy-main-connected-component") {
    // Test for:
    // https://bitbucket.org/viperproject/sample/pull-requests/12/context-insensitive-forward-analysis/activity#comment-37181754
    val programResult = run(
      """
        //
        // Similar test to dummy-main-recursive above. Here the two methods build a strongly connected component
        // at the top of the call graph. Both methods should therefore be promoted to "main methods" and be analysed
        // using the "initial" state.
        //
        method foo(a: Int) returns (x: Int) { // for a main-method a is expected to be Top
            if (a > 0) {
                x := bar(-a)                  // bar([-inf, -1]) => x -> [-inf, -1]
            } else {
                x := a                        // x -> [-inf, 0]
            }
        }                                     // expected result is x -> [-inf, 0]

        method bar(b: Int) returns (y: Int) { // for a main-method b is expected to be Top
            if (b > 0) {
              y := foo(-b)                    // foo([-inf, -1]) => y -> [-inf, -1]
            } else {
              y := b                          // y -> [-inf, 0]
            }
        }                                     // expected result is y -> [-inf, 0]
      """.stripMargin
    )
    checkVariableInExitState(programResult, "foo", "a", IntegerInterval.Top, "a should be Top")
    checkVariableInExitState(programResult, "bar", "b", IntegerInterval.Top, "b should be Top")
    checkVariableInExitState(programResult, "foo", "x", IntegerInterval.Inner(Int.MinValue, 0), "x should be [-inf, 0]")
    checkVariableInExitState(programResult, "bar", "y", IntegerInterval.Inner(Int.MinValue, 0), "y should be [-inf, 0]")
  }

  def run(s: String): ProgramResult[IntegerIntervalAnalysisState] = {
    InterproceduralIntegerIntervalAnalysis.run(
      Compilable.Code("(no name)", s)
    )
  }
}