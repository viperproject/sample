/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.td.analysis.{TouchDevelopAnalysisRunner, TouchRun}
import ch.ethz.inf.pm.td.cloud.CloudAnalysisState
import org.scalatest.FunSuite

/**
  * @author Lucas Brutschy
  */
class BasicWeakConsistencyTest extends FunSuite with SampleTest {

  test("We should support basic references") {
    run(
      """action main() {
        |   data→x := 0;
        |   data→x→◈add(1);
        |   contract→assert(data→x = 1, "test");
        |}
        |var x : Number {
        |   transient = true;
        |}
      """.stripMargin
    )
  }

  // Cloud fields // replicated variables.
  // - Boolean
  // - Color
  // - DateTime
  // - Json Object
  // - Location
  // - Link
  // - Number
  // - OAuth Response
  // - String
  // - User
  // Cloud keys:
  // - Boolean
  // - Color
  // - DateTime
  // - Location
  // - Number
  // - String
  // - User
  // - Vector3
  // In tables

//  test("We should be able to write a cloud-connected application") {
//
//    val stringsAndIntegers =
//      """action main() {
//        |   // === 4 times: Read stringGlobal
//        |   $x := data→stringGlobal;
//        |   data→stringGlobal→post_to_wall;
//        |   wall→prompt(data→stringGlobal);
//        |   $sGref := data→stringGlobal→◈ref;
//        |   $sGref→◈ref→◈ref→◈ref→◈get→post_to_wall;
//        |   // === 4 times: set / test and set / clear stringGlobal
//        |   data→stringGlobal := "dfsdfsdf";
//        |   $sGref→◈ref→◈test_and_set("99");
//        |   $sGref→◈set("88");
//        |   $sGref→◈clear;
//        |   // === 3 times: restricted reads of value
//        |   $sGref→◈get→starts_with("blublablu");
//        |   data→stringGlobal→is_invalid;
//        |   data→stringGlobal→count;
//        |   // === 3 Times, change numerical value
//        |   $nGref := data→numberGlobal→◈ref;
//        |   $nGref→◈add(1);
//        |   $nGref→◈clear;
//        |   data→numberGlobal→◈set(0);
//        |   // === 2 Times, restricted reads of ref3
//        |   $nGref→◈get < 5;
//        |   5 < $nGref→◈get;
//        |}
//        |var numberGlobal : Number {
//        |  cloudenabled = true;
//        |}
//        |var stringGlobal : String {
//        |  cloudenabled = true;
//        |}
//      """.stripMargin
//
//    TouchRun.runInThread(Compilable.Code("StringsAndIntegers", stringsAndIntegers))
//    println(AbstractEventGraph.toString)
//
//  }

  def run(s: String): Unit = {
    TouchDevelopAnalysisRunner.Default().run(Compilable.Code("A Test", s))
  }

}
