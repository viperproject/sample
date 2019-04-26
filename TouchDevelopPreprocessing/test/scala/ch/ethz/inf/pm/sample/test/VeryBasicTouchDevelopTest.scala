/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis.TouchDevelopAnalysisRunner
import org.scalatest.FunSuite

/**
  * @author Lucas Brutschy
  */
class VeryBasicTouchDevelopTest extends FunSuite with SampleTest {

  test("Assignment") {
    assert(run(
      """action main() {
        |   $x := 0;
        |   $x->post_to_wall();
        |   skip;
        |}
      """.stripMargin
    ).isEmpty)
  }

  test("Test") {
    assert(run(
      """action main() {
        |     $pic := media->create_picture(10,10);
        |     code→doSomething($pic);
        |}
        |
        |
        |action doSomething(p: Picture) {
        |    $p2 := $p;
        |    $p3 := $p2;
        |    $p4 := $p3;
        |
        |    contract->assert(not($p4->is_invalid), "");
        |}
        |""".stripMargin
    ).isEmpty)
  }

  def run(s: String): List[SampleMessage] = {
    TouchDevelopAnalysisRunner.Default().run(Compilable.Code("(no name)", s)).collect { case x: SampleMessage => x }
  }

}
