/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.td.cloud.{Z3Prover, Z3Sample}
import org.scalatest.FunSuite
import ch.ethz.inf.pm.sample.abstractdomain.ExpressionFactory._
import ch.ethz.inf.pm.sample.oorepresentation.DummyProgramPoint

class Z3ProverTest extends FunSuite {

  implicit val tm = TypeMap()
  implicit val pp = DummyProgramPoint

  test("version") {
    Z3Sample.withZ3 { z3 =>
      println(z3.z3Version())
    }
  }

  test("Direct from string") {
    Z3Sample.withZ3 { z3 =>
      z3.emit("(declare-const a Int)")
      z3.emit("(assert (> a 10))")
      assert(z3.check(Some(100)) == Z3Prover.Sat)
    }
  }

  test("From expression builder") {
    Z3Sample.withZ3 { z3 =>
      val a = IntVar("a")
      z3.assume(a > 10)
      assert(z3.check(Some(100)) == Z3Prover.Sat)
    }
  }

}
