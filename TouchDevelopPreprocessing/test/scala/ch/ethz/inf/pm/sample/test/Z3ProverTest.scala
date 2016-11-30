/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionFactory
import ch.ethz.inf.pm.td.cloud.Z3Prover
import org.scalatest.FunSuite

class Z3ProverTest extends FunSuite {

  test("version") {
    Z3Prover.withZ3 { z3 =>
      println(z3.z3Version())
    }
  }

  test("Direct from string") {
    Z3Prover.withZ3 { z3 =>
      z3.emit("(declare-const a Int)")
      z3.emit("(assert (> a 10))")
      assert(z3.check(Some(100)) == Z3Prover.Sat)
    }
  }

  test("Direct from string") {
    Z3Prover.withZ3 { z3 =>
      z3.assume(ExpressionFactory.createBinaryExpression())
      z3.emit("(assert (> a 10))")
      assert(z3.check(Some(100)) == Z3Prover.Sat)
    }
  }

}
