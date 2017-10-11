/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import ch.ethz.inf.pm.sample.abstractdomain.{BigMax, ConditionalExpression, Exists}
import ch.ethz.inf.pm.sample.elimination.{MaximumElimination, QuantifierElimination}
import ch.ethz.inf.pm.sample.oorepresentation.silver.IntType
import ch.ethz.inf.pm.sample.test.SampleTest
import ch.ethz.inf.pm.sample.util.SampleExpressions._
import org.scalatest.FunSuite

class EliminationTest extends FunSuite with SampleTest {

  val x = Variable("x", IntType)
  val y = Variable("y", IntType)
  val z = Variable("z", IntType)

  test("exists x: y <= x && x <= z") {
    val formula = Exists(x, And(LessEqual(y, x), LessEqual(x, z)))
    val expected = LessEqual(y, z)
    val result = QuantifierElimination.eliminate(formula)
    assert { result == expected }
  }

  test("k") {
    val l = ConditionalExpression(LessEqual(x, Two), Two, Zero)
    val r = ConditionalExpression(LessEqual(Three, x), One, Zero)
    val s = Plus(l, r)
    val c = And(LessEqual(One, x), LessEqual(x, Four))
    val formula = BigMax(x, ConditionalExpression(c, s, Zero))
    val expected = Two
    val result = MaximumElimination.eliminate(formula)
    assert { result == expected }
  }
}
