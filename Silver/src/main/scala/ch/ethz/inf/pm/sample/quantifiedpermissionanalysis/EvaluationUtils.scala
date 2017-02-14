/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{BinaryBooleanExpression, Expression}

/**
  * @author Severin Münger
  *         Added on 14.02.17.
  */
object EvaluationUtils {

  def countLiterals(expr: Expression): Int = {
    var count = 1
    expr.foreach {
      case _: BinaryBooleanExpression => count += 1
      case _ =>
    }
    count
  }

}
