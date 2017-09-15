/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.silver.PermType

/**
  * A permission expression.
  *
  * @author Jerome Dohrau
  */
trait PermissionExpression
  extends Expression {

  override def typ: Type = PermType
}

/**
  * An expression representing a fractional permission.
  *
  * @param left  The expression representing the numerator.
  * @param right The expression representing the denominator.
  */
case class FractionalPermissionExpression(left: Expression, right: Expression)
  extends PermissionExpression
    with BinaryExpression {

  override def transform(f: (Expression) => Expression): Expression =
    f(FractionalPermissionExpression(left.transform(f), right.transform(f)))

  override def toString: String = s"$left/$right"
}
