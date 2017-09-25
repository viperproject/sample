/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
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

/**
  * An expression representing the maximum of a permission and no permission.
  * This is useful to ensure that permissions are non-negative.
  *
  * @param permission The permission.
  */
case class Bound(permission: Expression)
  extends PermissionExpression {

  override def pp: ProgramPoint = permission.pp

  override def ids: IdentifierSet = permission.ids

  override def contains(f: (Expression) => Boolean): Boolean =
    f(this) || permission.contains(f)

  override def transform(f: (Expression) => Expression): Expression =
    f(Bound(permission.transform(f)))

  override def toString: String = s"bound($permission)"
}