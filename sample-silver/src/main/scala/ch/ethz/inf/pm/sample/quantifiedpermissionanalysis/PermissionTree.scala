/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._

sealed trait PermissionTree {

  def bound(): PermissionTree = Maximum(this, Empty)

  def assume(condition: Expression) = Conditional(condition, this, Empty)

  def transform(f: PermissionTree => PermissionTree): PermissionTree = this match {
    case Addition(left, right) => f(Addition(left.transform(f), right.transform(f)))
    case Subtraction(left, right) => f(Subtraction(left.transform(f), right.transform(f)))
    case Maximum(left, right) => f(Maximum(left.transform(f), right.transform(f)))
    case Conditional(condition, left, right) => f(Conditional(condition, left.transform(f), right.transform(f)))
    case tree => f(tree)
  }

  def compose(other: PermissionTree): PermissionTree = transform {
    case Initial => other
    case tree => tree
  }
}

object PermissionTree {
  /**
    * Represents no permissions.
    */
  case object Empty
    extends PermissionTree {
  }

  /**
    * Represents a placeholder to some arbitrary initial permissions.
    */
  case object Initial
    extends PermissionTree

  case class Leaf(expression: Expression)
    extends PermissionTree

  case class Addition(left: PermissionTree, right: PermissionTree)
    extends PermissionTree

  case class Subtraction(left: PermissionTree, right: PermissionTree)
    extends PermissionTree

  case class Maximum(left: PermissionTree, right: PermissionTree)
    extends PermissionTree

  case class Conditional(condition: Expression, left: PermissionTree, right: PermissionTree)
    extends PermissionTree
}