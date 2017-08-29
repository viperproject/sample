/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{AccessPathIdentifier, Expression, Identifier, VariableIdentifier}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Permission.{Read, Write}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Receiver.Path
import ch.ethz.inf.pm.sample.util.Maps

case class PermissionRecords(map: Map[Identifier, PermissionTree] = Map.empty) {
  def lub(other: PermissionRecords): PermissionRecords = {
    val updated = Maps.union(map, other.map, Maximum)
    PermissionRecords(updated)
  }

  def assume(condition: Expression): PermissionRecords = {
    val updated = map.mapValues(_.assume(condition))
    PermissionRecords(updated)
  }

  def assignVariable(variable: Expression, value: Expression): PermissionRecords = {
    // TODO: Implement me.
    this
  }

  def assignField(target: Expression, value: Expression): PermissionRecords = {
    // TODO: Implement me.
    this
  }

  def read(expression: Expression): PermissionRecords = access(expression, Read)

  def write(expression: Expression): PermissionRecords = access(expression, Write)

  def access(expression: Expression, permission: Permission): PermissionRecords = expression match {
    case _: VariableIdentifier => this
    case AccessPathIdentifier(path) =>
      if (path.length <= 1) this
      else {
        val receiver = AccessPathIdentifier(path.init)
        val field = path.last
        val leaf = Leaf(Path(receiver), permission)
        read(receiver).update(field, Maximum(_, leaf))
      }
    case _ => ???
  }

  def update(field: Identifier, f: PermissionTree => PermissionTree): PermissionRecords = {
    val updated = map + (field -> f(map(field)))
    PermissionRecords(updated)
  }
}

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

  case class Leaf(receiver: Receiver, permission: Permission)
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

sealed trait Receiver

object Receiver {

  case class Path(path: AccessPathIdentifier)
    extends Receiver

}

sealed trait Permission

object Permission {

  case object Read
    extends Permission

  case object Write
    extends Permission

}