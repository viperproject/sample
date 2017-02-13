/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.Expression
import scala.collection.immutable

/**
  * @author Severin Münger
  *         Added on 07.11.16.
  */
case class PermissionRecords(permissions: Map[String, PermissionTree] = Map())
  extends immutable.Map[String, PermissionTree] {

  private def withDefault(field: String) =
    if (!permissions.contains(field)) permissions + (field -> EmptyPermissionTree)
    else permissions

  def simplifySyntactially: PermissionRecords =
    copy(permissions = permissions.transform {
      case (_, tree) => tree.simplifySyntactically
    })

  def simplifySemantically(state: QuantifiedPermissionsState): PermissionRecords =
    copy(permissions = permissions.transform {
      case (_, tree) => tree.simplifySemantically(state)
    })

  def lub(cond: Expression, elsePermissions: PermissionRecords): PermissionRecords =
    copy(elsePermissions ++ permissions.transform { case (field, tree) =>
      if (elsePermissions.contains(field)) tree.condition(cond, elsePermissions(field))
      else tree
    })

  def add(field: String, receiver: ExpressionDescription, permission: SimplePermission): PermissionRecords =
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.add(receiver, permission)
      case (_, other) => other
    })

  def sub(field: String, receiver: ExpressionDescription, permission: FractionalPermission): PermissionRecords =
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.sub(receiver, permission)
      case (_, other) => other
    })

  def max(field: String, receiver: ExpressionDescription, permission: Permission): PermissionRecords =
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.max(PermissionLeaf(receiver, permission))
      case (_, other) => other
    })

  def lub(other: PermissionRecords): PermissionRecords =
    copy(permissions ++ other.permissions.transform { case (field, tree) =>
      if (permissions.contains(field)) tree.max(permissions(field))
      else tree
    })

  def undoLastRead(field: String): PermissionRecords =
    copy(permissions.updated(field, permissions(field).undoLastRead))

  override def +[B1 >: PermissionTree](kv: (String, B1)): Map[String, B1] = permissions + kv

  def +(kv: (String, PermissionTree)): PermissionRecords = PermissionRecords(permissions + kv)

  override def get(key: String): Option[PermissionTree] = permissions.get(key)

  override def iterator: Iterator[(String, PermissionTree)] = permissions.iterator

  override def -(key: String): Map[String, PermissionTree] = PermissionRecords(permissions - key)
}