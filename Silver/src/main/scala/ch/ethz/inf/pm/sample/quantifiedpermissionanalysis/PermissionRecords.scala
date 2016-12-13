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

  def copy(permissions: Map[String, PermissionTree] = permissions) = PermissionRecords(permissions)

  def add(field: String, receiver: ExpressionDescription, permission: Permission): PermissionRecords = {
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.add(PermissionLeaf(receiver, permission))
      case (_, other) => other
    })
  }

  def sub(field: String, receiver: ExpressionDescription, permission: Permission): PermissionRecords = {
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.sub(PermissionLeaf(receiver, permission))
      case (_, other) => other
    })
  }

  def max(field: String, receiver: ExpressionDescription, permission: Permission): PermissionRecords = {
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.max(PermissionLeaf(receiver, permission))
      case (_, other) => other
    })
  }

  def lub (other: PermissionRecords): PermissionRecords = {
    copy(permissions ++ other.permissions.transform { case (field, tree) =>
      if (permissions.contains(field)) tree.max(permissions(field))
      else tree
    })
  }

  def undoLastRead(field: String): PermissionRecords = {
    copy(permissions.updated(field, permissions(field).undoLastRead))
  }

  def transformExpressions(f: (Expression => Expression)): PermissionRecords = {
    PermissionRecords(permissions.transform { case (_, permissionTree) => permissionTree.transform(f) })
  }

  def existsPermissionTree(f: (PermissionTree => Boolean)): Boolean = {
    permissions.exists { case (_, permissionTree) => permissionTree.exists(f) }
  }

  override def +[B1 >: PermissionTree](kv: (String, B1)): Map[String, B1] = permissions + kv

  def +(kv: (String, PermissionTree)): PermissionRecords = PermissionRecords(permissions + kv)

  override def get(key: String): Option[PermissionTree] = permissions.get(key)

  override def iterator: Iterator[(String, PermissionTree)] = permissions.iterator

  override def -(key: String): Map[String, PermissionTree] = PermissionRecords(permissions - key)
}