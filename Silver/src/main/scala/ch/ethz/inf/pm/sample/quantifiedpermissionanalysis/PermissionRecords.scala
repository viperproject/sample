/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.Expression

/**
  * @author Severin Münger
  *         Added on 07.11.16.
  */
case class PermissionRecords(permissions: Map[String, PermissionTree] = Map()) {

  private def withDefault(field: String) =
    if (!permissions.contains(field)) permissions + (field -> EmptyPermissionTree)
    else permissions

  def copy(permissions: Map[String, PermissionTree] = permissions) = PermissionRecords(permissions)

  def add(field: String, receiver: Expression, permission: Permission): PermissionRecords = {
    copy(withDefault(field).transform { case (`field`, tree) => tree.add(PermissionLeaf(receiver, permission))})
  }

  def sub(field: String, receiver: Expression, permission: Permission): PermissionRecords = {
    copy(withDefault(field).transform { case (`field`, tree) => tree.sub(PermissionLeaf(receiver, permission))})
  }

  def max(field: String, receiver: Expression, permission: Permission): PermissionRecords = {
    copy(withDefault(field).transform { case (`field`, tree) => tree.max(PermissionLeaf(receiver, permission))})
  }

  def lub(other: PermissionRecords): PermissionRecords = {
    copy(permissions ++ other.permissions.map { case (field, tree) =>
      if (permissions.contains(field)) field -> tree.max(permissions(field))
      else field -> tree
    })
  }

  def undoLastRead(field: String): PermissionRecords = {
    copy(permissions.updated(field, permissions(field).undoLastRead))
  }

  def transform(f: (Expression => Expression)): PermissionRecords = {
    PermissionRecords(permissions.map { case (field, permissionTree) => (field, permissionTree.transform(f)) })
  }

  def exists(f: (PermissionTree => Boolean)): Boolean = {
    permissions.exists { case (_, permissionTree) => permissionTree.exists(f) }
  }
}