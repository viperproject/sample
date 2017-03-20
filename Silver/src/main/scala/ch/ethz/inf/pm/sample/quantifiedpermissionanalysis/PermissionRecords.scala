/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

import scala.collection.immutable

/**
  * @author Severin Münger
  *         Added on 07.11.16.
  */
case class PermissionRecords(permissions: Map[String, PermissionTree] = Map(), private var writeMap: Map[String, Boolean] = Map())
  extends immutable.Map[String, PermissionTree] {

  def addWrite(field: String, pp: ProgramPoint, receiver: Expression): PermissionRecords = {
    writeMap += field -> true
    max(field, ExpressionDescription(pp, receiver), WritePermission)
  }

  def addRead(field: String, pp: ProgramPoint, receiver: Expression): PermissionRecords = if (isLastWrite(field)) this else max(field, ExpressionDescription(pp, receiver), SymbolicReadPermission)

  def exhale(field: String, pp: ProgramPoint, receiver: Expression, permission: SimplePermission): PermissionRecords = (if (getOrElse(field, EmptyPermissionTree).hasRead) undoLastRead(field) else this).add(field, ExpressionDescription(pp, receiver), permission)

  def inhale(field: String, pp: ProgramPoint, receiver: Expression, permission: FractionalPermission): PermissionRecords = (if (getOrElse(field, EmptyPermissionTree).hasRead) undoLastRead(field) else this).sub(field, ExpressionDescription(pp, receiver), permission)

  def transformAssignVariable(variable: VariableIdentifier): PermissionRecords = copy(permissions.mapValues(_.transformAssignVariable(variable).transformForgetVariable(variable)))

  def transformAssignField(field: String): PermissionRecords = copy(permissions.mapValues(_.transformAssignField(field)))

  def lub(cond: Expression, elsePermissions: PermissionRecords): PermissionRecords =
    copy(Map((permissions.keySet ++ elsePermissions.keySet).map(field => (field, permissions.getOrElse(field, EmptyPermissionTree).condition(cond, elsePermissions.getOrElse(field, EmptyPermissionTree)))).toSeq :_*))

  def lub(other: PermissionRecords): PermissionRecords =
    copy(permissions ++ other.permissions.transform { case (field, tree) =>
      if (permissions.contains(field)) tree.max(permissions(field))
      else tree
    })

  def simplifySyntactically: PermissionRecords =
    copy(permissions = permissions.transform {
      case (_, tree) => tree.simplifySyntactically
    })

  def simplifySemantically(state: QuantifiedPermissionsState): PermissionRecords =
    copy(permissions = permissions.transform {
      case (_, tree) => tree.simplifySemantically(state)
    })

  def forget: PermissionRecords =
    copy(permissions = permissions.transform {
      case (_, tree) if tree.isIntegerDependent => tree.toForgottenTree
      case (_, other) => other
    })

  private def isLastWrite(field: String): Boolean = {
    val result = writeMap.getOrElse(field, false) && getOrElse(field, EmptyPermissionTree).isLastWrite
    writeMap += field -> false
    result
  }

  private def withDefault(field: String) =
    if (!permissions.contains(field)) permissions + (field -> EmptyPermissionTree)
    else permissions

  private def add(field: String, receiver: ExpressionDescription, permission: SimplePermission): PermissionRecords =
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.add(receiver, permission)
      case (_, other) => other
    })

  private def sub(field: String, receiver: ExpressionDescription, permission: FractionalPermission): PermissionRecords =
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.sub(receiver, permission)
      case (_, other) => other
    })

  private def max(field: String, receiver: ExpressionDescription, permission: Permission): PermissionRecords =
    copy(withDefault(field).transform {
      case (`field`, tree) => tree.max(PermissionLeaf(receiver, permission))
      case (_, other) => other
    })

  private def undoLastRead(field: String): PermissionRecords =
    copy(permissions.updated(field, permissions(field).undoLastRead))

  // Implementation of Map functions

  override def +[B1 >: PermissionTree](kv: (String, B1)): Map[String, B1] = permissions + kv

  def +(kv: (String, PermissionTree)): PermissionRecords = PermissionRecords(permissions + kv)

  override def get(key: String): Option[PermissionTree] = permissions.get(key)

  override def iterator: Iterator[(String, PermissionTree)] = permissions.iterator

  override def -(key: String): Map[String, PermissionTree] = PermissionRecords(permissions - key)
}