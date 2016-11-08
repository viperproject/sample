package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.Expression

import scala.collection.mutable

/**
  * @author Severin Münger
  *         Added on 07.11.16.
  */
case class PermissionRecords(permissions: mutable.Map[String, PermissionTree] = new mutable.HashMap) {

  def lub(other: PermissionRecords) = max(other)

  def copy = PermissionRecords(mutable.HashMap(permissions.toSeq: _*))

  def apply(field: String): PermissionTree = {
    if (!permissions.contains(field)) {
      permissions.put(field, EmptyPermissionTree)
    }
    permissions(field)
  }

  def add(field: String, receiver: Expression, permission: Permission) = {
    val thisCopy = copy
    thisCopy.permissions.put(field, thisCopy(field).add(PermissionLeaf(receiver, permission)))
    thisCopy
  }

  def sub(field: String, receiver: Expression, permission: Permission) = {
    val thisCopy = copy
    thisCopy.permissions.put(field, thisCopy(field).sub(PermissionLeaf(receiver, permission)))
    thisCopy
  }

  def max(field: String, receiver: Expression, permission: Permission) = {
    val thisCopy = copy
    thisCopy.permissions.put(field, thisCopy(field).max(PermissionLeaf(receiver, permission)))
    thisCopy
  }

  def max(other: PermissionRecords) = {
    val thisCopy = copy
    other.permissions.foreach { case (field: String, permissionTree: PermissionTree) =>
      if (thisCopy.permissions.contains(field)) {
        thisCopy.permissions.put(field, thisCopy(field).max(permissionTree))
      } else {
        thisCopy.permissions.put(field, permissionTree)
      }
    }
    thisCopy
  }
}
