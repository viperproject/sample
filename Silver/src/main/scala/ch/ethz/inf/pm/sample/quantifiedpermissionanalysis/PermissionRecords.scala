package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.Expression

import scala.collection.mutable

/**
  * @author Severin Münger
  *         Added on 07.11.16.
  */
case class PermissionRecords(permissions: mutable.Map[String, PermissionTree] = new mutable.HashMap) {

  def copy = PermissionRecords(mutable.HashMap(permissions.toSeq: _*))

  def apply(field: String): PermissionTree = {
    if (!permissions.contains(field)) {
      permissions.put(field, EmptyPermissionTree)
    }
    permissions(field)
  }

  def add(field: String, receiver: Expression, permission: Permission) = {
    val thisCopy = copy
    thisCopy(field).add()
    thisCopy
  }
}
