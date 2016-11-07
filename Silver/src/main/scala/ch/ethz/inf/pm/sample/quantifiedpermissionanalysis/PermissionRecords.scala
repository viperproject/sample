package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import scala.collection.mutable

/**
  * @author Severin Münger
  *         Added on 07.11.16.
  */
case class PermissionRecords(permissions: mutable.Map[String, PermissionTree] = new mutable.HashMap) {

  def copy = {
    PermissionRecords(permissions.mapValues(tree => tree.copy).asInstanceOf[mutable.Map[String, PermissionTree]])
  }

  def apply(field: String): PermissionTree = {
    if (!permissions.contains(field)) {
      permissions.put(field, ZeroPermission)
    }
    permissions(field)
  }
}
