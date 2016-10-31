package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {

}

trait BinaryPermission extends PermissionTree {
  def left: PermissionTree
  def right: PermissionTree
}

case class MaxPermissionTree(left: PermissionTree, right: PermissionTree) extends BinaryPermission {

}