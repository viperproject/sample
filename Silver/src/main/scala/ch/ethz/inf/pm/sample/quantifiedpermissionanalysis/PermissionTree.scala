package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.Expression

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {
  def toExpression: Expression = ???
}

trait Node extends PermissionTree {
  def left: PermissionTree
  def right: PermissionTree
}

trait Leaf extends PermissionTree {

}

case class BinaryPermission(left: PermissionTree, right: PermissionTree) extends Node {
  override def toExpression: Expression = ???
}