/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis.util

import ch.ethz.inf.pm.sample.abstractdomain.Identifier
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisTypes.AccessPath


/**
  * @param permission amount of permission for the root of the tree
  * @param children   maps fields (identifiers) to subtrees
  */
case class PermissionTree(permission: Permission = Permission.none,
                          children: Map[Identifier, PermissionTree] = Map.empty) {
  /** Is true if this tree contains no permission.
    */
  lazy val isEmpty: Boolean =
    permission.isNone && children.forall { case (_, child) => child.isEmpty }

  /** Is true if this tree contains some permission.
    */
  lazy val nonEmpty: Boolean =
    !isEmpty

  /** Returns the least upper bound of this permission tree and the other permission tree.
    *
    * @param other The other permission tree.
    */
  def lub(other: PermissionTree): PermissionTree = {
    // compute lub of permissions
    val newPermission = permission lub other.permission
    // compute child-wise lub of subtrees
    val newChildren = children.foldLeft(other.children) {
      case (map, (id, subtree)) => map.get(id) match {
        case Some(existing) => map + (id -> (subtree lub existing))
        case None => map + (id -> subtree)
      }
    }
    PermissionTree(newPermission, newChildren)
  }

  /** Returns the greatest lower bound of this permission tree and the other permission tree.
    *
    * @param other The other permission tree.
    */
  def glb(other: PermissionTree): PermissionTree = {
    // computer glb of permissions
    val newPermission = permission glb other.permission
    // compute child-wise glb of subtrees
    val newChildren = children.foldLeft(Map.empty[Identifier, PermissionTree]) {
      case (map, (id, subtree)) => other.children.get(id) match {
        case Some(existing) => map + (id -> (subtree glb existing))
        case None => map
      }
    }
    PermissionTree(newPermission, newChildren)
  }

  /** Returns the widening of this and the other permission tree.
    *
    * @param other The other permission tree.
    * @return The widening of this and the other permission tree.
    */
  def widening(other: PermissionTree): PermissionTree = {
    // TODO: Implement me properly. Currently this is only a point-wise widening
    // compute widening of permissions
    val newPermission = permission widening other.permission
    // compute child-wise widening of subtrees
    val newChildren = children.foldLeft(other.children) {
      case (map, (id, subtree)) => map.get(id) match {
        case Some(existing) => map + (id -> (subtree widening existing))
        case None => map + (id -> subtree)
      }
    }
    PermissionTree(newPermission, newChildren)
  }

  /** Returns whether the amount of permission of this permission tree is less
    * than or equal to the amount of permission  of the other tree.
    *
    * @param other The other permission.
    */
  def lessEqual(other: PermissionTree): Boolean = {
    if (permission lessEqual other.permission) {
      // check for all subtrees whether other has a corresponding subtree that has at least as much permission
      children.forall {
        case (id, subtree) => other.children.get(id) match {
          case Some(existing) => subtree lessEqual existing
          case None => false
        }

      }
    } else false // permission is larger
  }

  /** Returns the amount of permission for the given access path.
    *
    * @param path The access path.
    * @return The amount of permission for the given access path.
    */
  def get(path: AccessPath): Permission = {
    if (path.isEmpty) permission
    else children.get(path.head) match {
      case Some(child) => child.get(path.tail)
      case None => Permission.none
    }
  }

  /** Extracts the subtree at the specified path and returns the remainder of
    * the tree as well as the extracted subtree.
    *
    * @param path The path to the subtree to be extracted.
    * @return A tuple containing the remainder of the tree and the extracted
    *         subtree.
    */
  def extract(path: AccessPath): (PermissionTree, PermissionTree) = {
    if (path.isEmpty) {
      // base case: extract the entire subtree
      val remainder = PermissionTree(permission)
      val extracted = PermissionTree(Permission.none, children)
      (remainder, extracted)
    } else {
      // recursively extract subtree from child corresponding to head of path
      val id = path.head
      children.get(id) match {
        case Some(child) =>
          val (updated, extracted) = child.extract(path.tail)
          val remainder = PermissionTree(permission, children + (id -> updated))
          (remainder, extracted)
        case None => (this, PermissionTree()) // there is nothing to extract
      }
    }
  }

  /** Implants the specified permission tree at the specified path. If there is
    * already a non-empty subtree at that path the least upper bound is
    * computed.
    *
    * @param path  The tree to be implanted.
    * @param other The path to the place where the permission tree is to be
    *              implanted.
    * @return This permission tree with the other permission tree implanted.
    */
  def implant(path: AccessPath, other: PermissionTree): PermissionTree = {
    if (path.isEmpty) {
      // base case: implant other at root
      this lub other
    } else {
      // recursively implant other into subtree corresponding to head of path
      val id = path.head
      val updated = children.get(id) match {
        case Some(child) => child.implant(path.tail, other)
        case None =>
          // the path does not exist in the tree,
          // thus, we create it and implant te tree there
          path.tail.foldRight(other) {
            case (id, subtree) => PermissionTree(children = Map(id -> subtree))
          }
      }
      PermissionTree(permission, children + (id -> updated))
    }
  }

  /** Applies the specified function to all permissions stored in the tree. The
    * function takes as arguments the current access path and the permission to
    * be modified. At the root of the tree the path is assumed to be the
    * the variable the tree corresponds to.
    *
    * @param path The current access path.
    * @param f    The function to apply to all permissions in the tree.
    */
  def map(path: AccessPath, f: (AccessPath, PermissionTree) => Permission): PermissionTree = {
    val newPermission = f(path, this)
    val newChildren = children.map {
      case (id, tree) => (id, tree.map(path :+ id, f))
    }
    PermissionTree(newPermission, newChildren)
  }

  def fold[R](z: R)(path: AccessPath, f: (R, (AccessPath, PermissionTree)) => R): R =
    children.foldLeft(f(z, (path, this))) { case (res, (id, child)) =>
      child.fold(res)(path :+ id, f)
    }
}
