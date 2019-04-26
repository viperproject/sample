/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.permissionanalysis.util

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, Lattice}
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisTypes.{AccessPath, Tuple, Tuples}

/**
  * A permission tree that represents a set of tuples containing access paths
  * and permissions.
  *
  * @author Jerome Dohrau
  */
trait PermissionTree extends Lattice[PermissionTree] {
  override def factory(): PermissionTree = PermissionTree.empty

  override def top(): PermissionTree = PermissionTree.Top

  override def bottom(): PermissionTree = PermissionTree.Bottom

  /**
    * Returns the permission at the root of the tree.
    *
    * @return The permission at the root of the tree.
    */
  def permission(): Permission

  /**
    * Returns the children of the root.
    *
    * @return The children.
    */
  def children(): Map[Identifier, PermissionTree]

  /**
    * Returns whether the permission tree is empty, i.e., does not contain any
    * permissions.
    *
    * @return True if the permission tree is empty, otherwise false.
    */
  def isEmpty(): Boolean

  /**
    * Returns whether the permission tree is nonempty, i.e., contains some
    * permissions.
    *
    * @return True if the permission tree is nonempty, otherwise false.
    */
  def nonEmpty(): Boolean = !isEmpty()

  /**
    * Returns the point wise addition of this and the other permission tree.
    *
    * @param other The other permission tree.
    * @return The point wise addition of this and the other permission tree.
    */
  def plus(other: PermissionTree): PermissionTree

  /**
    * Returns the amount of permission for the given access path.
    *
    * @param path The access path.
    * @return The amount of permission for the given access path.
    */
  def get(path: AccessPath): Permission

  /**
    * Removes the subtree corresponding to the given identifier.
    *
    * @param id The identifier of the subtree to remove.
    * @return The permission tree with the subtree removed.
    */
  def remove(id: Identifier): PermissionTree

  /**
    * Extracts the subtree corresponding to the given path and returns the
    * remainder of the permission tree as well as the extracted subtree.
    *
    * @param path The path corresponding to the subtree to extract.
    * @return The remainder of the permission tree and the extracted subtree.
    */
  def extract(path: AccessPath): (PermissionTree, PermissionTree)

  /**
    * Implants the other permission tree at the subtree corresponding to the
    * given path. If  there is already a non-empty subtree the least upper bound
    * is computed.
    *
    * @param path  The access path.
    * @param other The tree to implant.
    * @return The permission tree with the other tree implanted.
    */
  def implant(path: AccessPath, other: PermissionTree): PermissionTree

  def makeSelfFraming(): PermissionTree

  def map(path: AccessPath = Nil)(f: (AccessPath, PermissionTree) => Permission): PermissionTree

  def fold[R](z: R, path: AccessPath = Nil)(f: (R, (AccessPath, PermissionTree)) => R): R

  def tuples(): Tuples = fold(List.empty[Tuple]) {
    case (list, (path, subtree)) => list :+ (path, subtree.permission)
  }.filter {
    case (location, permission) => location.nonEmpty && permission.isSome
  }
}

object PermissionTree {
  def empty: PermissionTree = PermissionTree(Permission.none, Map.empty)

  def create(path: AccessPath, permission: Permission): PermissionTree =
    path.foldRight(PermissionTree(permission, Map.empty)) {
      case (field, subtree) => PermissionTree(Permission.none, Map(field -> subtree))
    }

  def apply(permission: Permission, children: Map[Identifier, PermissionTree]): PermissionTree = {
    //assert(children.values.forall(_.nonEmpty()))
    if (permission.isBottom || children.values.exists(_.isBottom)) PermissionTree.Bottom
    else PermissionTree.Inner(permission, children)
  }

  /**
    * The top permission tree.
    */
  case object Top extends PermissionTree with Lattice.Top[PermissionTree] {
    override def permission(): Permission = Permission.Top

    override def children(): Map[Identifier, PermissionTree] = ???

    override def isEmpty(): Boolean = false

    override def plus(other: PermissionTree): PermissionTree = this

    override def get(path: AccessPath): Permission = Permission.Top

    override def remove(id: Identifier): PermissionTree = this

    override def extract(path: AccessPath): (PermissionTree, PermissionTree) = (this, this)

    override def implant(path: AccessPath, other: PermissionTree): PermissionTree = this

    override def makeSelfFraming(): PermissionTree = this

    override def map(path: AccessPath)(f: (AccessPath, PermissionTree) => Permission): PermissionTree = this

    override def fold[R](z: R, path: AccessPath)(f: (R, (AccessPath, PermissionTree)) => R): R
    = throw new UnsupportedOperationException("Cannot fold top permission tree.")

    override def toString: String = "⊤"
  }

  /**
    * The bottom permission tree.
    */
  case object Bottom extends PermissionTree with Lattice.Bottom[PermissionTree] {
    override def permission(): Permission = Permission.Bottom

    override def children(): Map[Identifier, PermissionTree] = ???

    override def isEmpty(): Boolean = true

    override def plus(other: PermissionTree): PermissionTree = other

    override def get(path: AccessPath): Permission = Permission.Bottom

    override def remove(id: Identifier): PermissionTree = this

    override def extract(path: AccessPath): (PermissionTree, PermissionTree) = (this, this)

    override def implant(path: AccessPath, other: PermissionTree): PermissionTree = this

    override def makeSelfFraming(): PermissionTree = this

    override def map(path: AccessPath)(f: (AccessPath, PermissionTree) => Permission): PermissionTree = this

    override def fold[R](z: R, path: AccessPath)(f: (R, (AccessPath, PermissionTree)) => R): R = z

    override def toString: String = "⊥"
  }

  /**
    * A permission tree that is neither top or bottom.
    *
    * @param permission The permission at the root of the permission tree.
    * @param children   The map containing the subtrees of the permission tree.
    */
  case class Inner(permission: Permission, children: Map[Identifier, PermissionTree])
    extends PermissionTree {
    override def isEmpty() =
      permission.isNone && children.values.forall(_.isEmpty())

    override def isTop: Boolean = false

    override def isBottom: Boolean = false

    override def lub(other: PermissionTree): PermissionTree = other match {
      case PermissionTree.Top => other
      case PermissionTree.Bottom => this
      case other: PermissionTree.Inner =>
        // compute lub of permissions
        val permission = this.permission lub other.permission
        // compute child wise lub of subtrees
        val children = this.children.foldLeft(other.children) {
          case (map, (id, subtree)) => map.get(id) match {
            case Some(existing) => map + (id -> (subtree lub existing))
            case None => map + (id -> subtree)
          }
        }
        PermissionTree(permission, children)
    }

    override def glb(other: PermissionTree): PermissionTree = other match {
      case PermissionTree.Top => this
      case PermissionTree.Bottom => other
      case other: PermissionTree.Inner =>
        // compute glb of permissions
        val permission = this.permission glb other.permission
        // compute child wise glb of subtrees
        val children = this.children.foldLeft(Map.empty[Identifier, PermissionTree]) {
          case (map, (id, subtree)) => other.children.get(id) match {
            case Some(existing) => map + (id -> (subtree glb existing))
            case None => map
          }
        }
        PermissionTree(permission, children)
    }

    override def widening(other: PermissionTree): PermissionTree = other match {
      case PermissionTree.Top => other
      case PermissionTree.Bottom => this
      case other: PermissionTree.Inner =>
        // TODO: Implement me properly.
        // Currently this is only a child wise widening.
        // compute widening of permissions
        val permission = this.permission widening other.permission
        // compute child wise widening of subtrees
        val children = this.children.foldLeft(other.children) {
          case (map, (id, subtree)) => map.get(id) match {
            case Some(existing) => map + (id -> (subtree widening existing))
            case None => map + (id -> subtree)
          }
        }
        PermissionTree(permission, children)
    }

    override def lessEqual(other: PermissionTree): Boolean = other match {
      case PermissionTree.Top => true
      case PermissionTree.Bottom => false
      case other: PermissionTree.Inner =>
        if (permission lessEqual other.permission) {
          // check for all subtrees whether other has a corresponding subtree
          // that has at least as much permission
          children.forall {
            case (id, subtree) => other.children.get(id) match {
              case Some(existing) => subtree lessEqual existing
              case None => false
            }
          }
        } else false
    }

    override def plus(other: PermissionTree): PermissionTree = other match {
      case PermissionTree.Top => other
      case PermissionTree.Bottom => this
      case other: PermissionTree.Inner =>
        // add permissions
        val permission = this.permission plus other.permission
        // compute child wise addition of subtrees
        val children = this.children.foldLeft(other.children) {
          case (map, (id, subtree)) => map.get(id) match {
            case Some(existing) => map + (id -> (subtree plus existing))
            case None => map + (id -> subtree)
          }
        }
        PermissionTree(permission, children)
    }

    override def get(path: AccessPath): Permission =
      if (path.isEmpty) permission
      else children.get(path.head) match {
        case Some(subtree) => subtree.get(path.tail)
        case None => Permission.none
      }

    override def remove(id: Identifier): PermissionTree =
      PermissionTree(permission, children - id)

    override def extract(path: AccessPath): (PermissionTree, PermissionTree) =
      if (path.isEmpty) {
        // extract entire tree
        val remainder = PermissionTree(permission, Map.empty)
        val extracted = PermissionTree(Permission.none, children)
        (remainder, extracted)
      } else {
        // recursively extract subtree
        children.get(path.head) match {
          case Some(subtree) =>
            val (updated, extracted) = subtree.extract(path.tail)
            val remainder = PermissionTree(subtree.permission(), children + (path.head -> updated))
            (remainder, extracted)
          case None => (this, PermissionTree.empty)
        }
      }

    override def implant(path: AccessPath, other: PermissionTree): PermissionTree =
      if (path.isEmpty) this lub other
      else {
        // recursively implant tree
        val updated = children.get(path.head) match {
          case Some(subtree) => subtree.implant(path.tail, other)
          case None => path.tail.foldRight(other) {
            case (id, subtree) => PermissionTree(Permission.none, Map(id -> subtree))
          }
        }
        PermissionTree(permission, children + (path.head -> updated))
      }

    override def makeSelfFraming(): PermissionTree = {
      val selfFramedChildren = children.mapValues(_.makeSelfFraming())
      if (permission.isNone && nonEmpty()) PermissionTree(Permission.read, selfFramedChildren)
      else PermissionTree(permission, selfFramedChildren)
    }

    override def map(path: AccessPath)(f: (AccessPath, PermissionTree) => Permission): PermissionTree = {
      val permission = f(path, this)
      val children = this.children.map { case (id, subtree) => (id, subtree.map(path :+ id)(f)) }
      PermissionTree(permission, children)
    }

    override def fold[R](z: R, path: AccessPath)(f: (R, (AccessPath, PermissionTree)) => R): R =
      children.foldLeft[R](f(z, (path, this))) {
        case (res, (id, subtree)) => subtree.fold(res, path :+ id)(f)
      }

    override def toString: String =
      tuples().map { case (location, amount) => s"(${location.mkString(".")}, $amount)" }.mkString(", ")
  }

}