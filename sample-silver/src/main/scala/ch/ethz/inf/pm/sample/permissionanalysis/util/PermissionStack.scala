/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis.util

import ch.ethz.inf.pm.sample.abstractdomain.Lattice
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisTypes.AccessPath
import ch.ethz.inf.pm.sample.permissionanalysis.util.PermissionStack.Entry

/**
  * A stack of permission trees. In addition the the permission trees the stack
  * also stores the set of access paths that appear on the left hand side of an
  * assignment.
  *
  * @author Jerome Dohrau
  */
trait PermissionStack extends Lattice[PermissionStack] {
  override def factory(): PermissionStack = PermissionStack.empty

  override def top(): PermissionStack = PermissionStack.Top

  override def bottom(): PermissionStack = PermissionStack.Bottom

  /**
    * Returns the entry at the top of the stack.
    *
    * @return The entry at the top of the stack.
    */
  def head: Entry = entry(0)

  /**
    * Returns the permission tree at the top of the stack.
    *
    * @return The permission tree at the top of the stack.
    */
  def headTree: PermissionTree = head.tree

  /**
    * Returns the set of access paths at the top of the stack.
    *
    * @return The set of access paths at the top of the stack.
    */
  def headPaths: Set[AccessPath] = head.paths

  /**
    * Returns the entry at the given position in the stack.
    *
    * @param n The position of the entry.
    * @return The entry at the given position.
    */
  def entry(n: Int): Entry = entries.drop(n).headOption.getOrElse(Entry.empty)

  /**
    * Returns the list of entries stored in the stack.
    *
    * @return The list of entries in the stack.
    */
  def entries: List[Entry]

  /**
    * Returns the list of permission trees stored in the stack.
    *
    * @return The list of permission trees in the stack.
    */
  def trees: List[PermissionTree] = entries.map(_.tree)

  /**
    * Pushes an empty entry onto the stack.
    *
    * @return The resulting stack.
    */
  def push: PermissionStack

  /**
    * Pops an entry from the stack and merges it with the new top entry.
    *
    * @return The resulting stack.
    */
  def pop: PermissionStack

  /**
    * Updates the entry at the top of the stack with the given function.
    *
    * @param f The update function.
    * @return The updated stack.
    */
  def update(f: Entry => Entry): PermissionStack =
    PermissionStack(f(head) :: entries.drop(1))

  /**
    * Updates the permission tree at the top of the stack with the given
    * function.
    *
    * @param f The update function.
    * @return The updated stack.
    */
  def updateTree(f: PermissionTree => PermissionTree): PermissionStack =
    update(entry => Entry(f(entry.tree), entry.paths))

  /**
    * Updates the set of access paths at the top of the stack with the given
    * function.
    *
    * @param f The update function.
    * @return The updated stack.
    */
  def updatePaths(f: Set[AccessPath] => Set[AccessPath]): PermissionStack =
    update(entry => Entry(entry.tree, f(entry.paths)))

  /**
    * Maps the given function across the entries of the stack.
    *
    * @param f The mapping function.
    * @return The mapped stack.
    */
  def map(f: Entry => Entry): PermissionStack =
    if (isTop || isBottom) this else PermissionStack(entries.map(f))

  /**
    * Maps the given function across the permission trees in the stack.
    *
    * @param f The mapping function.
    * @return The mapped stack.
    */
  def mapTrees(f: PermissionTree => PermissionTree): PermissionStack =
    map(entry => Entry(f(entry.tree), entry.paths))

  /**
    * Maps the given function across the set of access paths in the stack.
    *
    * @param f The mapping function.
    * @return The mapped stack.
    */
  def mapPaths(f: Set[AccessPath] => Set[AccessPath]): PermissionStack =
    map(entry => Entry(entry.tree, f(entry.paths)))

  /**
    * Maps the given function across the permissions in the stack.
    *
    * @param f The mapping function.
    * @return THe mapped stack.
    */
  def mapPermissions(f: (AccessPath, PermissionTree) => Permission): PermissionStack =
    map(entry => Entry(entry.tree.map()(f), entry.paths))

  override def toString: String =
    if (isTop) "⊤"
    else if (isBottom) "⊥"
    else entries.zipWithIndex.map(x => s" ${x._2}: ${x._1}").mkString("\n")
}

object PermissionStack {
  def empty: PermissionStack = PermissionStack(Nil)

  def apply(entries: List[Entry]): PermissionStack = {
    if (entries.exists(_.tree.isBottom)) Bottom
    else Inner(entries)
  }

  /**
    * An entry holding a permission tree and a set of access paths.
    *
    * @param tree  The permission tree.
    * @param paths The set of access paths.
    */
  case class Entry(tree: PermissionTree, paths: Set[AccessPath]) {
    def lub(other: Entry): Entry =
      Entry(tree lub other.tree, paths | other.paths)

    def glb(other: Entry): Entry =
      Entry(tree glb other.tree, paths & other.paths)

    def widening(other: Entry): Entry =
      Entry(tree widening other.tree, paths | other.paths)

    def lessEqual(other: Entry): Boolean =
      (tree lessEqual other.tree) && (paths subsetOf other.paths)
  }

  case object Entry {
    def empty = Entry(PermissionTree.empty, Set.empty)
  }

  /**
    * The top permission stack.
    */
  case object Top extends PermissionStack with Lattice.Top[PermissionStack] {
    override def entries: List[Entry] =
      throw new UnsupportedOperationException("Top permission stack has no entries.")

    override def push: PermissionStack = this

    override def pop: PermissionStack = this
  }

  /**
    * The bottom permission stack.
    */
  case object Bottom extends PermissionStack with Lattice.Bottom[PermissionStack] {
    override def entries: List[Entry] =
      throw new UnsupportedOperationException("Bottom permission stack has no entries.")

    override def push: PermissionStack = this

    override def pop: PermissionStack = this
  }

  /**
    * A permission stack that is neither top or bottom.
    *
    * @param entries The entries of the stack.
    */
  case class Inner(entries: List[Entry]) extends PermissionStack {
    override def push: PermissionStack =
      PermissionStack(Entry.empty :: entries)

    override def pop: PermissionStack =
      PermissionStack((get(0) lub get(1)) :: entries.drop(2))

    override def isTop: Boolean = false

    override def isBottom: Boolean = false

    override def lub(other: PermissionStack): PermissionStack = other match {
      case Top => other
      case Bottom => this
      case other: Inner => PermissionStack(zip(other, _ lub _))
    }

    override def glb(other: PermissionStack): PermissionStack = other match {
      case Top => this
      case Bottom => other
      case other: Inner => PermissionStack(zip(other, _ glb _))
    }

    override def widening(other: PermissionStack): PermissionStack = other match {
      case Top => other
      case Bottom => this
      case other: Inner => PermissionStack(zip(other, _ widening _))
    }

    override def lessEqual(other: PermissionStack): Boolean = other match {
      case Top => true
      case Bottom => true
      case other: Inner => zip(other, _ lessEqual _).forall(identity)
    }

    private def size: Int = entries.size

    private def get(n: Int): Entry = entries.drop(n).headOption.getOrElse(Entry.empty)

    private def zip[R](other: Inner, f: (Entry, Entry) => R): List[R] =
      for (i <- List.range(0, math.max(size, other.size))) yield f(get(i), other.get(i))
  }

}