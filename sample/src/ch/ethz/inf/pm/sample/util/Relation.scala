/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.SystemParameters

object Relation {
  def empty[V]:Relation[V] = new Relation[V](Map.empty,Map.empty)
}

class Relation[V](protected val forward:Map[V,Set[V]], protected val backward:Map[V,Set[V]]) {

  if (SystemParameters.DEBUG) {
    assert {
      val pairs1 = forward.map { x => x._2.map {
        (x._1, _)
      }
      }.flatten.toSet
      val pairs2 = backward.map { x => x._2.map {
        (_, x._1)
      }
      }.flatten.toSet
      pairs1.subsetOf(pairs2) && pairs2.subsetOf(pairs1)
    }
  }

  /** Most basic: add a relation */
  def add(k:V,v:V):Relation[V] = {
    val newForward = forward + (k -> (forward.getOrElse(k,Set.empty) + v ))
    val newBackward = backward + (v -> (backward.getOrElse(v,Set.empty) + k))
    new Relation(newForward,newBackward)
  }

  /** Most basic: remove a relation */
  def remove(k:V,v:V):Relation[V] = {
    (forward.get(k),backward.get(v)) match {
      case (Some(left),Some(right)) if left.contains(v) && right.contains(k) =>
        val newForward = if (left.size > 1) forward + (k -> (left - v)) else forward - k
        val newBackward = if (right.size > 1) backward + (v -> (right - k)) else backward - v
        new Relation(newForward, newBackward)
      case _ => throw new UnsupportedOperationException("Cannot remove non-existing key-value pair")
    }
  }

  // Remove operations

  def remove(v: V): Relation[V] =
    removeLeft(v).removeRight(v)
  def remove(vs: Set[V]): Relation[V] =
    vs.foldLeft(this)(_ remove _)
  def removeLeft(k:V):Relation[V] =
    getLeftOrElse(k,Set.empty).foldLeft(this)( (rel,v) => rel.remove(k,v) )
  def removeLeft(ks:Set[V]):Relation[V] =
    ks.foldLeft(this)(_ removeLeft _)
  def removeRight(v:V):Relation[V] =
    getRightOrElse(v,Set.empty).foldLeft(this)( (rel,k) => rel.remove(k,v) )
  def removeRight(vs:Set[V]):Relation[V] =
    vs.foldLeft(this)(_ removeRight _)

  // Fold operations. This is not the fold you are thinking of

  def fold(froms: Set[V], to: V): Relation[V] =
    foldLeft(froms, to).foldRight(froms, to)
  def foldLeft(froms: Set[V], to: V): Relation[V] =
    froms.foldLeft(this)( (rel,from) => getLeftOrElse(from,Set.empty).foldLeft(rel)( (rel2,right) => rel2.add(to,right)) ).removeLeft(froms - to)
  def foldRight(froms: Set[V], to: V): Relation[V] =
    froms.foldLeft(this)( (rel,from) => getRightOrElse(from,Set.empty).foldLeft(rel)( (rel2,left) => rel2.add(left,to)) ).removeRight(froms - to)

  // Expand operations

  def expand(from: V, tos: Set[V]): Relation[V] =
    expandLeft(from,tos).expandRight(from,tos)
  def expandLeft(from: V, tos: Set[V]): Relation[V] =
    tos.foldLeft(this)( (rel,to) => getLeftOrElse(from,Set.empty).foldLeft(rel)( (rel2,right) => rel2.add(to,right)) ).removeLeft(Set(from) --  tos)
  def expandRight(from: V, tos: Set[V]): Relation[V] =
    tos.foldLeft(this)( (rel,to) => getRightOrElse(from,Set.empty).foldLeft(rel)( (rel2,left) => rel2.add(left,to)) ).removeRight(Set(from) --  tos)

  // Rename operations

  def rename(from: V, to: V): Relation[V] =
    renameLeft(from,to).renameRight(from,to)
  def renameLeft(from: V, to: V): Relation[V] =
    getLeftOrElse(from,Set.empty).foldLeft(this)( (rel,right) => rel.add(to,right)).removeLeft(Set(from) -  to)
  def renameRight(from: V, to: V): Relation[V] =
    getRightOrElse(from,Set.empty).foldLeft(this)( (rel,left) => rel.add(left,to)).removeRight(Set(from) -  to)

  def isEmpty: Boolean = forward.isEmpty

  def subSetOf(other: Relation[V]): Boolean =
    MapUtil.mapToSetContainment(this.forward, other.forward)

  def union(other: Relation[V]): Relation[V] =
    new Relation(MapUtil.mapToSetUnion(this.forward, other.forward),MapUtil.mapToSetUnion(this.backward, other.backward))

  def intersect(other: Relation[V]): Relation[V] =
    new Relation(MapUtil.mapToSetIntersection(this.forward, other.forward),MapUtil.mapToSetIntersection(this.backward, other.backward))

  def getAll:Set[V] = getAllLeft ++ getAllRight
  def getAllLeft:Set[V] = forward.keySet
  def getAllRight:Set[V] = backward.keySet

  def getLeft(k:V):Option[Set[V]] = forward.get(k)
  def getRight(v:V):Option[Set[V]] = backward.get(v)

  def getLeftOrElse(k:V,v:Set[V]):Set[V] = forward.getOrElse(k,v)
  def getRightOrElse(v:V,k:Set[V]):Set[V] = backward.getOrElse(v,k)

  def add(k:V,vs:Set[V]):Relation[V] = vs.foldLeft(this)(_.add(k,_))
  def add(ks:Set[V],v:V):Relation[V] = ks.foldLeft(this)(_.add(_,v))
  def add(ks:Set[V],vs:Set[V]):Relation[V] = ks.foldLeft(this){ (x,k) => vs.foldLeft(x)(_.add(k,_)) }

  def setLeft(k:V,v:V):Relation[V] = removeLeft(k).add(k,v)
  def setLeft(k:V,vs:Set[V]):Relation[V] = removeLeft(k).add(k,vs)
  def setRight(k:V,v:V):Relation[V] = removeRight(v).add(k,v)
  def setRight(ks:Set[V],v:V):Relation[V] = removeRight(v).add(ks,v)

  def closure(el: V):Set[V] = {
    var toVisit = Set(el)
    var visited = Set.empty[V]
    while (toVisit.nonEmpty) {
      val first = toVisit.head
      visited = visited + first
      toVisit = (toVisit ++ forward.getOrElse(first,Set.empty) ++ backward.getOrElse(first,Set.empty)) -- visited
    }
    visited
  }

  def leftClosure(left: V):Set[V] = {
    var toVisit = Set(left)
    var visited = Set.empty[V]
    while (toVisit.nonEmpty) {
      val first = toVisit.head
      visited = visited + first
      toVisit = (toVisit ++ forward.getOrElse(first,Set.empty)) - first
    }
    visited
  }

  def rightClosure(right: V):Set[V] = {
    var toVisit = Set(right)
    var visited = Set.empty[V]
    while (toVisit.nonEmpty) {
      val first = toVisit.head
      visited = visited + first
      toVisit = (toVisit ++ backward.getOrElse(right,Set.empty)) - right
    }
    visited
  }


}
