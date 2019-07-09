/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain.Lattice

/**
  * A domain that lifts an existing domain to maps.
  *
  * @tparam K The type of the keys in the map.
  * @tparam V The type of the values in the map.
  * @author Jerome Dohrau
  */
sealed trait MapDomain[K, V <: Lattice[V]] extends Lattice[MapDomain[K, V]] {

  def default: V

  def get(key: K): V

  def set(key: K, value: V): MapDomain[K, V]

  def update(key: K, f: V => V): MapDomain[K, V]

  def map(f: (K, V) => V): MapDomain[K, V]

  def clear(): MapDomain[K, V]

  override def factory(): MapDomain[K, V] = MapDomain(default)

  override def top(): MapDomain[K, V] = MapDomain.Top(default)

  override def bottom(): MapDomain[K, V] = MapDomain.Bottom(default)
}

object MapDomain {

  def apply[K, V <: Lattice[V]](default: V): MapDomain[K, V] =
    MapDomain.Inner(Map.empty, default)

  case class Top[K, V <: Lattice[V]](default: V)
    extends MapDomain[K, V]
      with Lattice.Top[MapDomain[K, V]] {

    override def get(key: K): V = default.top()

    override def set(key: K, value: V): MapDomain[K, V] = this

    override def update(key: K, f: (V) => V): MapDomain[K, V] = this

    override def map(f: (K, V) => V): MapDomain[K, V] = this

    override def clear(): MapDomain[K, V] = this
  }

  case class Bottom[K, V <: Lattice[V]](default: V)
    extends MapDomain[K, V]
      with Lattice.Bottom[MapDomain[K, V]] {

    override def get(key: K): V = default.bottom()

    override def set(key: K, value: V): MapDomain[K, V] = this

    override def update(key: K, f: (V) => V): MapDomain[K, V] = this

    override def map(f: (K, V) => V): MapDomain[K, V] = this

    override def clear(): MapDomain[K, V] = this
  }

  case class Inner[K, V <: Lattice[V]](map: Map[K, V], default: V)
    extends MapDomain[K, V]
      with Lattice.Inner[MapDomain[K, V], Inner[K, V]] {

    override def get(key: K): V = map.getOrElse(key, default)

    override def set(key: K, value: V): MapDomain[K, V] =
      MapDomain.Inner(map.updated(key, value), default)

    override def update(key: K, f: (V) => V): MapDomain[K, V] =
      set(key, f(get(key)))

    override def map(f: (K, V) => V): MapDomain[K, V] =
      MapDomain.Inner(map.map { case (key, value) => key -> f(key, value) }, default)

    override def clear(): MapDomain[K, V] =
      MapDomain.Inner(Map.empty, default)

    override def lubInner(other: Inner[K, V]): MapDomain[K, V] = merge(this, other, _ lub _)

    override def glbInner(other: Inner[K, V]): MapDomain[K, V] = merge(this, other, _ glb _)

    override def wideningInner(other: Inner[K, V]): MapDomain[K, V] = merge(this, other, _ widening _)

    override def lessEqualInner(other: Inner[K, V]): Boolean = {
      val keys = map.keySet ++ other.map.keySet
      keys.forall { key => get(key) lessEqual other.get(key) }
    }
  }

  def merge[K, V <: Lattice[V]](left: MapDomain[K, V], right: MapDomain[K, V], combine: (V, V) => V): MapDomain[K, V] = (left, right) match {
    case (MapDomain.Inner(map1, default), MapDomain.Inner(map2, _)) =>
      val keys = map1.keySet ++ map2.keySet
      val map = keys.foldLeft(Map.empty[K, V]) { case (result, key) => result.updated(key, combine(left.get(key), right.get(key))) }
      MapDomain.Inner(map, default)
    case _ => ??? // TODO: Implement missing cases.
  }

}