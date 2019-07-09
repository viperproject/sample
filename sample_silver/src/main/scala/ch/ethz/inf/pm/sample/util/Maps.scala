/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.util

/**
  * Some utility functions for maps.
  *
  * @author Jerome Dohrau
  */
object Maps {
  /**
    * Returns the union of the two given maps. The resulting map associates a
    * key with a value if either of the maps associates the key with that value.
    * If both of the maps associate a value with some key then the given
    * function is used to combine the corresponding values.
    *
    * @param first   The first map.
    * @param second  The second map.
    * @param combine The function used to combine two elements.
    * @tparam K The type of the keys.
    * @tparam V The type of the values.
    * @return The union of the maps.
    */
  def union[K, V](first: Map[K, V], second: Map[K, V], combine: (V, V) => V): Map[K, V] = first.foldLeft(second) {
    case (map, (key, value)) => map.get(key) match {
      case Some(existing) => map + (key -> combine(value, existing))
      case None => map + (key -> value)
    }
  }

  /**
    * Returns the intersection of the two given maps. The resulting map
    * associates a key with a value if both maps associate a value with the key.
    * The given function is used to combine the corresponding values.
    *
    * @param first   The first map.
    * @param second  The second map.
    * @param combine The function used to combine two elements.
    * @tparam K The type of the keys.
    * @tparam V The type of the values.
    * @return The union of the maps.
    */
  def intersect[K, V](first: Map[K, V], second: Map[K, V], combine: (V, V) => V): Map[K, V] = first.foldLeft(Map.empty[K, V]) {
    case (map, (key, value)) => second.get(key) match {
      case Some(existing) => map + (key -> combine(value, existing))
      case None => map
    }
  }
}
