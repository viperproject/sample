/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.util

/**
 * A useful util class to operate on maps
 */
object MapUtil {

  /**
   * Merge two maps of the same type. It combines all their entries.
   * For keys just in one of them, just insert the entry into the resulting map
   * If there are entries with the same key, combine the values using function f
   */
  def mergeMaps[A, B](left: Map[A, B], right: Map[A, B])(f: (B, B) => B): Map[A, B] =
    (Map[A, B]() /: (left.toList ++ right.toList)) {
      (m, kv) =>
        val newentry = if (m.contains(kv._1))
          kv._1 -> f(m(kv._1), kv._2)
        else
          kv
        m + newentry

    }

  /**
   * Merge two maps of the same type. It combines all their entries.
   * Gives more control whether keys will exist
   */
  def mergeMapsOptional[A, B](left: Map[A, B], right: Map[A, B])(f: (Option[B], Option[B]) => Option[B]): Map[A, B] = {
    (left.keys ++ right.keys).flatMap { x =>
      f(left.get(x), right.get(x)) match {
        case Some(b) => Some(x -> b)
        case None => None
      }
    }.toMap
  }


  /**
   * Merge two maps of the same type. It combines all their entries.
   * Gives a single value in return
   */
  def mergeMapsOtherVal[A, B, C](left: Map[A, B], right: Map[A, B])(f: (Option[B], Option[B]) => Option[C]): Map[A,C] = {
    (left.keys ++ right.keys).flatMap { x =>
      f(left.get(x), right.get(x)) match {
        case Some(b) => Some(x -> b)
        case None => None
      }
    }.toMap
  }


  /**
   * For maps to sets, merge two maps, taking the union of all sets
   * Keys that only appear in one map will be kept as-is in the result
   */
  def mapToSetUnion[A, B](left: Map[A, Set[B]], right: Map[A, Set[B]]): Map[A, Set[B]] =
  {
    if (left eq right) return left
    MapUtil.mergeMapsOptional(left, right)({
      (b1, b2) =>
        Some(b1.getOrElse(Set.empty) ++ b2.getOrElse(Set.empty))
    })
  }

  /**
   * For maps to sets, merge two maps, taking the intersection of all sets
   * Keys that only appear in one map will be kept as-is in the result
   */
  def mapToSetIntersectionKeepUndefined[A, B](left: Map[A, Set[B]], right: Map[A, Set[B]]): Map[A, Set[B]] = {
    if (left eq right) return left
    MapUtil.mergeMapsOptional(left, right)({
      case (None, x) => x
      case (x, None) => x
      case (Some(b1), Some(b2)) => Some(b1 intersect b2)
    })
  }

  /**
   * For maps to sets, merge two maps, taking the intersection of all sets
   * Keys that only appear in one map will be kept as-is in the result
   */
  def mapToSetIntersection[A, B](left: Map[A, Set[B]], right: Map[A, Set[B]]): Map[A, Set[B]] = {
    if (left eq right) return left
    MapUtil.mergeMapsOptional(left, right)({
      case (None, x) => None
      case (x, None) => None
      case (Some(b1), Some(b2)) => Some(b1 intersect b2)
    })
  }

  /**
   * For maps to sets, merge two maps, taking the union of all sets
   * Keys that only appear in one map will be kept as-is in the result
   */
  def mapToSeqKeepLonger[A, B](left: Map[A, Seq[B]], right: Map[A, Seq[B]]): Map[A, Seq[B]] = {
    if (left eq right) return left
    MapUtil.mergeMapsOptional(left, right)({
      case (None, x) => x
      case (x, None) => x
      case (Some(b1), Some(b2)) => if (b1.length > b2.length) Some(b1) else Some(b2)
    })
  }

  def mapToSetContainment[A, B](left: Map[A, Set[B]], right: Map[A, Set[B]]): Boolean = {
    if (left eq right) return true
    for (id <- left.keySet ++ right.keySet)
      if (!left.getOrElse(id,Set.empty).subsetOf(right.getOrElse(id,Set.empty))) return false
    true
  }

}