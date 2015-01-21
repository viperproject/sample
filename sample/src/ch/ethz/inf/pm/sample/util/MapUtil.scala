package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.abstractdomain.Identifier

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
  def mergeMaps2[A, B](left: Map[A, B], right: Map[A, B])(f: (Option[B], Option[B]) => Option[B]): Map[A, B] = {
    (left.keys ++ right.keys).flatMap{x =>
      f(left.get(x),right.get(x)) match {
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
    MapUtil.mergeMaps2(left,right)({
      (b1,b2) =>
        Some(b1.getOrElse(Set.empty) ++ b2.getOrElse(Set.empty))
    })

  /**
   * For maps to sets, merge two maps, taking the intersection of all sets
   * Keys that only appear in one map will be kept as-is in the result
   */
  def mapToSetIntersectionKeepUndefined[A, B](left: Map[A, Set[B]], right: Map[A, Set[B]]): Map[A, Set[B]] =
    MapUtil.mergeMaps2(left,right)({
      case (None, x) => x
      case (x, None) => x
      case (Some(b1), Some(b2)) => Some(b1 intersect b2)
    })

  /**
   * For maps to sets, merge two maps, taking the intersection of all sets
   * Keys that only appear in one map will be kept as-is in the result
   */
  def mapToSetIntersection[A, B](left: Map[A, Set[B]], right: Map[A, Set[B]]): Map[A, Set[B]] =
    MapUtil.mergeMaps2(left,right)({
      case (None, x) => None
      case (x, None) => None
      case (Some(b1), Some(b2)) => Some(b1 intersect b2)
    })

  /**
   * For maps to sets, merge two maps, taking the union of all sets
   * Keys that only appear in one map will be kept as-is in the result
   */
  def mapToSeqKeepLonger[A, B](left: Map[A, Seq[B]], right: Map[A, Seq[B]]): Map[A, Seq[B]] =
    MapUtil.mergeMaps2(left,right)({
      case (None, x) => x
      case (x, None) => x
      case (Some(b1), Some(b2)) => if (b1.length > b2.length) Some(b1) else Some(b2)
    })

  def mapToSetContainment[A, B](left: Map[A, Set[B]], right: Map[A, Set[B]]): Boolean = {
    for (id <- left.keySet ++ right.keySet)
      if (!left.getOrElse(id,Set.empty).subsetOf(right.getOrElse(id,Set.empty))) return false
    return true
  }

}