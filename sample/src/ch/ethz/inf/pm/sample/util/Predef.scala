package ch.ethz.inf.pm.sample.util

object Predef {
  implicit def extendedBoolean(a: Boolean) = new {
    def implies(b: => Boolean) = {
      !a || b
    }
  }

  implicit def extendedList[T](l: List[T]) = new {
    /** Returns whether there are any duplicate items in the given list. */
    def containsDuplicates = l.distinct.size != l.size
  }

  /**
   * Gived a merge operation and a neutral element of the values, this merges to maps
   *
   * @param map1 The left map
   * @param map2 The right map
   * @param neutral An element e for which (x merge e) = x
   * @param merge The merge operation
   * @tparam X The key type
   * @tparam Y The value type
   */
  def mergeMaps[X, Y](map1: Map[X, Y], map2: Map[X, Y], neutral: Y, merge: (Y, Y) => Y): Map[X, Y] = {
    (map2 ++ map1).map {
      case (k, v) => k -> merge(v, map2.getOrElse(k, neutral))
    }
  }

  def mergePartitionings[X](part1: Map[X, Set[X]], part2: Map[X, Set[X]]): Map[X, Set[X]] = {

    def setForAll(part: Map[X, Set[X]], xs: Set[X]): Map[X, Set[X]] = {
      part ++ (for (x <- xs) yield {
        x -> xs
      })
    }

    var newPartitioning: Map[X, Set[X]] = Map.empty
    var toCover = part1.keySet ++ part2.keySet
    while (!toCover.isEmpty) {
      val cur = toCover.head
      newPartitioning = setForAll(newPartitioning, part1.getOrElse(cur, Set.empty) ++ part2.getOrElse(cur, Set.empty) ++ newPartitioning.getOrElse(cur, Set.empty))
      toCover = toCover -- newPartitioning.keySet
    }

    newPartitioning
  }

}
