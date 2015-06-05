package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, IdentifierSet, SetDomain}

object Predef {
  implicit def extendedBoolean(a: Boolean): Object {def implies(b: => Boolean): Boolean} = new {
    def implies(b: => Boolean) = {
      !a || b
    }
  }

  implicit def extendedList[T](l: List[T]): Object {def containsDuplicates: Boolean} = new {
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

  def mergePartitionings(part1: Map[Identifier, IdentifierSet], part2: Map[Identifier, IdentifierSet]): Map[Identifier, IdentifierSet] = {

    def setForAll(part: Map[Identifier, IdentifierSet], xs: IdentifierSet): Map[Identifier, IdentifierSet] = {
      xs match {
        case IdentifierSet.Bottom =>
          part
        case IdentifierSet.Top =>
          part ++ (for (x <- part.keySet) yield {
            x -> xs
          })
        case IdentifierSet.Inner(v) =>
          part ++ (for (x <- v) yield {
            x -> xs
          })
      }
    }

    var newPartitioning: Map[Identifier, IdentifierSet] = Map.empty
    var toCover = part1.keySet ++ part2.keySet
    while (toCover.nonEmpty) {
      val cur = toCover.head
      newPartitioning = setForAll(newPartitioning, part1.getOrElse(cur, IdentifierSet.Bottom) ++ part2.getOrElse(cur, IdentifierSet.Bottom) ++ newPartitioning.getOrElse(cur, IdentifierSet.Bottom))
      toCover = toCover -- newPartitioning.keySet
    }

    newPartitioning
  }

}
