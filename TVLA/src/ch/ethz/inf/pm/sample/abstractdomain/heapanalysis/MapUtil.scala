package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

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
}