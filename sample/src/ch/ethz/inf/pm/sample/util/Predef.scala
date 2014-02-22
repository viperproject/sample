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
}
