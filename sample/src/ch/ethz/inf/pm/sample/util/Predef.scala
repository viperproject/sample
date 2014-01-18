package ch.ethz.inf.pm.sample.util

object Predef {
  implicit def extendedBoolean(a: Boolean) = new {
    def implies(b: => Boolean) = {
      !a || b
    }
  }
}
