package ch.ethz.inf.pm.td.analysis.interpreter

object InterpreterImplicits extends InterpreterImplicits

trait InterpreterImplicits {
  implicit def num2NumberV(n: NumberType): NumberV = NumberV(n)
  implicit def bool2BooleanV(b: Boolean): BooleanV = BooleanV(b)
  implicit def str2StringV(s: String): StringV = StringV(s)
}
