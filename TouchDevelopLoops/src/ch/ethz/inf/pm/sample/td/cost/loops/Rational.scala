package ch.ethz.inf.pm.sample.td.cost.loops

/*
    A rational number.
 */
class Rational (var numerator: Int, var denominator : Int) {

  simplify()

  def this(i: Int) = this (i, 1)

  private def simplify() = {
    if (denominator < 0) {
      denominator = -denominator
      numerator = -numerator
    }
    val g : Int = gcd(numerator, denominator)
    numerator /= g
    denominator /= g
  }

  private def gcd(a: Int, b: Int) : Int = {
    if (b==0) a
    else gcd(b, a%b)
  }

  def isZero() : Boolean = numerator == 0
  def smaller(i: Int) : Boolean = numerator.toDouble / denominator.toDouble < i.toDouble
  def smallerEqual(i: Int) : Boolean = numerator.toDouble / denominator.toDouble <= i.toDouble
  def larger(i: Int) : Boolean = numerator.toDouble / denominator.toDouble > i.toDouble
  def largerEqual(i: Int) : Boolean = numerator.toDouble / denominator.toDouble >= i.toDouble

  def times(n: Int) : Rational = new Rational(numerator*n, denominator)

  override def toString : String = {
       if (denominator == 1) numerator.toString else numerator + "/" + denominator
  }

}










