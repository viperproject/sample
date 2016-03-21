package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._
import scala.language.implicitConversions

object ApronTools {
  implicit def intToScalar(i: Int): Scalar = new apron.DoubleScalar(i.toDouble)
  implicit def doubleToScalar(d: Double): Scalar = new apron.DoubleScalar(d)


  def intervalBounds(itv: apron.Interval): (Scalar, Scalar) = (itv.inf(), itv.sup())

  def isIntervalBounded(itv: apron.Interval): Boolean = isIntervalBoundedBelow(itv) && isIntervalBoundedAbove(itv)

  def isIntervalBoundedAbove(itv: apron.Interval): Boolean = {
    val sup = itv.sup()
    sup.isInfty == 0
  }

  def isIntervalBoundedBelow(itv: apron.Interval): Boolean = {
    val inf = itv.inf()
    inf.isInfty == 0
  }

  def isIntervalZero(itv: apron.Interval): Boolean = {
    val (l,h) = intervalBounds(itv)
    l.isZero && h.isZero
  }

  def topInterval: apron.Interval = new apron.Interval(neginf, inf)

  def singletonInterval(a: Double): apron.Interval = new apron.Interval(a,a)

  def singletonInterval(a: Int): apron.Interval = new apron.Interval(a,a)

  def singletonInterval(a: Scalar): apron.Interval = new apron.Interval(a,a)

  def inf: Scalar = {
    val s = new DoubleScalar()
    s.setInfty(+1)
    s
  }

  def neginf: Scalar = {
    val s = new DoubleScalar()
    s.setInfty(-1)
    s
  }

}
