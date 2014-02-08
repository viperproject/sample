package ch.ethz.inf.pm.sample.abstractdomain

object BooleanDomain {

  // Helper values
  def domBottom = (new BooleanDomain()).bottom()
  def domTrue = (new BooleanDomain()).add(true)
  def domFalse = (new BooleanDomain()).add(false)
  def domInvalid = (new BooleanDomain()).add(true)
  def domValid = (new BooleanDomain()).add(false)
  def domTop = (new BooleanDomain()).top()

}

case class BooleanDomain(
    value: Set[Boolean] = Set.empty[Boolean],
    isTop: Boolean = false,
    isBottom: Boolean = false)
  extends SetDomain[Boolean, BooleanDomain] {

  def setFactory(
      value: Set[Boolean] = Set.empty[Boolean],
      isTop: Boolean = false,
      isBottom: Boolean = false) =
    BooleanDomain(value, isTop, isBottom)

  def canBeTrue = isTop || value.contains(true)
  def canBeFalse = isTop || value.contains(false)

  def intersect(x: BooleanDomain, y: BooleanDomain) = x.glb(y)

}
