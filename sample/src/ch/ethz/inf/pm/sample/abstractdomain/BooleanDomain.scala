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

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:49 AM
 * 
 */
class BooleanDomain(_value: Set[Boolean] = Set.empty[Boolean], _isTop: Boolean = false, _isBottom: Boolean = false)
  extends SetDomain[Boolean,BooleanDomain](_value,_isTop,_isBottom) {

  def setFactory (_value: Set[Boolean] = Set.empty[Boolean], _isTop: Boolean = false, _isBottom: Boolean = false): BooleanDomain
    = new BooleanDomain(_value,_isTop,_isBottom)

  def canBeTrue = isTop || value.contains(true)
  def canBeFalse = isTop || value.contains(false)

  // intersection corresponds to glb
  def intersect(x : BooleanDomain, y : BooleanDomain) = {
    glb(x,y)
  }

}
