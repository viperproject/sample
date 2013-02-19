package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.SetDomain

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:49 AM
 * 
 */
class BooleanDomain extends SetDomain[Boolean,BooleanDomain] {

  override def factory() = new BooleanDomain()
  def canBeTrue = isTop || value.contains(true)
  def canBeFalse = isTop || value.contains(false)

  // intersection corresponds to glb
  def intersect(x : BooleanDomain, y : BooleanDomain) = {
    glb(x,y)
  }

}
