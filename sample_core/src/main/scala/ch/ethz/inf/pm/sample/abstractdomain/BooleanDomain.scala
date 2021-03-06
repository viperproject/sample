/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain

object BooleanDomain {

  val True =  Inner(Set(true))
  val False = Inner(Set(false))

  object Bottom extends BooleanDomain with SetDomain.Bottom[Boolean,BooleanDomain] {
    def canBeTrue = false
    def canBeFalse = false
  }

  object Top extends BooleanDomain with SetDomain.Top[Boolean,BooleanDomain] {
    def canBeTrue = true
    def canBeFalse = true
  }

  case class Inner(value: Set[Boolean] = Set.empty[Boolean]) extends BooleanDomain
      with SetDomain.Inner[Boolean,BooleanDomain,Inner] {
    def canBeTrue  = value.contains(true)
    def canBeFalse = value.contains(false)
  }

}

trait BooleanDomain extends SetDomain[Boolean,BooleanDomain] {

  def bottom() =                BooleanDomain.Bottom
  def top() =                   BooleanDomain.Top
  def factory(v:Set[Boolean]) = BooleanDomain.Inner(v)

  def intersect(x: BooleanDomain, y: BooleanDomain) = x.glb(y)

}
