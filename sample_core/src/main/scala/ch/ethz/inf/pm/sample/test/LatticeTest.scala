/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.abstractdomain.Lattice
import org.scalatest.FunSuite

/**
  * Implements generic, property-based testing for lattice elements. These check well-formedness properties of lattice
  * elements which have proven helpful to provide more robust implementations. In particular:
  *
  *  - Uniqueness of Top/Bottom, typical properties of Top/Bottom for LessEqual
  *  - Idempotency of lub, widening and glb
  *  - Commutativity of lub and glb
  *
  * To use this basic infrastructure, extend this trait, and implement factory to create an empty element.
  * Also extend the return value of instances to produce more interesting instances of the lattice than
  * just bottom/top.
  *
  * @author Lucas Brutschy
  */
trait LatticeTest[T <: Lattice[T]] extends FunSuite with SampleTest {

  /**
    * Creates an instance of T
    */
  def factory:T

  /**
    * Overwrite this to extend the test suite
    */
  def instances:Set[T] = Set(
    factory,factory.bottom(),factory.top()
  )

  test("There is only one top/bottom element") {
    for (a <- instances) {
      assert { a.isTop == (a == a.top) && a.isBottom == (a == a.bottom) }
    }
  }

  test("bottom lessEqual everything") {
    for (a <- instances) {
      assert { (a.bottom lessEqual a) && ((a lessEqual a.bottom) == a.isBottom) }
    }
  }

  test("everything lessEqual top") {
    for (a <- instances) {
      assert { (a lessEqual a.top) && ((a.top lessEqual a) == a.isTop) }
    }
  }

  test("glb lessEqual lub") {
    for (a <- instances; b <- instances) {
      assert { (a glb b) lessEqual (a lub b) }
    }
  }

  val ignoreLubLessEqualWidening = false
  test("lub lessEqual widening") {
    if (!ignoreLubLessEqualWidening)
      for (a <- instances; b <- instances) {
        assert { (a lub b) lessEqual (a widening b) }
      }
  }

  test("glb on yourself is identity") {
    for (a <- instances) {
      assert { (a glb a) equivalent a }
    }
  }

  test("lub on yourself is identity") {
    for (a <- instances) {
      assert { (a lub a) equivalent a }
    }
  }

  test("widening on yourself is identity") {
    for (a <- instances) {
      assert { (a widening a) equivalent a }
    }
  }

  test("lub is commutative") {
    for (a <- instances; b <- instances) {
      assert { (a lub b) equivalent (b lub a) }
    }
  }

  test("glb is commutative") {
    for (a <- instances; b <- instances) {
      assert { (a glb b) equivalent (b glb a) }
    }
  }

}
