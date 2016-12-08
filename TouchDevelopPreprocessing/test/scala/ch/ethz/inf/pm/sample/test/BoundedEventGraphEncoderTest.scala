/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.td.cloud.BoundedEventGraph._
import ch.ethz.inf.pm.td.cloud.{BoundedEventGraph, BoundedEventGraphEncoder, SystemSpecification}
import org.scalatest.FunSuite

class BoundedEventGraphEncoderTest extends FunSuite {

  test("Single read, single write") {
    val x = BoundedEventGraph.Graph(
      events = List(
        Event("u","u","put"),
        Event("q","q","get")
      ),
      programOrder = List(
        Edge("u","q",True)
      ),
      system = SystemSpecification.PutGetMap
    )

    assert(BoundedEventGraphEncoder.findViolations(x).isEmpty)
  }

  test("Dekker R/W") {
    val x = BoundedEventGraph.Graph(
      events = List(
        Event("u1","u1","wx"),
        Event("u2","u2","wy"),
        Event("q1","q1","ry"),
        Event("q2","q2","rx")
      ),
      programOrder = List(
        Edge("u1","q1",True),
        Edge("u2","q2",True)
      ),
      system = SystemSpecification.ReadWriteRegister
    )

    assert(BoundedEventGraphEncoder.findViolations(x).isDefined)
  }

  test("Dekker") {
    val x = BoundedEventGraph.Graph(
      events = List(
        Event("u1","u1","put"),
        Event("u2","u2","put"),
        Event("q1","q1","get"),
        Event("q2","q2","get")
      ),
      programOrder = List(
        Edge("u1","q1",True),
        Edge("u2","q2",True)
      ),
      system = SystemSpecification.PutGetMap
    )

    assert(BoundedEventGraphEncoder.findViolations(x).isDefined)
  }

}
