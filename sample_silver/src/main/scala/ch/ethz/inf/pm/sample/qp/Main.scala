/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.qp

object Main {
  def main(arguments: Array[String]): Unit = {
    val existing = List(1, 2, 3, 4, 5)
    val inferred = existing // List(6, 2, 4, 3, 5, 7)

    inferred.foldLeft(existing) { case (xx, item) =>
      if (xx.isEmpty) {
        println(s"append $item")
        xx
      } else {
        val idx = xx.indexOf(item)
        if (idx == -1) {
          println(s"insert $item before ${xx.head}")
          xx
        } else {
          val (aa, bb) = xx.splitAt(idx)
          aa.foreach { x => println(s"delete $x") }
          bb.tail
        }
      }
    }
  }
}
