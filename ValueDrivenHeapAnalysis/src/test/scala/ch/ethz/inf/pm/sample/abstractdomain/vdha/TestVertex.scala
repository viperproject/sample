/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

object TestVertex {

  def main(args: Array[String]) {
    val v1 = LocalVariableVertex("a")(null)
    val v2 = LocalVariableVertex("b")(null)
    if (v1 < v2)
      println("v1 < v2")
    else
      println("v1 !< v2")

    val s1 : Option[String] = Some("str")
    val s2 : Option[String] = Some("str")

    if (s1.equals(s2)) println("s1 = s2")
    else println("s1 != s2")
  }

}
