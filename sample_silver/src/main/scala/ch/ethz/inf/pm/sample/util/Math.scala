/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.util

object Math {
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a.abs
    else gcd(b, a % b)

  def gcd(numbers: Iterable[Int]): Int =
    if (numbers.isEmpty) 0
    else numbers.reduce(gcd)

  def lcm(a: Int, b: Int): Int =
    (a * b).abs / gcd(a, b)

  def lcm(numbers: Iterable[Int]): Int =
    if (numbers.isEmpty) 1
    else numbers.reduce(lcm)
}
