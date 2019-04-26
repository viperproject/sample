/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._

case class PrefixAndSuffix(_1: Prefix, _2: Suffix)
  extends SemanticCartesianProductDomain[Prefix, Suffix, PrefixAndSuffix] {

  def factory(a: Prefix, b: Suffix) = PrefixAndSuffix(a, b)

  override def merge(r: Replacement) = PrefixAndSuffix(this._1.merge(r), this._2.merge(r))

  override def getStringOfId(id: Identifier): String = _1.getStringOfId(id) + "; " + _2.getStringOfId(id)
}