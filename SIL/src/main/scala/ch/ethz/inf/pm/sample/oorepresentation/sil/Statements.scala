/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.oorepresentation._
import viper.silver.{ast => sil}

/** Sample `ProgramPoint` that wraps a SIL `Position`. */
case class WrappedProgramPoint(pos: sil.HasLineColumn) extends LineColumnProgramPoint {
  def getLine: Int = pos.line

  def getColumn: Int = pos.column

  override def toString: String = description
}