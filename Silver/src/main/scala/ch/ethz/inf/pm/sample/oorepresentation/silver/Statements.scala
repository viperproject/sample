/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.oorepresentation._
import viper.silver.ast.Position
import viper.silver.{ast => sil}

/** Sample `ProgramPoint` that wraps a SIL `Position`. */
case class WrappedProgramPoint(pos: sil.HasLineColumn) extends LineColumnProgramPoint {
  def getLine: Int = pos.line

  def getColumn: Int = pos.column

  override def toString: String = description
}

