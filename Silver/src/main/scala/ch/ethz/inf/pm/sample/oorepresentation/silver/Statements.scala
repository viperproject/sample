package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.oorepresentation._
import viper.silver.{ast => sil}

/** Sample `ProgramPoint` that wraps a SIL `Position`. */
case class WrappedProgramPoint(pos: sil.HasLineColumn) extends LineColumnProgramPoint {
  def getLine: Int = pos.line

  def getColumn: Int = pos.column

  override def toString: String = description
}