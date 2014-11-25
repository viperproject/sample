package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.oorepresentation._
import viper.silver.{ast => sil}

/** Sample `ProgramPoint` that wraps a SIL `Position`. */
case class WrappedProgramPoint(pos: sil.RealPosition) extends LineColumnProgramPoint {
  def getLine: Int = pos.line

  def getColumn: Int = pos.column

  override def toString: String = description
}