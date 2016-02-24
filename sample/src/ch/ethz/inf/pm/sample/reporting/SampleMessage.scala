package ch.ethz.inf.pm.sample.reporting

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

trait SampleMessage {
  def id: String
  def pp: ProgramPoint
}

case class SampleError(id: String, message: String, pp: ProgramPoint, causes:Set[(String,ProgramPoint)]) extends SampleMessage {
  override def toString: String = "ERROR: " + message + " " + pp.toString
}

case class SampleInfo(id: String, message: String, pp: ProgramPoint) extends SampleMessage {
  override def toString: String = "WARNING: " + message + " " + pp
}

