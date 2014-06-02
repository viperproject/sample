package ch.ethz.inf.pm.sample.reporting

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

trait SampleMessage {
  def id: String

  def pp: ProgramPoint
}

case class SampleError(id: String, message: String, pp: ProgramPoint, causes:Set[(String,ProgramPoint)]) extends SampleMessage

case class SampleInfo(id: String, message: String, pp: ProgramPoint) extends SampleMessage

