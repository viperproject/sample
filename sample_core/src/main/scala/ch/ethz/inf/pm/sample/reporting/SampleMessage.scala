/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.reporting

import ch.ethz.inf.pm.sample.execution.AnalysisResult
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

trait SampleMessage extends AnalysisResult {
  override def displayName: String = id
  def id: String
  def message: String
  def pp: ProgramPoint
}

case class SampleError(id: String, message: String, pp: ProgramPoint, causes:Set[(String,ProgramPoint)]) extends SampleMessage {
  override def toString: String = "ERROR: " + message + " " + pp.toString +
    (if (causes.nonEmpty) ", since " + causes.map { x => x._1}.mkString(" or ") else "")
}

case class SampleInfo(id: String, message: String, pp: ProgramPoint) extends SampleMessage {
  override def toString: String = "WARNING: " + message + " " + pp
}
