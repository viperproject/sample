/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.reporting

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

object Reporter {

  var seenErrors = Set.empty[SampleError]
  var seenInfos = Set.empty[SampleInfo]
  var seenImprecision = Set[(String,ProgramPoint)]()
  var seenBottom = Set[(String,ProgramPoint)]()

  var enableOutputOfAlarms: Boolean = true
  var enableOutputOfInfos: Boolean = true
  var enableOutputOfPrecisionWarnings: Boolean = true
  var enableOutputOfDummyWarnings: Boolean = true
  var enableOutputOfBottomWarnings: Boolean = true

  def enableAllOutputs(): Unit = {
    enableOutputOfAlarms = true
    enableOutputOfInfos = true
    enableOutputOfPrecisionWarnings = true
    enableOutputOfBottomWarnings = true
    enableOutputOfDummyWarnings = true
  }

  def disableAllOutputs(): Unit = {
    enableOutputOfAlarms = false
    enableOutputOfInfos = false
    enableOutputOfPrecisionWarnings = false
    enableOutputOfBottomWarnings = false
    enableOutputOfDummyWarnings = false
  }

  def hasError(err: SampleError):Boolean = seenErrors.contains(err)
  def hasImprecision(message:String,pp:ProgramPoint):Boolean = seenImprecision.contains((message,pp))
  def hasBottom(message:String,pp:ProgramPoint):Boolean = seenBottom.contains((message,pp))

  def getErrors(pp:ProgramPoint):Set[String] = seenErrors filter (_.pp == pp) map (_.message)
  def getImprecision(pp:ProgramPoint):Set[String] = seenImprecision.filter(_._2 == pp).map(_._1)
  def getBottom(pp:ProgramPoint):Set[String] = seenBottom.filter(_._2 == pp).map(_._1)

  def reportError(err: SampleError) {
    if (!hasError(err) && enableOutputOfAlarms) {
      SystemParameters.progressOutput.put("ALARM: " + err.message + " at " + err.pp.toString +
        (if (err.causes.nonEmpty) ", since " + err.causes.map { x => x._1}.mkString(" or ") else ""))
      seenErrors += err
    }
  }

  def reportError(message:String,pp:ProgramPoint, causes: Set[(String,ProgramPoint)] = Set.empty, id: String = "assert.failed") {
    val err = SampleError(id, message, pp, causes)
    reportError(err)
  }

  def reportInfo(message: String, pp: ProgramPoint, id: String = "warning"): Unit = {
    val info = SampleInfo(id, message, pp)
    seenInfos += info
  }

  def reportImprecision(message:String,pp:ProgramPoint) {
    if (!hasImprecision(message,pp) && enableOutputOfPrecisionWarnings) {
      SystemParameters.progressOutput.put("PRECISION: "+message+" at "+pp.toString)
      seenImprecision += ((message,pp))
    }
  }

  def reportDummy(message:String,pp:ProgramPoint) {
    if (!hasImprecision(message,pp) && enableOutputOfDummyWarnings) {
      SystemParameters.progressOutput.put("SOUND DUMMY: "+message+" at "+pp.toString)
      seenImprecision += ((message,pp))
    }
  }

  def reportBottom(message:String,pp:ProgramPoint) {
    if (!hasBottom(message,pp) && enableOutputOfBottomWarnings) {
      SystemParameters.progressOutput.put("BOTTOM: "+message+" at "+pp.toString)
      seenBottom += ((message,pp))
    }
  }

  def reset(): Unit = {
    seenErrors = Set.empty[SampleError]
    seenBottom = Set.empty[(String,ProgramPoint)]
    seenImprecision = Set.empty[(String,ProgramPoint)]
  }

}
