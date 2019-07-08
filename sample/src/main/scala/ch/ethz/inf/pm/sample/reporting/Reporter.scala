/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.reporting

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.Reporter.MessageClass.MessageClass
import ch.ethz.inf.pm.sample.reporting.Reporter.ReportingLevel.ReportingLevel

object Reporter {

  object ReportingLevel extends Enumeration {
    type ReportingLevel = Value
    val Info, Error, Off = Value
  }

  object MessageClass extends Enumeration {
    type MessageClass = Value
    val UnreachableCode = Value("unreachable")
    val AssertionViolation = Value("assert.failed")
    val ImpreciseSemantics = Value("imprecision")
    val DummyImplementation = Value("dummy")
    val GenericWarning = Value("warning")
  }

  var messages = Set.empty[SampleMessage]
  var reportingLevels = Map.empty[MessageClass, ReportingLevel]

  enableAllOutputs()

  def enableAllOutputs(): Unit = {
    reportingLevels = Map(
      MessageClass.UnreachableCode -> ReportingLevel.Info,
      MessageClass.AssertionViolation -> ReportingLevel.Error,
      MessageClass.DummyImplementation -> ReportingLevel.Info,
      MessageClass.ImpreciseSemantics -> ReportingLevel.Info,
      MessageClass.GenericWarning -> ReportingLevel.Info
    )
  }

  def disableAllOutputs(): Unit = {
    reportingLevels = Map(
      MessageClass.UnreachableCode -> ReportingLevel.Off,
      MessageClass.AssertionViolation -> ReportingLevel.Off,
      MessageClass.DummyImplementation -> ReportingLevel.Off,
      MessageClass.ImpreciseSemantics -> ReportingLevel.Off,
      MessageClass.GenericWarning -> ReportingLevel.Off
    )
  }

  def assertionViolations:Set[SampleMessage] =
    getClass(MessageClass.AssertionViolation)

  def genericWarnings:Set[SampleMessage] =
    getClass(MessageClass.GenericWarning)

  def unreachableCode:Set[SampleMessage] =
    getClass(MessageClass.UnreachableCode)

  def impreciseSemantics:Set[SampleMessage] =
    getClass(MessageClass.ImpreciseSemantics)

  def lookupAssertionViolations(pp: ProgramPoint): Set[String] =
    lookup(MessageClass.AssertionViolation,pp)

  def lookupImpreciseSemantics(pp: ProgramPoint): Set[String] =
    lookup(MessageClass.ImpreciseSemantics,pp)

  def reportAssertionViolation(message: String, pp: ProgramPoint, causes: Set[(String, ProgramPoint)] = Set.empty) =
    report(MessageClass.AssertionViolation, message, pp)

  def reportImpreciseSemantics(message: String, pp: ProgramPoint) =
    report(MessageClass.ImpreciseSemantics, message, pp)

  def reportDummyImplementation(message: String, pp: ProgramPoint) =
    report(MessageClass.DummyImplementation, message, pp)

  def reportUnreachableCode(message: String, pp: ProgramPoint) =
    report(MessageClass.UnreachableCode, message, pp)

  def reportGenericWarning(message: String, pp: ProgramPoint) =
    report(MessageClass.GenericWarning, message, pp)

  def reset(): Unit = {
    messages = Set.empty[SampleMessage]
  }

  private def getClass(m: MessageClass) = {
    messages.filter(x => x.id == m.toString)
  }

  private def lookup(m: MessageClass, pp: ProgramPoint) = {
    messages.filter(x => x.pp == pp && x.id == m.toString).map(_.message)
  }

  private def report(m: MessageClass, msg: String, pp: ProgramPoint, causes: Set[(String, ProgramPoint)] = Set.empty) = {
    val res =
      reportingLevels(m) match {
        case ReportingLevel.Info => Some(SampleInfo(m.toString, msg, pp))
        case ReportingLevel.Error => Some(SampleError(m.toString, msg, pp, causes))
        case _ => None
      }
    res match {
      case Some(x) if !messages.contains(x) =>
        SystemParameters.progressOutput.put(x.toString)
        messages += x
      case _ => ()
    }
  }

}
