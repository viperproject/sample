/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.web

import ch.ethz.inf.pm.td.analysis._

/**
 * Implements web interface for TouchDevelop
 */
class TouchDevelopApp extends App {

  /** Provides all test files that the user can choose to analyze. */
  override def fileProvider: TestFileProvider = TouchDevelopFileProvider

  /** Provides additional test cases, identified only by there ID */
  override def identifierProvider: Option[IdentifierProvider] = Some(TouchDevelopIdentifierProvider)

  /** List of pre-defined analysis runners. */
  override def availableAnalysisRunners = Seq(TouchDevelopAnalysisRunner.Default())

  val prefix = "td"
}

object TouchDevelopIdentifierProvider extends IdentifierProvider {

  override def identifiers: Seq[IdentifierTest] = Seq(
    IdentifierTest("dekker example (Fixed) (fekcblzqer)", "td://fekcblzqer"),
    IdentifierTest("Events (Fixed) (hqttcleqlj)", "td://hqttcleqlj"),
    IdentifierTest("CSCE 1030 DASHBOARD (Fixed) (nekvbalhdm)", "td://nekvbalhdm"),
    IdentifierTest("sky locale (Fixed) (pmzxrhbsrv)", "td://pmzxrhbsrv"),
    IdentifierTest("tetris (Fixed) (xrqeregnpk)", "td://xrqeregnpk"),
    IdentifierTest("cloud list (blqz)", "td://blqz"),
    IdentifierTest("TouchDatabase (cavke)", "td://cavke"),
    IdentifierTest("Super Chat (cvuz)", "td://cvuz"),
    IdentifierTest("Save Passwords (eddm)", "td://eddm"),
    IdentifierTest("ec2 demo chat (eijba)", "td://eijba"),
    IdentifierTest("Contest Voting (etww)", "td://etww"),
    IdentifierTest("Chatter box (fqaba)", "td://fqaba"),
    IdentifierTest("Hubstar (gbtxe)", "td://gbtxe"),
    IdentifierTest("tetris (gcane)", "td://gcane"),
    IdentifierTest("NuvolaList 2 (kjxzcgcv)", "td://kjxzcgcv"),
    IdentifierTest("FieldGPS (kmac)", "td://kmac"),
    IdentifierTest("HackER (kqfnc)", "td://kqfnc"),
    IdentifierTest("Cloud Example (kzwue)", "td://kzwue"),
    IdentifierTest("instant poll (nggfa)", "td://nggfa"),
    IdentifierTest("expense recorder (nvoha)", "td://nvoha"),
    IdentifierTest("keyboard hero (ohgxa)", "td://ohgxa"),
    IdentifierTest("CSCE 1030 DASHBOARD (ornb)", "td://ornb"),
    IdentifierTest("dekker example (oxhs)", "td://oxhs"),
    IdentifierTest("sky locale (padg)", "td://padg"),
    IdentifierTest("metaverse (qnpge)", "td://qnpge"),
    IdentifierTest("Events (qwidc)", "td://qwidc"),
    IdentifierTest("TouchDevelop Jr. (qzeua)", "td://qzeua"),
    IdentifierTest("cloud card (qzju)", "td://qzju"),
    IdentifierTest("Relatd (ruef)", "td://ruef"),
    IdentifierTest("Cloud Paper Scissors (sxjua)", "td://sxjua"),
    IdentifierTest("pentix (uvjba)", "td://uvjba"),
    IdentifierTest("Color Line (uvlma)", "td://uvlma"),
    IdentifierTest("unique poll (wbuei)", "td://wbuei"),
    IdentifierTest("Online Tic Tac Toe Multiplayer  (wccqepeb)", "td://wccqepeb"),
    IdentifierTest("Vulcanization calculator (whpgc)", "td://whpgc"),
    IdentifierTest("Expense Splitter (wkvhc)", "td://wkvhc"),
    IdentifierTest("guess multi-player demo (ycxbc)", "td://ycxbc")
  )

}

object TouchDevelopFileProvider extends ResourceTestFileProvider(namePattern = ".*\\.(td|json)")



