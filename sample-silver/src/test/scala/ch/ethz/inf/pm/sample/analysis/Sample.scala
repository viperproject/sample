/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.analysis

import viper.silver.ast.{Position, Program}
import viper.silver.frontend.SilFrontend
import viper.silver.verifier.{AbstractError, Dependency, Success, Verifier}

final case class DummySilverFrontend() extends SilFrontend {
  def createVerifier(fullCmd: String) = ???
  def configureVerifier(args: Seq[String]) = ???
}

case class AssertFailure(pos: Position) extends AbstractError {
  def fullId: String = "sample.assert.failed"
  def readableMessage: String = "the assertion may not hold"
}

trait SampleAnalyzer extends Verifier {
  def version: String = "0.1"

  def buildVersion: String = ""

  def copyright: String = ""

  def debugInfo(info: Seq[(String, Any)]): Unit = {}

  def dependencies: Seq[Dependency] = Nil

  def parseCommandLine(args: Seq[String]): Unit = {}
}

object DummyAnalyzer extends SampleAnalyzer {
  def name = "dummy"

  def verify(program: Program) = Success

  override def start(): Unit = ()

  override def stop(): Unit = ()
}
