package ch.ethz.inf.pm.sample.oorepresentation.silver

import viper.silver.frontend.{SilFrontend}
import viper.silver.verifier._
import viper.silver.ast.{Position, Program}
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.silver.verifier.Failure
import viper.silver.testing.SilSuite

/**
 * Just a dummy front-end such that we gain easy access to the fully parsed
 * and type-checked SIL program.
 */
final case class DummySilverFrontend() extends SilFrontend {
  def createVerifier(fullCmd: String) = ???

  def configureVerifier(args: Seq[String]) = ???
}

trait SampleVerifier extends Verifier {
  def version: String = "0.1"

  def buildVersion: String = ""

  def copyright: String = ""

  def debugInfo(info: Seq[(String, Any)]): Unit = {}

  def dependencies: Seq[Dependency] = Nil

  def parseCommandLine(args: Seq[String]): Unit = {}
}

case class SampleAssertFailure(pos: Position) extends AbstractError {
  def fullId: String = "sample.assert.failed"

  def readableMessage: String = "the assertion may not hold"
}