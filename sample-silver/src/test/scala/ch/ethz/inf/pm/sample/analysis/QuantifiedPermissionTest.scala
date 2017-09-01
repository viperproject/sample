/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.inference.SilverExtender
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.{QuantifiedPermissionsAnalysisRunner, QuantifiedPermissionsState}
import ch.ethz.inf.pm.sample.util.Verifiers
import viper.silver.testing._
import viper.silver.verifier._

/**
  * A test suite for inference.
  *
  * @author Jerome Dohrau
  */
abstract class InferenceTest[S <: State[S]]
  extends AnnotationBasedTestSuite {

  def inference: SilverExtender[S]

  override def systemsUnderTest: Seq[SystemUnderTest] = Seq(InferenceUnderTest(inference))

  private case class InferenceUnderTest[S <: State[S]](inference: SilverExtender[S]) extends
    SystemUnderTest with TimingUtils {

    override val projectInfo: ProjectInfo = new ProjectInfo(List("Sample"))

    override def run(input: AnnotatedTestInput): Seq[AbstractOutput] = {
      assert(input.files.length == 1, "Tests should consist of exactly one file.")

      val path = input.files.head.toString
      val program = inference.compile(path)

      val methods = program.methods.filterNot(_.name.endsWith("_expected"))

      val filtered = program.copy(methods = methods)(program.pos, program.info, program.errT)
      val extended = inference.extend(filtered)

      println(s"ORIGINAL:\n$program")
      println(s"EXTENDED:\n$extended")

      val verifier = carbon
      val result = verifier.verify(extended)

      result match {
        case Success => Seq.empty
        case Failure(errors) => errors.map {
          case error: AbstractVerificationError => SilOutput(error.transformedError())
          case error => SilOutput(error)
        }
      }
    }

    /**
      * Returns an instance of the silicon verifier.
      *
      * @return A silicon instance.
      */
    def silicon: Verifier = Verifiers.silicon

    /**
      * Returns an instance of the carbon verifier.
      *
      * @return A carbon instance.
      */
    def carbon: Verifier = Verifiers.carbon
  }

}

/**
  * Tests for the quantified permission analysis.
  *
  * @author Jerome Dohrau
  */
class QuantifiedPermissionTest
  extends InferenceTest[QuantifiedPermissionsState] {

  override def testDirectories: Seq[String] = Seq("silver/quantified")

  override def inference: SilverExtender[QuantifiedPermissionsState] = QuantifiedPermissionsAnalysisRunner
}
