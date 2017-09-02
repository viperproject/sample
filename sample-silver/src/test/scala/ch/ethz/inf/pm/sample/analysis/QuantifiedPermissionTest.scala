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
import viper.silver.{ast => sil}
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
      val specifications = program.methods
        .filter(_.name.endsWith("_expected"))
        .foldLeft(Map.empty[String, sil.Method]) { (result, method) => result + (method.name.dropRight(9) -> method) }

      val filtered = program.copy(methods = methods)(program.pos, program.info, program.errT)
      val extended = inference.extend(filtered)

      val checks = extended.methods.flatMap { inferred =>
        specifications.get(inferred.name) match {
          case Some(expected) => createChecks(inferred, expected)
          case None => Seq.empty
        }
      }

      val extendedWithChecks = extended.copy(methods = extended.methods ++ checks)(pos = extended.pos, info = extended.info, errT = extended.errT)
      println(extendedWithChecks)

      val verifier = carbon
      val result = verifier.verify(extendedWithChecks)

      result match {
        case Success => Seq.empty
        case Failure(errors) => errors.map {
          case error: AbstractVerificationError => SilOutput(error.transformedError())
          case error => SilOutput(error)
        }
      }
    }

    def createChecks(inferred: sil.Method, expected: sil.Method): Seq[sil.Method] = {
      val name = inferred.name
      val parameters = inferred.formalArgs
      val body = sil.Seqn(Seq.empty, Seq.empty)()

      val check0 = sil.Method(name + "_pre_inferred_implies_expected", parameters, Seq.empty, inferred.pres, expected.pres, body)()
      val check1 = sil.Method(name + "_pre_expected_implies_inferred", parameters, Seq.empty, expected.pres, inferred.pres, body)()
      val check2 = sil.Method(name + "_post_inferred_implies_expected", parameters, Seq.empty, inferred.posts, expected.posts, body)()
      val check3 = sil.Method(name + "_post_expected_implies_inferred", parameters, Seq.empty, expected.posts, inferred.posts, body)()

      Seq(check0, check1, check2, check3)
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
