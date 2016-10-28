/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Path

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.execution.MethodAnalysisResult
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSilverConverter
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisState.SimplePermissionAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis._
import ch.ethz.inf.pm.sample.test.LatticeTest
import viper.silicon.Silicon
import viper.silver.ast._
import viper.silver.frontend.Frontend
import viper.silver.testing.SilSuite
import viper.silver.verifier._

/** Testing of Permission Analysis.
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
class PermissionAnalysisTest extends SilSuite {
  override def testDirectories = Seq("silver/inference")

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilverFrontend()
    fe.init(verifier); fe.reset(files); fe
  }

  override def verifiers: Seq[Verifier] = Seq(createSiliconInstance())

  private def createSiliconInstance(): Silicon = {   // copied from silicon/src/test/scala/SiliconTests.scala
    val silicon = new SiliconWithPermissionAnalysis(Seq(("startedBy", "viper.silicon.SiliconTests")))
    val args = optionsFromScalaTestConfigMap(configMap) ++ Seq("dummy.sil")
    silicon.parseCommandLine(args); silicon.config.initialize { case _ => silicon.config.initialized = true }; silicon
  }

  private def optionsFromScalaTestConfigMap(configMap: Map[String, Any]): Seq[String] =
    configMap.flatMap{case (k, v) => Seq("--" + k, v.toString)}.toSeq
}

class SiliconWithPermissionAnalysis(private var debugInfo: Seq[(String, Any)] = Nil) extends Silicon {
  override val name: String = "sample"

  override def verify(program: Program): VerificationResult = {
    val runner = PermissionInference
    val results = runner.run(program).collect{ case x:MethodAnalysisResult[SimplePermissionAnalysisState] => x } // run the permission inference
    // extend the program with the inferred permissions
    val extendedProgram = runner.extendProgram(DefaultSilverConverter.prog, results)

    try {
      // use silicon to verify the extended program
      start()
      val result = super.verify(extendedProgram)

      // use silicon to verify that the inferred preconditions are feasible
      val errors = feasibilityPrograms(extendedProgram).foldLeft(List.empty[AbstractError]) {
        case (err, prog) => super.verify(prog) match {
          case Success =>
            // the precondition is infeasible since the "assert(false)" succeeded
            val method = program.methods.head
            InfeasiblePrecondition(method.name, method.pos) :: err
          case Failure(_) => err
        }
      }

      if (errors.nonEmpty) result match {
        case Failure(verificationErrors) => Failure(verificationErrors ++ errors)
        case Success => Failure(errors)
      } else result
    } catch {
      case _: Throwable => Success // something went wrong with the verifier (not our fault)
    }
  }

  /** Creates a program for every method in the given program that contains only
    * that method with a feasibility check. The feasibility check is an
    * "assert(false)" at the beginning of the body. If the precondition is
    * feasible then the assertion should fail. If the assertion succeeds then
    * the precondition is infeasible.
    *
    * @param program The program.
    * @return The sequence of programs.
    */
  private def feasibilityPrograms(program: Program): Seq[Program] = {
    program.methods.map { method =>
      // the feasibility check
      val check = Assert(FalseLit()())(method.pos)
      // add the feasibility check to the body of the current method
      val body = method.body match {
        case sequence: Seqn => sequence.copy(check +: sequence.ss)(sequence.pos, sequence.info)
        case statement => Seqn(check :: statement :: Nil)(statement.pos, statement.info)
      }
      // create program that only contains the current method with the check
      val methods = Seq(method.copy(_body = body)(method.pos, method.info))
      program.copy(methods = methods)(program.pos, program.info)
    }
  }
}

/** Property-based testing of lattice elements for Permission Analysis.
  *
  * @author Caterina Urban
  */
class PermissionAnalysisLatticeTest extends LatticeTest[SimplePermissionAnalysisState] {
  SystemParameters.typ = DummyRefType
  override def factory: SimplePermissionAnalysisState = PermissionAnalysisEntryState.topState
}

case class InfeasiblePrecondition(method: String, pos: Position) extends AbstractError {
  override def fullId: String = "precondition.infeasible"
  override def readableMessage: String = s"Inferred precondition of method $method is infeasible."
}