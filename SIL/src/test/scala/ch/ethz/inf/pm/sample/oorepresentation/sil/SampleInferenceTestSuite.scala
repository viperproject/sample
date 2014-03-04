package ch.ethz.inf.pm.sample.oorepresentation.sil

import semper.sil.testing.SilSuite
import semper.sil.verifier.{VerificationResult, Verifier}
import java.nio.file.Path
import semper.sil.frontend.Frontend
import semper.silicon.Silicon
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface

class SiliconWithInference(private var debugInfo: Seq[(String, Any)] = Nil)
  extends Silicon {

  override def verify(program: sil.Program) = {
    val runner = OnePhasePredicateAnalysisRunner
    val results = runner.run(program)

    val programExtender = ProgramExtender[ApronInterface.Default](runner.compiler)
    val extendedProgram = programExtender.extend(program, results)

    super.verify(extendedProgram)
  }
}

class SampleInferenceTestSuite extends SilSuite {
  def testDirectories = Seq("sil/inference")

  def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilFrontend()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  def verifiers: Seq[Verifier] = Seq(createSiliconInstance())

  // Copied from silicon/src/test/scala/SiliconTests.scala
  private def createSiliconInstance() = {
    val silicon = new SiliconWithInference(Seq(("startedBy", "semper.silicon.SiliconTests")))

    val args = optionsFromScalaTestConfigMap(configMap) ++ Seq("dummy.sil")

    silicon.parseCommandLine(args)
    silicon.config.initialize {case _ =>}

    silicon
  }

  private def optionsFromScalaTestConfigMap(configMap: Map[String, Any]): Seq[String] =
    configMap.flatMap{case (k, v) => Seq("--" + k, v.toString)}.toSeq
}