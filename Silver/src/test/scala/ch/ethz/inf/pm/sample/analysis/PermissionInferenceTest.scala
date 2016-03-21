package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Path

import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSilConverter
import ch.ethz.inf.pm.sample.permissionanalysis.{PermissionIntervalsAnalysisRunner, PermissionPolyhedraAnalysisRunner}
import viper.silicon.Silicon
import viper.silver.ast.Program
import viper.silver.frontend.Frontend
import viper.silver.testing.SilSuite
import viper.silver.verifier.{Success, Verifier}

class SiliconWithPermissionIntervalsInferenceTestSuite extends SilSuite {
  override def testDirectories = Seq("silver/permissions/basic")

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new DummySilFrontend()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override def verifiers: Seq[Verifier] = Seq(createSiliconInstance())

  // copied from silicon/src/test/scala/SiliconTests.scala
  private def createSiliconInstance(): Silicon = {
    val silicon = new SiliconWithPermissionIntervalsInference(Seq(("startedBy", "viper.silicon.SiliconTests")))
    val args = optionsFromScalaTestConfigMap(configMap) ++ Seq("dummy.sil")

    silicon.parseCommandLine(args)
    silicon.config.initialize { case _ => silicon.config.initialized = true }

    silicon
  }

  private def optionsFromScalaTestConfigMap(configMap: Map[String, Any]): Seq[String] =
    configMap.flatMap{case (k, v) => Seq("--" + k, v.toString)}.toSeq

}

class SiliconWithPermissionIntervalsInference(private var debugInfo: Seq[(String, Any)] = Nil) extends Silicon {

  override val name: String = "sample"

  override def verify(program: Program) = {
    val runner = PermissionIntervalsAnalysisRunner
    val results = runner.run(program) // run the permission inference
    // extend the program with the inferred permissions
    val extendedProgram = runner.extendProgram(DefaultSilConverter.prog,results)
    // use silicon to verify the extended program
    start(); super.verify(extendedProgram)
  }

}

class SiliconWithPermissionPolyhedraInference(private var debugInfo: Seq[(String, Any)] = Nil) extends Silicon {

  override val name: String = "sample"

  override def verify(program: Program) = {
    val runner = PermissionPolyhedraAnalysisRunner
    val results = runner.run(program) // run the permission inference
    // extend the program with the inferred permissions
    val extendedProgram = runner.extendProgram(DefaultSilConverter.prog,results)
    // use silicon to verify the extended program
    super.verify(extendedProgram)
  }

}

object DummyAnalyzer extends SampleAnalyzer {
  def name = "dummy"

  def verify(program: Program) = Success

  override def start(): Unit = ()

  override def stop(): Unit = ()
}
