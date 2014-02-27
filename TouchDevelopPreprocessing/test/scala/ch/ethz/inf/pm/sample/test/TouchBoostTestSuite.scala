package ch.ethz.inf.pm.sample.test

import semper.sil.testing.{AnnotatedTestInput, AbstractOutput, SystemUnderTest, AnnotationBasedTestSuite}
import java.nio.file.Path
import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis.{ReportingParams, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchProgramPoint

abstract class TouchBoostTestSuite extends AnnotationBasedTestSuite {

  def systemsUnderTest: Seq[SystemUnderTest] = Seq(TouchAnalysisUnderTest)

  case class SampleOutput(message: SampleMessage) extends AbstractOutput {
    def isSameLine(file: Path, lineNr: Int): Boolean = {
      val messageLine =
        message.pp match {
          case TouchProgramPoint(_, Some(lineColumnPos), customPos) =>
            lineColumnPos.line
          case _ => sys.error( "SampleError PP does not have expected structure")
        }
      messageLine == lineNr
    }

    def fullId: String = message.id
  }

  private object TouchAnalysisUnderTest extends SystemUnderTest {
    val projectName: String = "TouchBoost"

    def run(input: AnnotatedTestInput): Seq[AbstractOutput] = {
      val fp = input.file
      val sampleMessages = runOnFile(fp.toString)
      sampleMessages map SampleOutput
    }
  }

  def runOnFile(file: String): Seq[SampleMessage]

  def touchBoostOptions: TouchAnalysisParameters = {
    TouchAnalysisParameters(reporting = ReportingParams(silent = true))
  }
}
