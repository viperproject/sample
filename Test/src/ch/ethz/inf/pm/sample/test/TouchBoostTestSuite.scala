package ch.ethz.inf.pm.sample.test

import semper.sil.testing.{AnnotatedTestInput, AbstractOutput, SystemUnderTest, AnnotationBasedTestSuite}
import java.nio.file.Path
import ch.ethz.inf.pm.sample.reporting.SampleMessage

abstract class TouchBoostTestSuite extends AnnotationBasedTestSuite {

  def systemsUnderTest: Seq[SystemUnderTest] = Seq(TouchAnalysisUnderTest)

  case class SampleOutput(message: SampleMessage) extends AbstractOutput {
    lazy val PPPattern = """PP\(.*:(\d+)\.\d+(_it.*)?\)""".r

    def isSameLine(file: Path, lineNr: Int): Boolean = {
      val pp = message.pp.toString
      val matchedLine =
        pp match {
          case PPPattern(ppline, it) => ppline.toInt
          case _ => sys.error( "SampleError PP does not have expected structure")
        }
      matchedLine == lineNr
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
}
