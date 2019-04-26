/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.test

import java.net.{URI, URL}
import java.nio.file._
import java.util.regex.Pattern

import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.reporting.SampleMessage
import ch.ethz.inf.pm.td.analysis.TouchRun
import ch.ethz.inf.pm.td.compiler.{SpaceSavingProgramPoint, TouchProgramPointRegistry}
import org.scalatest._

import scala.collection.JavaConversions._
import scala.io.Source

class FileBasedTestSuite extends TouchGuruTestSuite {

  def testDirectories: Seq[String] = Seq("automated_tests")

  override def runOnFile(path: Path): Seq[SampleMessage] = {
    val res = TouchRun.runInThread(Compilable.Path(path))
    assert(!TouchRun.threadFailed)
    res.collect{ case s:SampleMessage => s }
  }

}

abstract class TouchGuruTestSuite extends AnnotationBasedTestSuite {

  def systemsUnderTest: Seq[SystemUnderTest] = Seq(TouchAnalysisUnderTest)

  case class SampleOutput(message: SampleMessage) extends AbstractOutput {
    def isSameLine(file: Path, lineNr: Int): Boolean = {
      val messageLine =
        message.pp match {
          case x: SpaceSavingProgramPoint =>
            TouchProgramPointRegistry.reg(x.id).lineColumnPosition.line
          case _ =>
            sys.error("SampleError PP does not have expected structure")
        }
      messageLine == lineNr
    }

    def fullId: String = message.id
  }

  private object TouchAnalysisUnderTest extends SystemUnderTest {
    val projectName: String = "TouchGuru"

    def run(input: AnnotatedTestInput): Seq[AbstractOutput] = {
      val fp = input.file
      val sampleMessages = runOnFile(fp)
      sampleMessages map SampleOutput
    }
  }

  def runOnFile(path: Path): Seq[SampleMessage]
}

/**
  * A test suite for end-to-end toolchain testing that operates on source files
  * in resource directories.
  *
  * This abstract class is agnostic w.r.t. to the kind of testing performed
  * on the test input. It just locates the test files and builds the test input.
  * Subclasses need to implement the actual testing logic in `registerTest`.
  *
  * @author Stefan Heule
  */
abstract class ResourceBasedTestSuite extends FunSuite with SampleTest {
  // Subclasses can extend the test input with further information
  // such as annotations
  type InputType <: TestInput

  /**
    * The test directories where tests can be found.
    * The directories must be relative because they are resolved via
    * [[java.lang.ClassLoader]].
    *
    * @see http://stackoverflow.com/a/7098501/491216.
    * @return A sequence of test directories.
    */
  def testDirectories: Seq[String]

  /**
    * Registers a test based on the test input.
    * To be implemented by subclasses.
    *
    * @param input the test input as built in `buildTestInput`
    */
  def registerTest(input: InputType)

  /**
    * Builds the test input from the given source file.
    *
    * @param file the canonical test source file. The test input implementation
    *             will decide what other files are part of the same test, if any
    * @param prefix part of the test file path. Together with the test file name
    *               it should uniquely identify the test
    * @return the test input
    */
  def buildTestInput(file: Path, prefix: String): InputType

  /**
    * Recursively registers all files found in the given directory as a test.
    *
    * The prefix is used for naming and tagging the ScalaTest test case that is eventually
    * generated for each test file found. Subdirectories of `dir` will be appended to the
    * initial prefix.
    *
    * It is thus reasonable to make the initial prefix the (relative) root test directory.
    * For example, given the following directories and files
    *   .../issues/test1.scala
    *              test2.scala
    *   .../all/basic/test1.scala
    *                 test2.scala
    *                 test3.scala
    * it would be reasonable to make the calls
    *   registerTestDirectory(path_of(".../issues"), "issues")
    *   registerTestDirectory(path_of(".../all/basic"), "all/basic")
    * or
    *   registerTestDirectory(path_of(".../issues"), "issues")
    *   registerTestDirectory(path_of(".../all"), "all")
    * to - in both cases - get ScalaTest test cases that can be identified by
    *   issues/test1.scala
    *   issues/test2.scala
    *   all/basic/test1.scala
    *   all/basic/test2.scala
    *   all/basic/test3.scala
    *
    * @param dir The directory to recursively search for files. Every file in the directory is
    *            assumed to be a test file.
    * @param prefix The initial prefix used for naming and tagging the resulting ScalaTest tests.
    */
  private def registerTestDirectory(dir: Path, prefix: String) {
    assert(dir != null, "Directory must not be null")
    assert(Files.isDirectory(dir), "Path must represent a directory")

    val directoryStream = Files.newDirectoryStream(dir)
    val dirContent = directoryStream.toList
    val namePattern = configMap.getOrElse("includeTests", ".*").toString

    for (f: Path <- dirContent
         if Files.isDirectory(f)) {

      val newPrefix = prefix + "/" + f.getName(f.getNameCount - 1)
      registerTestDirectory(f, newPrefix)
    }

    for (f: Path <- dirContent
         // If a file is renamed while Sbt runs, AccessDeniedExceptions
         // might be thrown. Apparently, because the old file still exists in
         // target/.../test-classes, but it is somehow locked. Weird stuff.
         // Once the SBT REPL is closed, the "ghost" file disappears.
         if Files.isReadable(f)

         if !Files.isDirectory(f)
         if f.toString.matches(namePattern)) {

      val testInput = buildTestInput(f, prefix)

      // The files that belong to the same test.
      if (testInput.files.head == f) {
        // Only register the multi file test once, not for every file it contains.
        registerTest(testInput)
      }
    }
  }

  private var _testsRegistered = false

  private def registerTests() {
    if (_testsRegistered) return

    for (testDir <- testDirectories) {
      val resource = classLoader.getResource(testDir)
      assert(resource != null, s"Test directory $testDir couldn't be found")

      val path = pathFromResource(classLoader.getResource(testDir))
      registerTestDirectory(path, testDir)
    }

    _testsRegistered = true
  }

  /**
    * Returns a class loader that can be used to access resources
    * such as test files via [[java.lang.ClassLoader]].
    *
    * @return A class loader for accessing resources.
    */
  private def classLoader: ClassLoader = getClass.getClassLoader

  private var openFileSystems: Seq[FileSystem] = Seq()
  addShutdownHookForOpenFileSystems()

  /**
    * Creates a path from the given URL, which, for example, could have been
    * obtained by calling [[java.lang.ClassLoader]]. The current implementation
    * can handle URLs that point into the normal file system (file:...) or
    * into a jar file (jar:...).
    *
    * Based on code taken from http://stackoverflow.com/a/15718001/491216.
    *
    * @param resource The URL to turn into a path.
    * @return The path obtained from the URL.
    */
  private def pathFromResource(resource: URL): Path = {
    assert(resource != null, "Resource URL must not be null")

    val uri = resource.toURI

    uri.getScheme match {
      case "file" => Paths.get(uri)

      case "jar" =>
        val uriStr = uri.toString
        val separator = uriStr.indexOf("!/")
        val entryName = uriStr.substring(separator + 2)
        val fileURI = URI.create(uriStr.substring(0, separator))

        // 2013-05-03 Malte:
        // The following bit of code is quite nasty, but I wasn't able to get anything less
        // nasty to work reliably. There are two main problems:
        //
        // 1. It is not obvious when to close the file system, because several components of
        //    our tool chain keep references to path objects that in turn have references
        //    to their underlying file system. If these path objects are used (not all usages
        //    seem dangerous, though) after the underlying file system has been closed, an
        //    exception is thrown.
        //
        // 2. If the test suite is executed from an Sbt prompt then the shutdown hook of
        //    the runtime environment doesn't seem to fire and the file systems don't seem
        //    to be closed. Thus, if the tests are executed again without existing the
        //    Sbt prompt, FileSystemAlreadyExistsExceptions can be thrown because some
        //    file systems are still open.
        //
        // The list of open file systems (openFileSystems) unfortunately doesn't seem to
        // survive, otherwise it might have been possible to maintain a map from file URI
        // to file system and only create a new file system if there is no map entry for
        // the given file URI yet.
        //
        // I also tried to use a finalizer method instead of the shutdown hook, but the
        // finalizer also doesn't seem to be called if the Sbt prompt is not existed.

        var fs: FileSystem = null

        try {
          fs = FileSystems.newFileSystem(fileURI, Map[String, Object]())
          openFileSystems = fs +: openFileSystems
        } catch {
          case e: java.nio.file.FileSystemAlreadyExistsException =>
            fs = FileSystems.getFileSystem(fileURI)
            assert(fs.isOpen, "The reused file system is expected to still be open")
        } finally {
          assert(fs != null, s"Could not get hold of a file system for $fileURI (from $uriStr)")
        }

        fs.getPath(entryName)

      case other => sys.error(s"Resource $uri of scheme $other is not supported.")
    }
  }

  /** Closes all open file systems stored in `openFileSystems`. */
  private def addShutdownHookForOpenFileSystems() {
    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run() {
        if (openFileSystems != null) {
          openFileSystems foreach (fs => if (fs.isOpen) {fs.close()})
        }
      }
    })
  }

  override def testNames = {
    registerTests()
    super.testNames
  }

  /** The config map passed to ScalaTest. */
  protected var configMap: ConfigMap = ConfigMap()

  protected override def runTest(
                                  testName: String,
                                  args:Args):Status = {
    this.configMap = args.configMap
    registerTests()
    super.runTest(testName, args)
  }
}


/**
  * End-to-end test suite that extracts [[TestAnnotations]]
  * from input files and asserts that the output of the system under test
  * agrees with the annotations.
  *
  * It's possible to test multiple systems (e.g. verifiers) at once.
  * The test suite will run each test on each system.
  */
abstract class AnnotationBasedTestSuite extends ResourceBasedTestSuite {
  override type InputType = AnnotatedTestInput

  /**
    * The systems to test each input on.
    *
    * This method is not modeled as a constant field deliberately, such that
    * subclasses can instantiate a new [[SystemUnderTest]]
    * for each test input.
    */
  def systemsUnderTest: Seq[SystemUnderTest]

  def buildTestInput(file: Path, prefix: String) =
    DefaultAnnotatedTestInput(file, prefix)

  /** Registers a given test input for a given system under test. */
  def registerTest(input: AnnotatedTestInput, system: SystemUnderTest) = {
    test(input.name, input.tags :_*) {
      // Error for parse failures of test annotations
      val parserErrors = input.annotations.errors

      // Match expected outputs with actual outputs
      val actualOutputs = system.run(input)
      val expectedOutputs = input.annotations.outputAnnotations
      val outputErrors = OutputMatcher(actualOutputs, expectedOutputs).errors

      // All errors
      val errors = parserErrors ++ outputErrors

      // If there were any outputs that could not be matched up
      // (or other problems), make the test fail
      if (errors.nonEmpty) {
        val title = s"${errors.size} errors"
        val body = errors.groupBy(_.errorType).map({
          case (typ, es) =>
            TestErrorType.message(typ) + ":\n" + es.map("  " + _).mkString("\n")
        }).mkString("\n\n")
        fail(title + "\n\n" + body + "\n\n")
      }
    }
  }

  def registerTest(input: AnnotatedTestInput) = {
    require(input != null)

    for (system <- systemsUnderTest) {
      // Builds a new test input for the specific system under test.
      // For example, it may ignore files if necessary.
      val newInput = input.makeForProject(system.projectName)
      if (newInput.files.isEmpty)
        ignore(newInput.name, newInput.tags: _*) {}
      else
        registerTest(newInput, system)
    }
  }
}

/**
  * Finds the inconsistencies between the expected output and the actual
  * output of the system under test and builds the corresponding errors.
  *
  * It assumes that all annotations are relevant. That is, filter the
  * annotations first to exclude annotations specific to other projects
  * before giving them to the matcher.
  *
  * @param actualOutputs the output produced by the system under test
  * @param expectedOutputs the output expected according to the annotations
  */
case class OutputMatcher(
                          actualOutputs: Seq[AbstractOutput],
                          expectedOutputs: Seq[LocatedAnnotation]) {

  lazy val errors: Seq[TestError] = {
    var errors = List[TestError]()
    var remainingOutputs = expectedOutputs

    // Go through all actual outputs and try to match them up
    // with the expected ones
    actualOutputs foreach (a => findOutput(a) match {
      case Some(m: MissingOutput) =>
        errors ::= TestMissingButPresentOutputError(m, a)
      case Some(_) => // expected this error
      case None =>
        errors ::= TestAdditionalOutputError(a)
    })

    def findOutput(actual: AbstractOutput): Option[TestAnnotation] = {
      expectedOutputs.filter({
        case OutputAnnotation(id, file, lineNr) =>
          id.matches(actual.fullId) && actual.isSameLine(file, lineNr)
        case IgnoreOthers(file, lineNr, _) =>
          actual.isSameLine(file, lineNr)
      }) match {
        case Nil => None
        case l => l.find(_.isInstanceOf[OutputAnnotation]) match {
          case Some(x) =>
            // Remove the output from the list of expected output
            // (i.e. only match once)
            remainingOutputs = remainingOutputs.filterNot(_.eq(x))
            Some(x)
          case None =>
            Some(l.head) // IgnoreOthers should not be removed
        }
      }
    }

    // Separate missing outputs from remaining outputs
    val missingOutputs = remainingOutputs.collect{case m: MissingOutput => m}
    remainingOutputs = remainingOutputs.filterNot(missingOutputs.contains)

    // Process remaining outputs that have not been matched
    remainingOutputs.foreach {
      case e: ExpectedOutput =>
        if (!missingOutputs.exists(_.sameSource(e)))
          errors ::= TestExpectedButMissingOutputError(e)
      case u: UnexpectedOutput =>
        errors ::= TestUnexpectedButMissingOutputError(u)
      case _: IgnoreOthers =>
      case _: MissingOutput =>
        sys.error("Should not occur because they were previously filtered")
    }

    errors
  }
}

/** The system that produces output given the test input. */
trait SystemUnderTest {
  /** For filtering test annotations. Does not need to be unique. */
  val projectName: String

  def run(input: AnnotatedTestInput): Seq[AbstractOutput]
}

/**
  * An output produced by a system under test.
  *
  * Its `toString` method will be used to output an error message
  * if the output was not supposed occur.
  */
trait AbstractOutput {
  /** Whether the output belongs to the given line in the given file. */
  def isSameLine(file: Path, lineNr: Int): Boolean

  /** A short and unique identifier for this output. */
  def fullId: String
}

/** Test input that also includes test annotations. */
trait AnnotatedTestInput extends TestInput {
  val annotations: TestAnnotations

  /** Create a test input that is specific to the given project. */
  def makeForProject(projectName: String): AnnotatedTestInput
}

/** Test input that also includes test annotations. */
case class DefaultAnnotatedTestInput(
                                      name: String,
                                      prefix: String,
                                      files: Seq[Path],
                                      tags: Seq[Tag],
                                      annotations: TestAnnotations) extends AnnotatedTestInput {

  /**
    * Create a test input that is specific to the given project.
    *
    * It creates an additional tag, filters files according to annotations
    * and also filters the annotations themselves.
    */
  def makeForProject(projectName: String): DefaultAnnotatedTestInput = copy(
    name = s"$name [$projectName]",
    files = files.filter(!annotations.isFileIgnored(_, projectName)),
    tags = Tag(projectName) :: tags.toList,
    annotations = annotations.filterByProject(projectName))
}

object DefaultAnnotatedTestInput extends TestAnnotationParser {
  /**
    * Creates an annotated test input by parsing all annotations in the files
    * that belong to the given test input.
    */
  def apply(i: TestInput): DefaultAnnotatedTestInput =
    DefaultAnnotatedTestInput(i.name, i.prefix, i.files, i.tags,
      parseAnnotations(i.files))

  def apply(file: Path, prefix: String): DefaultAnnotatedTestInput =
    apply(DefaultTestInput(file, prefix))
}


/**
  * The result of parsing the test annotations in a single file.
  *
  * @param errors The errors encountered during parsing.
  * @param annotations The test annotations found.
  * @author Stefan Heule
  */
sealed case class TestAnnotations(
                                   errors: Seq[TestAnnotationParseError],
                                   annotations: Seq[TestAnnotation]) {

  def isFileIgnored(file: Path, project: String): Boolean = annotations exists {
    case _: IgnoreFileList => true
    case IgnoreFile(f, _, prj, _) =>
      f.toAbsolutePath == file.toAbsolutePath && prj.equalsIgnoreCase(project)
    case _ => false
  }

  def hasErrors: Boolean = errors.nonEmpty

  /** Returns all the annotations without IgnoreFile and IgnoreFileList. */
  def outputAnnotations: Seq[LocatedAnnotation] = {
    annotations collect {
      case x: LocatedAnnotation => x
    }
  }

  /**
    * Returns all test annotations except those that are specific
    * to a different project than the given one.
    *
    * @param project the name of the project
    * @return the filtered test annotations
    */
  def filterByProject(project: String): TestAnnotations =
    copy(annotations = annotations filter {
      case a: ProjectSpecificAnnotation => a.project.equalsIgnoreCase(project)
      case _ => true
    })

  /**
    * Returns all test annotations except those output annotations
    * whose key id does not start with the given key id prefix.
    */
  def filterByKeyIdPrefix(keyIdPrefix: String): TestAnnotations =
    copy(annotations = annotations filter {
      case OutputAnnotation(id, _, _) => id.keyId.startsWith(keyIdPrefix)
      case _ => true
    })
}

/** A trait for test annotations. */
sealed trait TestAnnotation

case class OutputAnnotationId(keyId: String, valueId: Option[String]) {
  override def toString = {
    valueId match {
      case None => keyId
      case Some(i) => s"$keyId:$i"
    }
  }

  def matches(id: String): Boolean = {
    val ids = id.split(":")
    if (ids.size == 1) {
      return keyId == id && valueId.isEmpty
    }
    assert(ids.size == 2, s"Expected full ID, but got $id.")
    valueId match {
      case None => keyId == ids(0)
      case Some(s) => keyId == ids(0) && s == ids(1)
    }
  }
}

/** Annotations that refer to a location. */
sealed trait LocatedAnnotation extends TestAnnotation {
  def file: Path
  def forLineNr: Int

  def sameSource(other: LocatedAnnotation) =
    Files.isSameFile(this.file, other.file) && forLineNr == other.forLineNr
}

/**
  * Test annotations that have a location and an identifier
  * (i.e. describe an output of some sort).
  */
sealed trait OutputAnnotation extends LocatedAnnotation {
  def id: OutputAnnotationId

  override def toString = s"$id (${file.getFileName.toString}:$forLineNr)"
}

/**
  * Test annotation that is specific to a certain project.
  * Further details should be given by the referenced issue.
  */
sealed trait ProjectSpecificAnnotation extends TestAnnotation {
  def project: String
  def issueNr: Int
}

object OutputAnnotation {
  def unapply(e: OutputAnnotation) = Some((e.id, e.file, e.forLineNr))
}

case class ExpectedOutput(
                           id: OutputAnnotationId,
                           file: Path,
                           forLineNr: Int,
                           annotationLineNr: Int) extends OutputAnnotation

case class UnexpectedOutput(
                             id: OutputAnnotationId,
                             file: Path,
                             forLineNr: Int,
                             annotationLineNr: Int,
                             project: String,
                             issueNr: Int) extends OutputAnnotation with ProjectSpecificAnnotation

case class MissingOutput(
                          id: OutputAnnotationId,
                          file: Path,
                          forLineNr: Int,
                          annotationLineNr: Int,
                          project: String,
                          issueNr: Int) extends OutputAnnotation with ProjectSpecificAnnotation

case class IgnoreOthers(
                         file: Path,
                         forLineNr: Int,
                         annotationLineNr: Int) extends LocatedAnnotation

case class IgnoreFile(
                       file: Path,
                       annotationLineNr: Int,
                       project: String,
                       issueNr: Int) extends ProjectSpecificAnnotation

case class IgnoreFileList(
                           file: Path,
                           annotationLineNr: Int,
                           project: String,
                           issueNr: Int) extends ProjectSpecificAnnotation

/**
  * The types of errors that can be revealed by
  * [[AnnotationBasedTestSuite]].
  *
  * This enumeration makes it easy to group errors by type and to
  * store data that is not specific to an individual error, but an error type.
  */
object TestErrorType extends Enumeration {
  type TestErrorType = Value

  val AnnotationParsing = Value
  val AdditionalOutput = Value
  val ExpectedButMissingOutput = Value
  val UnexpectedButMissingOutput = Value
  val MissingButPresentOutputs = Value

  /** The message to be displayed before all errors of the same given type. */
  def message(error: TestErrorType): String = error match {
    case AnnotationParsing =>
      "Encountered the following errors while parsing " +
        "the test annotations (these annotations will be ignored)"
    case AdditionalOutput =>
      "The following output occurred during testing, " +
        "but should not have according to the test annotations"
    case ExpectedButMissingOutput =>
      "The following outputs were expected according to the test " +
        "annotations, but did not occur during testing"
    case UnexpectedButMissingOutput =>
      "The following outputs was specified to occur erroneously " +
        "(UnexpectedOutput) according to the test annotations, " +
        "but did not occur during testing"
    case MissingButPresentOutputs =>
      "The following outputs were specified to be missing erroneously " +
        "(MissingOutput) according to the test annotations, but did occur " +
        "during testing (this might be cause by invalid test annotations)"
  }
}

/**
  * An error revealed by the test suite.
  *
  * It would be possible to create separate case classes for each type of error,
  * but for the moment, the error type and an error message is enough.
  */
abstract class TestError(val errorType: TestErrorType.Value) {
  def message: String
}

case class TestAnnotationParseError(
                                     offendingLine: String,
                                     file: Path,
                                     lineNr: Int) extends TestError(TestErrorType.AnnotationParsing) {
  def message =
    s"Line $lineNr in ${file.toString} looks like a test annotation " +
      s"(it starts with '//::'), but it was not possible to parse it correctly. " +
      s"The line is '$offendingLine'."
}

case class TestAdditionalOutputError(output: AbstractOutput)
  extends TestError(TestErrorType.AdditionalOutput) {
  def message = output.toString
}

case class TestExpectedButMissingOutputError(expectedOutput: ExpectedOutput)
  extends TestError(TestErrorType.ExpectedButMissingOutput) {
  def message = expectedOutput.toString
}

case class TestUnexpectedButMissingOutputError(unexpectedOutput: UnexpectedOutput)
  extends TestError(TestErrorType.UnexpectedButMissingOutput) {
  def message = unexpectedOutput.toString
}

case class TestMissingButPresentOutputError(missingOutput: MissingOutput, output: AbstractOutput)
  extends TestError(TestErrorType.MissingButPresentOutputs) {
  def message = output.toString
}


/** Provides the name, files and tags from which to create a test. */
trait TestInput {
  /** The name of the test, e.g. the name of the canonical source file. */
  val name: String

  /** Zero or more parent folders of the test file. */
  val prefix: String

  /** The list of files which belong to the same test as the given file. */
  val files: Seq[Path]

  /** The canonical test source file. */
  def file: Path = files.head

  /** The tags to be used for the test. */
  val tags: Seq[Tag]
}

case class DefaultTestInput(
                             name: String,
                             prefix: String,
                             files: Seq[Path],
                             tags: Seq[Tag]) extends TestInput {}

object DefaultTestInput {
  val fileListRegex = """(.*)_file\d*.*""".r

  /**
    * Creates the default test input from the given source file and prefix.
    *
    * It is possible to create a single test with multiple files by naming
    * the files foo_file1.ext foo_file2.ext etc. and putting them into
    * the same directory. They are ordered according to their number.
    *
    * Tests are named by their first file, e.g. `basic/functions.sil`. Tests are
    * also tagged by their name and their file name (with and without extension):
    * In the example the tags would be `basic/functions.sil`, `functions.sil` and
    * `functions`. These tags can be used to execute just a single test:
    * `test-only * -- -n functions`.
    *
    * @param file the canonical test file representing the test
    * @param prefix zero or more parent folders of the test file
    */
  def apply(file: Path, prefix: String): DefaultTestInput = {
    val name = file.getFileName.toString match {
      case fileListRegex(n) => prefix + "/" + n
      case n => prefix + "/" + n
    }

    val files: Seq[Path] = file.toString match {
      case fileListRegex(n) =>
        // Create a regex for files that belong to the same test.
        val regex = (Pattern.quote(n) + """_file(\d*).*""").r
        // Collect all files that match this regex and their numbers.
        var files = List[(Path, Int)]()
        val dirStream = Files.newDirectoryStream(file.getParent)
        dirStream foreach { f =>
          f.toString match {
            case regex(index) => files = (f, index.toInt) :: files
            case _ =>
          }
        }
        // Sort the file by their numbers and return the files only.
        // (We only needed the numbers for ordering)
        files.sortBy(_._2).map(_._1)
      case _ => List(file)
    }

    val tags: Seq[Tag] = {
      val relativeFileName = prefix + "/" + file.getName(file.getNameCount - 1)
      val fileName = file.getFileName.toString
      val fileNameWithoutExt = fileName.substring(0, fileName.lastIndexOf("."))

      List(
        Tag(relativeFileName),
        Tag(file.toString),
        Tag(fileName),
        Tag(fileNameWithoutExt))
    }

    DefaultTestInput(name, prefix, files, tags)
  }
}


/**
  * A parser for test annotations.  For an explanation of possible annotations and their syntax see
  * [[https://bitbucket.org/semperproject/sil/wiki/End-to-End%20Testing%20of%20Verifiers%20Based%20on%20SIL the description on the wiki]].
  *
  * @author Stefan Heule
  */
trait TestAnnotationParser {

  /**
    * Sequence that starts a comment in the language that is parsed. Can be overridden if a language is used that
    * has something else than `//` as the start of a single line comment.
    */
  val commentStart = "//"

  /**
    * Takes a sequence of files as input and parses all test annotations present in those
    * files and returns an object describing the result.
    */
  def parseAnnotations(files: Seq[Path]): TestAnnotations = {
    val (parseErrors, annotations) = (files map parseAnnotations).unzip
    TestAnnotations(parseErrors.flatten, annotations.flatten)
  }

  /** Takes a file as input and parses all test annotations present in that file and
    * returns an object describing the result.
    */
  def parseAnnotations(file: Path) = {
    val lines = Source.fromInputStream(Files.newInputStream(file))
      .mkString
      .replace("""\r""", "")
      .split("\n")
      .iterator
      .buffered
    var curLineNr = 0
    var curAnnotations: List[TestAnnotation] = Nil
    var finalAnnotations: List[TestAnnotation] = Nil
    var parseErrors: List[TestAnnotationParseError] = Nil
    // indicates that the last line we saw was a test annotation, and that we are currently looking for more test
    // annotations on the coming lines (recall that the target line for a test annotation is the next line that
    // is not a comment or another test annotation
    var parsingAnnotations = false

    // go through all lines to find test annotations
    while (lines.hasNext) {
      var l = lines.next().trim
      curLineNr += 1

      // found a line that looks like a test annotations
      if (l.startsWith(commentStart + "::")) {
        if (l.startsWith(commentStart + ":: ")) {
          l = l.substring(5)

          // what kind of annotation is it?
          isExpectedOutput(l, file, curLineNr,
            () => isUnexpectedOutput(l, file, curLineNr,
              () => isMissingOutput(l, file, curLineNr,
                () => isIgnoreOthers(l, file, curLineNr,
                  () => isIgnoreFile(l, file, curLineNr,
                    () => isIgnoreFileList(l, file, curLineNr)))))) match {
            case Some(e) =>
              curAnnotations ::= e
            case None =>
              // could not parse it
              parseErrors ::= TestAnnotationParseError(l, file, curLineNr)
          }

          parsingAnnotations = true
        } else {
          // there should be a space -> report error
          parseErrors ::= TestAnnotationParseError(l, file, curLineNr)
        }
      } else if (l.startsWith(commentStart)) {
        // ignore comments
      } else {
        // finish parsing annotations
        if (parsingAnnotations) {
          finalAnnotations :::= finishAnnotations(curAnnotations, curLineNr)
          curAnnotations = Nil
          parsingAnnotations = false
        }
      }
    }
    (parseErrors.reverse, finalAnnotations.reverse)
  }

  /** At the time we parse a test annotation, we cannot know the `forLineNr` yet, so add it correctly now. */
  private def finishAnnotations(annotations: List[TestAnnotation], forLineNr: Int): List[TestAnnotation] = {
    for (a <- annotations) yield {
      a match {
        case ExpectedOutput(id, file, _, lineNr) => ExpectedOutput(id, file, forLineNr, lineNr)
        case UnexpectedOutput(id, file, _, lineNr, project, issueNr) => UnexpectedOutput(id, file, forLineNr, lineNr, project, issueNr)
        case MissingOutput(id, file, _, lineNr, project, issueNr) => MissingOutput(id, file, forLineNr, lineNr, project, issueNr)
        case IgnoreOthers(file, _, lineNr) => IgnoreOthers(file, forLineNr, lineNr)
        case _ => a
      }
    }
  }

  /**
    * A regular expression that matches an output id,
    * either only using the output reason, or with the full id.
    */
  val outputIdPattern = "([^:]*)(:(.*))?"

  /** Try to parse the annotation as `ExpectedOutput`, and otherwise use `next`. */
  private def isExpectedOutput(annotation: String, file: Path, lineNr: Int, next: () => Option[TestAnnotation] = () => None): Option[TestAnnotation] = {
    val regex = ("""^ExpectedOutput\(""" + outputIdPattern + """\)$""").r
    annotation match {
      case regex(keyId, _, null) =>
        Some(ExpectedOutput(OutputAnnotationId(keyId, None), file, -1, lineNr))
      case regex(keyId, _, valueId) =>
        Some(ExpectedOutput(OutputAnnotationId(keyId, Some(valueId)), file, -1, lineNr))
      case _ => next()
    }
  }

  /** Try to parse the annotation as `UnexpectedOutput`, and otherwise use `next`. */
  private def isUnexpectedOutput(annotation: String, file: Path, lineNr: Int, next: () => Option[TestAnnotation] = () => None): Option[TestAnnotation] = {
    val regex = ("""^UnexpectedOutput\(""" + outputIdPattern + """, /(.*)/issue/([0-9]+)/\)$""").r
    annotation match {
      case regex(reasonId, _, null, project, issueNr) =>
        Some(UnexpectedOutput(OutputAnnotationId(reasonId, None), file, -1, lineNr, project, issueNr.toInt))
      case regex(keyId, _, valueId, project, issueNr) =>
        Some(UnexpectedOutput(OutputAnnotationId(keyId, Some(valueId)), file, -1, lineNr, project, issueNr.toInt))
      case _ => next()
    }
  }

  /** Try to parse the annotation as `MissingOutput`, and otherwise use `next`. */
  private def isMissingOutput(annotation: String, file: Path, lineNr: Int, next: () => Option[TestAnnotation] = () => None): Option[TestAnnotation] = {
    val regex = ("""^MissingOutput\(""" + outputIdPattern + """, /(.*)/issue/([0-9]+)/\)$""").r
    annotation match {
      case regex(reasonId, _, null, project, issueNr) =>
        Some(MissingOutput(OutputAnnotationId(reasonId, None), file, -1, lineNr, project, issueNr.toInt))
      case regex(keyId, _, valueId, project, issueNr) =>
        Some(MissingOutput(OutputAnnotationId(keyId, Some(valueId)), file, -1, lineNr, project, issueNr.toInt))
      case _ => next()
    }
  }

  /** Try to parse the annotation a ``IgnoreOthers``, and otherwise use `next`. */
  private def isIgnoreOthers(annotation: String, file: Path, lineNr: Int, next: () => Option[TestAnnotation] = () => None): Option[TestAnnotation] = {
    val regex = """^IgnoreOthers$""".r
    annotation match {
      case regex() => Some(IgnoreOthers(file, -1, lineNr))
      case _ => next()
    }
  }

  /** Try to parse the annotation a ``IgnoreFile``, and otherwise use `next`. */
  private def isIgnoreFile(annotation: String, file: Path, lineNr: Int, next: () => Option[TestAnnotation] = () => None): Option[TestAnnotation] = {
    val regex = """^IgnoreFile\(/(.*)/issue/([0-9]+)/\)$""".r
    annotation match {
      case regex(project, issueNr) => Some(IgnoreFile(file, lineNr, project, issueNr.toInt))
      case _ => next()
    }
  }

  /** Try to parse the annotation a ``IgnoreFileList``, and otherwise use `next`. */
  private def isIgnoreFileList(annotation: String, file: Path, lineNr: Int, next: () => Option[TestAnnotation] = () => None): Option[TestAnnotation] = {
    val regex = """^IgnoreFileList\(/(.*)/issue/([0-9]+)/\)$""".r
    annotation match {
      case regex(project, issueNr) => Some(IgnoreFileList(file, lineNr, project, issueNr.toInt))
      case _ => next()
    }
  }

}
