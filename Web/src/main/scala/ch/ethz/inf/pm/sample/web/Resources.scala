/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.web

import java.net.{URI, URL, URLEncoder}
import java.nio.file._

import scala.collection.JavaConversions._

case class IdentifierTest(name:String, id:String) extends Ordered[IdentifierTest] {

  def compare(that: IdentifierTest) =
    name.compareTo(that.name)

  def urlEncoded: String = URLEncoder.encode(id, "UTF-8")

}

/** Holds the path to a single test file that can be analyzed. */
case class TestFile(path: Path, prefix: String) extends Ordered[TestFile] {
  require(Files.isRegularFile(path), "path must be a regular file")
  require(Files.isReadable(path), "path must be a readable file")
  require(prefix != null, "prefix must not be null")

  override def toString =
    prefix + "/" + path.getFileName

  def compare(that: TestFile) =
    path.compareTo(that.path)
}

/** Provides a sequence of test files that can be analyzed. */
trait TestFileProvider {
  def testFiles: Seq[TestFile]
}

/** Provides a sequence of identifiers that can be analyzed. */
trait IdentifierProvider {
  def identifiers: Seq[IdentifierTest]
}

/** Finds all test files that are among the resources on the class path.
  * That is, test files may even be located inside of a JAR.
  *
  * @param testDirs the resource name prefixes to search recursively
  * @param namePattern the pattern that the file names must satisfy
  *
  * @todo much of the code has been copied from SIL's `ResourceBasedTestSuite`
  */
case class ResourceTestFileProvider(
    testDirs: Seq[String] = Seq(""),
    namePattern: String = ".*")
  extends TestFileProvider {

  lazy val testFiles: Seq[TestFile] = {
    testDirs.flatMap { testDir =>
      val resource = classLoader.getResource(testDir)
      assert(resource != null, s"Test directory $testDir couldn't be found")

      val path = pathFromResource(classLoader.getResource(testDir))
      testFilesRecursive(path, testDir)
    }.sorted
  }
  private var openFileSystems: Seq[FileSystem] = Seq()

  private def testFilesRecursive(dir: Path, prefix: String): Seq[TestFile] = {
    require(Files.isDirectory(dir), "Path must represent a directory")

    val directoryStream = Files.newDirectoryStream(dir)
    val dirContent = directoryStream.toList

    dirContent.collect({
      case f if Files.isDirectory(f) =>
        val newPrefix = prefix + "/" + f.getName(f.getNameCount - 1)
        testFilesRecursive(f, newPrefix)
      case f
        // If a file is renamed while Sbt runs, AccessDeniedExceptions
        // might be thrown. Apparently, because the old file still exists in
        // target/.../test-classes, but it is somehow locked. Weird stuff.
        // Once the SBT REPL is closed, the "ghost" file disappears.
        if Files.isReadable(f) && f.toString.matches(namePattern) =>
        Seq(TestFile(f, prefix))
    }).flatten
  }

  /**
   * Returns a class loader that can be used to access resources
   * such as test files via [[java.lang.ClassLoader]].
   *
   * @return A class loader for accessing resources.
   */
  private def classLoader: ClassLoader = getClass.getClassLoader
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
}