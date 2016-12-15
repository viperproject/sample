/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import java.io.{File, FileWriter, IOException}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

/**
  * Custom logger with Mongo backend
  */
object Logger {

  val enableMongoLogging: Boolean = System.getProperty("ENABLE_MONGO_LOGGING", "false").toBoolean
  var curTestRun: String = System.getProperty("TESTRUN", System.nanoTime().toString)
  var curTmpDir = Files.createTempDirectory("logger")
  var tabs = 0

  def tempDir = curTmpDir

  def reset() {
    tabs = 0
    curTmpDir = Files.createTempDirectory("logger")
  }

  def setNewTempDir(str: String): String = {
    val oldTmpDir = curTmpDir
    curTmpDir = Paths.get(str)

    val dir = new java.io.File(str)
    if (dir.exists()) {
      removeRecursive(dir.toPath)
    }
    dir.mkdir()

    for (i <- 0 to 3) {
      new File(curTmpDir + File.separator + "logger" + i).delete()
    }

    oldTmpDir.toString
  }

  def removeRecursive(path: Path) {
    Files.walkFileTree(path, new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        if (exc == null) {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        } else {
          throw exc
        }
      }
    })

  }

  def setExistingTempDir(str: String) {
    curTmpDir = Paths.get(str)
  }

  def incIndent() {
    tabs += 1
  }

  def decIndent() {
    tabs -= 1
  }

  def log(str: String, ll: Int = 0) {
    val str2 = (" " * (tabs * 2)) + str.split("\n").mkString("\n" + (" " * (tabs * 2)))
    println(str2)
    for (i <- 0 to ll) {

      appendFile(Logger.curTmpDir + File.separator + "logger" + i, str2 + "\n")

      if (enableMongoLogging) {
        MongoExporter.log(curTestRun, "logger" + i, str2)
      }
    }
  }

  def appendFile(fileName: String, line: String) = {
    val fw = new FileWriter(fileName, true)
    try {
      fw.write(line)
    } finally fw.close()
  }

}