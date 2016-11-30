/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.cloud

import java.io._
import java.nio.file.Paths

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyBooleanType, DummyProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

object Z3Prover {

  def withZ3[A](f: Z3Prover => A): A = {
    val z3 = Z3Prover()
    try {
      val res = f(z3)
      z3.stop()
      res
    } catch {
      case t: Throwable =>
        z3.stop()
        throw t
    }
  }

  trait ExpressionConverter {

    def declare(expr: VariableIdentifier): String

    def convert(expr: Expression): String

    def fresh(str: String): String

    def freshVariableIdentifier(str: String, typ: Type): VariableIdentifier =
      VariableIdentifier(fresh(str))(typ, DummyProgramPoint)

  }

  sealed abstract class Result

  case class Version(str: String)

  case class InteractionFailed(str: String) extends Exception(str)

  case class Config(
      z3Exe: String = "z3",
      z3Args: Option[String] = None,
      z3Timeout: Int = 100,
      assertionMode: AssertionMode.Value = AssertionMode.PushPop,
      ideModeAdvanced: Boolean = false
  )

  case class Bookkeeper() {

    var assertionCounter = 0
    var assumptionCounter = 0

  }

  object Sat extends Result

  object Unsat extends Result

  object Unknown extends Result

  object AssertionMode extends Enumeration {
    val SoftConstraints, PushPop = Value
  }

}

import ch.ethz.inf.pm.td.cloud.Z3Prover._

case class Z3Prover(
    converter: Z3Prover.ExpressionConverter = Z3DefaultExpressionConverter(),
    config: Z3Prover.Config = Z3Prover.Config(),
    bookkeeper: Z3Prover.Bookkeeper = Z3Prover.Bookkeeper()
) extends LazyLogging {

  private val z3: Process = createZ3Instance()
  private val input: BufferedReader = new BufferedReader(new InputStreamReader(z3.getInputStream))
  private val output: PrintWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(z3.getOutputStream)), true)

  emitPreamble()

  private var pushPopScopeDepth = 0
  private var lastTimeout: Int = -1

  def z3Version(): Version = {
    val versionPattern = """\(?\s*:version\s+"(.*?)"\)?""".r
    var line = ""

    writeLine("(get-info :version)")

    line = input.readLine()

    line match {
      case versionPattern(v) => Version(v)
      case _ => throw InteractionFailed(s"Unexpected output of Z3 while getting version: $line")
    }
  }

  def stop() {
    this.synchronized {
      output.flush()
      input.close()
      output.close()
      z3.destroy()
    }
  }

  def push(n: Int = 1) {
    pushPopScopeDepth += n
    val cmd = (if (n == 1) "(push)" else "(push " + n + ")") + " ; " + pushPopScopeDepth
    writeLine(cmd)
    readSuccess()
  }

  def pop(n: Int = 1) {
    val cmd = (if (n == 1) "(pop)" else "(pop " + n + ")") + " ; " + pushPopScopeDepth
    pushPopScopeDepth -= n
    writeLine(cmd)
    readSuccess()
  }

  def assume(term: Expression): Any = {
    assume(converter.convert(term))
  }

  def assume(term: String) {
    bookkeeper.assumptionCounter += 1

    writeLine("(assert " + term + ")")
    readSuccess()
  }

  private def readSuccess() {
    val answer = readLine()

    if (answer != "success")
      throw InteractionFailed(s"Unexpected output of Z3. Expected 'success' but found: $answer")
  }

  private def readLine(): String = {
    var repeat = true
    var result = ""

    while (repeat) {
      result = input.readLine()
      if (result.toLowerCase != "success") logger.debug(result)

      val warning = result.startsWith("WARNING")
      if (warning) logger.debug(s"Z3: $result")

      repeat = warning
    }

    result
  }

  private def writeLine(out: String) = {
    logger.debug(out)
    output.println(out)
  }

  def assert(goal: Expression, timeout: Option[Int] = None): Boolean =
    assert(converter.convert(goal), timeout)

  def assert(goal: String, timeout: Option[Int]): Boolean = {
    bookkeeper.assertionCounter += 1

    setTimeout(timeout)

    val (result, duration) = config.assertionMode match {
      case AssertionMode.SoftConstraints => assertUsingSoftConstraints(goal)
      case AssertionMode.PushPop => assertUsingPushPop(goal)
    }

    logger.debug("Asserted " + goal + " in " + duration + " milliseconds")

    result
  }

  def check(timeout: Option[Int] = None): Result = {
    setTimeout(timeout)

    emit("(check-sat)")

    val res = readLine() match {
      case "sat" => Sat
      case "unsat" => Unsat
      case "unknown" => Unknown
    }

    res
  }

  def emit(content: String) {
    writeLine(content)
    readSuccess()
  }

  private def setTimeout(timeout: Option[Int]) {
    val effectiveTimeout = timeout.getOrElse(config.z3Timeout)

    /* [2015-07-27 Malte] Setting the timeout unnecessarily often seems to
     * worsen performance, if only a bit. For the current test suite of
     * 199 Silver files, the total verification time increased from 60s
     * to 70s if 'set-option' is emitted every time.
     */
    if (lastTimeout != effectiveTimeout) {
      lastTimeout = effectiveTimeout

      writeLine(s"(set-option :timeout $effectiveTimeout)")
      //readSuccess()
    }
  }

  def statistics(): Map[String, String] = {
    var repeat = true
    var line = ""
    var stats = scala.collection.immutable.SortedMap[String, String]()
    val entryPattern = """\(?\s*:([A-za-z\-]+)\s+((?:\d+\.)?\d+)\)?""".r

    writeLine("(get-info :all-statistics)")

    do {
      line = input.readLine()
      logger.debug(line)

      /* Check that the first line starts with "(:". */
      if (line.isEmpty && !line.startsWith("(:"))
        throw InteractionFailed(s"Unexpected output of Z3 while reading statistics: $line")

      line match {
        case entryPattern(entryName, entryNumber) =>
          stats = stats + (entryName -> entryNumber)
        case _ =>
      }

      repeat = !line.endsWith(")")
    } while (repeat)

    stats
  }

  def declareFresh(name: String, resultSort: Type): VariableIdentifier = {
    val id = converter.freshVariableIdentifier(name, resultSort)
    emit(converter.declare(id))
    id
  }

  def resetCounters() {
    resetAssertionCounter()
    resetAssumptionCounter()
  }

  def resetAssertionCounter() {
    bookkeeper.assertionCounter = 0
  }

  def resetAssumptionCounter() {
    bookkeeper.assumptionCounter = 0
  }

  private def createZ3Instance() = {
    logger.debug(s"Starting Z3 at $z3Path")

    val userProvidedZ3Args: Array[String] = config.z3Args match {
      case None =>
        Array()

      case Some(args) =>
        logger.debug(s"Additional command-line arguments are $args")
        args.split(' ').map(_.trim)
    }

    val builder = new ProcessBuilder(z3Path.toFile.getPath +: "-smt2" +: "-in" +: userProvidedZ3Args: _*)
    builder.redirectErrorStream(true)

    val process = builder.start()

    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run() {
        process.destroy()
      }
    })

    process
  }

  private def z3Path = Paths.get(config.z3Exe)

  private def assertUsingPushPop(goal: String): (Boolean, Long) = {
    push()

    writeLine("(assert (not " + goal + "))")
    readSuccess()

    val startTime = System.currentTimeMillis()
    writeLine("(check-sat)")
    val result = readUnsat()
    val endTime = System.currentTimeMillis()

    if (!result) {
      printModel()
    }

    pop()

    (result, endTime - startTime)
  }

  private def printModel(): Unit = {
    if (config.ideModeAdvanced) {
      writeLine("(get-model)")
      val model = readModel().trim()
      println(model + "\r\n")
    }
  }

  private def assertUsingSoftConstraints(goal: String): (Boolean, Long) = {
    val guard = declareFresh("grd", DummyBooleanType)

    writeLine(s"(assert (implies $guard (not $goal)))")
    readSuccess()

    val startTime = System.currentTimeMillis()
    writeLine(s"(check-sat $guard)")
    val result = readUnsat()
    val endTime = System.currentTimeMillis()

    if (!result) {
      printModel()
    }

    (result, endTime - startTime)
  }

  private def readUnsat(): Boolean = readLine() match {
    case "unsat" => true
    case "sat" => false
    case "unknown" => false

    case result =>
      throw InteractionFailed(s"Unexpected output of Z3 while trying to refute an assertion: $result")
  }

  private def readModel(): String = {
    try {
      var endFound = false
      var result = ""
      var firstTime = true
      while (!endFound) {
        val nextLine = input.readLine()
        if (nextLine.trim().endsWith("\"") || (firstTime && !nextLine.startsWith("\""))) {
          endFound = true
        }
        result = result + " " + nextLine
        firstTime = false
      }
      result
    } catch {
      case e: Exception =>
        println("Error reading model: " + e)
        ""
    }
  }

  private def emitPreamble(): Unit = {

    """
      |(set-option :print-success true) ; Boogie: false
      |(set-option :global-decls true) ; Boogie: default
      |(set-option :auto_config false) ; Usually a good idea
      |(set-option :smt.mbqi false)
      |(set-option :model.v2 true)
      |(set-option :smt.phase_selection 0)
      |(set-option :smt.restart_strategy 0)
      |(set-option :smt.restart_factor |1.5|)
      |(set-option :smt.arith.random_initial_value true)
      |(set-option :smt.case_split 3)
      |(set-option :smt.delay_units true)
      |(set-option :smt.delay_units_threshold 16)
      |(set-option :nnf.sk_hack true)
      |(set-option :smt.qi.eager_threshold 100)
      |(set-option :smt.qi.cost "(+ weight generation)")
      |(set-option :type_check true)
      |(set-option :smt.bv.reflect true)
      |
      |""".stripMargin.split("\n").filter(_.nonEmpty) foreach emit

  }

}