/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.util.{AccumulatingTimer, Timer}
import scala.util.DynamicVariable
import ch.ethz.inf.pm.sample.reporting.Reporter

/**
 * <code>SystemParameters</code> contains all the parameters of Sample
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object SystemParameters {

  /**
   * Enables costly debugging checks
   */
  val DEBUG = false

  /**
   * Enables costly timing flags
   */
  val TIME = true

  /**
   * Flag that informs whether the running analysis is ValueDrivenHeapAnalysis
   */
  var isValueDrivenHeapAnalysis = false

  /**
   * The number of iterations after which widening is applied
   */
  var wideningLimit: Int = 2

  /**
   * The semantics of methods defined by hand
   */
  var nativeMethodsSemantics: List[NativeMethodSemantics] = Nil

  /** Stores the currently analyzed method */
  val analysisUnitImpl: DynamicVariable[AnalysisUnitContext] = new DynamicVariable(null)
  def analysisUnitContext: AnalysisUnitContext = analysisUnitImpl.value
  def withAnalysisUnitContext[R](context: AnalysisUnitContext)(f: => R): R = {
    analysisUnitImpl.withValue(context)(f)
  }

  /** Stores the library boundary (the program point at which we called into a library) */
  val libraryBoundaryUnitImpl: DynamicVariable[ProgramPoint] = new DynamicVariable(null)
  def libraryBoundaryContext: ProgramPoint = libraryBoundaryUnitImpl.value
  def withLibraryBoundaryContext[R](context: ProgramPoint)(f: => R): R = {
    libraryBoundaryUnitImpl.withValue(context)(f)
  }

  /**
   * The output for the window that shows the progresses of the analysis
   */
  var progressOutput: ScreenOutput = null

  /**
   * The output for the window that shows the results of the analysis
   */
  var analysisOutput: ScreenOutput = null

  /**
   * An instance of the current type system
   */
  var typ: Type = null

  /**
   * The compiler used to compile the given files
   */
  var compiler: Compiler = null

  /**
   * The checked property
   */
  var property: Property = null


  def reset() = {
    progressOutput = null
    analysisOutput = null
    typ = null
    compiler = null
    property = null
    nativeMethodsSemantics = Nil
    Reporter.reset()
  }

  def addNativeMethodsSemantics(l: List[NativeMethodSemantics]) = {
    for (s1 <- l) {
      var already = false
      for (s2 <- nativeMethodsSemantics)
        if (s1 == s2) already = true
      if (!already)
        nativeMethodsSemantics = nativeMethodsSemantics ::: s1 :: Nil
    }
  }

  def resetNativeMethodsSemantics(): Unit = nativeMethodsSemantics = Nil

  def setProperty(p: Property) = property = p

  def setCompiler(c: Compiler) = compiler = c

  def setProgressOutput(p: ScreenOutput) = progressOutput = p

  def setAnalysisOutput(p: ScreenOutput) = analysisOutput = p

  def resetOutput(): Unit = {
    Reporter.enableAllOutputs()
    if (progressOutput != null) progressOutput.reset()
    if (analysisOutput != null) analysisOutput.reset()
    Reporter.reset()
  }

}

abstract class ScreenOutput {

  def appendString(s: String)

  def getString: String

  private var indent = 0

  def begin(s: String) {
    put("{ " + s)
    Timer.start
    indent += 1
  }

  //def put(s:String) { appendString("  "*indent + s.replaceAll("[\n\r]+","\n  "*indent)) }
  def put(s: String) {
    appendString("  " * indent + s)
  }

  def end(s: String) {
    indent -= 1
    put("} " + s + " (time: " + Timer.stop + ")")
  }

  def end() {
    end("")
  }

  def reset() {
    indent = 0
  }

}

class StringCollector extends ScreenOutput {

  var s: String = ""

  override def appendString(s: String) {
    this.s = this.s + "\n" + s
  }

  override def getString: String = {
    this.s
  }

}

class StdOutOutput extends ScreenOutput {

  override def getString: String = {
    ""
  }

  override def appendString(s: String) {
    println(s)
  }

}

class CombinedOutput(a: ScreenOutput, b: ScreenOutput) extends ScreenOutput {

  def getString: String = {
    a.getString + b.getString
  }

  def appendString(s: String) {
    a.appendString(s)
    b.appendString(s)
  }

}

class Timer {
  var lastValue: Option[Long] = None
  var totalTime: Long = 0

  def start() = lastValue = Some(System.currentTimeMillis())

  def stop() = lastValue match {
    case Some(l) => totalTime = totalTime + (System.currentTimeMillis() - l)
    case None => System.out.println("Timer not started before!");
  }

  def reset() = totalTime = 0

  lastValue = None
}