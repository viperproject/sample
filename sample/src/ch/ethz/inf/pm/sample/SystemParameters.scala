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
   * Enables costly debugging flags
   */
  val DEBUG = true

  /**
   * Enables costly debugging flags
   */
  val TIME = true

  /**
   * Flag that informs whether the running analysis is ValueDrivenHeapAnalysis
   */
  var isValueDrivenHeapAnalysis = false

  /**
  The number of iterations after whom widening is applied
    */
  var wideningLimit: Int = 3

  /**
  The semantics of methods defined by hand
    */
  var nativeMethodsSemantics: List[NativeMethodSemantics] = Nil


  val analysisUnitImpl: DynamicVariable[AnalysisUnitContext] = new DynamicVariable(null)

  def analysisUnitContext: AnalysisUnitContext = analysisUnitImpl.value

  def withAnalysisUnitContext[R](context: AnalysisUnitContext)(f: => R): R = {
    analysisUnitImpl.withValue(context)(f)
  }

  //TODO:Remove it
  var semanticsComputing: Boolean = false

  /**
   * Ir true Sample supposes that if we invoke numerical methods like + on an object of any type we are
   * performing arithmetical operations
   */
  var ignoreTypeForNumericalMethods: Boolean = false

  /**
  The output for the window that shows the progresses of the analysis
    */
  var progressOutput: ScreenOutput = null
  /**
  The output for the window that shows the results of the analysis
    */
  var analysisOutput: ScreenOutput = null
  /**
  The timer that collects the amount of time spent by the heap abstraction
    */
  val heapTimer: Timer = new Timer
  /**
  The timer that collects the amount of time spent by the semantic analysis
    */
  val domainTimer: Timer = new Timer
  /**
  The timer that collects the amount of time spent by the property checker
    */
  val propertyTimer: Timer = new Timer
  /**
  The timer that collects the amount of time spent by the compiler
    */
  val compilerTimer: Timer = new Timer
  /**
  An instance of the current type system
    */
  var typ: Type = null
  /**
  The compiler used to compile the given files
    */
  var compiler: Compiler = null
  /**
  The checked property
    */
  var property: Property = null

  def getType() = typ

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
    AccumulatingTimer.reset
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