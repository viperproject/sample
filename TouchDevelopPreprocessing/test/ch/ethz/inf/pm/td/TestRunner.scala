package ch.ethz.inf.pm.td

import compiler.TouchException
import compiler.TouchException
import webapi.{Scripts, NoMoreScriptsException, NewScripts}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.TopHeapIdentifier
import ch.ethz.inf.pm.sample.SystemParameters
import heapanalysis.TopHeapIdentifier
import numericaldomain.NonRelationalNumericalAnalysis
import scala.Some
import ch.ethz.inf.pm.sample.property.OutputCollector

/**
 *
 * Lucas Brutschy
 * Date: 9/18/12
 * Time: 11:03 AM
 *
 */
object TestRunner {

  def apply(urls:List[String],func:(String => Unit)) {
    for (url <- urls) apply(url,func)
  }

  def apply(scr:Scripts,num:Int,func:(String => Unit)) {
    try {
      for (i <- 1 to num) {
        val script = scr.get()
        if (!script.haserrors) {
          val url = script.getCodeURL
          apply(url,func)
        }
      }
    } catch {
      case e:NoMoreScriptsException => println("end of script list.")
    }
    println("done.")
  }

  def apply(url:String,func:(String=>Unit)) {
    try {
      func(url)
    } catch {
      case e:TouchException => println(e.msg + " (Position: " + e.pos + ")"); e.printStackTrace()
    }
  }

}

