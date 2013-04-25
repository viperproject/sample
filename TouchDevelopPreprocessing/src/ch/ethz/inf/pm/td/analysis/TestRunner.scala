package ch.ethz.inf.pm.td.analysis

import java.io.{FileFilter, File}
import ch.ethz.inf.pm.td.webapi.{NoMoreScriptsException, Scripts}
import ch.ethz.inf.pm.td.compiler.TouchException
import ch.ethz.inf.pm.td.domain.{TouchApronRun, TouchRun}

/**
 *
 * Lucas Brutschy
 * Date: 9/18/12
 * Time: 11:03 AM
 *
 */
object TestRunner {

  def basePath:String = "TouchDevelopPreprocessing"+File.separator+"testfiles"+File.separator

  def basePath(dir:String):String = basePath+dir+File.separator

  def apply(pubIds:List[String],func:(String => Unit)) {
    for (pubId <- pubIds) apply(pubId,func)
  }

  def apply(scr:Scripts,num:Int,func:(String => Unit)) {
    try {
      for (i <- 1 to num) {
        val script = scr.get()
        if (!script.haserrors) {
          val id = script.id
          apply(id,func)
        }
      }
    } catch {
      case e:NoMoreScriptsException => println("end of script list.")
    }
    println("done.")
  }

  def apply(id:String,func:(String=>Unit)) {
    try {
      func(id)
    } catch {
      case e:TouchException => println(e.msg + " (Position: " + e.pos + ")"); e.printStackTrace()
    }
  }

  def runDirectory(dir:String) {
    TouchRun.main(new File(basePath(dir)).listFiles(
      new FileFilter {
        def accept(p1: File): Boolean = { p1.getName.matches(".*\\.td$") }
      }
    ) map (basePath(dir)+_.getName) toList)
  }

  def runFile(file:String) {
    TouchRun.main(List(basePath+file))
  }

  def runDirectoryWithApron(dir:String) {
    TouchApronRun.main(new File(basePath(dir)).listFiles(
      new FileFilter {
        def accept(p1: File): Boolean = { p1.getName.matches(".*\\.td$") }
      }
    ).map(basePath(dir)+_.getName).toList.sortWith((a,b) => a.compare(b) < 0))
  }

  def runFileWithApron(file:String) {
    TouchApronRun.main(List(basePath+file))
  }

  def runId(id:String) {
    TouchRun.main(List("td://"+id))
  }

  def runIdWithApron(id:String) {
    TouchApronRun.main(List("td://"+id))
  }



}

