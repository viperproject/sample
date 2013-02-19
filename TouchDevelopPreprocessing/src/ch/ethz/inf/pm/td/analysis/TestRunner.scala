package ch.ethz.inf.pm.td

import compiler.TouchException
import domain.{TouchApronRun, TouchRun}
import webapi.{Scripts, NoMoreScriptsException}
import java.io.{FileFilter, FilenameFilter, File}

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
    ) map (basePath(dir)+_.getName) toList)
  }

  def runFileWithApron(file:String) {
    TouchApronRun.main(List(basePath+file))
  }


}

