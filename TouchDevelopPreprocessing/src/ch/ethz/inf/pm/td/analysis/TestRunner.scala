package ch.ethz.inf.pm.td.analysis

import java.io.{FileFilter, File}
import ch.ethz.inf.pm.td.webapi.{NoMoreScriptsException, ScriptQuery}
import ch.ethz.inf.pm.td.compiler.TouchException

/**
 ** Lucas Brutschy
 * Date: 9/18/12
 * Time: 11:03 AM
 *
 */
object TestRunner {

  def basePath:String = "Test"+File.separator+"test"+File.separator+"TouchDevelop"+File.separator

  def basePath(dir:String):String = basePath+dir+File.separator

  def apply(pubIds:List[String],func:(String => Unit)) {
    for (pubId <- pubIds) apply(pubId,func)
  }

  def apply(scr:ScriptQuery,num:Int,func:(String => Unit)) {
    for (script <- scr) {
      if (!script.haserrors) {
        val id = script.id
        apply(id,func)
      }
    }
  }

  def apply(id:String,func:(String=>Unit)) {
    try {
      func(id)
    } catch {
      case e:TouchException => println(e.msg + " (Position: " + e.pos + ")"); e.printStackTrace()
    }
  }

  def runDirectoryWithApron(dir:String) {
    TouchApronRun.main(new File(basePath(dir)).listFiles(
      new FileFilter {
        def accept(p1: File): Boolean = { p1.getName.matches(".*\\.td$") || p1.getName.matches(".*\\.json$") }
      }
    ).map(basePath(dir)+_.getName).toArray.sortWith((a,b) => a.compare(b) < 0))
  }

  def runFileWithApron(file:String) {
    TouchApronRun.main(Array(basePath+file))
  }

  def runIdWithApron(id:String) {
    TouchApronRun.main(Array("td://"+id))
  }



}

