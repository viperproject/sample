import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property.OutputCollector
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.compiler.{TouchException, TouchCompiler}
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.{Scripts, URLFetcher, TopScripts}
import heapanalysis.{HeapEnv, VariableEnv, NonRelationalHeapDomain, NullProgramPointHeapIdentifier}
import java.io.File
import scala.List

/**
 * User: lucas
 * Date: 11/22/12
 * Time: 5:50 PM
 */

def analyzer(url:String) {

  val id = Scripts.pubIDfromURL(url)

  printToFile(new File("TouchDevelopPreprocessing"+File.separator+"testfiles"+File.separator+"top10"+File.separator+id+".td"))(p => {
    p.println(URLFetcher.fetchFile(url))
  })

}

def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
  val p = new java.io.PrintWriter(f)
  try { op(p) } finally { p.close() }
}

TestRunner(new TopScripts,10,analyzer _)