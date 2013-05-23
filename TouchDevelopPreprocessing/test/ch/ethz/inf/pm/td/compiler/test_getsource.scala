import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.webapi._

def analyzer(id:String) {
  val comp = new TouchCompiler
  println(comp.getSourceCode("td://"+id))
}

TestRunner(new NewScripts,10000,analyzer _)
