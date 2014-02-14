package ch.ethz.inf.pm.sample.td.cost.loops

import ch.ethz.inf.pm.td.webapi.{ScriptQuery, ScriptRecord}
import ch.ethz.inf.pm.sample.SystemParameters

trait LoopFilter extends ScriptQuery {

  override def filter(s : ScriptRecord) : Boolean = {
    val compiler=new LoopCostCompiler()
    SystemParameters.setCompiler(compiler)
    compiler.compileFile(s.getCodeURL)
    super.filter(s) && compiler.loops
  }

  override def label = label + ",loops"

}