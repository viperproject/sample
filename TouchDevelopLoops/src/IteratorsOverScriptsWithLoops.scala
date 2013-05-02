package ch.ethz.inf.pm.sample.td.cost.loops;

import ch.ethz.inf.pm.td.webapi.{RootScripts, Scripts, ScriptRecord}
import ch.ethz.inf.pm.sample.SystemParameters

class RootScriptsWithLoops() extends RootScripts {
  override protected def filter(s : List[ScriptRecord]) : List[ScriptRecord]= {

    super.filter(s).filter( (t : ScriptRecord) => {
      val compiler=new LoopCostCompiler();
      SystemParameters.setCompiler(compiler);
      compiler.compileFile(t.getCodeURL);
      compiler.loops;
    }
    )
  }


  override def getLabel() = "TouchDevelop root scripts with loops"
}