package ch.ethz.inf.pm.sample.td.cost.loops;

import ch.ethz.inf.pm.td.webapi.{Scripts, ScriptRecord}

class RootScriptsWithLoops() extends Scripts {
  override protected def filter(s : List[ScriptRecord]) : List[ScriptRecord]=
    super.filter(s).filter( (t : ScriptRecord) => {
      val compiler=new LoopCostCompiler();
      compiler.compileFile(t.getCodeURL);
      compiler.loops;
    }
    )


  override def getLabel() = "TouchDevelop root scripts with loops"
}