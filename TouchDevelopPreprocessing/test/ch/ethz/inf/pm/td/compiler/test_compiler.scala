import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.webapi.NewScripts

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:59 PM
 *
 */

SystemParameters.compiler = new TouchCompiler

def compiler(id:String) {
  println("== Compiling "+id)
  val cds = SystemParameters.compiler.compileFile("td://"+id)
//  for(cd <- cds; m <- cd.methods) {
//    ShowGraph.ShowControlFlowGraph(m.body)
//  }
}

def compileFromFile(path:String) {
  println("== Compiling "+path)
  val cds = SystemParameters.compiler.compileFile(path)
//  for(cd <- cds; m <- cd.methods) {
//    ShowGraph.ShowControlFlowGraph(m.body)
//  }
}


//compileFromFile("TouchDevelopPreprocessing"+File.separator+"testfiles"+File.separator+"waller"+File.separator+"waller.td")
TestRunner("xfgjqnqm",compiler _) // HUGE
TestRunner(new NewScripts,100000,compiler _)
//TestRunner("https://www.touchdevelop.com/api/qssc/text?original=true",compiler _)
//TestRunner("https://www.touchdevelop.com/api/fdzm/text",compiler _)

while(true) ()
