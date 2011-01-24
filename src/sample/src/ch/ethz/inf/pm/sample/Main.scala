package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._

object Main {
  def main(args : Array[String]) : Unit = {}
  
    def analyze[S <: State[S]](file : String, toAnalyze : Map[String, Set[String]], entryState : S) : Unit = {
	    var classes = SystemParameters.compiler.compileFile(file);
	    var output = new OutputCollector();
	    for(c <- classes)
	      if(toAnalyze.keySet.contains(c.name.toString())) {
	     	val methods = toAnalyze.apply(c.name.toString());
	        for(x <- c.methods)
	        	if(methods.contains(x.name.toString()))
				    SystemParameters.property.check(x.asInstanceOf[MethodDeclaration].forwardSemantics[S](entryState), output);
	        
	      }
	    System.out.println(SystemParameters.output.output()+"STATISTICS\n"+SystemParameters.output.statistics())
     }
}
