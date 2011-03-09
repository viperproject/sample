package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.util.Timer

object Main {
  	var classes : List[ClassDefinition]= Nil;
	def compile(files : List[String]) = {
	    for(f <- files) 
	    	classes=classes ::: SystemParameters.compiler.compileFile(f);
	}
	
    def analyze[S <: State[S]](toAnalyze : String => Set[String], entryState : S) : Unit = {
    	Timer.start;
	    var output = new OutputCollector();
	    for(c <- classes) {
	    	SystemParameters.currentClass = c.name.getThisType();
	     	val methods = toAnalyze.apply(c.name.toString());
	        for(x <- c.methods)
	        	if(methods.contains(x.name.toString()))
				    SystemParameters.property.check(c.name.getThisType(), x.name.toString(), x.asInstanceOf[MethodDeclaration].forwardSemantics[S](entryState), output);
	      }
	    SystemParameters.property.finalizeChecking();
	    System.out.println(SystemParameters.output.output()+"STATISTICS\n"+SystemParameters.output.statistics())
	    System.out.println("Time of analyisis: " + Timer.stop);
     }
    
}
