package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._

object Main {
  	var classes : List[ClassDefinition]= Nil;
	def compile(files : List[String]) = {
	    for(f <- files) 
	    	classes=classes ::: SystemParameters.compiler.compileFile(f);
	}
	
    def analyze[S <: State[S]](toAnalyze : String => Set[String], entryState : S) : Unit = {
	    var output = new OutputCollector();
	    for(c <- classes) {
	     	val methods = toAnalyze.apply(c.name.toString());
	        for(x <- c.methods)
	        	if(methods.contains(x.name.toString()))
				    SystemParameters.property.check(c.name.toString(), x.name.toString(), x.asInstanceOf[MethodDeclaration].forwardSemantics[S](entryState), output);
	        
	      }
	    SystemParameters.property.finalizeChecking();
	    System.out.println(SystemParameters.output.output()+"STATISTICS\n"+SystemParameters.output.statistics())
     }
    
}
