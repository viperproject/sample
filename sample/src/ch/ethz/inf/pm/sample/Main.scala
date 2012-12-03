package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.util.Timer
import java.io.File

object Main {
	var verbose : Boolean = true;
  	var classes : List[ClassDefinition]= Nil;


  def reset() = classes=Nil;


  def compile(f : File) = classes=classes ::: SystemParameters.compiler.compileFile(f.getAbsolutePath());

  def compile(f : String) = classes=classes ::: SystemParameters.compiler.compileFile(f);

	def compile(files : List[String]) = {
	    for(f <- files) 
	    	classes=classes ::: SystemParameters.compiler.compileFile(f);
	}


  def analyze[S <: State[S]](toAnalyze : String, entryState : S, output : OutputCollector) : Unit =
      this.analyze( toAnalyze ::Nil, entryState, output);

  def analyze[S <: State[S]](toAnalyze : List[String], entryState : S, output : OutputCollector) : Unit = {
    Timer.start;
    for (methodName <- toAnalyze) {
      val methods = SystemParameters.compiler.getMethods(methodName)
      for((c,x) <- methods) {
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.appendString("Analyzing method "+x.name.toString()+" in class "+c.name.toString());
        SystemParameters.currentMethod = x.name.toString
        val s = x.forwardSemantics[S](entryState)
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.appendString("End of the analysis of method "+x.name.toString()+" in class "+c.name.toString());
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.appendString("Checking the property over method "+x.name.toString()+" in class "+c.name.toString());
        if(SystemParameters.property!=null) {
          SystemParameters.property.check(c.name.getThisType(), x.name.toString(), s, output)
        }
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.appendString("End of the check of the property over method "+x.name.toString()+" in class "+c.name.toString());
        SystemParameters.currentMethod = null
      }
    }
    if(SystemParameters.property!=null) {
      SystemParameters.progressOutput.appendString("Finalizing the checking the property")
      SystemParameters.property.finalizeChecking(output)
      SystemParameters.progressOutput.appendString("End of the checking of the property")
    }
    if(verbose) {
      System.out.println(output.output()+"STATISTICS [ Property validated:"+output.validated()+", Warnings:"+output.notvalidated()+", Inferred contracts:"+output.inferredcontracts()+", Time of analyisis: " + Timer.stop+" ]")
    }
  }
}
