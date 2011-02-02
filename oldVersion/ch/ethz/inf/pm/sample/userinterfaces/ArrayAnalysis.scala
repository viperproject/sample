/*package ch.ethz.inf.pm.sample.userinterfaces

import ch.ethz.inf.pm.sample._
import AVPProject.ExampleAnalysis;

import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Settings
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing.plugin._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing

object ArrayAnalysis {

  type HeapId = HeapIdAndSetDomain[TopHeapIdentifier];
  type HeapDomain = NonRelationalHeapDomain[TopHeapIdentifier];
  type State = GenericAbstractState[AVP2010Analysis, HeapDomain, HeapId];
  type AbstractValue = SymbolicAbstractValue[State];
  type HeapAndAnother = HeapAndAnotherDomain[AVP2010Analysis, HeapDomain, HeapId];
  
  type ConditionsSingleMethod = Map[String, Map[FieldAccess, Int]]
  type Conditions = Map[String, ConditionsSingleMethod]
	
  def main(args : Array[String]) : Unit = {
    //ArrayAnalysis.analyze("AVP2010", "example1", "C:\\Users\\Pietro\\workspace\\Sample\\src\\Examples\\AVP2010.scala", new Interval(0, 0))
    //NonRelationalNumericalDomainAndHeapAnalysis.analyze("List2", "Numerical", "C:\\Users\\Pietro\\workspace\\Sample\\src\\Examples\\Temp.scalaprocessing", new Sign(SignValues.+), new ClassHeapIdentifier(null))
	  ArrayAnalysis.analyze("example4", "C:\\Users\\Pietro\\workspace\\Sample\\src\\Examples\\AVP2010.scala", new ExampleAnalysis());
  }
  def analyze(method : String, file : String, numericalDomain : AVP2010Analysis) : Unit = {

		SystemParameters.nativeMethodsSemantics=SystemParameters.nativeMethodsSemantics ::: ArrayNativeMethodSemantics :: Nil;
		
  	    SystemParameters.showgraph=true;
	    val settings = new Settings

        // WORK
	    val command = new CompilerCommand(List(file), settings) {
     
	      /** The command name that will be printed in in the usage message.
	       *  This is automatically set to the value of 'plugin.commandname' in the
	       *  file build.properties.
	       */
	      override val cmdName = "scala2cfg"
	    
	    }
	
	    if (!command.ok)
	      return()
	
	    /** The version number of this plugin is read from the properties file
	     */
	    if (settings.version.value) {
	      println(command.cmdName +" version 1.0")
	      return()
	    }
	    if (settings.help.value) {
	      println(command.usageMsg)
	      return()
	    }
	
	    val heapid = new SingleHeapIdentifier(null);
	    val runner = new PluginRunner(settings)
	    val run = new runner.Run
	    run.compile(command.files)
	    for(c <- SystemParameters.classes) {
	      if(c.name.toString().equals("AVP2010")) {
	        for(x <- c.methods)
	        	//if(x.name.toString().length() > 6 && x.name.toString().substring(0, 7).equals(method)) {
	        	if(x.name.toString().equals(method)) {
			    	heapid.typ=SystemParameters.scalaType.asInstanceOf[Type];
			    	val heapDomain : HeapDomain= new HeapDomain(heapid.getType, new HeapIdAndSetDomain[TopHeapIdentifier](heapid), heapid);
			     	val entrydomain  = new HeapAndAnother(numericalDomain, heapDomain);
				    var entryvalue =new AbstractValue(None, Some(SystemParameters.scalaType.asInstanceOf[Type]))
				    var entryState =new State(entrydomain, entryvalue)
				    entryvalue=new AbstractValue(Some(entryState), Some(SystemParameters.scalaType.asInstanceOf[Type]))
				    entryState =new State(entrydomain, entryvalue)
			      
				    ShowGraph.Show(x.asInstanceOf[MethodDeclaration].forwardSemantics[State](entryState));
	        	}
	      }
	    }
	    //System.out.println(SystemParameters.output.output()+"STATISTICS\n"+SystemParameters.output.statistics())
     }

}
*/