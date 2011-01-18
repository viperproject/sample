package ch.ethz.inf.pm.sample.userinterfaces

import apron._
import ch.ethz.inf.pm.sample._
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Settings
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing.plugin._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing

object ApronAndHeapAnalysis {
  def main(args : Array[String]) : Unit = {
    ApronAndHeapAnalysis.analyze("B", "Numerical", "C:\\Users\\Pietro\\workspace\\sample\\src\\Examples\\Temp.scala", new ApronInterface(new Box()), new ClassHeapIdentifier(null))
    //NonRelationalNumericalDomainAndHeapAnalysis.analyze("List2", "Numerical", "C:\\Users\\Pietro\\workspace\\Sample\\src\\Examples\\Temp.scalaprocessing", new Sign(SignValues.+), new ClassHeapIdentifier(null))
  }
  def analyze[I <: NonRelationalHeapIdentifier[I]](classe : String, method : String, file : String, numerical : ApronInterface, heapid : I) : Unit = {

     
  	    SystemParameters.showgraph=true;
	    val settings = new Settings

        // WORK
	    val command = new CompilerCommand(List(file), settings) {
     
	      /** The command name that will be printed in in the usage message.
	       *  This is autmatically set to the value of 'plugin.commandname' in the
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
	
	    val runner = new PluginRunner(settings)
	    val run = new runner.Run
	    run.compile(command.files)
	    for(c <- SystemParameters.classes) {
	      if(c.name.toString().equals(classe)) {
	        for(x <- c.methods)
	        	if(x.name.toString().equals(method)) {
			    	heapid.typ=SystemParameters.scalaType.asInstanceOf[Type];
			    	val heapDomain : NonRelationalHeapDomain[I]= new NonRelationalHeapDomain[I](heapid.getType, new HeapIdAndSetDomain(heapid), heapid);
			     	val entrydomain  = new HeapAndAnotherDomain[ApronInterface, NonRelationalHeapDomain[I], HeapIdAndSetDomain[I]](numerical, heapDomain);
				    var entryvalue =new SymbolicAbstractValue[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[I], HeapIdAndSetDomain[I]]](None, Some(SystemParameters.scalaType.asInstanceOf[Type]))
				    var entryState =new GenericAbstractState[ApronInterface, NonRelationalHeapDomain[I], HeapIdAndSetDomain[I]](entrydomain, entryvalue)
				    entryvalue=new SymbolicAbstractValue[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[I], HeapIdAndSetDomain[I]]](Some(entryState), Some(SystemParameters.scalaType.asInstanceOf[Type]))
				    entryState =new GenericAbstractState[ApronInterface, NonRelationalHeapDomain[I], HeapIdAndSetDomain[I]](entrydomain, entryvalue)
			      
				    ShowGraph.Show(x.asInstanceOf[MethodDeclaration].forwardSemantics[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[I], HeapIdAndSetDomain[I]]](entryState));
	        	}
	      }
	    }
	    System.out.println(SystemParameters.output.output()+"STATISTICS\n"+SystemParameters.output.statistics())
     }

}
