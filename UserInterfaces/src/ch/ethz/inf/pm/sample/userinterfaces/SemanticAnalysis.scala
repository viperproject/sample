/*package ch.ethz.inf.pm.sample.userinterfaces

import ch.ethz.inf.pm.sample._
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Settings
import ch.ethz.inf.pm.sample.abstractdomain._
//import ch.ethz.inf.pm.sample.oorepresentation._

object SemanticAnalysis {
  def main(args : Array[String]) : Unit = {
//        SemanticAnalysis.analyze("List2", "Insert", "C:\\Users\\Pietro\\workspace\\Sample\\src\\Examples\\Temp.scalaprocessing", new ContainedCharacters(), ContainedCharactersSemantics)
  }
  
  def analyze[S <: SemanticDomain[S]](classe : String, method : String, file : String, semantic : S, methodsemantics : NativeMethodSemantics) : Unit = {

     
	    SystemParameters.nativeMethodsSemantics=SystemParameters.nativeMethodsSemantics ::: methodsemantics :: Nil;
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
			    	val heapid : ClassHeapIdentifier=new ClassHeapIdentifier(null);
		   	    	heapid.typ=SystemParameters.scalaType.asInstanceOf[Type];
			    	val heapDomain : NonRelationalHeapDomain[ClassHeapIdentifier] = new NonRelationalHeapDomain[ClassHeapIdentifier](heapid.getType, new HeapIdAndSetDomain(heapid), heapid);
		
		      
		   	    	val entrydomain  = new HeapAndAnotherDomain[S, NonRelationalHeapDomain[ClassHeapIdentifier], HeapIdAndSetDomain[ClassHeapIdentifier]](semantic, heapDomain);
				    val entryvalue =new SymbolicAbstractValue[GenericAbstractState[S, NonRelationalHeapDomain[ClassHeapIdentifier], HeapIdAndSetDomain[ClassHeapIdentifier]]](None, None)
				    val entryState =new GenericAbstractState[S, NonRelationalHeapDomain[ClassHeapIdentifier], HeapIdAndSetDomain[ClassHeapIdentifier]](entrydomain, entryvalue)
		
			    	//val entrydomain  = new HeapAndAnotherDomain[S, ProgramPointHeapDomain, SetProgramPointHeapIdentifier](semantic, heapDomain);
				    //val entryvalue =new SymbolicAbstractValue[GenericAbstractState[S, ProgramPointHeapDomain, SetProgramPointHeapIdentifier]](None, None)
				    //val entryState =new GenericAbstractState[S, ProgramPointHeapDomain, SetProgramPointHeapIdentifier](entrydomain, entryvalue)
			        ShowGraph.Show(x.asInstanceOf[MethodDeclaration].forwardSemantics[GenericAbstractState[S, NonRelationalHeapDomain[ClassHeapIdentifier], HeapIdAndSetDomain[ClassHeapIdentifier]]](entryState));
	        	}
	      }
	    }
	    System.out.println(SystemParameters.output.output()+"STATISTICS\n"+SystemParameters.output.statistics())
     }
  
}
*/