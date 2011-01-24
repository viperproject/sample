package ch.ethz.inf.pm.sample.userinterfaces

import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Settings
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing.plugin._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._;
import java.io._

object AccessPermissionsInference {
  type Permissions = SymbolicPermissionsDomain[ProgramPointHeapIdentifier]
  type HeapId = HeapIdAndSetDomain[ProgramPointHeapIdentifier];
  type HeapDomain = NonRelationalHeapDomain[ProgramPointHeapIdentifier];
  type State = GenericAbstractState[Permissions, HeapDomain, HeapId];
  type AbstractValue = SymbolicAbstractValue[State];
  type HeapAndAnother = HeapAndAnotherDomain[Permissions, HeapDomain, HeapId];
  
  type ConditionsSingleMethod = Map[String, Map[FieldAccess, Int]]
  type Conditions = Map[String, ConditionsSingleMethod]
  
  def main(args : Array[String]) : Unit = {
    //AccessPermissionsInference.analyze("C:\\Users\\Pietro\\workspace\\Sample\\src\\Examples\\Temp.scala")
    //AccessPermissionsInference.apply("C:\\Users\\Pietro\\Desktop\\ScalaImplementation\\ScalaAbstractInterpreter\\src\\Examples\\ChaliceExamples.scala", "C100", "Inc")
    AccessPermissionsInference.analyze("C:\\Users\\Pietro\\workspaceSample\\sample\\src\\Chalice\\Chalice.scala")
    //AccessPermissionsInference.analyze("C:\\Users\\Pietro\\workspace\\sample\\src\\Examples\\Temp.scala")
    AccessPermissionsInference.analyze("C:\\Users\\Pietro\\workspaceSample\\sample\\src\\Examples\\Chalice2\\RunningExample.scala")
    //AccessPermissionsInference.analyze("C:\\Users\\Pietro\\Desktop\\ScalaImplementation\\ScalaAbstractInterpreter\\src\\Examples\\Chalice2\\AssociationList.scala")
    System.out.println("\nTime of the analysis: "+AnalysisTimer.totalTime+" msec\nTime of LP: "+LPTimer.totalTime+" msec")
  }
  
  	private val methods : List[String] = "Try" :: "Inc" :: "main" :: "at" :: "size" :: "setLeft" :: "setRight" :: "shift" :: "getLeft" :: "getRight" :: "main2" :: "main3" :: "main4" :: Nil;
  	
  private def analyze(file : String) {
	
	Settings.unsoundInhaling = true;
	Settings.unsoundDischarging = true;
	Settings.priorityContracts = 1;
	Settings.priorityInvariants = 2;
	Settings.priorityPredicates = 3;
	NonRelationalHeapDomainSettings.unsoundEntryState = true;
	NonRelationalHeapDomainSettings.maxInitialNodes = 10;
	  
    this.compile(file)
    System.out.println("Inferring monitor invariants\n---------------------------------");
    this.inferInvariants();
    //System.out.println("Inferring predicates\n---------------------------------");
    //this.inferPredicates();
    //System.out.println("Inferring pre- and post- conditions\n---------------------------------");
    //this.inferPrePostConditions();
    //System.out.println("Inferring loop invariants\n---------------------------------");
    //this.inferLoopInvariants();
    //System.out.println("Inferred annotation:\n")
    //Annotation.printAnnotation();
  }
  
  private def getAllMethods() : Set[(String, String)] = {
  	var toAnalyze : Set[(String, String)] = Set.empty; 
  	for(classe <- SystemParameters.classes)
     for(method <- classe.methods) {
    	toAnalyze=toAnalyze+((classe.name.toString(), method.name.toString()));
    }
  	toAnalyze;
   }
  
  private def inferInvariants() {
	//var monitorInvariants : Map[String, Map[FieldAccess, FractionalLevelPermission]] = Map.empty;
  	var analyses : List[ControlFlowGraphExecution[State]] = this.analyze(this.getAllMethods()).values.toList;
    //Annotation.monitorInvariants=AnnotationGuess.guessInvariants(analyses);
  }
  def compile(file : String) : Unit = {
     
  	    SystemParameters.showgraph=true;
	    val settings = new Settings
     
	    System.out.println("Analyzing file "+file);

        // WORK
	    val command = new CompilerCommand(List(file), settings) {
     
	      /** The command name that will be printed in in the usage message.
	       *  This is automatically set to the value of 'plugin.commandname' in the
	       *  file build.properties.
	       */
	      override val cmdName = "scala2cfg"
	    
	    }
	
	    if (!command.ok)
	      return null
	
	    /** The version number of this plugin is read from the properties file
	     */
	    if (settings.version.value) {
	      println(command.cmdName +" version 1.0")
	      return null
	    }
	    if (settings.help.value) {
	      println(command.usageMsg)
	      return null
	    }
	
	    val runner = new PluginRunner(settings)
	    val run = new runner.Run
	    run.compile(command.files)	    
     }

  	def analyze(toBeAnalyzed : Set[(String, String)]) : Map[(String, String), ControlFlowGraphExecution[State]] = {

	    SystemParameters.nativeMethodsSemantics=SystemParameters.nativeMethodsSemantics ::: ChaliceNativeMethodSemantics :: Nil;
  	  
  	  	var result : Map[(String, String), ControlFlowGraphExecution[State]] = Map.empty;
	    var constraints : Set[Constraint] = Set.empty[Constraint];
	    for(c <- SystemParameters.classes) {
	      val className : String=c.name.toString;
	      for(m <- c.methods) {
	        val methodName : String=m.name.toString;
	        if(toBeAnalyzed.contains((className, methodName))) {
				    if(! excludeMethods(className, methodName)/*methodName.equals("Try") || methodName.equals("Inc") || methodName.equals("Get") || methodName.equals("Add")*/) 
				    {
				    	ParameterIds.n=0;
				    	System.out.println("Method "+methodName);
				    	SystemParameters.analyzedMethods=SystemParameters.analyzedMethods+1;
				    	SystemParameters.currentClass = className;
				    	SystemParameters.currentMethod = methodName;
			      
				    	val heapid : ProgramPointHeapIdentifier = new StaticProgramPointHeapIdentifier(SystemParameters.scalaType.asInstanceOf[Type]);
			      	    val heapDomain : HeapDomain= new HeapDomain(heapid.getType, new HeapIdAndSetDomain(heapid), heapid);
			            
				    	val domain : Permissions =new SymbolicPermissionsDomain();
				    	val entrydomain  = new HeapAndAnother(domain, heapDomain);
					    var entryvalue =new AbstractValue(None, None)
					    var entryState =new State(entrydomain, entryvalue)
					    entryvalue =new AbstractValue(Some(entryState), Some(SystemParameters.scalaType.asInstanceOf[Type]))
					    entryState =new State(entrydomain, entryvalue)
			      
					    val exitHeapDomain : HeapDomain = new HeapDomain(heapid.getType, new HeapIdAndSetDomain(heapid), heapid);
			                 
					    val exitDomain1 : Permissions=new SymbolicPermissionsDomain();
				    	val exitDomain  = new HeapAndAnother(exitDomain1, exitHeapDomain);
					    var exitValue =new AbstractValue(None, None)
					    var exitState =new State(exitDomain, exitValue)
					    exitValue =new AbstractValue(Some(exitState), Some(SystemParameters.scalaType.asInstanceOf[Type]))
					    exitState =new State(exitDomain, exitValue)		    
		
			      	    AnalysisTimer.start();
					    ConstraintsInference.emptyConstraints();
					
					    val res=m.forwardSemantics[State](entryState)
					    ConstraintsInference.addPostconditionConstraints(res.exitState());
					    constraints=constraints.union(ConstraintsInference.getConstraints());
		
					    AnalysisTimer.stop();
		
					    result=result+(((className, methodName), res));
					
						ShowGraph.Show(res);
			    }
			 }
	      }
        }
	    LPTimer.start();
	    val solution=ConstraintsInference.solve(constraints);
	    ConstraintsInference.printConstraints(constraints);
	    if(solution!=null) {
	      val loopInvariants=ConstraintsInference.giveLoopInvariants(result.values.iterator, solution);
	      LPTimer.stop();
	      System.out.println("LOOP INVARIANTS\n--------------------\n"+loopInvariants.toString());
	    }
	    else LPTimer.stop();
	    return result;
  	}
  	
  	
  	private def excludeMethods(className : String, name : String) : Boolean =
  		className.equals("Chalice") ||  
  		! methods.contains(name)
  		//(name.length>=2 && name.substring(name.length-2, name.length).equals("_=")) ||
  		//name.equals("$tag") ||
  		//name.equals("this")
}

private object LPTimer {
	var lastValue : Option[Long] = None
	var totalTime : Long = 0;
  	
 	def start() = lastValue=Some(System.currentTimeMillis())
  
 	def stop() = lastValue match {
 	  case Some(l) => totalTime=totalTime+(System.currentTimeMillis()-l)
 	  case None => System.out.println("Timer not started before!");
    }
 	
 	def reset() = totalTime=0; lastValue=None;
}