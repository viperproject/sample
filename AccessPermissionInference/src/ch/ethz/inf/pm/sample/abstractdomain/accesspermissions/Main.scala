package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions

import ch.ethz.inf.pm.sample._
import abstractdomain._
import abstractdomain.heapanalysis._
import abstractdomain.accesspermissions._
import oorepresentation._
import oorepresentation.scalalang._
import property._;

object Main {
  type Permissions = SymbolicPermissionsDomain[ProgramPointHeapIdentifier]
  type HeapId = ProgramPointHeapIdentifier;
  type HeapDomain = NonRelationalHeapDomain[ProgramPointHeapIdentifier];
  type State = AbstractState[Permissions, HeapDomain, HeapId];
  type AbstractValue = ExpressionSet;
  type HeapAndAnother = HeapAndAnotherDomain[Permissions, HeapDomain, HeapId];

  private val methods : List[String] = "get" :: "set" :: "cuncurrentSet" :: "twoParallelSets" :: "Try" :: "Inc" :: "main" :: "at" :: "size" :: "setLeft" :: "setRight" :: "shift" :: "getLeft" :: "getRight" :: "main2" :: "main3" :: "main4" :: Nil;

  def main(args : Array[String]) : Unit = {
	//Particular parameters of the analysis
	Settings.unsoundInhaling = true;
	Settings.unsoundDischarging = true;
	Settings.priorityContracts = 2;
	Settings.priorityInvariants = 1;
	Settings.priorityPredicates = 3;
	Settings.permissionType = FractionalPermissions;
	NonRelationalHeapDomainSettings.unsoundEntryState = true;
	NonRelationalHeapDomainSettings.maxInitialNodes = 10;
	
	SystemParameters.addNativeMethodsSemantics(ChaliceNativeMethodSemantics :: Nil);
	
	
	//Mandatory global settings
	SystemParameters.compiler = new ScalaCompiler;
	SystemParameters.property = new InferenceProperty;

	
	//Files paths
	val f1 = "C:\\Users\\Pietro\\Sample\\AccessPermissionInference\\test\\Chalice\\Chalice.scala";
	val f2 = "C:\\Users\\Pietro\\Sample\\AccessPermissionInference\\test\\ChaliceExamples\\TestCases.scala";

  SystemParameters.compiler.compile(f1 :: f2 :: Nil);
	
	//EntryState
	val heapid : ProgramPointHeapIdentifier = new StaticProgramPointHeapIdentifier(SystemParameters.typ, null);
	val heapDomain : HeapDomain= new HeapDomain(heapid.getType, new MaybeHeapIdSetDomain(), heapid);
	val domain : Permissions =new SymbolicPermissionsDomain();
	val entrydomain  = new HeapAndAnother(domain, heapDomain);
	var entryvalue =new AbstractValue(SystemParameters.typ.top())
	var entryState =new State(entrydomain, entryvalue)
  var analyzer = new SimpleAnalyzer("AccessPermissionInference")
  analyzer.analyze(methods, entryState, new OutputCollector);
  }
}
