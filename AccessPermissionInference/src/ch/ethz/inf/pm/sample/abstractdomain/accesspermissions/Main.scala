package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.scalalang._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._;

object Main {
  type Permissions = SymbolicPermissionsDomain[ProgramPointHeapIdentifier]
  type HeapId = HeapIdAndSetDomain[ProgramPointHeapIdentifier];
  type HeapDomain = NonRelationalHeapDomain[ProgramPointHeapIdentifier];
  type State = GenericAbstractState[Permissions, HeapDomain, HeapId];
  type AbstractValue = SymbolicAbstractValue[State];
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
	
	SystemParameters.nativeMethodsSemantics=SystemParameters.nativeMethodsSemantics ::: ChaliceNativeMethodSemantics :: Nil;
	
	
	//Mandatory global settings
	SystemParameters.compiler = new ScalaCompiler;
	SystemParameters.property = new InferenceProperty;
	
	//Files paths
	val f1 = "C:\\Users\\Pietro\\Sample\\AccessPermissionInference\\test\\Chalice\\Chalice.scala";
	val f2 = "C:\\Users\\Pietro\\Sample\\AccessPermissionInference\\test\\ChaliceExamples\\TestCases.scala";
	
	ch.ethz.inf.pm.sample.Main.compile(f1 :: f2 :: Nil);
	
	//EntryState
	val heapid : ProgramPointHeapIdentifier = new StaticProgramPointHeapIdentifier(SystemParameters.typ);
	val heapDomain : HeapDomain= new HeapDomain(heapid.getType, new HeapIdAndSetDomain(heapid), heapid);
	val domain : Permissions =new SymbolicPermissionsDomain();
	val entrydomain  = new HeapAndAnother(domain, heapDomain);
	var entryvalue =new AbstractValue(None, None)
	var entryState =new State(entrydomain, entryvalue)
	entryvalue =new AbstractValue(Some(entryState), Some(SystemParameters.typ))
	entryState =new State(entrydomain, entryvalue)

	ch.ethz.inf.pm.sample.Main.analyze(_ match {case _ => methods.toSet}, entryState);
  }
}
