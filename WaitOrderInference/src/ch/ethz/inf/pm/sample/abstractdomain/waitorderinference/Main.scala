package ch.ethz.inf.pm.sample.abstractdomain.waitorderinference

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._;

object Main {
  type WaitOrder = WaitOrderDomain[ProgramPointHeapIdentifier]
  type HeapId = HeapIdAndSetDomain[ProgramPointHeapIdentifier];
  type HeapDomain = NonRelationalHeapDomain[ProgramPointHeapIdentifier];
  type State = GenericAbstractState[WaitOrder, HeapDomain, HeapId];
  type AbstractValue = SymbolicAbstractValue[State];
  type HeapAndAnother = HeapAndAnotherDomain[WaitOrder, HeapDomain, HeapId];

  private val methods : List[String] = "Try" :: "Inc" :: "main" :: "at" :: "size" :: "setLeft" :: "setRight" :: "shift" :: "getLeft" :: "getRight" :: "main2" :: "main3" :: "main4" :: Nil;

  def main(args : Array[String]) : Unit = {
	//Particular parameters of the analysis
	NonRelationalHeapDomainSettings.unsoundEntryState = true;
	NonRelationalHeapDomainSettings.maxInitialNodes = 10;

	//Mandatory global settings
	SystemParameters.compiler = new ScalaCompiler;
	SystemParameters.property = new WaitOrderInferenceProperty;
	
	//Files paths
	val f1 = "C:\\Users\\Pietro\\workspaceSample\\sample\\src\\Chalice\\Chalice.scala";
	val f2 = "C:\\Users\\Pietro\\workspaceSample\\sample\\src\\Examples\\Chalice2\\RunningExample.scala";
	
	ch.ethz.inf.pm.sample.Main.compile(f1 :: f2 :: Nil);
	
	//EntryState
	val heapid : ProgramPointHeapIdentifier = new StaticProgramPointHeapIdentifier(SystemParameters.typ);
	val heapDomain : HeapDomain= new HeapDomain(heapid.getType, new HeapIdAndSetDomain(heapid), heapid);
	val domain : WaitOrder =new WaitOrderDomain[ProgramPointHeapIdentifier]();
	val entrydomain  = new HeapAndAnother(domain, heapDomain);
	var entryvalue =new AbstractValue(None, None)
	var entryState =new State(entrydomain, entryvalue)
	entryvalue =new AbstractValue(Some(entryState), Some(SystemParameters.typ))
	entryState =new State(entrydomain, entryvalue)

	ch.ethz.inf.pm.sample.Main.analyze(_ match {case _ => methods.toSet}, entryState);
  }
}
