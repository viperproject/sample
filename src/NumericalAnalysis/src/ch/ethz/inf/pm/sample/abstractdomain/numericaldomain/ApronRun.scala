package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._;
import apron._;

class ApronProperty extends Property {
	
	  override def check[S <: State[S]](className : String, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = ShowGraph.Show(result);
	  
	  override def finalizeChecking() : Unit = Unit
	   
}

object ApronRun {
  type HeapId = ClassHeapIdentifier;

  private val methods : List[String] = "Numerical" :: Nil;
  
  def main(args : Array[String]) : Unit = {
	//Mandatory global settings
	SystemParameters.compiler = new ScalaCompiler;
	SystemParameters.property = new ApronProperty;
	
	//Files paths
	val f1 = "/root/workspace/sample/src/src/Examples/Temp.scala";
	
	ch.ethz.inf.pm.sample.Main.compile(f1 :: Nil);
	
	//EntryState
	val domain=new Box();
	val numerical = new ApronInterface(new Abstract1(domain, new Environment()), domain);
	val heapid = new ClassHeapIdentifier(null);
	heapid.typ=SystemParameters.typ;
	val heapDomain : NonRelationalHeapDomain[HeapId]= new NonRelationalHeapDomain[HeapId](heapid.getType, new HeapIdAndSetDomain(heapid), heapid);
	val entrydomain  = new HeapAndAnotherDomain[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]](numerical, heapDomain);
	var entryvalue =new SymbolicAbstractValue[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]]](None, Some(SystemParameters.typ.asInstanceOf[Type]))
	var entryState =new GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]](entrydomain, entryvalue)
	entryvalue=new SymbolicAbstractValue[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]]](Some(entryState), Some(SystemParameters.typ.asInstanceOf[Type]))
	entryState =new GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]](entrydomain, entryvalue)
			      

	ch.ethz.inf.pm.sample.Main.analyze(_ match {case _ => methods.toSet}, entryState);
  }
}
