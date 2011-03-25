package ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis

import ch.ethz.inf.pm.sample._
//import AVPProject.ExampleAnalysis;


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.gui._


class ArrayProperty extends Property {
	
	  override def check[S <: State[S]](className : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = ShowGraph.Show(result);
	  
	  override def finalizeChecking() : Unit = Unit
	   
}

object ArrayAnalysis {
  
	

  type HeapId = HeapIdAndSetDomain[TopHeapIdentifier];
  type HeapDomain = NonRelationalHeapDomain[TopHeapIdentifier];
  type State = GenericAbstractState[AVP2010Analysis, HeapDomain, HeapId];
  type AbstractValue = SymbolicAbstractValue[State];
  type HeapAndAnother = HeapAndAnotherDomain[AVP2010Analysis, HeapDomain, HeapId];
  
  type ConditionsSingleMethod = Map[String, Map[FieldAccess, Int]]
  type Conditions = Map[String, ConditionsSingleMethod]
	

  private var methods : List[String] = Nil;//"ex1" /*:: "ex2" :: "ex3" :: "ex4" */:: Nil;
  
  def analyze(method : String, file : String, numericalDomain : AVP2010Analysis) : Unit = {
	  
	SystemParameters.nativeMethodsSemantics=SystemParameters.nativeMethodsSemantics ::: ArrayNativeMethodSemantics :: Nil;
  
	//Mandatory global settings
	SystemParameters.compiler = new ScalaCompiler;
	SystemParameters.property = new ArrayProperty;
	methods = method :: Nil;
	
	//Files paths
	val f1 = file;
	
	ch.ethz.inf.pm.sample.Main.compile(f1 :: Nil);
	
	val heapid = new SingleHeapIdentifier(null);
	heapid.typ=SystemParameters.typ.asInstanceOf[Type];
	val heapDomain : HeapDomain= new HeapDomain(heapid.getType, new HeapIdAndSetDomain[TopHeapIdentifier](heapid), heapid);
	val entrydomain  = new HeapAndAnother(numericalDomain, heapDomain);
	var entryvalue =new AbstractValue(None, Some(SystemParameters.typ.asInstanceOf[Type]))
	var entryState =new State(entrydomain, entryvalue)
	entryvalue=new AbstractValue(Some(entryState), Some(SystemParameters.typ.asInstanceOf[Type]))
	entryState =new State(entrydomain, entryvalue)
	
	ch.ethz.inf.pm.sample.Main.verbose=false; 
	ch.ethz.inf.pm.sample.Main.analyze(_ match {case _ => methods.toSet}, entryState);
  }
}
