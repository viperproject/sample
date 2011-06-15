package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.userinterfaces._
import ch.ethz.inf.pm.sample.property._;
import apron._
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler;

class ApronProperty extends Property {
	override def getLabel() : String = "Show graph"
	
	override def check[S <: State[S]](className : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = ShowGraph.Show(result);
	  
	override def finalizeChecking(printer : OutputCollector) : Unit = Unit
	   
}

object ApronRun {
  type HeapId = ClassHeapIdentifier;
  
  println("ApronRun");

  private val methods : List[String] = "ex1" /*:: "ex2" :: "ex3" :: "ex4" */:: Nil;
  
  def main(args : Array[String]) : Unit = {
	//Mandatory global settings
	SystemParameters.compiler = new ScalaCompiler;
	SystemParameters.property = new ApronProperty;

	SystemParameters.analysisOutput = new ApronOutput();
	SystemParameters.progressOutput = new ApronOutput();
	
	//Files paths
	val f1 = "/home/samlik/IdeaProjects/Semper/NumericalAnalysis/test/ExamplesForConstrainedPolyhedra.scala";
	
	ch.ethz.inf.pm.sample.Main.compile(f1 :: Nil);
	
	//EntryState
	//val domain=new Polka(false);
	val domain=new PplPoly(false);
	val numerical = new ApronInterface(new Abstract1(domain, new Environment()), domain);
	val heapid = new ClassHeapIdentifier(null, null);
	heapid.typ=SystemParameters.typ;
	val heapDomain : NonRelationalHeapDomain[HeapId]= new NonRelationalHeapDomain[HeapId](heapid.getType, new MaybeHeapIdSetDomain(heapid), heapid);
	val entrydomain  = new HeapAndAnotherDomain[ApronInterface, NonRelationalHeapDomain[HeapId], HeapId](numerical, heapDomain);
	var entryvalue =new SymbolicAbstractValue[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapId]](None, Some(SystemParameters.typ.asInstanceOf[Type]))
	var entryState =new GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapId](entrydomain, entryvalue)
	entryvalue=new SymbolicAbstractValue[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapId]](Some(entryState), Some(SystemParameters.typ.asInstanceOf[Type]))
	entryState =new GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapId](entrydomain, entryvalue)
			      

	ch.ethz.inf.pm.sample.Main.analyze(_ match {case _ => methods.toSet}, entryState, new OutputCollector);
	
  }


}

class ApronOutput extends ScreenOutput {
	def getString(): String = {
		return "";
	}

	def appendString(s: String) = {
		//do nothing
	}
}
