package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._;
import apron._;

import scala.collection.mutable.HashSet

object ConstriandPolyhedraApronRun {
	
	System.out.println("ConstriandPolyhedraApronRun");

	type HeapId = ClassHeapIdentifier;
	
	private val methods : List[String] = /*"ex1" :: */"ex2" :: /*"ex3" :: "ex4" :: */Nil;
	  
	def main(args : Array[String]) : Unit = {
		//Mandatory global settings
		SystemParameters.compiler = new ScalaCompiler;
		SystemParameters.property = new ApronProperty;
		
		//Files paths
		val f1 = "/home/samlik/ScalaWorkspace/NumericalAnalysis/test/numerical.scala";
		
		ch.ethz.inf.pm.sample.Main.compile(f1 :: Nil);
		
		//EntryState
		val domain=new PplPoly(false);
		System.out.println(domain.getLibrary + " " + domain.getVersion)
		val numerical = new ConstrainedPolyhedra(new Abstract1(domain, new Environment()), domain, new HashSet[Int], 10);
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