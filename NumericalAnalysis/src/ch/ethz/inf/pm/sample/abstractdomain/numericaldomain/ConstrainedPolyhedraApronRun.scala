package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._

import apron._
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler
import property.OutputCollector
;

object ConstrianedPolyhedraApronRun {
	
	System.out.println("ConstrainedPolyhedraApronRun");

	type HeapId = ClassHeapIdentifier;
	
	private val methods : List[String] = "ex1" /*:: "ex2" :: "ex3" :: "ex4"*/ :: Nil;
	  
	def main(args : Array[String]) : Unit = {
		//Mandatory global settings
		SystemParameters.compiler = new ScalaCompiler;
		SystemParameters.property = new ApronProperty;
		
		//Files paths
		val f1 = "/home/samlik/IdeaProjects/Semper/NumericalAnalysis/test/ExamplesForConstrainedPolyhedra.scala";
		
		ch.ethz.inf.pm.sample.Main.compile(f1 :: Nil);
		
		//EntryState
		//val domain=new Polka(false);
		val domain=new PplPoly(false);
		System.out.println(domain.getLibrary + " " + domain.getVersion)
		
		var coefSet = Set.empty[Int];
		coefSet.+=(-1);
		coefSet.+=(1);
		
		val numerical = new ConstrainedPolyhedra(new Abstract1(domain, new Environment()), domain, coefSet, 2, Set.empty[String]);
		//val numerical = new ConstrainedPolyhedra(new Abstract1(domain, new Environment()), domain, Set.empty[Int], 0, Set.empty[String]);
		val heapid = new ClassHeapIdentifier(null, null);
		heapid.typ=SystemParameters.typ;
		val heapDomain : NonRelationalHeapDomain[HeapId]= new NonRelationalHeapDomain[HeapId](heapid.getType, new HeapIdAndSetDomain(heapid), heapid);
		val entrydomain  = new HeapAndAnotherDomain[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]](numerical, heapDomain);
		var entryvalue =new SymbolicAbstractValue[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]]](None, Some(SystemParameters.typ.asInstanceOf[Type]))
		var entryState =new GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]](entrydomain, entryvalue)
		entryvalue=new SymbolicAbstractValue[GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]]](Some(entryState), Some(SystemParameters.typ.asInstanceOf[Type]))
		entryState =new GenericAbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapIdAndSetDomain[HeapId]](entrydomain, entryvalue)
				      
	
		ch.ethz.inf.pm.sample.Main.analyze(_ match {case _ => methods.toSet}, entryState, new OutputCollector);
	  }
}