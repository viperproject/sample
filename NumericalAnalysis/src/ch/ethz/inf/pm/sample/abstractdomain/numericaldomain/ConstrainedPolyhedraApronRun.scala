package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._

import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler
import property.{Property, OutputCollector}
import apron._
;

object ConstrianedPolyhedraApronRun {
	
	System.out.println("ConstrainedPolyhedraApronRun");

	type HeapId = ClassHeapIdentifier;
	
	private val methods : List[String] = "ex1" /*:: "ex2" :: "ex3" :: "ex4"*/ :: Nil;
	  
	def main(args : Array[String]) : Unit = {
		//Mandatory global settings
		SystemParameters.compiler = new ScalaCompiler;
		SystemParameters.property = new ApronProperty().asInstanceOf[Property];
		
		//Files paths
		val f1 = "/home/samlik/IdeaProjects/Sample/NumericalAnalysis/test/ExamplesForConstrainedPolyhedra.scala";
		
		ch.ethz.inf.pm.sample.Main.compile(f1 :: Nil);
		
		//EntryState
		//val domain=new Polka(false);
		val domain=new Polka(false);
		System.out.println(domain.getLibrary + " " + domain.getVersion)
		
		var coefSet = Set.empty[Int];
		coefSet.+=(-1);
		coefSet.+=(1);
		
		val numerical = new ConstrainedPolyhedra(new Abstract1(domain, new Environment()), domain, coefSet, 2, Set.empty[String], Set.empty[Identifier]);
		//val numerical = new ConstrainedPolyhedra(new Abstract1(domain, new Environment()), domain, Set.empty[Int], 0, Set.empty[String]);
		val heapid = new ClassHeapIdentifier(null, null);
		heapid.typ=SystemParameters.typ;
		val heapDomain : NonRelationalHeapDomain[HeapId]= new NonRelationalHeapDomain[HeapId](heapid.getType, new MaybeHeapIdSetDomain(), heapid);
		val entrydomain  = new HeapAndAnotherDomain[ApronInterface, NonRelationalHeapDomain[HeapId], HeapId](numerical, heapDomain);
		var entryvalue =new ExpressionSet(SystemParameters.typ.top())
		var entryState =new AbstractState[ApronInterface, NonRelationalHeapDomain[HeapId], HeapId](entrydomain, entryvalue)
				      
	
		ch.ethz.inf.pm.sample.Main.analyze(_ match {case _ => methods.toSet}, entryState, new OutputCollector);
	  }
}