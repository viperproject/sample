package ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis

import ch.ethz.inf.pm.sample._
//import AVPProject.ExampleAnalysis;


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.scalalang._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.userinterfaces._


class ArrayProperty extends Property {

    def getLabel() : String = "Show graph"
	
	  override def check[S <: State[S]](className : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = ShowGraph.Show(result);
	  
	  override def finalizeChecking(printer : OutputCollector) : Unit = Unit
	   
}

object ArrayAnalysis {
  
	

  type HeapId = TopHeapIdentifier;
  type HeapDomain = NonRelationalHeapDomain[TopHeapIdentifier];
  type State = AbstractState[AVP2010Analysis, HeapDomain, HeapId];
  type AbstractValue = ExpressionSet;
  type HeapAndAnother = HeapAndAnotherDomain[AVP2010Analysis, HeapDomain, HeapId];
  
  type ConditionsSingleMethod = Map[String, Map[FieldAccess, Int]]
  type Conditions = Map[String, ConditionsSingleMethod]
	

  private var methods : List[String] = Nil;//"ex1" /*:: "ex2" :: "ex3" :: "ex4" */:: Nil;
  
  def analyze(method : String, file : String, numericalDomain : AVP2010Analysis) : Unit = {

    SystemParameters.addNativeMethodsSemantics(ArrayNativeMethodSemantics :: Nil);

    //Mandatory global settings
    SystemParameters.compiler = new ScalaCompiler;
    SystemParameters.property = new ArrayProperty;
    methods = method :: Nil;

    //Files paths
    val f1 = file;

    SystemParameters.compiler.compile(f1 :: Nil);

    val heapid = new TopHeapIdentifier(null, null);
    heapid.typ=SystemParameters.typ.asInstanceOf[Type];
    val heapDomain : HeapDomain= new HeapDomain(heapid.getType, new MaybeHeapIdSetDomain[TopHeapIdentifier](), heapid);
    val entrydomain  = new HeapAndAnother(numericalDomain, heapDomain);
    var entryvalue =new AbstractValue(SystemParameters.typ.top())
    var entryState =new State(entrydomain, entryvalue)
    var analyzer = new SimpleAnalyzer("ArrayAnalysis")

    analyzer.analyze(methods, entryState, new OutputCollector);
  }
}