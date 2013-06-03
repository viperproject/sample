package ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis

import ch.ethz.inf.pm.sample._


import abstractdomain._
import abstractdomain.heapanalysis._
import abstractdomain.numericaldomain._
import abstractdomain.arrayanalysis._
import oorepresentation._
import oorepresentation.scalalang._
import property._
import userinterfaces._


class ArrayProperty extends Property {

    def getLabel() : String = "Show graph"
	
	  override def check[S <: State[S]](className : Type, methodName : MethodDeclaration, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = ShowGraph.Show(result);
	  
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

    val heapid = new TopHeapIdentifier(SystemParameters.typ.asInstanceOf[Type], null);
    val heapDomain : HeapDomain= new HeapDomain(heapid.getType, new MaybeHeapIdSetDomain[TopHeapIdentifier](), heapid);
    val entrydomain  = new HeapAndAnother(numericalDomain, heapDomain);
    var entryvalue =new AbstractValue(SystemParameters.typ.top())
    var entryState =new State(entrydomain, entryvalue)
    var analyzer = new SimpleAnalyzer("ArrayAnalysis")

    analyzer.analyze(methods, entryState, new OutputCollector);
  }
}