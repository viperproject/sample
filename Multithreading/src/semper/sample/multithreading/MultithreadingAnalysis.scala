package semper.sample.multithreading

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.{OutputCollector, DivisionByZero, SingleStatementProperty, Property}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.SystemParameters

class MultithreadingAnalysis[S <: SemanticDomain[S]] extends SemanticAnalysis[S] {
  var domain: Option[SemanticDomain[_]] = None ;

  def getLabel(): String = "Multithreading nonrelational analysis"

  def parameters(): List[(String, Any)] = List(("Domain", List("Interval", "Sign")))

  def setParameter(label: String, value: Any) = label match {
    case "Domain" => value match {
      case "Sign" => domain = Some(new BoxedNonRelationalNumericalDomain(new Sign(SignValues.T)))
      case "Interval" => domain = Some(new BoxedNonRelationalNumericalDomain(new Interval(0, 0)))
    }
  }

  def getInitialState(): S = domain.get.asInstanceOf[S]

  override def reset(): Unit = Unit

  def getProperties(): Set[Property] = Set(new InterferenceInferenceProperty(), new ApronProperty().asInstanceOf[Property], new SingleStatementProperty(DivisionByZero))

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil


  def fixpointComputation[D <: SemanticDomain[D], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](toAnalyze : List[String], initialState : D, output : OutputCollector, heap : H, typ : Type) = {
    val entrySemanticState = new FlowSensitivePartitioning[D]().add(new OtherThreads, initialState);
    val entryState : AbstractState[FlowSensitivePartitioning[D], H, I] =
      new AbstractState[FlowSensitivePartitioning[D], H, I](
        new HeapAndAnotherDomain[FlowSensitivePartitioning[D], H, I](entrySemanticState, heap),
        new ExpressionSet(typ)
      )


    SystemParameters.progressOutput.appendString("Start of the multithreading semantics")
    SystemParameters.progressOutput.appendString("Iteration 1")
    ch.ethz.inf.pm.sample.Main.analyze(toAnalyze, entryState, output)
    finalize(heap);
    var prev : Interferences[D] = ComputedInterference.value.asInstanceOf[Interferences[D]];
    SystemParameters.progressOutput.appendString("Iteration 2")
    ch.ethz.inf.pm.sample.Main.analyze(toAnalyze, entryState, output)
    finalize(heap);
    var next : Interferences[D]= ComputedInterference.value.asInstanceOf[Interferences[D]];
    //ComputedInterference.value=next;
    finalize();
    var i : Int = 3;
    while(! next.lessEqual(prev)) {
      SystemParameters.progressOutput.appendString("Iteration "+i)
      System.out.println("Iteration "+i)
      prev = if(i<=5) prev.lub(prev, next) else prev.widening(prev, next);
      ComputedInterference.value=prev.asInstanceOf[Interferences[Nothing]];
      ch.ethz.inf.pm.sample.Main.analyze(toAnalyze, entryState, output)
      finalize(heap);
      next = ComputedInterference.value.asInstanceOf[Interferences[D]];
      //ComputedInterference.value=null;
      i=i+1;
    }
    SystemParameters.progressOutput.appendString("End of the multithreading semantics")
  };
  private def finalize[N <: SemanticDomain[N], S <: State[S], I <: HeapIdentifier[I], H <: HeapDomain[H, I]](heap : HeapDomain[_, _]) = {
    for ((s, cfg) <-SystemParameters.property.asInstanceOf[InterferenceInferenceProperty].results)
      ComputedInterference.value=ComputedInterference.value.add(s, InterferenceInference.extractAssignedValue[N,S,H,I](cfg.asInstanceOf[ControlFlowGraphExecution[S]]).asInstanceOf[ProgramPointAssignment[Nothing]])
    SystemParameters.property.asInstanceOf[InterferenceInferenceProperty].results=Set.empty[(String, ControlFlowGraphExecution[_])];
    heap.reset();
  }

}
