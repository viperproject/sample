package ch.ethz.inf.pm.sample.abstractdomain.waitorderinference;

import ch.ethz.inf.pm.sample._

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._;
import java.io._


	//(s, n, true/false) represents that the share s should be above/below n
class SingleConstraint(val s : SymbolicOrderValue, val n : Node, val above : Boolean) extends Tuple3(s, n, above){
	override def toString() = 
		if(above)
			s.toString()+" above "+n;
		else s.toString()+" below "+n;
			
}

object WaitOrderInferenceVisitor extends Visitor {
	type I = ProgramPointHeapIdentifier
	type W = WaitOrderDomain[I]
	
	var result : Set[SingleConstraint] = Set.empty;
	
   private def extractInfo[S <: State[S]](state : S, methodCall : MethodCall) : (SymbolicAbstractValue[S], String, List[Statement]) = {
	  val body : Statement = methodCall.method.normalize();
	  //Method call used to represent a goto statement to a while label
      if(body.isInstanceOf[Variable] && body.asInstanceOf[Variable].getName().length>=5 && body.asInstanceOf[Variable].getName().substring(0, 5).equals("while")) 
        throw new Exception("This should not appear here!");
      
	  val castedStatement : FieldAccess = body.asInstanceOf[FieldAccess]
      val calledMethod : String = castedStatement.field
      val parameters : List[Statement] = methodCall.parameters
      val typeparameters : List[Type] = methodCall.parametricTypes
      val returnedtype : Type = methodCall.returnedType
      val objs = castedStatement.objs;
	  objs match {
	 	  case obj :: Nil =>
			  val (calledExpr, resultingState) = UtilitiesOnStates.forwardExecuteStatement[S](state, obj);
			  (calledExpr, calledMethod, parameters) 
	 	  case _ => throw new WaitOrderInferenceException("Not yet supported");
	  }
  }
   
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit = statement match {
	  case s : MethodCall => extractInfo(state, s) match {
	 	  case (thisExpr, "acquire", x :: Nil) if (thisExpr.getType().getName.equals("Chalice")) =>
	 	    val castedState=state.asInstanceOf[GenericAbstractState[W, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]
	 	    val (parameter, resultingState) = UtilitiesOnStates.forwardExecuteStatement[S](state, x);
	 	    if(parameter.getExpressions().size != 1) throw new WaitOrderInferenceException("Not yet supported");
	 	    val id = parameter.getExpressions().iterator.next();
	 	    if(! id.isInstanceOf[VariableIdentifier]) throw new WaitOrderInferenceException("This should not happen");
	 	    val heapid = castedState._1._2.get(id.asInstanceOf[VariableIdentifier]); 
	 	    val path = id.asInstanceOf[VariableIdentifier].getName() :: Nil; //TODO: Find out the path!!!
	 	    val node = new AbstractObject(heapid, new Path(path));
	 	    val localMaxlock = InitialLockLevel(SystemParameters.currentClass.getName(), SystemParameters.currentMethod);
	 	    val newVal = castedState._1._1._1.get((localMaxlock, node))
	 	    if(newVal.isTop || newVal.isBottom) throw new WaitOrderInferenceException("This should not happen");
	 	    for(v <- newVal.value)
	 	    	result=result+(new SingleConstraint(v, localMaxlock, true)) //TODO: Or swapped?
	 	    	
	 	  case (thisExpr, "acquire", _) if (thisExpr.getType().getName.equals("Chalice")) =>
	 	   throw new WaitOrderInferenceException("This should not happen");
	 	  case _ => Unit;
	  }
	  case _ => Unit;
  }
}

class WaitOrderInferenceProperty extends SingleStatementProperty(WaitOrderInferenceVisitor) {
	
	  override def check[S <: State[S]](className : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
	 	  ShowGraph.Show(result);
	 	  super.check(className, methodName, result, printer);
	  }
	  
	  override def finalizeChecking() : Unit = System.out.println(WaitOrderInferenceVisitor.result.toString());
	   
}


private object LPTimer {
	var lastValue : Option[Long] = None
	var totalTime : Long = 0;
  	
 	def start() = lastValue=Some(System.currentTimeMillis())
  
 	def stop() = lastValue match {
 	  case Some(l) => totalTime=totalTime+(System.currentTimeMillis()-l)
 	  case None => System.out.println("Timer not started before!");
    }
 	
 	def reset() = totalTime=0; lastValue=None;
}