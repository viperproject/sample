package ch.ethz.inf.pm.sample.abstractdomain.waitorderinference;

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._;
import java.io._

object WaitOrderInferenceVisitor extends Visitor {
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit = Unit
}

class WaitOrderInferenceProperty extends SingleStatementProperty(WaitOrderInferenceVisitor) {
	
	  override def check[S <: State[S]](className : String, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
	 	  ShowGraph.Show(result);
	 	  super.check(className, methodName, result, printer);
	  }
	  
	  override def finalizeChecking() : Unit = Unit;
	   
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