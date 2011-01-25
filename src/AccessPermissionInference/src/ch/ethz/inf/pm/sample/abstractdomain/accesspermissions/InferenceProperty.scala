package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions._
import ch.ethz.inf.pm.sample.oorepresentation._
//import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing.plugin._
import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._;
import java.io._

private object CollectedResults {
	  var constraints : Set[Constraint] = Set.empty[Constraint];
  	  var r : Map[(String, String), ControlFlowGraphExecution[ConstraintsInference.State]] = Map.empty;
}

class InferenceProperty extends Property {
	
	  override def check[S <: State[S]](className : String, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
		  CollectedResults.r=CollectedResults.r+(((className, methodName), result.asInstanceOf[ControlFlowGraphExecution[ConstraintsInference.State]]));
	 	  ShowGraph.Show(result);
		  ConstraintsInference.addPostconditionConstraints(result.exitState().asInstanceOf[ConstraintsInference.State]);
		  CollectedResults.constraints=CollectedResults.constraints.union(ConstraintsInference.getConstraints());
	  }
	  
	  override def finalizeChecking() : Unit = {
	    LPTimer.start();
	    val solution=ConstraintsInference.solve(CollectedResults.constraints);
	    ConstraintsInference.printConstraints(CollectedResults.constraints);
	    if(solution!=null) {
	      val loopInvariants=ConstraintsInference.giveLoopInvariants(CollectedResults.r.values.iterator, solution);
	      LPTimer.stop();
	      System.out.println("LOOP INVARIANTS\n--------------------\n"+loopInvariants.toString());
	    }
	    else LPTimer.stop();
	  }
	   
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