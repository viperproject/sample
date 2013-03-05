package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._;
import ch.ethz.inf.pm.sample.userinterfaces._;

class ShowGraphAndContractsProperty extends Property {


    def getLabel() : String = "Show graphs and inferred contracts";
	
	  override def check[S <: State[S]](className : Type, methodName : MethodDeclaration, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
		  CollectedResults.r=CollectedResults.r+(((className.toString(), methodName.name.toString), result.asInstanceOf[ControlFlowGraphExecution[ConstraintsInference.State]]));
      ShowGraph.check(className,methodName,result,printer)
		  ConstraintsInference.addPostconditionConstraints(result.exitState().asInstanceOf[ConstraintsInference.State], className, methodName.name.toString);
		  CollectedResults.constraints=CollectedResults.constraints.union(ConstraintsInference.getConstraints());
	  }
	  
	  override def finalizeChecking(printer : OutputCollector) : Unit = {
	    LPTimer.start();
      val res = ConstraintsInference.solve(CollectedResults.constraints);
	    if(res!=null) {
        val solution=res._1
        val epsilon=res._2
        ConstraintsInference.printConstraints(CollectedResults.constraints);
        val loopInvariants=ConstraintsInference.giveLoopInvariants(CollectedResults.r.values.iterator, solution);
        ConstraintsInference.printLoopInvariants(loopInvariants, epsilon);
        LPTimer.stop();
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
 	  case None => SystemParameters.analysisOutput.put("Timer not started before!");
    }
 	
 	def reset() = totalTime=0; lastValue=None;
}