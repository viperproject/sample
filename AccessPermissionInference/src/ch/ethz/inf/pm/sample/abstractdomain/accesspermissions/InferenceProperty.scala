package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions._
import ch.ethz.inf.pm.sample.oorepresentation._
import sun.security.util.Resources_de
import javax.annotation.Resource
import collection.immutable.Map._

//import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing.plugin._
//import ch.ethz.inf.pm.sample.userinterfaces._
import ch.ethz.inf.pm.sample.property._;
import java.io._

private object CollectedResults {
	  var constraints : Set[Constraint] = Set.empty[Constraint];
  	  var r : Map[(String, String), ControlFlowGraphExecution[ConstraintsInference.State]] = Map.empty;
}


class InferenceProperty extends Property {


    def getLabel() : String = "Inference";
	
	  override def check[S <: State[S]](className : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
		  CollectedResults.r=CollectedResults.r+(((className.toString(), methodName), result.asInstanceOf[ControlFlowGraphExecution[ConstraintsInference.State]]));
		  ConstraintsInference.addPostconditionConstraints(result.exitState().asInstanceOf[ConstraintsInference.State], className, methodName);
		  CollectedResults.constraints=CollectedResults.constraints.union(ConstraintsInference.getConstraints());
	  }
	  
	  override def finalizeChecking(printer : OutputCollector) : Unit = {
      val res = ConstraintsInference.solve(CollectedResults.constraints);
	    if(res!=null) {
        val solution=res._1
        val epsilon=res._2
        for(s <- solution.keySet) {
          val perm : String =Settings.permissionType.permissionToString(ConstraintsInference.clean(solution.apply(s)), epsilon);
          s match {
            case SymbolicMonitorInvariant(c, p) => printer.add(new InferredContract(new Invariant(c, "acc("+s.path+", "+perm+")")));
            case SymbolicAbstractPredicates(c, name, p) => printer.add(new InferredContract(new Predicate(c, name, "acc("+s.path+", "+perm+")")))
            case SymbolicPreCondition(c, m, p) => printer.add(new InferredContract(new PreCondition(c, m, "acc("+s.path+", "+perm+")")))
            case SymbolicPostCondition(c, m, p) => printer.add(new InferredContract(new PostCondition(c, m, "acc("+s.path+", "+perm+")")))
            case Epsilon =>
          }
        }
        val loopInvariants=ConstraintsInference.giveLoopInvariants(CollectedResults.r.values.iterator, solution);
        for(pp <- loopInvariants.keySet) {
          val f : Map[Statement, Double] = loopInvariants.apply(pp);
          for(statement <- f.keySet) {
            val perm : String =Settings.permissionType.permissionToString(f.apply(statement), epsilon);
            printer.add(new InferredContract(new LoopInvariant(pp, "acc("+statement.toString+", "+perm+")")))
          }
        }
	    }
    }
	   
}