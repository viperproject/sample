package semper.sample.multithreading

import ch.ethz.inf.pm.sample.property.{InferredContract, OutputCollector, Property}
import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.InferredContract
import ch.ethz.inf.pm.sample.oorepresentation.PreCondition
import ch.ethz.inf.pm.sample.oorepresentation.Predicate
import ch.ethz.inf.pm.sample.oorepresentation.Invariant
import ch.ethz.inf.pm.sample.oorepresentation.PostCondition
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph

/**
 * Created with IntelliJ IDEA.
 * User: Pietro
 * Date: 06/11/12
 * Time: 15.50
 * To change this template use File | Settings | File Templates.
 */
class InterferenceInferenceProperty extends Property {
  var results : Set[(String, ControlFlowGraphExecution[_])] = Set.empty[(String, ControlFlowGraphExecution[_])];

  def getLabel() : String = "Interference inference";

  override def check[S <: State[S]](className : Type, methodName : MethodDeclaration, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
    SystemParameters.currentMethod=methodName.name.toString;
    SystemParameters.currentCFG=result.cfg;
    results = results + ((methodName.name.toString, result));
    println("Method "+className.getName()+"."+methodName+"\nINTERFERENCES\n\n"+InterferenceInference.extractAssignedValue(result));
    SystemParameters.currentCFG=null;
    SystemParameters.currentMethod=null;
  }

  override def finalizeChecking(printer : OutputCollector) : Unit = {
    //for (r <- results)
    //  ShowGraph.Show(r._2);
  }


}
