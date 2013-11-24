package ch.ethz.inf.pm.sample.td.cost.loops

import ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph


/*
     "Cost model" that defines the cost of basic statements (i.e., statements that are not a control structure)
 */
abstract class CostModel () {

   // the cost of all statements in the body of the loop at node 'nodeId' that are not a control structure themselves
   def loopCostExpression(cfg: ControlFlowGraph, nodeId: Int) : LinearExpression

  // the cost of all statements in the 'branch' branch of the conditional at node 'nodeId' that are not a control structure themselves
  def conditionalCostExpression(cfg: ControlFlowGraph, nodeId: Int, branch: Boolean) : LinearExpression

}


/*
     The "cost model" from section 3.3 in the thesis.
 */
class ParameterizedCostModel () extends CostModel {

  def loopCostExpression(cfg: ControlFlowGraph, nodeId: Int) : LinearExpression = {
    val parameter = new PubsVariable("c"+nodeId, true)
    LinearExpressionFactory.fromVariable(parameter)
  }

  def conditionalCostExpression(cfg: ControlFlowGraph, nodeId: Int, branch: Boolean) : LinearExpression = {
    val parameter = new PubsVariable(if (branch) "t"+nodeId else "f"+nodeId, true)
    LinearExpressionFactory.fromVariable(parameter)
  }

}