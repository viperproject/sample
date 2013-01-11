package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Constant, VariableIdentifier, State}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import ch.ethz.inf.pm.td.parser.VariableDefinition
import ch.ethz.inf.pm.td.analysis.MethodSummaries
import ch.ethz.inf.pm.td.semantics.{TouchField, RichNativeSemantics, AAny}

class CallableMethodDeclaration(
                                 programpoint_ : ProgramPoint,
                                 ownerType_ : Type,
                                 modifiers_ : List[Modifier],
                                 name_ : MethodIdentifier,
                                 parametricType_ : List[Type],
                                 arguments_ : List[List[VariableDeclaration]],
                                 returnType_ : Type,
                                 body_ : ControlFlowGraph,
                                 precond_ : Statement,
                                 postcond_ : Statement)
  extends MethodDeclaration( programpoint_, ownerType_, modifiers_, name_, parametricType_, arguments_, returnType_, body_, precond_, postcond_) {

  override def forwardSemantics[S <: State[S]](state : S) : ControlFlowGraphExecution[S] = {
    SystemParameters.currentCFG=body
    SystemParameters.currentMethod=name.toString
    SystemParameters.semanticsComputing=true
    val r=new ControlFlowGraphExecution[S](body, state).forwardSemantics(state)
    SystemParameters.semanticsComputing=false
    SystemParameters.currentMethod=null
    SystemParameters.currentCFG=null
    r
  }
}



  /**
 *
 * A runnable method is a method that can be directly executed by the user. Usually this is the "main" method, but
 * in TouchDevelop, arbitrarily many such methods can be defined (all methods that are not defined "private" are
 * runnable).
 *
 * The execution model of running a "runnable method" is to
 *
 * (1) Initialize the global state to invalid
 * (2) Repeat:
 *    (2.1) Reset the local state
 *    (2.2) Run the method (interprocedurally)
 *    (2.3) Compute lfp (lambda x -> lub_e\in E(e(x))) where E is the set of events
 *
 */
class RunnableMethodDeclaration(
                                 events : Seq[MethodDeclaration],
                                 globalData : Seq[FieldDeclaration],
                                 programpoint_ : ProgramPoint,
                                 ownerType_ : Type,
                                 modifiers_ : List[Modifier],
                                 name_ : MethodIdentifier,
                                 parametricType_ : List[Type],
                                 arguments_ : List[List[VariableDeclaration]],
                                 returnType_ : Type,
                                 body_ : ControlFlowGraph,
                                 precond_ : Statement,
                                 postcond_ : Statement)
  extends MethodDeclaration( programpoint_, ownerType_, modifiers_, name_, parametricType_, arguments_, returnType_, body_, precond_, postcond_) {

  override def forwardSemantics[S <: State[S]](state : S) : ControlFlowGraphExecution[S] = {

    MethodSummaries.reset()

    var curState = state

    // Global state is invalid
    // TODO: Numbers, Booleans and Strings are not initialized to invalid but to 0, false, ""
    for (v <- globalData) {
      val variable = VariableIdentifier(CFGGenerator.globalReferenceIdent(v.name.getName()),v.typ,programpoint_)
      val leftExpr = new ExpressionSet(v.typ).add(variable)
      val rightExpr = new ExpressionSet(v.typ).add(Constant("invalid",v.typ,programpoint_))
      curState = curState.createVariable(leftExpr,v.typ,programpoint_)
      curState = curState.assignVariable(leftExpr,rightExpr)
    }

    // Initialize the fields of singletons
    for (sem <- SystemParameters.compiler.asInstanceOf[TouchCompiler].getNativeMethodsSemantics()) {
      val typ = sem.asInstanceOf[AAny].getTyp
      if(typ.isSingleton) {
        for (field <- typ.getPossibleFieldsSorted()) {
          // curState = curState.field.asInstanceOf[TouchField].default
          (); // TODO
        }
      }
    }

    val result = lfp(curState, {(lastExecution:ControlFlowGraphExecution[S], initialState:S) =>

      // TODO: Copy global state from last execution
      val execution = new ControlFlowGraphExecution[S](body, state).forwardSemantics(initialState)

      // Compute the fixpoint over all events
      lfp(execution.exitState(),{s:S =>
        var cur = s
        for (e <- events) {
          // TODO: We should come up with propert initializations here
          val parameters = e.arguments.flatten.map( {
            x:VariableDeclaration => new ExpressionSet(x.typ).add(new Constant("valid",x.typ,e.programpoint))
          })
          val newState = MethodSummaries.collect(e.programpoint,e,s,parameters)
          cur = cur.lub(cur,newState)
        }
        cur
      })

      execution

    })

    val resultWithSum = new ControlFlowGraphExecution[S](result.cfg,result.state) with Summaries[S]
    resultWithSum.nodes = result.nodes
    resultWithSum.edges = result.edges
    resultWithSum.summaries = MethodSummaries.getSummaries.asInstanceOf[Map[ProgramPoint,ControlFlowGraphExecution[S]]]

    resultWithSum
  }

  /**
   * Computes the least fix point for states
   */
  private def lfp[S <: State[S]](initialState:S,singleIteration:(S => S)):S = {

    var iteration = 1
    var prev = initialState
    var cur = prev.lub(prev,singleIteration(prev))
    while(!cur.lessEqual(prev)) {
      prev = cur
      iteration=iteration+1
      if(iteration > SystemParameters.wideningLimit) cur = prev.widening(prev,singleIteration(prev))
      else cur = prev.lub(prev,singleIteration(prev))
    }

    cur

  }

  /**
   * Computes the least fix point for executions
   */
  private def lfp[S <: State[S]](initialState:S,singleIteration:((ControlFlowGraphExecution[S],S) => ControlFlowGraphExecution[S])):ControlFlowGraphExecution[S] = {

    var iteration = 1
    var prev = new ControlFlowGraphExecution[S](body,initialState)
    var cur = prev.lub(singleIteration(prev,initialState))
    while(!cur.lessEqual(prev)) {
      prev = cur
      iteration=iteration+1
      if(iteration > SystemParameters.wideningLimit) cur=prev.widening(singleIteration(prev, initialState))
      else cur=prev.lub(singleIteration(prev, initialState))
    }

    cur

  }

}

