package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Constant, VariableIdentifier, State}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import ch.ethz.inf.pm.td.parser.VariableDefinition
import ch.ethz.inf.pm.td.analysis.MethodSummaries
import ch.ethz.inf.pm.td.semantics.{TString, RichNativeSemantics, TouchField, AAny}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

class CallableMethodDeclaration(
                                 programPoint_ : ProgramPoint,
                                 ownerType_ : Type,
                                 modifiers_ : List[Modifier],
                                 name_ : MethodIdentifier,
                                 parametricType_ : List[Type],
                                 arguments_ : List[List[VariableDeclaration]],
                                 returnType_ : Type,
                                 body_ : ControlFlowGraph,
                                 precondition_ : Statement,
                                 postcondition_ : Statement)
  extends MethodDeclaration( programPoint_, ownerType_, modifiers_, name_, parametricType_, arguments_, returnType_,
    body_, precondition_, postcondition_) {

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
                                 programPoint_ : ProgramPoint,
                                 ownerType_ : Type,
                                 modifiers_ : List[Modifier],
                                 name_ : MethodIdentifier,
                                 parametricType_ : List[Type],
                                 arguments_ : List[List[VariableDeclaration]],
                                 returnType_ : Type,
                                 body_ : ControlFlowGraph,
                                 precondition_ : Statement,
                                 postcondition_ : Statement)
  extends MethodDeclaration( programPoint_, ownerType_, modifiers_, name_, parametricType_, arguments_, returnType_,
    body_, precondition_, postcondition_) {

  override def forwardSemantics[S <: State[S]](state : S) : ControlFlowGraphExecution[S] = {

    MethodSummaries.reset()

    var curState = state

    // Global state is invalid
    for (v <- globalData) {

      val variable = VariableIdentifier(CFGGenerator.globalReferenceIdent(v.name.getName()),v.typ,programPoint_)
      val leftExpr = new ExpressionSet(v.typ).add(variable)
      curState = curState.createVariable(leftExpr,v.typ,programPoint_)

      // Numbers, Booleans and Strings are not initialized to invalid but to 0, false, ""
      val rightVal = v.typ.getName() match {
        case "String" =>
          curState = RichNativeSemantics.New[S](TString.typ)(curState,programPoint_)
          curState.getExpression().getSetOfExpressions.head
        case "Number" => Constant("0",v.typ,programPoint_)
        case "Boolean" => Constant("false",v.typ,programPoint_)
        case _ =>

          // There are three types of global data:
          //  (1) Regular global variables / objects, which are initialized to invalid
          //  (2) Global objects that are read-only and are initialized to some default object (Tile)
          //  (3) Global objects that represents read-only artwork that is initialized from some URL.
          if(v.modifiers.contains(ResourceModifier)) {
            curState = RichNativeSemantics.Top[S](v.typ.asInstanceOf[TouchType])(curState,programPoint_)
            curState.getExpression().getSetOfExpressions.head
          } else if (v.modifiers.contains(ReadOnlyModifier)) {
            curState = RichNativeSemantics.New[S](v.typ.asInstanceOf[TouchType])(curState,programPoint_)
            curState.getExpression().getSetOfExpressions.head
          } else {
            Constant("invalid",v.typ.asInstanceOf[TouchType],programPoint_)
          }

      }

      val rightExpr = new ExpressionSet(v.typ).add(rightVal)
      curState = curState.assignVariable(leftExpr,rightExpr)
    }

    // Initialize the fields of singletons (the environment)
    for (sem <- SystemParameters.compiler.asInstanceOf[TouchCompiler].getNativeMethodsSemantics()) {
      if(sem.isInstanceOf[AAny]) {
        val typ = sem.asInstanceOf[AAny].getTyp
        if(typ.isSingleton && SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.getName)) {
          val singletonProgramPoint = TouchSingletonProgramPoint(typ.getName)
          curState = RichNativeSemantics.Top[S](typ)(curState,singletonProgramPoint)
          val obj = curState.getExpression()
          val variable = new ExpressionSet(typ).add(VariableIdentifier(typ.getName,typ,programPoint_))
          curState = RichNativeSemantics.Assign[S](variable,obj)(curState,singletonProgramPoint)
        }
      }
    }

    val result = lfp(curState, {(lastExecution:ControlFlowGraphExecution[S], initialState:S) =>

      var curInitialState = initialState

      // TODO: Copy global state from last execution

      // Initialize in-parameters to top
      arguments.apply(0).foreach({
        x:VariableDeclaration =>
          curInitialState = x.forwardSemantics(curInitialState)
          curInitialState = Top[S](x.typ.asInstanceOf[TouchType])(curInitialState,x.programpoint)
          curInitialState = curInitialState.assignVariable(toExpressionSet(x.variable.id),curInitialState.getExpression())
      })

      // Initialize out-parameters to invalid
      arguments.apply(1).foreach({
        x:VariableDeclaration =>
          curInitialState = x.forwardSemantics(curInitialState)
          curInitialState = curInitialState.assignVariable(toExpressionSet(x.variable.id),Invalid(x.typ)(x.programpoint))
      })

      // Execute abstract semantics of the runnable method
      val execution = new ControlFlowGraphExecution[S](body, state).forwardSemantics(curInitialState)

      // Compute the fixpoint over all events
      lfp(execution.exitState(),{s:S =>
        var cur = s
        for (e <- events) {
          var entryState = cur
          val parameters = e.arguments.flatten.map( {
            x:VariableDeclaration =>
              entryState = Top[S](x.typ.asInstanceOf[TouchType])(entryState,x.programpoint)
              entryState.getExpression()
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

