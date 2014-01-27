package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.CFGState

/**
 * The check of a property
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait Property {

  	/**
	   * A short label to identify and display the property
	   */
  def getLabel() : String;

   	/**
	   * Check the property over the abstract results of a single method
	   *
	   * @param classe the class
	   * @param methodName the name of the method
	   * @param result the abstract result
	   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
	   */
  def check[S <: State[S]](classe : Type, methodName : MethodDeclaration, result: CFGState[S], printer : OutputCollector) : Unit;


  /**
   * Check the property over the abstract results of multiple method
   *
   * @param results a list of the results, consisting of class type, method declaration and cfg
   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
   */
  def check[S <: State[S]](results : List[(Type,MethodDeclaration, CFGState[S])], printer : OutputCollector):Unit = {
    for ((c,m,g) <- results) {
      check(c,m,g,printer)
    }
  }

   	/**
	   * The finalizing of the property after that all methods have been checked
	   *
	   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
	   */
  def finalizeChecking(printer : OutputCollector) : Unit;
}

/**
 * A visitor that checks the property over a single state
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait Visitor {

  	/**
	   * A short label to identify and display the property
	   */
  def getLabel() : String;

    /**
	   * Check the property over a single state
	   *
	   * @param state the abstract state
	   * @param statement the statement that was executed after the given abstract state
	   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
	   */
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit;
}

/**
 * The class that has to be used to check the property over single states
 *
 * @param visitor the visitor that will check the property over single abstract states
 * @author Pietro Ferrara
 * @version 0.1
 */
class SingleStatementProperty(val visitor : Visitor) extends Property {
  override def getLabel() : String = visitor.getLabel();
  override def check[S <: State[S]](className : Type, methodName : MethodDeclaration, result : CFGState[S], printer : OutputCollector) : Unit = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(methodName)) {
      for(i <- 0 to result.cfg.nodes.size-1)
        for(k <- 0 to result.cfg.nodes.apply(i).size-1) {
          val statement = result.cfg.nodes.apply(i).apply(k)
          val blockStates = result.statesOfBlock(i)
          if(k > blockStates.size-1)
            visitor.checkSingleStatement[S](
              blockStates.apply(blockStates.size-1).bottom(),
              statement,
              printer
            )
          else {
            val state = blockStates.apply(k)
            checkStatement(className, methodName, visitor, state, statement, printer)
          }
        }
    }
  }
  override def finalizeChecking(printer : OutputCollector) : Unit = Unit;
  private def checkStatement[S <: State[S]](className : Type, methodName : MethodDeclaration, visitor : Visitor, state : S, statement : Statement, printer : OutputCollector) : Unit = statement match {
        	  	case Assignment(programpoint, left, right) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		this.checkStatement(className, methodName, visitor, state, left, printer)
        	  		this.checkStatement(className, methodName, visitor, left.forwardSemantics[S](state), right, printer)
        	  	case VariableDeclaration(programpoint, variable, typ, right) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		if (right.isDefined) checkStatement(className, methodName, visitor, state, right.get, printer)
        	  	case Variable(programpoint, id) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  	case FieldAccess(pp, obj, field, typ) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
                checkStatement(className, methodName, visitor, state, obj, printer)
        	  	case MethodCall(pp, method, parametricTypes, parameters, returnedType) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		checkStatement(className, methodName, visitor, state, method, printer)
        	  		for(par <- parameters)
        	  		  checkStatement(className, methodName, visitor,state, par, printer)
        	  	case New(pp, typ) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  	case oorepresentation.ConstantStatement(pp, value, typ) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  	case Throw(programpoint, expr) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		checkStatement(className, methodName, visitor,state, expr, printer)
        	  	case x : ControlFlowGraph =>
        	  		val result=new ControlFlowGraphExecution[S](x, state).forwardSemantics(state);
        	  		this.check(className, methodName, result, printer);
              case _ => () // TODO
        	  }
  
}


class PropertyException(s : String) extends Exception(s)