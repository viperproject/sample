package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._

trait Lattice[T <: Lattice[T]] {
  def factory() : T;
  def top() : T
  def bottom() : T
  def lub(left : T, right : T) : T
  def glb(left : T, right : T) : T
  def widening(left : T, right : T) : T
  def lessEqual(r : T) : Boolean
}


trait State[S <: State[S]] extends Lattice[S] {
  def createObject(typ : Type, pp : ProgramPoint) : S
  def createObjectForParameter(typ : Type, pp : ProgramPoint, path : List[String]) : S
  def createVariableForParameter(x : SymbolicAbstractValue[S], typ : Type) : S
  def createVariable(x : SymbolicAbstractValue[S], typ : Type) : S
  def assignVariable(x : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S]) : S
  def setParameter(x : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S]) : S
  def setVariableToTop(x : SymbolicAbstractValue[S]) : S
  def removeVariable(x : SymbolicAbstractValue[S]) : S
  def throws(throwed : SymbolicAbstractValue[S]) : S
  def getVariableValue(id : Identifier) : S
  def getFieldValue(objs : List[SymbolicAbstractValue[S]], field : String, typ : Type) : S
  def backwardGetVariableValue(id : Identifier) : S
  def backwardGetFieldValue(objs : List[SymbolicAbstractValue[S]], field : String, typ : Type) : S
  def backwardAssignVariable(x : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S]) : S
  def evalNumericalConstant(value : String, typ : Type) : S
  def assert(cond : SymbolicAbstractValue[S]) : S
  def assume(cond : SymbolicAbstractValue[S]) : S
  def getExpression() : SymbolicAbstractValue[S]
  def setExpression(expr : SymbolicAbstractValue[S]) : S
  def removeExpression() : S
  def testTrue() : S
  def testFalse() : S
  def getType(variable : Identifier) : Type
}

object UtilitiesOnStates {
   
	def forwardExecuteStatement[S <: State[S]](state : S, statement : Statement) : (SymbolicAbstractValue[S], S)= {
	  val finalState : S =statement.forwardSemantics[S](state);
	  val expr=finalState.getExpression();
	  var resultingState=finalState.bottom()
	  for(exp <- expr.getExpressions())
		  resultingState=resultingState.lub(resultingState, expr.get(exp));
	  (expr, resultingState);
	}

 	def backwardExecuteStatement[S <: State[S]](state : S, statement : Statement) : (SymbolicAbstractValue[S], S)= {
	  val finalState : S =statement.backwardSemantics[S](state);
	  val expr=finalState.getExpression();
	  (expr, finalState.removeExpression());
	}
 
	def forwardExecuteListStatements[S <: State[S]](state : S, statements : List[Statement]) : (List[SymbolicAbstractValue[S]], S)= statements match {
	  case Nil => (Nil, state)
	  case statement :: xs =>
	  	val state1 : S =statement.forwardSemantics[S](state);
	  	val expr=state1.getExpression();
	  	val (otherExpr, finalState)= forwardExecuteListStatements[S](state1, xs)
	  	(expr :: otherExpr, finalState.removeExpression());
	}
 
	def backwardExecuteListStatements[S <: State[S]](state : S, statements : List[Statement]) : (List[SymbolicAbstractValue[S]], S)= statements match {
	  case Nil => (Nil, state)
	  case statement :: xs =>
	  	val state1 : S =statement.normalize().backwardSemantics[S](state);
	  	val expr=state1.getExpression();
	  	val (otherExpr, finalState)= backwardExecuteListStatements[S](state1, xs)
	  	(expr :: otherExpr, finalState);
	}
}