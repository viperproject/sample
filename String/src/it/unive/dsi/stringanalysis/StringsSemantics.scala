package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

object StringSemantics extends NativeMethodSemantics {
  	def applyForwardNativeSemantics[S <: State[S]](
		thisExpr : SymbolicAbstractValue[S], 
		operator : String, 
		parameters : List[SymbolicAbstractValue[S]], 
		typeparameters : List[Type], 
		returnedtype : Type,
    programpoint : ProgramPoint,
		state : S
	) : Option[S] = {
     if(! thisExpr.getType().toString().equals("String")) return None;
     operator match {
	  case "+" => 
	    return Some(state.setExpression(thisExpr.createAbstractOperator(thisExpr, 
             parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, 
             state, returnedtype)));
     case "concat" => 
	    return Some(state.setExpression(thisExpr.createAbstractOperator(thisExpr, 
             parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, 
             state, returnedtype)));
     case "substring" => 
	    return Some(state.setExpression(thisExpr.createAbstractOperator(thisExpr, 
             parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, 
             state, returnedtype)));     
     case "contains" => 
	    return Some(state.setExpression(thisExpr.createAbstractOperator(thisExpr, 
             parameters, typeparameters, AbstractOperatorIdentifiers.stringContains, 
             state, returnedtype)));     
     case "indexOf" => 
	    return Some(state.setExpression(thisExpr.createAbstractOperator(thisExpr, 
             parameters, typeparameters, AbstractOperatorIdentifiers.stringIndexof, 
             state, returnedtype))); 
     case "lastIndexOf" => 
	    return Some(state.setExpression(thisExpr.createAbstractOperator(thisExpr, 
             parameters, typeparameters, AbstractOperatorIdentifiers.stringLastindexof, 
             state, returnedtype))); 
  	  case _ => None
	}
  }
  
  private def createBinaryArithmeticExpression[S <: State[S]]
       (state : S, thisExpr : SymbolicAbstractValue[S], parameters : List[SymbolicAbstractValue[S]], 
        operator : ArithmeticOperator.Value, returnedtype : Type) : Some[S] = parameters match {
	    	case x :: Nil => new Some(state.setExpression(thisExpr.createBinaryExpression(thisExpr, x, operator, state, returnedtype)));
	    	case _ => new Some(state.top())
  }

  private def extractExpression[S <: State[S]](s : SymbolicAbstractValue[S]) : Option[Expression] = s.getExpressions() match {
	  case x if x.size==1 => return Some(x.elements.next)
	  case _ => return None;
  }
   
	def applyBackwardNativeSemantics[S <: State[S]](
		thisExpr : SymbolicAbstractValue[S], 
		operator : String, 
		parameters : List[SymbolicAbstractValue[S]], 
		typeparameters : List[Type], 
		returnedtype : Type,
    programpoint : ProgramPoint,
		state : S
	) : Option[S] = throw new StringException("Backward analysis not implemented/existing :)");
}

class StringException(s : String) extends Exception(s)
