package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

object StringSemantics extends NativeMethodSemantics {
  	def applyForwardNativeSemantics[S <: State[S]](
		thisExpr : ExpressionSet, 
		operator : String, 
		parameters : List[ExpressionSet], 
		typeparameters : List[Type], 
		returnedtype : Type,
    programpoint : ProgramPoint,
		state : S
	) : Option[S] = {
     if(! thisExpr.getType().toString().equals("String")) return None;
     operator match {
	  case "+" => 
	    return Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, 
             returnedtype)));
     case "concat" => 
	    return Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, 
             returnedtype)));
     case "substring" => 
	    return Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, 
             returnedtype)));
     case "contains" => 
	    return Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringContains, 
             returnedtype)));
     case "indexOf" => 
	    return Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringIndexof, 
             returnedtype)));
     case "lastIndexOf" => 
	    return Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringLastindexof, 
             returnedtype)));
  	  case _ => None
	}
  }
  
  private def createBinaryArithmeticExpression[S <: State[S]]
       (state : S, thisExpr : ExpressionSet, parameters : List[ExpressionSet], 
        operator : ArithmeticOperator.Value, returnedtype : Type) : Some[S] = parameters match {
	    	case x :: Nil => new Some(state.setExpression(ExpressionFactory.createBinaryExpression(thisExpr, x, operator, returnedtype)));
	    	case _ => new Some(state.top())
  }

  private def extractExpression[S <: State[S]](s : ExpressionSet) : Option[Expression] = s.setOfExpressions match {
	  case x if x.size==1 => return Some(x.elements.next)
	  case _ => return None;
  }
   
	def applyBackwardNativeSemantics[S <: State[S]](
		thisExpr : ExpressionSet, 
		operator : String, 
		parameters : List[ExpressionSet], 
		typeparameters : List[Type], 
		returnedtype : Type,
    programpoint : ProgramPoint,
		state : S
	) : Option[S] = throw new StringException("Backward analysis not implemented/existing :)");
}

class StringException(s : String) extends Exception(s)
