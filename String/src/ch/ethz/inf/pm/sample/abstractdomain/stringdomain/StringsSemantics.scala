/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

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
     if(! thisExpr.typ.toString.equals("String")) return None
      operator match {
	  case "+" => 
	    Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, 
             returnedtype)));
     case "concat" => 
	    Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, 
             returnedtype)));
     case "substring" => 
	    Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, 
             returnedtype)));
     case "contains" => 
	    Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringContains, 
             returnedtype)));
     case "indexOf" => 
	    Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringIndexof, 
             returnedtype)));
     case "lastIndexOf" => 
	    Some(state.setExpression(ExpressionFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringLastindexof, 
             returnedtype)));
  	  case _ => None
	}
  }
  
  private def createBinaryArithmeticExpression[S <: State[S]]
       (state : S, thisExpr : ExpressionSet, parameters : List[ExpressionSet], 
        operator : ArithmeticOperator.Value, returnedtype : Type) : Some[S] = parameters match {
	    	case x :: Nil => Some(state.setExpression(ExpressionFactory.createBinaryExpression(thisExpr, x, operator, returnedtype)));
	    	case _ => Some(state.top())
  }

  private def extractExpression[S <: State[S]](s : ExpressionSet) : Option[Expression] = s.toSetOrFail match {
	  case x if x.size==1 => Some(x.head)
	  case _ => None;
  }
   
	def applyBackwardNativeSemantics[S <: State[S]](
		thisExpr : ExpressionSet, 
		operator : String, 
		parameters : List[ExpressionSet], 
		typeparameters : List[Type], 
		returnedtype : Type,
    programpoint : ProgramPoint,
		state : S) : Option[S] = throw new StringException("Backward analysis not implemented/existing :)")
}

class StringException(s : String) extends Exception(s)
