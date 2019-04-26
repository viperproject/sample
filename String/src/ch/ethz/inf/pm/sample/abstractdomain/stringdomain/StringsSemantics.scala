/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
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
      Some(state.setExpression(ExpressionSetFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, 
             returnedtype)));
     case "concat" =>
       Some(state.setExpression(ExpressionSetFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, 
             returnedtype)));
     case "substring" =>
       Some(state.setExpression(ExpressionSetFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, 
             returnedtype)));
     case "contains" =>
       Some(state.setExpression(ExpressionSetFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringContains, 
             returnedtype)));
     case "indexOf" =>
       Some(state.setExpression(ExpressionSetFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringIndexof, 
             returnedtype)));
     case "lastIndexOf" =>
       Some(state.setExpression(ExpressionSetFactory.createAbstractOperator(thisExpr,
             parameters, typeparameters, AbstractOperatorIdentifiers.stringLastindexof, 
             returnedtype)));
  	  case _ => None
	}
  }
  
	def applyBackwardNativeSemantics[S <: State[S]](
      thisExpr: ExpressionSet,
      operator: String,
      parameters: List[ExpressionSet],
      typeparameters: List[Type],
		returnedtype : Type,
    programpoint : ProgramPoint,
      state: S): Option[S] = throw new StringException("Backward analysis not implemented/existing")

}

class StringException(s : String) extends Exception(s)
