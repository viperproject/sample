/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.scalalang


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

object ObjectNativeMethodSemantics extends NativeMethodSemantics {
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = operator match {
	  case "$isInstanceOf" => parameters match {
	    case Nil => typeparameters match {
        case t :: Nil => Some(state.setExpression(ExpressionSetFactory.createAbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.isInstanceOf, returnedtype)));
          case _ => throw new MethodSemanticException("isInstanceOf must have exactly one type parameters")
        }
	    case _ => throw new MethodSemanticException("isInstanceOf cannot have parameters")
	  } 
    case "$asInstanceOf" => parameters match {
	    case Nil => typeparameters match {
        case t :: Nil => Some(state.setExpression(ExpressionSetFactory.createAbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.asInstanceOf, returnedtype)));
          case _ => throw new MethodSemanticException("asInstanceOf must have exactly one type parameters")
        }
	    case _ => throw new MethodSemanticException("asInstanceOf cannot have parameters")
	  } 
   	  //TODO: this conflicts with == over Int, I should have a hierarchy over the native semantics!!!
   	  /*case "==" => parameters match {
	    case p1 :: Nil => typeparameters match {
	      case Nil => return Some(state.setExpression(thisExpr.createAbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.==, state, returnedtype)));
          case _ => throw new MethodSemanticException("asInstanceOf must have exactly one type parameters")
        }
	    case _ => None//throw new MethodSemanticException("asInstanceOf cannot have parameters")
	  }*/
    case "this" => parameters match {
      case Nil => Some(state.setExpression(thisExpr));
       case _ => this.analyzeConstructor(thisExpr, parameters, state, returnedtype)
    }
    case "<init>" => parameters match {
      case Nil => Some(state.setExpression(thisExpr));
      case _ => this.analyzeConstructor(thisExpr, parameters, state, returnedtype)
    }
    case x : String if x.length >= 2 && x.substring(x.length - 2, x.length).equals("_=") => //obj.field_=expr is adopted to assign fields
      throw new MethodSemanticException("This should not be here");
      /*parameters match {
        case assigned :: Nil =>
          val field=x.substring(0, x.length-2)
          var fieldaccess=state.getFieldValue(thisExpr :: Nil, field, returnedtype.top())
          val fieldexpr=fieldaccess.getExpression();
          fieldaccess=fieldaccess.removeExpression();
          return Some(fieldaccess.assignVariable(fieldexpr, assigned))
        case _ => return None;//throw new MethodSemanticException("I can only assign an expression to a field!")
      }
      None*/
    case x => parameters match {
      case Nil =>
        val fields = thisExpr.typ.possibleFields
        for(field <- fields) {
          if(field.equals(x)) {
            val fieldAccess = state.getFieldValue(thisExpr, x, returnedtype)
            return Some(fieldAccess)
        }
       }
       None;
    case _ => None
    }
	}
	
	def analyzeConstructor[S <: State[S]](thisExpr : ExpressionSet, parameters : List[ExpressionSet], state : S, returnedType : Type) : Option[S] = returnedType.toString match {
	  case "Array" => None /*parameters match {
		        case x :: Nil =>
		        	if(thisExpr.getExpressions().size != 1) throw new MethodSemanticException("This is not yet supported!");
		        	//if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new MethodSemanticException("This is not yet supported!");
		        	//val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	var result = state.bottom(); 
		        	for(exp <- x.getExpressions) {
		        		val st = x.get(exp);
		        		result=result.lub(result, state.setExpression(new SymbolicAbstractValue(new ArrayCreation(exp, returnedType), state)));
		        	}
		        	//val arrayLength = new SymbolicAbstractValue(new LengthArray(), state)
		        	//val tempResult=result.assignVariable(x, x)
		        	return Some(result);
		      }  */
	  case _ => Some(state.setExpression(thisExpr));//or None? It depends, this is used to call the contructor...
	}
 
	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = operator match {
      case "this" => parameters match {
        case Nil => Some(state.removeExpression());
        case _ => None
      }
      case x : String if x.length >= 2 && x.substring(x.length - 2, x.length).equals("_=") => //obj.field_=expr is adopted to assign fields
        throw new MethodSemanticException("This should not be here");
        /*parameters match {
          case assigned :: Nil =>
          	val field=x.substring(0, x.length-2)
            var fieldaccess=state.backwardGetFieldValue(thisExpr :: Nil, field, returnedtype.top())
            val fieldexpr=fieldaccess.getExpression();
            fieldaccess=fieldaccess.removeExpression();
            var result=fieldaccess.setVariableToTop(fieldexpr);
            val condition=result.getExpression().createBinaryExpression(fieldexpr, assigned, ArithmeticOperator.==, result, fieldexpr.getType(result).top());//TODO type is wrong
            result=result.setExpression(condition);
            return Some(result.testTrue().backwardAssignVariable(fieldexpr, assigned));
          case _ => throw new MethodSemanticException("I can only assign an expression to a field!")
        }*/
     case x : String =>
       parameters match {
          case Nil =>
            val fields = thisExpr.typ.possibleFields
	          for(field <- fields) {
	            if(field.equals(x)) {
                val fieldAccess = state.refiningGetFieldValue(thisExpr, field.getName, returnedtype.top())
                return Some(fieldAccess)
		        }
	         }
         None;
      case _ => None
     }
  }
}

object IntegerNativeMethodSemantics extends NativeMethodSemantics {
  	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = None
  
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] =
		if(thisExpr.typ.toString.equals("Int"))
			operator match {
			  case ">=" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.>=, returnedtype);
			  case "<=" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.<=, returnedtype);
			  case "==" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.==, returnedtype);
			  case "!=" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.!=, returnedtype);
			  case ">" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.>, returnedtype);
			  case "<" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.<, returnedtype);
		      case "+" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.+, returnedtype);
		      case "*" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.*, returnedtype);
		      case "-" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.-, returnedtype);
		      case "/" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator./, returnedtype);
          case "%" => createBinaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.%, returnedtype);
		      case "unary_-" => createUnaryArithmeticExpression[S](state, thisExpr, parameters, ArithmeticOperator.-, returnedtype);
			  case _ => None;
			}
		else
			//Possible reference comparison
			operator match {
			  case "==" => createReferenceComparisonExpression[S](state, thisExpr, parameters, ArithmeticOperator.==, returnedtype);
			  case "!=" => createReferenceComparisonExpression[S](state, thisExpr, parameters, ArithmeticOperator.!=, returnedtype);
			  case _ => None;
			}
 
 
	private def createUnaryArithmeticExpression[S <: State[S]](state : S, thisExpr : ExpressionSet, parameters : List[ExpressionSet], operator : ArithmeticOperator.Value, returnedtype : Type) : Option[S] = parameters match {
    case Nil => Some(state.setExpression(ExpressionSetFactory.createUnaryExpression(thisExpr, operator, returnedtype)));
	    case _ => None
    }
 
	private def createBinaryArithmeticExpression[S <: State[S]](state : S, thisExpr : ExpressionSet, parameters : List[ExpressionSet], operator : ArithmeticOperator.Value, returnedtype : Type) : Option[S] = parameters match {
    case x :: Nil => Some(state.setExpression(ExpressionSetFactory.createBinaryExpression(thisExpr, x, operator, returnedtype)));
	    case _ => None
    }

  private def createReferenceComparisonExpression[S <: State[S]](state : S, thisExpr : ExpressionSet, parameters : List[ExpressionSet], operator : ArithmeticOperator.Value, returnedtype : Type) : Option[S] = parameters match {
    case x :: Nil => Some(state.setExpression(ExpressionSetFactory.createReferenceComparisonExpression(thisExpr, x, operator, returnedtype)));
      case _ => None
    }

}


object BooleanNativeMethodSemantics extends NativeMethodSemantics {
  	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = None
  
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = {
		if(thisExpr.typ.toString.equals("Boolean"))
			operator match {
			  case "&&" => createBinaryBooleanExpression[S](state, thisExpr, parameters, BooleanOperator.&&, returnedtype);
			  case "||" => createBinaryBooleanExpression[S](state, thisExpr, parameters, BooleanOperator.||, returnedtype);
			  case _ => None;
			}
		else None
	}

	private def createBinaryBooleanExpression[S <: State[S]](state : S, thisExpr : ExpressionSet, parameters : List[ExpressionSet], operator : BooleanOperator.Value, returnedtype : Type) : Option[S] = parameters match {
    case x :: Nil => Some(state.setExpression(ExpressionSetFactory.createBooleanBinaryExpression(thisExpr, x, operator, returnedtype)));
	    case _ => None
    }

 
}
                                                         
                                                         
class MethodSemanticException(message : String) extends Exception(message)