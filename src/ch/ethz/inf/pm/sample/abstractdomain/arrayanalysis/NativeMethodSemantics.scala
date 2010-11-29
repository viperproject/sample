package ch.ethz.inf.pm.sample.preprocessing.scalaprocessing

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
 
object ArrayNativeMethodSemantics extends NativeMethodSemantics {
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = thisExpr.getType().toString() match {
		case "Array" => operator match {
		      case "this" => parameters match {
		        case x :: Nil => return Some(state.setExpression(thisExpr));//create a new array of x dimension
		      }
		      case "update" => parameters match {
		     	case index :: value :: Nil => return Some(state.setExpression(thisExpr));//arr(index)=value
		      }
		      case "apply" => parameters match {
		     	case index :: Nil => return None;//arr(index)=value
		      }
		      case "length" => parameters match {
		     	  case Nil =>return None;//return arr.length
		      }
		      //case _ => System.out.println(operator); return None
		}
		case _ => return None;
	}
	
	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = None
}
