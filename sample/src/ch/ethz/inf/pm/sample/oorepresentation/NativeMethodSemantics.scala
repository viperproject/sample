package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.abstractdomain._

object ArrayNativeMethodSemantics extends NativeMethodSemantics {
	
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = thisExpr.getType().toString() match {
		case "Array" => operator match {
		      case "this" => parameters match {
		        case x :: Nil =>
              var newState = state.createArray(x, returnedtype, programpoint);
              val arrayId = newState.getExpression();
              newState = newState.getArrayLength(arrayId);
              val arrayLengthId = newState.getExpression();
              newState = newState.assignVariable(arrayLengthId, x);
              var newSymbAV=new SymbolicAbstractValue[S]();
              for(exp <- arrayId.getExpressions())
                newSymbAV=newSymbAV.add(exp, newState);
              newState=newState.setExpression(newSymbAV)
              return Some(newState);
              //return Some(state.createArray(x, returnedtype, programpoint))
		      }
		      case "update" => parameters match {
		     	case index :: value :: Nil =>
		        	return Some(state.assignArrayCell(thisExpr, index, value, returnedtype));
		      }
		      case "apply" => parameters match {
		     	case index :: Nil =>
              thisExpr.getType(state).getArrayElementsType match {
                case None => throw new ArrayAnalysisException("The expressions should be an array");
                case Some(s) => return Some(state.getArrayCell(thisExpr, index, s))
              }
		      }
		      case "length" => parameters match {
		     	  case Nil =>
		        	return Some(state.getArrayLength(thisExpr));
		      }
		      case _ => return None
		}
		case _ => return None;
	}
	
	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = None
}

class ArrayAnalysisException(s : String) extends Exception(s)