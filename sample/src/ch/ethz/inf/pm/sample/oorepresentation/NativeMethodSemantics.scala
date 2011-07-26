package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.abstractdomain._

object ArrayNativeMethodSemantics extends NativeMethodSemantics {
	
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = thisExpr.getType().toString() match {
		case "Array" => operator match {
		      case "this" => parameters match {
		        case x :: Nil =>
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	//if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	//val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	var result = state.bottom();
              val r1 = state.createObject(returnedtype, programpoint);
              val hid = r1.getExpression();
		        	for(exp <- x.getExpressions) {
                var r2 = r1.bottom();
                val arrayId = r1.getExpression();
                for(e1 <- arrayId.getExpressions())
                  for(heapid <- e1.asInstanceOf[HeapIdSetDomain[_]].value)
                    r2=r2.lub(r2, arrayId.get(e1).assignVariable(new SymbolicAbstractValue(new LengthArray(heapid.asInstanceOf[Identifier], returnedtype.top()), arrayId.get(e1)), x))
		        		result=result.lub(result, r2);
		        	}
              var newExpr = new SymbolicAbstractValue[S]();
              for(exp <- hid.getExpressions())
                newExpr=newExpr.add(exp, result);
		        	return Some(result.setExpression(newExpr));
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
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	return Some(state.setExpression(new SymbolicAbstractValue(new LengthArray(id, returnedtype), state)));
		      }
		      case _ => return None
		}
		case _ => return None;
	}
	
	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = None
}

class ArrayAnalysisException(s : String) extends Exception(s)