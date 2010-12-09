package ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
 
object ArrayNativeMethodSemantics extends NativeMethodSemantics {
	
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = thisExpr.getType().toString() match {
		case "Array" => operator match {
		      case "this" => parameters match {
		        case x :: Nil =>
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	if(! checkCast(state)) throw new ArrayAnalysisException("Only an array analysis can deal with arrays!");
		        	var result = state.bottom(); 
		        	for(exp <- x.getExpressions) {
		        		val st = x.get(exp);
		        		result=result.lub(result, casts(result).createArray(id, exp));
		        	}
		        	return Some(result);
		      }
		      case "update" => parameters match {
		     	case index :: value :: Nil => 
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	if(! checkCast(state)) throw new ArrayAnalysisException("Only an array analysis can deal with arrays!");
		        	var result = state.bottom(); 
		        	for(ind <- index.getExpressions) {
		        		for(valu <- value.getExpressions()) {
		        			val st = value.get(valu);
		        			result=result.lub(result, casts(result).updateArray(id, ind, valu));
		        		}
		        	}
		        	return Some(result);
		      }
		      case "apply" => parameters match {
		     	case index :: Nil => 
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	if(! checkCast(state)) throw new ArrayAnalysisException("Only an array analysis can deal with arrays!");
		        	var result = state.bottom(); 
		        	for(exp <- index.getExpressions) {
		        		val st = index.get(exp);
		        		result=result.lub(result, casts(result).getArrayValue(id, exp));
		        	}
		        	return Some(result);
		      }
		      case "length" => parameters match {
		     	  case Nil => 
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	if(! checkCast(state)) throw new ArrayAnalysisException("Only an array analysis can deal with arrays!");
		        	var result = state.bottom();
		        	return Some(casts(state).getArrayLength(id));
		      }
		      //case _ => System.out.println(operator); return None
		}
		case _ => return None;
	}
	
	//I can't add another generic to applyForwardNativeSemantics but I need a bounded type as generic of ArrayAnalysis
	//Then I needed to write this (horrible) workaround
	private def casts[S <: State[S], A <: ArrayAnalysis[A]](s : S) : ArrayAnalysis[A] = s.asInstanceOf[A];
	private def checkCast[S <: State[S], A <: ArrayAnalysis[A]](s : S) : Boolean = s.isInstanceOf[ArrayAnalysis[A]];
	
	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = None
}

class ArrayAnalysisException(s : String) extends Exception(s)