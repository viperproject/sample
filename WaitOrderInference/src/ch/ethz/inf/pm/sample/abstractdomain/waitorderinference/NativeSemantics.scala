package ch.ethz.inf.pm.sample.abstractdomain.waitorderinference


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
 
object ChaliceNativeMethodSemantics extends NativeMethodSemantics {

	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = thisExpr.getType().toString() match {
	  case "Chalice" =>
	    parameters match {
		    case x :: Nil =>
		      operator match {
			    case "share" => return Some(state);
			    case "unshare" => return Some(state);
			    case "acquire" => return Some(state);
			    case "release" => return Some(state);
			    case "free" => return Some(state);
		      }
		    case x :: y :: Nil =>
		    	operator match {
			    	case "fold" => return Some(state);
			        case "unfold" => return Some(state);
				    case "fork" => return Some(state);
				    case "join" => return Some(state);
		        }
		      } 
		  	case _ => None
	}
	
    def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = 
		throw new MethodSemanticException("Backward semantics not yet supported");
}
	
class MethodSemanticException(message : String) extends Exception(message)