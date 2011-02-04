package ch.ethz.inf.pm.sample.abstractdomain.waitorderinference


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
 
object ChaliceNativeMethodSemantics extends NativeMethodSemantics {
	type W = WaitOrderDomain[ProgramPointHeapIdentifier]

	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = thisExpr.getType().toString() match {
	  case "Chalice" =>
	  	val castedState=state.asInstanceOf[GenericAbstractState[W, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]
	  	val heapNodes = castedState._1._2._1.getAddresses++castedState._1._2._2.getAddresses;
	    parameters match {
		    case x :: Nil =>
		      operator match {
			    case "share" => {
			      if(x.getExpressions.size!=1) throw new MethodSemanticException("Only one variable is allowed here");
			      var result : W = castedState._1._1;
			      x.getExpressions.iterator.next match {
			    	    case id : ProgramPointHeapIdentifier => 
			    	    	for(heapNode <- heapNodes) {
			    	    		ReachabilityAnalysis.reachable(new VariableIdentifier("this", null), heapNode, castedState._1._2._1, castedState._1._2._2) match {
			    	    			case (p, true) => 
			    	    				//TODO: This is definitely wrong!!!
			    	    				var l=result.get((new AbstractObject(id), new AbstractObject(heapNode)));
			    	    				l=l.add(new SymbolicObjectSharing(id, null))
			    	    				result=result.add((new AbstractObject(id), new AbstractObject(heapNode)), l);
			    	    				l=result.get((new AbstractObject(heapNode), new AbstractObject(id)));
			    	    				l=l.add(new SymbolicObjectSharing(id, null))
			    	    				result=result.add((new AbstractObject(heapNode), new AbstractObject(id)), l)
			    	    			case (p, false) =>
			    	    		}
			    	    	}
			      }
			      Some(state)
			    }
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