package ch.ethz.inf.pm.sample.abstractdomain.waitorderinference


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
 
object ChaliceWaitOrderNativeMethodSemantics extends NativeMethodSemantics {
	type I = ProgramPointHeapIdentifier
	type W = WaitOrderDomain[I]
	type S = SymbolicOrderRelationshipsDomain[I]

	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = thisExpr.getType().toString() match {
	  case "Chalice" =>
	  	val castedState=state.asInstanceOf[GenericAbstractState[W, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]
	  	val heapNodes = castedState._1._2._1.getAddresses++castedState._1._2._2.getAddresses;
	    parameters match {
		    case x :: Nil =>
		      operator match {
			    case "share" => {
			    	var result = castedState._1._1._1;
			    	val (id, variable) = extractHeapId(castedState, x.getExpressions);
			    	for(heapNode <- heapNodes) {
			    		ReachabilityAnalysis.reachable(new VariableIdentifier("this", SystemParameters.currentClass), heapNode, castedState._1._2._1, castedState._1._2._2) match {
			    			case (p, true) => 
			    	    		var l=result.get((new AbstractObject(id, new Path(variable.getName() :: Nil)), new AbstractObject(heapNode, new Path("this" :: p))));
			    	    		l=l.add(new SymbolicObjectSharing(id, programpoint))
			    	    		result=result.add((new AbstractObject(id, new Path(variable.getName() :: Nil)), new AbstractObject(heapNode, new Path("this" :: p))), l);
			    	    		l=result.get((new AbstractObject(heapNode, new Path("this" :: p)), new AbstractObject(id, new Path(variable.getName() :: Nil))));
			    	    		l=l.add(new SymbolicObjectSharing(id, programpoint))
			    	    		result=result.add((new AbstractObject(heapNode, new Path("this" :: p)), new AbstractObject(id, new Path(variable.getName() :: Nil))), l)
			    	    	case (p, false) =>
			    	    	}
			    	}
    	    		var l=result.get((new AbstractObject(id, new Path(variable.getName() :: Nil)), new InitialLockLevel(SystemParameters.currentClass.getName(), SystemParameters.currentMethod)));
    	    		l=l.add(new SymbolicObjectSharing(id, programpoint))
			    	result=result.add((new AbstractObject(id, new Path(variable.getName() :: Nil)), new InitialLockLevel(SystemParameters.currentClass.getName(), SystemParameters.currentMethod)), l);
			    	l=result.get((new InitialLockLevel(SystemParameters.currentClass.getName(), SystemParameters.currentMethod), new AbstractObject(id, new Path(variable.getName() :: Nil))));
			    	l=l.add(new SymbolicObjectSharing(id, programpoint))
			    	result=result.add((new InitialLockLevel(SystemParameters.currentClass.getName(), SystemParameters.currentMethod), new AbstractObject(id, new Path(variable.getName() :: Nil))), l)
			    	Some(new GenericAbstractState[W, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](new HeapAndAnotherDomain(new WaitOrderDomain(result, castedState._1._1._2), castedState._1._2), castedState._2).asInstanceOf[S])
			    }
			    case "unshare" => return Some(state);
			    case "acquire" => {
			    	val (id, variable) = extractHeapId(castedState, x.getExpressions);
			    	Some(new GenericAbstractState[W, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](new HeapAndAnotherDomain(castedState._1._1.wait(id), castedState._1._2), castedState._2).asInstanceOf[S])
			    }
			    case "release" => {
			    	val (id, variable) = extractHeapId(castedState, x.getExpressions);
			    	Some(new GenericAbstractState[W, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](new HeapAndAnotherDomain(castedState._1._1.release(id), castedState._1._2), castedState._2).asInstanceOf[S])
			    }
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
	
	private def extractHeapId(state : GenericAbstractState[W, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]], id : scala.collection.Set[Expression]) : (I, VariableIdentifier) = {
		if(id.size!=1) throw new MethodSemanticException("Only one variable is allowed here");
		id.iterator.next match {
			    	    case variable : VariableIdentifier =>
			    	    	val ids = state._1._2.get(variable).value;
			    	    	if(ids.size != 1) throw new MethodSemanticException("Only one abstract node is allowed here");
			    	    	return (state._1._2.get(variable).value.iterator.next, variable);
		}
	}
	
    def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = 
		throw new MethodSemanticException("Backward semantics not yet supported");
}
	
class MethodSemanticException(message : String) extends Exception(message)