package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample._
import oorepresentation._

object ChaliceNativeMethodSemantics extends NativeMethodSemantics {
  type P = SymbolicPermissionsDomain[ProgramPointHeapIdentifier]

	def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = thisExpr.getType().toString match {
	  case "Chalice" =>
	    val castedState=state.asInstanceOf[AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier]]
	    var result : P=castedState._1._1;
	    parameters match {
	    case x :: Nil =>
	      val ids = x.getSetOfExpressions;
	      operator match {
		    case "share" =>   
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.exhaleInvariants(exp.asInstanceOf[Identifier], exp.typ.name, castedState)
			    	    case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result = Annotation.exhaleInvariants(exp.asInstanceOf[MaybeHeapIdSetDomain[ProgramPointHeapIdentifier]], exp.typ.name, castedState)
			    	  }
		    case "unshare" => 
		      /*if(ids.size == 1) 
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.inhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }*/
		    case "acquire" =>  
		      if(ids.size == 1) 
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.inhaleInvariants(id, exp.typ.name, castedState)
                case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result=Annotation.inhaleInvariants(id, exp.typ.name, castedState)
			    	  }
		    case "release" =>  
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.exhaleInvariants(id, exp.typ.name, castedState)
                case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result=Annotation.exhaleInvariants(id, exp.typ.name, castedState)
			    	  }
		    case "free" =>
		    	  for(exp <- ids)
		    	 	  exp match {
		    	 	  	case id : Identifier => result = Annotation.exhaleEverything(id, castedState)
		    	 	  	case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result = Annotation.exhaleEverything(id, castedState)
		    	  	  }
	      }
    
	    case x :: y :: Nil =>
	      val ids = x.getSetOfExpressions;
	      val pexpr=y.getSetOfExpressions;
	      if(pexpr.size!=1) return None;
	      val predicate=pexpr.head;
	      predicate match {
	        case Constant(s, typ, pp) => operator match {
		         case "fold" =>  
				      if(ids.size == 1)
					      for(exp <- ids)
					    	  exp match {
					    	    case id : Identifier => result = Annotation.exhalePredicate(id, s, castedState)
                    case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result=Annotation.inhalePredicate(id, s, castedState)
					    	  }
		         case "unfold" =>  
					  for(exp <- ids)
					  	  exp match {
					   	    case id : Identifier => result = Annotation.inhalePredicate(id, s, castedState)
                  case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result=Annotation.inhalePredicate(id, s, castedState)
					  	  }
			    case "fork" =>
			      if(y.getSetOfExpressions.size != 1) return None;
				    //It applies pre and post conditions if these exist
			      y.getSetOfExpressions.head match {
			        case Constant(s, typ, pp) =>
					    val castedState=state.asInstanceOf[AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier]];
					    var result=castedState._1._1;
				        for(exp <- x.getSetOfExpressions)
							    	  exp match {
							    	    case id : Identifier => result = Annotation.exhalePrecondition(id, x.getType().toString, s, castedState, result);
                        case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result = Annotation.exhalePrecondition(id, x.getType().toString, s, castedState, result);
							    	  }
				        val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier](result, castedState._1._2);
					    val entryvalue =thisExpr.top().asInstanceOf[ExpressionSet]
					    return new Some(new AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier](d1, entryvalue).asInstanceOf[S])
					    /*val methodName=s;
					    val className=x.getType().toString;
					    if(this.getPrePostConditions(className, methodName)==None) return None;
					  	val precondition=this.getPrePostConditions(className, methodName).get._1;
					  	val method : Option[MethodDeclaration] = findMethod(className, methodName);
					  	if(method==None) return None;
					  	val solvedPrecondition = solveParameters(method.get.arguments, precondition, parameters, x, state)
					  	return exhale[S, P](solvedPrecondition, state);*/
				}
			    case "join" =>
			      if(y.getSetOfExpressions.size != 1) return None;
				    //It applies pre and post conditions if these exist
			      y.getSetOfExpressions.head match {
			        case Constant(s, typ, pp) =>
					    val castedState=state.asInstanceOf[AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier]];
					    var result=castedState._1._1;
				        for(exp <- x.getSetOfExpressions)
							    	  exp match {
							    	    case id : Identifier => result = Annotation.inhalePostcondition(id, x.getType().toString, s, castedState, result);
                        case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => Annotation.inhalePostcondition(id, x.getType().toString, s, castedState, result);
							    	  }
				        val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier](result, castedState._1._2);
					    val entryvalue =thisExpr.top().asInstanceOf[ExpressionSet]
					    return new Some(new AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier](d1, entryvalue).asInstanceOf[S])
					    /*val methodName=s;
					    val className=x.getType().toString;
					    if(this.getPrePostConditions(className, methodName)==None) return None;
					  	val postcondition=this.getPrePostConditions(className, methodName).get._2;
					  	val method : Option[MethodDeclaration] = findMethod(className, methodName);
					  	if(method==None) return None;
					  	val solvedPostcondition = solveParameters(method.get.arguments, postcondition, parameters, x, state)
					  	return inhale[S, P](solvedPostcondition, state);*/
				    }
	        }
	      } 
	    case _ => None 
	  }
      val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier](result, castedState._1._2);
	  val entryvalue =thisExpr.top().asInstanceOf[ExpressionSet]
	  //new SymbolicAbstractValue[AbstractState[PermissionsDomain, ProgramPointHeapDomain, SetProgramPointHeapIdentifier]](None, None)
	  //We suppose that S = AbstractState[PermissionsDomain, ProgramPointHeapDomain, SetProgramPointHeapIdentifier]
	  //On the other hand, we want to be more flexible, that is, we want to reuse all the other parts of this class even if
	  //the state is not like that, and Chalice class is not used.
	  return new Some(new AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier](d1, entryvalue).asInstanceOf[S])
      
	  case "Int" => return None; //Before it was new Some(state);
   
	  case "Boolean" => return None; //Before it was Some(state); TODO: Potentially not sound
   
	  case className => {
	 	if(operator.equals("==") || operator.equals("!=") || operator.equals("$asInstanceOf") || operator.equals("$isInstanceOf")) //to avoid comparison between references and type casts
	 		return new Some(state);
	  val castedState=state.asInstanceOf[AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier]];
	  var result=castedState._1._1;
    for(exp <- thisExpr.getSetOfExpressions)
		  exp match {
		    case id : Identifier => result = Annotation.exhalePrecondition(id, className, operator, castedState, result);
        case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result = Annotation.exhalePrecondition(id, className, operator, castedState, result);
		  }
    for(exp <- thisExpr.getSetOfExpressions)
		  exp match {
		    case id : Identifier => result = Annotation.inhalePostcondition(id, className, operator, castedState, result);
		    case id : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] => result = Annotation.inhalePostcondition(id, className, operator, castedState, result);
		  }
    val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier](result, castedState._1._2);
	  val entryvalue =thisExpr.top()
	  return new Some(new AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier](d1, entryvalue).asInstanceOf[S])
	    //It applies pre and post conditions if these exist
	    /*val methodName=operator;
	  	val prepostcondition = this.getPrePostConditions(className, methodName);
	  	if(prepostcondition==None) return None;
	  	val precondition=prepostcondition.get._1;
	  	val postcondition=prepostcondition.get._2;
	  	val method : Option[MethodDeclaration] = findMethod(className, methodName);
	  	if(method==None) return None;
	  	val solvedPrecondition = solveParameters(method.get.arguments, precondition, parameters, thisExpr, state)
	  	val solvedPostcondition = solveParameters(method.get.arguments, postcondition, parameters, thisExpr, state)
	  	val exhaled = exhale[S, P](solvedPrecondition, state);
	  	if(exhaled==None) return None
	  	else return inhale[S, P](solvedPostcondition, exhaled.get);*/
	  }
	}
	       /*
	/** 
	* Inhale several permissions (the ones that are in a pre or post condition) on a state
	*/
	private def inhale[S<:State[S], P <: PermissionsDomain[P]](cond : Map[ExpressionSet, Int], state : S) : Option[S]= {
	  //I suppose the state is AbstractState[PermissionsDomain, NonRelationalHeapDomain[ProgramPointHeapIdentifier], MaybeHeapIdSetDomain[ProgramPointHeapIdentifier]]
	  var result=state.asInstanceOf[AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], MaybeHeapIdSetDomain[ProgramPointHeapIdentifier]]];
	  for(s <- cond.keySet) {
	    if(s.getExpressions().size==1) {
	      val id = s.getExpressions().head
	      if(id.isInstanceOf[Identifier]) {
	    	  val newPerm=result._1._1.inhale(id.asInstanceOf[Identifier], cond.apply(s));
	    	  result=new AbstractState(new HeapAndAnotherDomain(newPerm, result._1._2), result._2);
	        }
	      else throw new Exception("This should not happen");
	    }
	  }
	  return Some(result.asInstanceOf[S])
	}
 
	/** 
	* Inhale several permissions on a state
	*/
	private def exhale[S<:State[S], P <: PermissionsDomain[P]](cond : Map[ExpressionSet, Int], state : S) : Option[S] = {
	  //I suppose the state is AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], MaybeHeapIdSetDomain[ProgramPointHeapIdentifier]]
	  var result=state.asInstanceOf[AbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], MaybeHeapIdSetDomain[ProgramPointHeapIdentifier]]];
	  for(s <- cond.keySet) {
	    if(s.getExpressions().size==1) {
	      val id = s.getExpressions().head
	      if(id.isInstanceOf[Identifier]) {
	    	  val newPerm=result._1._1.exhale(id.asInstanceOf[Identifier], cond.apply(s));
	    	  result=new AbstractState(new HeapAndAnotherDomain(newPerm, result._1._2), result._2);
	        }
	      else throw new Exception("This should not happen");
	    }
	  }
	  return Some(result.asInstanceOf[S])
	}
         */
	/**
	* Given the parameters of a method, the (pre/post)conditions, and the arguments passed to the method, it returns a map relating
	* each symbolic abstract value to the level permission that is related to that
	* In order to achieve this result, it has to resolve all the parameters and extrapolates the expressions (e.g., heap identifiers)
	* that are related to them 
	*/
	private def solveParameters[S <: State[S]](parameters : List[List[VariableDeclaration]], cond : Map[FieldAccess, Int], par : List[ExpressionSet], thisExpr : ExpressionSet, state : S) : Map[ExpressionSet, Int] = {
	  var result : Map[ExpressionSet, Int] = Map.empty; 
	  if(parameters.size>1) throw new Exception("This should not happen");
	  val pars = parameters.head;
    for (access <- cond.keySet) {
      val p = extractParameter(access.obj)
      var p1: ExpressionSet = null
      if (p.getName.equals("this"))
        p1 = thisExpr
      else for (i <- 0 to pars.length - 1)
        if (pars.apply(i).variable.getName.equals(p)) //It's not possible to have 2 parameters with the same name
          p1 = par.apply(i)
      if (p1 == null) throw new Exception("This should not happen"); //Contract defined on something that is not a parameter of the method?
      val v = accessField[S](p1, access, state)
      result = result + ((v, cond.apply(access)))
    }
	  result;
	}
	/**
	 * It returns the result of the access of the given field on the given symbolic abstract value 
	 */
	private def accessField[S <: State[S]](obj : ExpressionSet, field : Statement, state : S) : ExpressionSet = field match {
	  case FieldAccess(pp, fieldObj, field, typ) =>
      val newState = state.getFieldValue(accessField(obj, fieldObj, state), field, typ)
	    newState.expr
	  case v: Variable => obj
	}
	
	/**
	 * Return the identifier of the most leftwing access (e.g., this.a.m => this)
	 */
  private def extractParameter(s: Statement): Identifier = s match {
    case FieldAccess(_, obj, _, _) => extractParameter(obj)
    case Variable(_, id) => id
  }
   
	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S, oldPreState: S) : Option[S] =
		throw new MethodSemanticException("Backward semantics not yet supported");
		
	private def getPrePostConditions(className : String, methodName : String) : Option[(Map[FieldAccess, Int], Map[FieldAccess, Int])] = {
	  	val classPreconditions=Annotation.preconditions.get(className);
	  	val classPostconditions=Annotation.postconditions.get(className);
	  	if(classPreconditions==None || classPostconditions==None) return None;
	  	val methodPreconditions=classPreconditions.get.get(methodName);
	  	val methodPostconditions=classPostconditions.get.get(methodName);
	  	if(methodPreconditions==None || methodPostconditions==None) return None;
	  	return Some((methodPreconditions.get, methodPostconditions.get))
	}

}

class MethodSemanticException(message : String) extends Exception(message)