package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._
 
object ChaliceNativeMethodSemantics extends NativeMethodSemantics {
  type P = SymbolicPermissionsDomain[ProgramPointHeapIdentifier]

	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = thisExpr.getType().toString() match {
	  case "Chalice" =>
	    val castedState=state.asInstanceOf[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]
	    var result : P=castedState._1._1;
	    parameters match {
	    case x :: Nil =>
	      val ids = x.getExpressions;
	      operator match {
		    case "share" =>   
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.exhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }
		    case "unshare" => 
		      /*if(ids.size == 1) 
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.inhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }*/
		    	//TODO: add the constraints
		    case "acquire" =>  
		      if(ids.size == 1) 
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.inhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }
		    case "release" =>  
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.exhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }
		    case "free" =>
		    	  for(exp <- ids)
		    	 	  exp match {
		    	 	  	case id : Identifier => result = Annotation.exhaleEverything(exp.asInstanceOf[Identifier], castedState) 
		    	  	  }
	      }
    
	    case x :: y :: Nil =>
	      val ids = x.getExpressions;
	      val pexpr=y.getExpressions();
	      if(pexpr.size!=1) return None;
	      val predicate=pexpr.elements.next();
	      predicate match {
	        case Constant(s, typ) => operator match {
		         case "fold" =>  
				      if(ids.size == 1)
					      for(exp <- ids)
					    	  exp match {
					    	    case id : Identifier => result = Annotation.exhalePredicate(exp.asInstanceOf[Identifier], s, castedState)
					    	  }
		         case "unfold" =>  
					  for(exp <- ids)
					  	  exp match {
					   	    case id : Identifier => result = Annotation.inhalePredicate(exp.asInstanceOf[Identifier], s, castedState)
					  	  }
			    case "fork" =>
			      if(y.getExpressions().size != 1) return None;
				    //It applies pre and post conditions if these exist
			      y.getExpressions().elements.next() match {
			        case Constant(s, typ) => 
					    val castedState=state.asInstanceOf[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]];
					    var result=castedState._1._1;
				        for(exp <- x.getExpressions)
							    	  exp match {
							    	    case id : Identifier => result = Annotation.exhalePrecondition(id, x.getType().toString(), s, castedState, result);
							    	  }
				        val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](result, castedState._1._2);
					    val entryvalue =thisExpr.top().asInstanceOf[SymbolicAbstractValue[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]] 
					    return new Some(new GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](d1, entryvalue).asInstanceOf[S])
					    /*val methodName=s;
					    val className=x.getType().toString();
					    if(this.getPrePostConditions(className, methodName)==None) return None;
					  	val precondition=this.getPrePostConditions(className, methodName).get._1;
					  	val method : Option[MethodDeclaration] = findMethod(className, methodName);
					  	if(method==None) return None;
					  	val solvedPrecondition = solveParameters(method.get.arguments, precondition, parameters, x, state)
					  	return exhale[S, P](solvedPrecondition, state);*/
				}
			    case "join" =>
			      if(y.getExpressions().size != 1) return None;
				    //It applies pre and post conditions if these exist
			      y.getExpressions().elements.next() match {
			        case Constant(s, typ) =>  
					    val castedState=state.asInstanceOf[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]];
					    var result=castedState._1._1;
				        for(exp <- x.getExpressions)
							    	  exp match {
							    	    case id : Identifier => result = Annotation.inhalePostcondition(id, x.getType().toString(), s, castedState, result);
							    	  }
				        val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](result, castedState._1._2);
					    val entryvalue =thisExpr.top().asInstanceOf[SymbolicAbstractValue[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]] 
					    return new Some(new GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](d1, entryvalue).asInstanceOf[S])
					    /*val methodName=s;
					    val className=x.getType().toString();
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
      val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](result, castedState._1._2);
	  val entryvalue =thisExpr.top().asInstanceOf[SymbolicAbstractValue[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]] 
	  //new SymbolicAbstractValue[GenericAbstractState[PermissionsDomain, ProgramPointHeapDomain, SetProgramPointHeapIdentifier]](None, None)
	  //We suppose that S = GenericAbstractState[PermissionsDomain, ProgramPointHeapDomain, SetProgramPointHeapIdentifier]
	  //On the other hand, we want to be more flexible, that is, we want to reuse all the other parts of this class even if
	  //the state is not like that, and Chalice class is not used.
	  return new Some(new GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](d1, entryvalue).asInstanceOf[S])
      
	  case "Int" => return new Some(state);
   
	  case "Boolean" => return new Some(state); //TODO: Potentially not sound
   
	  case className => {
	 	if(operator.equals("==")) //to avoid comparison between references
	 		return new Some(state);
	    val castedState=state.asInstanceOf[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]];
	    var result=castedState._1._1;
        for(exp <- thisExpr.getExpressions)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.exhalePrecondition(id, className, operator, castedState, result);
			    	  }
        for(exp <- thisExpr.getExpressions)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.inhalePostcondition(id, className, operator, castedState, result);
			    	  }
        val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](result, castedState._1._2);
	    val entryvalue =thisExpr.top().asInstanceOf[SymbolicAbstractValue[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]] 
	    return new Some(new GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](d1, entryvalue).asInstanceOf[S])
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
	
	/** 
	* Inhale several permissions (the ones that are in a pre or post condition) on a state
	*/
	private def inhale[S<:State[S], P <: PermissionsDomain[P]](cond : Map[SymbolicAbstractValue[S], Int], state : S) : Option[S]= {
	  //I suppose the state is GenericAbstractState[PermissionsDomain, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	  var result=state.asInstanceOf[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]];
	  for(s <- cond.keySet) {
	    if(s.getExpressions().size==1) {
	      val id = s.getExpressions().elements.next;
	      if(id.isInstanceOf[Identifier]) {
	    	  val newPerm=result._1._1.inhale(id.asInstanceOf[Identifier], cond.apply(s));
	    	  result=new GenericAbstractState(new HeapAndAnotherDomain(newPerm, result._1._2), result._2);
	        }
	      else throw new Exception("This should not happen");
	    }
	  }
	  return Some(result.asInstanceOf[S])
	}
 
	/** 
	* Inhale several permissions on a state
	*/
	private def exhale[S<:State[S], P <: PermissionsDomain[P]](cond : Map[SymbolicAbstractValue[S], Int], state : S) : Option[S] = {
	  //I suppose the state is GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	  var result=state.asInstanceOf[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]];
	  for(s <- cond.keySet) {
	    if(s.getExpressions().size==1) {
	      val id = s.getExpressions().elements.next;
	      if(id.isInstanceOf[Identifier]) {
	    	  val newPerm=result._1._1.exhale(id.asInstanceOf[Identifier], cond.apply(s));
	    	  result=new GenericAbstractState(new HeapAndAnotherDomain(newPerm, result._1._2), result._2);
	        }
	      else throw new Exception("This should not happen");
	    }
	  }
	  return Some(result.asInstanceOf[S])
	}
 
	/**
	* Given the parameters of a method, the (pre/post)conditions, and the arguments passed to the method, it returns a map relating
	* each symbolic abstract value to the level permission that is related to that
	* In order to achieve this result, it has to resolve all the parameters and extrapolates the expressions (e.g., heap identifiers)
	* that are related to them 
	*/
	private def solveParameters[S <: State[S]](parameters : List[List[VariableDeclaration]], cond : Map[FieldAccess, Int], par : List[SymbolicAbstractValue[S]], thisExpr : SymbolicAbstractValue[S], state : S) : Map[SymbolicAbstractValue[S], Int] = {
	  var result : Map[SymbolicAbstractValue[S], Int] = Map.empty; 
	  if(parameters.size>1) throw new Exception("This should not happen");
	  val pars = parameters.elements.next;
	  for(access <- cond.keySet) {
	    if(access.objs.size==1) {
	      val obj=access.objs.elements.next;
	      val p = extractParameter(obj);
	      var p1 : SymbolicAbstractValue[S]=null;
	      if(p.getName().equals("this"))
	        p1=thisExpr;
	      else for(i <- 0 to pars.length-1)
	    	  		if(pars.apply(i).variable.getName().equals(p)) //It's not possible to have 2 parameters with the same name
	      				p1=par.apply(i);
	      if(p1==null) throw new Exception("This should not happen"); //Contract defined on something that is not a parameter of the method?
	      val v = accessField[S](p1, access, state);
	      result=result+((v, cond.apply(access)));
	    }
     }
	  result;
	}
	/**
	 * It returns the result of the access of the given field on the given symbolic abstract value 
	 */
	private def accessField[S <: State[S]](obj : SymbolicAbstractValue[S], field : Statement, state : S) : SymbolicAbstractValue[S] = field match {
	  case FieldAccess(pp, objs, field, typ) if(objs.size==1) => 
	    if(objs.size>1) throw new Exception("This should not happen");
	    val o=objs.elements.next;
	    val newState=state.getFieldValue(accessField(obj, o, state) :: Nil, field, typ);
	    return newState.getExpression(); 
	  case Variable(programpoint, id) => return obj;
	}
	
	/**
	 * Return the identifier of the most leftwing access (e.g., this.a.m => this)
	 */
	private def extractParameter(s : Statement) : Identifier = s match {
	  case FieldAccess(pp, objs, field, typ) if(objs.size==1) => return extractParameter(objs.elements.next); 
	  case Variable(programpoint, id) => return id;
	}
   
 /**
  * Given the name of the class and the name of the method, it returns the given method if it finds it
  */
	private def findMethod(className : String, methodName : String) : Option[MethodDeclaration] = {
	  val classes : List[ClassDefinition] = SystemParameters.classes;
	  for(classe <- classes )
		  if(classe.name.toString().equals(className)) {
		    for(method <- classe.methods )
		    	if(method.name.toString().equals(methodName))
		    		return Some(method);
		    return None;
		  }
	  return None	
	}
 
	private def getPrePostConditions(className : String, methodName : String) : Option[(Map[FieldAccess, Int], Map[FieldAccess, Int])] = {
	  	val classPreconditions=Annotation.preconditions.get(className);
	  	val classPostconditions=Annotation.postconditions.get(className);
	  	if(classPreconditions==None || classPostconditions==None) return None;
	  	val methodPreconditions=classPreconditions.get.get(methodName);
	  	val methodPostconditions=classPostconditions.get.get(methodName);
	  	if(methodPreconditions==None || methodPostconditions==None) return None;
	  	return Some((methodPreconditions.get, methodPostconditions.get))
	}

	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = thisExpr.getType().toString() match {
	  case "Chalice" =>
	    val castedState=state.asInstanceOf[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]]
	    var result : P=castedState._1._1;
	    parameters match {
	    case x :: Nil =>
	      val ids = x.getExpressions;
	      operator match {
		    case "share" =>   
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => 
			    	      result = Annotation.inhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }
		    case "unshare" => 
		      if(ids.size == 1) 
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.exhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }
		    case "acquire" =>  
		      if(ids.size == 1) 
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.exhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }
		    case "release" =>  
			      for(exp <- ids)
			    	  exp match {
			    	    case id : Identifier => result = Annotation.inhaleInvariants(exp.asInstanceOf[Identifier], exp.getType().getName(), castedState)
			    	  }
	      }

	    case x :: y :: Nil =>
	      val ids = x.getExpressions;
	      val pexpr=y.getExpressions();
	      if(pexpr.size!=1) return None;
	      val predicate=pexpr.elements.next();
	      predicate match {
	        case Constant(s, typ) => operator match {
		         case "fold" =>  
				      if(ids.size == 1)
					      for(exp <- ids)
					    	  exp match {
					    	    case id : Identifier => result = Annotation.inhalePredicate(exp.asInstanceOf[Identifier], s, castedState)
					    	  }
		         case "unfold" =>  
					  for(exp <- ids)
					  	  exp match {
					   	    case id : Identifier => result = Annotation.exhalePredicate(exp.asInstanceOf[Identifier], s, castedState)
					  	  }
       			 case "fork" =>
       			 	if(y.getExpressions().size != 1) return None;
				      y.getExpressions().elements.next() match {
				        case Constant(s, typ) => 
						    val methodName=s;
						    val className=x.getType().toString();
						    if(this.getPrePostConditions(className, methodName)==None) return None;
						  	val precondition=this.getPrePostConditions(className, methodName).get._1;
						  	val method : Option[MethodDeclaration] = findMethod(className, methodName);
						  	if(method==None) return None;
						  	val solvedPrecondition = solveParameters(method.get.arguments, precondition, parameters, x, state)
						  	return inhale[S, P](solvedPrecondition, state);
					}
			    case "join" =>  
				      if(y.getExpressions().size != 1) return None;
				        y.getExpressions().elements.next() match {
				        case Constant(s, typ) => 
						    val methodName=s;
						    val className=x.getType().toString();
						    if(this.getPrePostConditions(className, methodName)==None) return None;
						  	val postcondition=this.getPrePostConditions(className, methodName).get._2;
						  	val method : Option[MethodDeclaration] = findMethod(className, methodName);
						  	if(method==None) return None;
						  	val solvedPostcondition = solveParameters(method.get.arguments, postcondition, parameters, x, state)
						  	return exhale[S, P](solvedPostcondition, state);
					    }
	        }
	      }
       }
	  val d1 = new HeapAndAnotherDomain[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](result, castedState._1._2);
	  val entryvalue =new SymbolicAbstractValue[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]](None, None)
	  //We suppose that S = GenericAbstractState[P, ProgramPointHeapDomain, SetProgramPointHeapIdentifier]
	  //On the other hand, we want to be more flexible, that is, we want to reuse all the other parts of this class even if
	  //the state is not like that, and Chalice class is not used.
	  return new Some(new GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]](d1, entryvalue).asInstanceOf[S])
   
	  case "Int" => return new Some(state);
   
   	  case className => {
   	    throw new PermissionsException("Not yet supported!");
   	    
   	    /*
	    //It applies pre and post conditions if these exist
	    val methodName=operator;
	    val conds = Annotation.preconditions;
	  	val prepostcondition = this.getPrePostConditions(className, methodName);
	  	if(prepostcondition==None) return None;
	  	val precondition=prepostcondition.get._1;
	  	val postcondition=prepostcondition.get._2;
	  	val method : Option[MethodDeclaration] = findMethod(className, methodName);
	  	if(method==None) return None;
	  	val solvedPrecondition = solveParameters(method.get.arguments, precondition, parameters, thisExpr, state)
	  	val solvedPostcondition = solveParameters(method.get.arguments, postcondition, parameters, thisExpr, state)
	  	val inhaled = inhale[S, P](solvedPrecondition, state);
	  	if(inhaled==None) return None
	  	else return exhale[S, P](solvedPostcondition, inhaled.get);*/
	  }
   
      //case _ => return None;
	}
 
}

class MethodSemanticException(message : String) extends Exception(message)