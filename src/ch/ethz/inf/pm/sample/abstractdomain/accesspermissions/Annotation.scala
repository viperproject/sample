package ch.ethz.inf.pm.sample.chalice

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._

object Annotation {
	var monitorInvariants : Map[String, Map[FieldAccess, Int]] = Map.empty;
 	//For sake of simplicity, we do not consider method overloading
 	var preconditions : Map[String, Map[String, Map[FieldAccess, Int]]] = Map.empty
 	//Example of hand written contract
 	//+(("C100", Map.empty+(("Dispose", Map.empty+((new FieldAccess(null, new Variable(null, new VariableIdentifier("this", null)) :: Nil, "x", null), new Int(new LocalInteger(100))))))));
 	var postconditions : Map[String, Map[String, Map[FieldAccess, Int]]] = Map.empty
 	//+(("C100", Map.empty+(("Dispose", Map.empty))));
	var predicates : Map[String, Map[FieldAccess, Int]] = Map.empty;

 	var loopinvariants : Map[ProgramPoint, Map[FieldAccess, Int]] = Map.empty;

 	def printAnnotation() = {
 	  System.out.println("Predicates\n------------------------\n");
 	  var classes=this.unifySets[String](monitorInvariants.keySet, preconditions.keySet);
 	  classes=this.unifySets[String](classes, postconditions.keySet);
 	  for(key <- predicates.keySet)
 		  System.out.println(key+"\n"+this.localConditionToString(predicates.apply(key)))
 	  System.out.println("Loop invariants\n------------------------\n"+this.extractMethods(loopinvariants));
      for(classe <- classes) {
 	    System.out.println("Class: "+classe+"\n------------------------");
 	    if(monitorInvariants.keySet.contains(classe))
 	    	System.out.println("Monitor invariants\n------------------------\n"+this.localConditionToString(monitorInvariants.apply(classe)));
 	    for(method <- this.unifySets[String](extractMethods(preconditions, classe), extractMethods(postconditions, classe)) ) {
	 	    System.out.println("Method: "+method+"\n------------------------");
	 	    if(preconditions.keySet.contains(classe))
	 	    	System.out.println("Preconditions\n------------------------\n"+this.extractMethods(preconditions, classe, method));
	 	    if(postconditions.keySet.contains(classe))
	 	    	System.out.println("Postconditions\n------------------------\n"+this.extractMethods(postconditions, classe, method));
 	      
 	    }
 	  }
 	}
 
 	def unifySets[A](a : scala.collection.Set[A], b : scala.collection.Set[A]) : scala.collection.mutable.Set[A] = {
 	  var result : scala.collection.mutable.Set[A] =new scala.collection.mutable.HashSet[A]().++(a);
 	  for(k <- b)
 		  if(! result.contains(k))
 			  result=result+(k);
 	  result;
 	}
  
  
 	def extractMethods(cond : Map[ProgramPoint, Map[FieldAccess, Int]]) : String = {
 		var result : String = "";
 		for(pp <- cond.keySet)
 			result="Loop at "+pp.toString+". Invariant:\n"+localConditionToString(cond.apply(pp))+"\n";
 		return result;
 	}
  
 	def extractMethods(cond : Map[String, Map[String, Map[FieldAccess, Int]]], key : String) : scala.collection.Set[String] =
 		if(cond.keySet.contains(key)) 
 			cond.apply(key).keySet;
 		else Set.empty;
 
   
 	def extractMethods(cond : Map[String, Map[String, Map[FieldAccess, Int]]], classe : String, method : String) : String = {
 		if(cond.keySet.contains(classe)) 
 			if(cond.apply(classe).keySet.contains(method))
 				return localConditionToString(cond.apply(classe).apply(method));
 		return "";
   }
   
 	def localConditionToString(cond : Map[FieldAccess, Int]) : String = {
 		var result : String = "";
 		for(fieldAccess <- cond.keySet) {
 			if(! cond.apply(fieldAccess).toString().equals("0"))
 				result=result+fieldAccess+" -> "+cond.apply(fieldAccess)+"\n";
 		}
 		return result;
 	}
  
 	def addPreconditions(a : Map[String, Map[String, Map[FieldAccess, Int]]]) =
 		preconditions=unify(preconditions, a);
 
 	def addPostconditions(a : Map[String, Map[String, Map[FieldAccess, Int]]]) =
 		postconditions=unify(postconditions, a);
  
 	def unify(a : Map[String, Map[String, Map[FieldAccess, Int]]], b : Map[String, Map[String, Map[FieldAccess, Int]]]) : Map[String, Map[String, Map[FieldAccess, Int]]] = {
 	  var result : Map[String, Map[String, Map[FieldAccess, Int]]] = a;
 	  for(key <- b.keySet) {
 	    if(! a.keySet.contains(key))
 	    	result=result+((key, b.apply(key)));
 	    else result=result+((key, unifyMethods(a.apply(key), b.apply(key))));
 	  }
 	  return result; 	  
 	}
 
   	private def unifyMethods(a : Map[String, Map[FieldAccess, Int]], b : Map[String, Map[FieldAccess, Int]]) : Map[String, Map[FieldAccess, Int]]= {
 	  var result : Map[String, Map[FieldAccess, Int]] = a;
 	  for(key <- b.keySet) {
 	    if(! a.keySet.contains(key))
 	    	result=result+((key, b.apply(key)));
 	    else result=result+((key, unifyFields(a.apply(key), b.apply(key))));
 	  }
 	  return result; 	  
 	}
  
    private def unifyFields(a : Map[FieldAccess, Int], b : Map[FieldAccess, Int]) : Map[FieldAccess, Int]= {
 	  var result : Map[FieldAccess, Int] = a;
 	  for(key <- b.keySet) {
 	    if(! a.keySet.contains(key))
 	    	result=result+((key, b.apply(key)));
 	    else result=result+((key, Math.max(a.apply(key), b.apply(key))));
 	  }
 	  return result; 	  
 	}
    
    
 	def inhalePredicate[P <: PermissionsDomain[P]](
 					  id : Identifier,
                      predicate : String,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P = {
	  var p = s._1._1;
	  if(predicates.keys.toList.contains(predicate))
	    return this.inhaleFieldsPermissions(id, predicates.get(predicate).get, p, s)
	  else return this.inhaleReachable(id, s._1._1, s._1._2._1, s._1._2._2, SymbolicAbstractPredicates(id.getType().toString(), predicate, null));
	}

 	def exhalePredicate[P <: PermissionsDomain[P]](
 					  id : Identifier,
                      predicate : String,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P =  {
	  var p = s._1._1;
	  if(predicates.keys.toList.contains(predicate))
	    return this.exhaleFieldsPermissions(id, predicates.get(predicate).get, p, s)
	  else return this.exhaleReachable(id, s._1._1, s._1._2._1, s._1._2._2, SymbolicAbstractPredicates(id.getType().toString(), predicate, null));
	}
 
	def inhaleInvariants[P <: PermissionsDomain[P]](
					  id : Identifier, 
                      t : String,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P = {
	  var p = s._1._1;
	  if(monitorInvariants.keys.toList.contains(t))
	    return this.inhaleFieldsPermissions(id, monitorInvariants.get(t).get, p, s) //val localInv = monitorInvariants.get(t).get;
	  else return this.inhaleReachable(id, s._1._1, s._1._2._1, s._1._2._2, SymbolicMonitorInvariant(t, null));
	}
   
 	def exhalePrecondition[P <: PermissionsDomain[P]](
					  id : Identifier,
                      className : String,
                      methodName : String,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]],
                      p : P
	) : P = {
 	  if(methodName.equals("this")) return p; //Avoiding constructors
	  if(this.getPreCondition(className, methodName)==None) 
     //TODO: I should add also the arguments!!!
		  return this.exhaleReachable(id, p, s._1._2._1, s._1._2._2, SymbolicPreCondition(className, methodName, null));
	  else throw new PermissionsException("Not yet supported");
	  //return this.exhaleCondition(id, s._1._1, s._1._2._1, s._1._2._2);
	}
 

 	def inhalePrecondition[P <: PermissionsDomain[P]](
					  id : Identifier,
                      className : String,
                      methodName : String,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]],
                      p : P
	) : P = {
	  if(methodName.equals("this")) return p; //Avoiding constructors
	  if(this.getPreCondition(className, methodName)==None) 
		  return this.inhaleReachable(id, p, s._1._2._1, s._1._2._2, SymbolicPreCondition(className, methodName, null));
	  else throw new PermissionsException("Not yet supported");
	  //return this.exhaleCondition(id, s._1._1, s._1._2._1, s._1._2._2);
	}
  
 	def exhalePostcondition[P <: PermissionsDomain[P]](
					  id : Identifier,
                      className : String,
                      methodName : String,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]],
                      p : P
	) : P = {
	  if(methodName.equals("this")) return p; //Avoiding constructors
	  if(this.getPostCondition(className, methodName)==None) 
		  return this.exhaleReachable(id, p, s._1._2._1, s._1._2._2, SymbolicPostCondition(className, methodName, null));
	  else throw new PermissionsException("Not yet supported");
	  //return this.exhaleCondition(id, s._1._1, s._1._2._1, s._1._2._2);
	}
 

 	def inhalePostcondition[P <: PermissionsDomain[P]](
					  id : Identifier,
                      className : String,
                      methodName : String,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]],
                      p : P
	) : P = {
	  if(methodName.equals("this")) return p; //Avoiding constructors
	  if(this.getPostCondition(className, methodName)==None) 
		  return this.inhaleReachable(id, p, s._1._2._1, s._1._2._2, SymbolicPostCondition(className, methodName, null));
	  else throw new PermissionsException("Not yet supported");
	  //return this.exhaleCondition(id, s._1._1, s._1._2._1, s._1._2._2);
	}
  
  	private def getPreCondition(className : String, methodName : String) : Option[Map[FieldAccess, Int]] = {
	  	val classPreconditions=Annotation.preconditions.get(className);
	  	if(classPreconditions==None) return None;
	  	val methodPreconditions=classPreconditions.get.get(methodName);
	  	if(methodPreconditions==None) return None;
	  	return Some(methodPreconditions.get)
	}
   
 	private def getPostCondition(className : String, methodName : String) : Option[Map[FieldAccess, Int]] = {
	  	val classPostconditions=Annotation.postconditions.get(className);
	  	if(classPostconditions==None) return None;
	  	val methodPostconditions=classPostconditions.get.get(methodName);
	  	if(methodPostconditions==None) return None;
	  	return Some(methodPostconditions.get)
	}
  
	private def inhaleFieldsPermissions[P <: PermissionsDomain[P]](
					  id : Identifier,
					  localInv : Map[FieldAccess, Int],
					  domain : P,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P = {
		var p = domain;
	  	for(field <- localInv.keys)
	      p=Annotation.inhaleSingle(id, convertFieldAccessToListStrings(field), localInv.get(field).get, p, s);
	    return p;
	}
 
 	def exhaleInvariants[P <: PermissionsDomain[P]](
					  id : Identifier, 
                      t : String,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P =  {
	  var p = s._1._1;
	  if(monitorInvariants.keys.toList.contains(t))
	    return this.exhaleFieldsPermissions(id, monitorInvariants.get(t).get, p, s) //val localInv = monitorInvariants.get(t).get;
	  else return this.exhaleReachable(id, s._1._1, s._1._2._1, s._1._2._2, SymbolicMonitorInvariant(t, null));
	  //else return p;
	}
   
 	def exhaleEverything[P <: PermissionsDomain[P]](
					  id : Identifier,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P =  {
	  var p = s._1._1;
	  var h = s._1._2;
	  for((s, t) <- id.getType().getPossibleFields()) {
		  var fieldId = h.getFieldIdentifier(id, s, t);
	 	  p = p.free(fieldId);
	  }
	  return p;
	}
 	
  	private def exhaleFieldsPermissions[P <: PermissionsDomain[P]](
					  id : Identifier,
					  localInv : Map[FieldAccess, Int],
					  domain : P,
                      s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P = {
		var p = domain;
	  	for(field <- localInv.keys)
	      p=Annotation.exhaleSingle(id, convertFieldAccessToListStrings(field), localInv.get(field).get, p, s);
	    return p;
	}

	private def inhaleSingle[P <: PermissionsDomain[P]](
	  id : Identifier,
	  field : List[String],
	  p: Int, 
	  pd : P, 
	  s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P = {
	  	var id1 = accessSequenceOfFields(s.getVariableValue(id).getExpression(), field, s);
        val exprs = id1.getExpressions();
        if(exprs.size!=1)
        	return pd;
        val exp=exprs.elements.next();
	    val perm=pd.inhale(exp.asInstanceOf[Identifier], p);
	    return perm;
	}

	private def exhaleSingle[P <: PermissionsDomain[P]](
	  id : Identifier,
	  field : List[String],
	  p: Int, 
	  pd : P, 
	  s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]
	) : P = {
	  	var id1 = accessSequenceOfFields(s.getVariableValue(id).getExpression(), field, s);
        val exprs = id1.getExpressions();
        var perm=pd;
        for(exp <- exprs.elements )
	    	perm=perm.exhale(exp.asInstanceOf[Identifier], p);
	    return perm;
	}
 
 	private def exhaleReachable[P <: PermissionsDomain[P]](id : Identifier, state : P, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier], s : SymbolicValue) : P = {
 	  var result=state;
 	  val addresses=if(state.isInstanceOf[AddressedDomain[ProgramPointHeapIdentifier]]) state.asInstanceOf[AddressedDomain[ProgramPointHeapIdentifier]].getAddresses++store.getAddresses++env.getAddresses; else store.getAddresses++env.getAddresses;  
 	  val it=addresses.elements;
 	  while(it.hasNext) { //the for(... <- ...) was not effective for debugging purposes :(
 		  it.next match {
 		    case x : VariableIdentifier => result=this.exhaleReachable(env.get(x), state, env, store, s);
 		    case x : FieldAndProgramPoint =>
		    	  this.reachable(id, x, env, store) match {
		    	    case (in :: path, true) => result=result.exhale(x, s.factory().setPath(new Path("this" :: path))); //To express the constraint on the local object
		    	    case (Nil, true) => //I'm not sure about that: result=result.exhale(x, s.factory().setPath(new Path(Nil)));
		    	    case _ => 
		    	  }
 		    case x : ProgramPointHeapIdentifier =>
          }
      }
 	  return result;
 	}
   	
  private def inhaleReachable[P <: PermissionsDomain[P]](id : Identifier, state : P, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier], s : SymbolicValue) : P = {
 	  var result=state;
 	  val addresses=if(state.isInstanceOf[AddressedDomain[ProgramPointHeapIdentifier]]) state.asInstanceOf[AddressedDomain[ProgramPointHeapIdentifier]].getAddresses++store.getAddresses++env.getAddresses; else store.getAddresses++env.getAddresses;
 	  val it=addresses.elements;
 	  while(it.hasNext) { //the for(... <- ...) was not effective for debugging purposes :(
 		  it.next match {
 		    case x : VariableIdentifier => result=this.inhaleReachable(env.get(x), state, env, store, s);
 		    case x : FieldAndProgramPoint =>
		    	  this.reachable(id, x, env, store) match {
		    	    case (in :: path, true) => result=result.inhale(x, s.factory().setPath(new Path("this" :: path))); //To express the constraint on the local object 
		    	    case (Nil, true) => //I'm not sure about that: result=result.inhale(x, s.factory().setPath(new Path(Nil)))
		    	    case _ =>
		    	  }
 		    case x : ProgramPointHeapIdentifier =>
          }
 	  }
 	  return result;
 	}
  
  private def reachable(from : Identifier, to : ProgramPointHeapIdentifier, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) : (List[String], Boolean)= from match {
    case x : VariableIdentifier => 
    	for(hi <- env.get(x).value) {
    		reachable(hi, to, env, store) match {
    		  case (path, true) => return (x.toString()::path, true);
    		  case _ =>
    		}
    		for((field, typ) <- from.getType().getPossibleFields())
    			reachable(new FieldAndProgramPoint(hi, field, typ), to, env, store) match {
    			  case (path, true) => return (x.toString()::field::path, true);
    			  case _ =>
    			}
    	}
    	return (Nil, false);
     
    case x : ProgramPointHeapIdentifier =>
      	if(x.equals(to)) return (Nil, true);
        val res=store.get(x).value
        if(res.contains(to)) return (Nil, true);
        for(resSingle <- res)
          this.isAccessibleThroughField(resSingle, to, env, store) match {
	          case Some(s) =>
	            return (s :: Nil, true);
	          case None =>
        }
	    for(hi <- res)
	    	reachable(hi, to, env, store) match {
	    	  case (path, true) => return (path, true);
	    	  case _ =>
	    	}
        return (Nil, false);
    case x : HeapIdAndSetDomain[ProgramPointHeapIdentifier] =>
      if(x.value.contains(to)) return (Nil, true);
      for(k <- x.value) {
        val res=store.get(k).value
        if(res.contains(to)) return (Nil, true);
	      else
	    	for(hi <- res)
	    		reachable(hi, to, env, store) match {
	    		  case (path, true) => return (path, true)
	    		  case _ =>
	    		}
      }
      return (Nil, false);
  }
  
  private def isAccessibleThroughField[I <: HeapIdentifier[I]](from : Identifier, to : ProgramPointHeapIdentifier, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) : Option[String] = {
    for((field, typ) <- from.getType().getPossibleFields()) {
      if(from.isInstanceOf[I] && from.asInstanceOf[I].extractField(from.asInstanceOf[I], field, typ).equals(to)) return Some(field);
    }
    return None;
  }
  
 	//private def exhaleSingleUnknown[P <: PermissionsDomain[P]](state : P, id : Identifier) : P = state.setMinimalPermissionLevel(id, 0);
 	//private def inhaleSingleUnknown[P <: PermissionsDomain[P]](state : P, id : Identifier) : P = state.setMaximalPermissionLevel(id, 100);
 
 	private def accessSequenceOfFields[P <: PermissionsDomain[P]](thisExpr : SymbolicAbstractValue[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]], fields : List[String], s : GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]) : SymbolicAbstractValue[GenericAbstractState[P, NonRelationalHeapDomain[ProgramPointHeapIdentifier], HeapIdAndSetDomain[ProgramPointHeapIdentifier]]] = fields match {
 	  case Nil => thisExpr;
 	  case "this" :: x1 => accessSequenceOfFields(thisExpr, x1, s); 
 	  case x :: x1 => accessSequenceOfFields(s.getFieldValue(thisExpr :: Nil, x, null).getExpression(), x1, s);
 	}
 	
 	private def convertFieldAccessToListStrings(fieldAccess : FieldAccess) : List[String] = fieldAccess match {
 	  case FieldAccess(pp, objs, field, typ) => objs match {
 	    case Variable(pp, name) :: Nil => name.getName() :: field :: Nil
 	    case x :: Nil if x.isInstanceOf[FieldAccess] => convertFieldAccessToListStrings(x.asInstanceOf[FieldAccess]) ::: field :: Nil
 	  }
    }
  
}
