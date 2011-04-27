package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.oorepresentation._;
import ch.ethz.inf.pm.sample.abstractdomain._;
import lpsolve._;

object Settings {

  val lowestValue : Double = 0.1;

	var unsoundInhaling : Boolean = false;
	var unsoundDischarging : Boolean = false;
	var permissionType : PermissionsType = null;
	
	
	  
  //The lower integer value, the higher priority
  var priorityContracts : Int = 3;
  var priorityInvariants : Int = 1;
  var priorityPredicates : Int = 2;
}


sealed trait PermissionsType {
	def ensureWriteLevel(level : ArithmeticExpression) : Constraint;
	def ensureReadLevel(level : ArithmeticExpression) : Constraint;
	def maxLevel : Int;
	def minLevel : Int;
  def float : Boolean;
  def epsilon : Boolean;
}

case object FractionalPermissions extends PermissionsType {
	override def ensureWriteLevel(level : ArithmeticExpression) : Constraint = new Eq(level, new SimpleVal(1));
	override def ensureReadLevel(level : ArithmeticExpression) : Constraint = new Greater(level, new SimpleVal(0));
	override def maxLevel : Int = 1
	override def minLevel : Int = 0
  override def float : Boolean = true;
  override def epsilon : Boolean = false;
}

case object CountingPermissions extends PermissionsType {
	override def ensureWriteLevel(level : ArithmeticExpression) : Constraint = new Geq(new SimpleVal(0), level);
	override def ensureReadLevel(level : ArithmeticExpression) : Constraint = new Geq(level, new SimpleVal(0));
	override def maxLevel : Int = 0
	override def minLevel : Int = -1000//TODO:What should be there?
  override def float : Boolean = false;
  override def epsilon : Boolean = false;
}

case object ChalicePermissions extends PermissionsType {
	override def ensureWriteLevel(level : ArithmeticExpression) : Constraint = new Eq(level, new SimpleVal(100));
	override def ensureReadLevel(level : ArithmeticExpression) : Constraint = new Geq(level, new Multiply(1, Epsilon));
	override def maxLevel : Int = 100
	override def minLevel : Int = 0
  override def float : Boolean = true;
  override def epsilon : Boolean = true;
}



sealed trait Constraint

case class Eq(left : ArithmeticExpression, right : ArithmeticExpression) extends Constraint {
  override def toString()=left.toString()+"="+right.toString();
}

case class Geq(left : ArithmeticExpression, right : ArithmeticExpression) extends Constraint {
  override def toString()=left.toString()+">="+right.toString();
}

case class Greater(left : ArithmeticExpression, right : ArithmeticExpression) extends Constraint {
  override def toString()=left.toString()+">"+right.toString();
}


sealed abstract trait ArithmeticExpression 

case class Add(left : ArithmeticExpression, right : ArithmeticExpression) extends Constraint with ArithmeticExpression {
  override def toString()=left.toString()+"+"+right.toString();
}

case class SimpleVal(value : Int) extends Constraint with ArithmeticExpression {
  override def toString()=value.toString();
}

case class Multiply(mul : Int, right : SymbolicValue) extends Constraint with ArithmeticExpression {
  override def toString()=mul.toString()+"*"+right.toString();
}

object ConstraintsInference {
  private var constraints : Set[Constraint] = Set.empty[Constraint];

  
  type Permissions = SymbolicPermissionsDomain[ProgramPointHeapIdentifier]
  type HeapId = HeapIdAndSetDomain[ProgramPointHeapIdentifier];
  type HeapDomain = NonRelationalHeapDomain[ProgramPointHeapIdentifier];
  type State = GenericAbstractState[Permissions, HeapDomain, HeapId];
  
  def emptyConstraints() = constraints=Set.empty[Constraint] 
  
  def getConstraints() = constraints;
  
  def addConstraint(c : Constraint) = {
    if(SystemParameters.semanticsComputing)
      constraints=constraints+c;
  }
  def convert(s : SymbolicLevelPermission) : ArithmeticExpression = this.convert(s.value)
  
  def convert(s : Set[CountedSymbolicValues]) : ArithmeticExpression = {
    if(s.isEmpty) return new SimpleVal(0);
    var result : ArithmeticExpression = null;
    for(value <- s)
      if(result==null)
        result=this.convert(value);
      else result=new Add(result, this.convert(value));
    return result;
  }
  
  def convert(s : CountedSymbolicValues) : ArithmeticExpression = s.s match {
    case null => s.n match {
      case WrappedInt(i) => return new SimpleVal(i);
      case _ => throw new PermissionsException("I should consider also *");
      }
    case symbolicvalue => s.n match {
      case WrappedInt(i) => return new Multiply(i, symbolicvalue);
      case _ => throw new PermissionsException("I should consider also *");
      }
  }
  
  def addPostconditionConstraints(s : State, className : Type, methodName : String) : Unit = this.addPostconditionConstraints(s._1._1, className.getName(), methodName, s._1._2._1, s._1._2._2); 
  
  def addPostconditionConstraints(p : Permissions, classe : String, method : String, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) = {
    for(variable <- p.value.keySet) {
      val expr = this.convert(p.get(variable));
      
      //I removed this check because in this way I avoid to consider local variables
      //if(variable.isInstanceOf[VariableIdentifier])
      //  constraints=constraints+new Eq(new Multiply(1, new SymbolicPostCondition(classe, method, new Path(variable.toString() :: Nil))), expr);
      //else 
      if(! variable.isInstanceOf[VariableIdentifier] && (Settings.unsoundDischarging || (variable.representSingleVariable)))
    	  reach(variable, env, store) match {
	        case None =>
	        case Some(s :: Nil) =>
	          val string : List[String]=this.statementToListString(s);
	          constraints=constraints+new Eq(new Multiply(1, new SymbolicPostCondition(classe, method, new Path(string))), expr);
	      }
    }
  }
  
  private def statementToListString(s : Statement) : List[String] = s match {
    //case x : VariableIdentifier => return x.toString() :: Nil
    case Variable(pp, id) => return id.toString() :: Nil
    case x : FieldAccess =>
      if(x.objs.size != 1) throw new PermissionsException("This should NEVER happens HERE!");
      else return statementToListString(x.objs.elements.next) ::: x.field :: Nil; 
  }
  
  def printConstraints() = {
    SystemParameters.analysisOutput.appendString("INFERRED CONSTRAINTS\n---------------------\n\n");
    for(c <- constraints)
      SystemParameters.analysisOutput.appendString(c.toString());
  }
  
  def printConstraints(constraints : Set[Constraint]) = {
    SystemParameters.analysisOutput.appendString("INFERRED CONSTRAINTS\n---------------------\n\n");
    for(c <- constraints)
      SystemParameters.analysisOutput.appendString(c.toString());
  }
  
  private object Log extends LogListener with AbortListener with MsgListener with BbListener {
   def logfunc(problem : LpSolve, userhandle : Object, buf : String) : Unit = {}
   def abortfunc(problem : LpSolve, userhandle : Object) : Boolean = true
   def bbfunc(problem : LpSolve, userhandle : Object, buf : Int) : Int = 0
   def msgfunc(problem : LpSolve, userhandle : Object, buf : Int) : Unit = {}
   }
  def solve(constraints : Set[Constraint]) : Map[SymbolicValue, (Double, Int)] = {
      try {
        val vars : List[SymbolicValue] = this.extractVariables(constraints).toList;
        val solver : LpSolve= LpSolve.makeLp(0, vars.size);
          
        for(i <- 1 to vars.size) {
        	if(Settings.permissionType.epsilon && vars.apply(i-1).equals(Epsilon)) {
        		val min = 0.00001//Double.Epsilon
        		val max = 1 //Because in this case the Epsilon would be "lost" (because of floating point rounding) and anyway there will be a stricter constraints afterwards
        		solver.setBounds(i, min, max);
        	}
        	else {
        		if(! Settings.permissionType.float) solver.setInt(i, true); //All permissions are integer values
        		solver.setBounds(i, Settings.permissionType.minLevel, Settings.permissionType.maxLevel);
        	}
        }
        //This is just to shutdown output on stdout and it does not work!!!
        /*solver.putLogfunc(Log, Log);
        solver.putAbortfunc(Log, Log);
        solver.putMsgfunc(Log, Log, LpSolve.MSG_INVERT | LpSolve.MSG_ITERATION | LpSolve.MSG_LPBETTER | LpSolve.MSG_LPEQUAL | LpSolve.MSG_LPFEASIBLE | LpSolve.MSG_LPOPTIMAL | LpSolve.MSG_MILPBETTER
         | LpSolve.MSG_MILPEQUAL | LpSolve.MSG_MILPFEASIBLE | LpSolve.MSG_MILPOPTIMAL | LpSolve.MSG_MILPSTRATEGY | LpSolve.MSG_NONE | LpSolve.MSG_PERFORMANCE | LpSolve.MSG_PRESOLVE | LpSolve.MSG_INITPSEUDOCOST);
        solver.putBbBranchfunc(Log, Log);*/

        this.addConstraints(constraints, vars, solver);
        
        this.addObjectiveFunction(vars, solver);
        
        if(Settings.permissionType.epsilon) this.addEpsilonConstraint(vars, solver);
        solver.printLp();
        solver.setMinim();
        // solve the problem
        solver.solve();
        val obj=solver.getObjective();
        // print solution
        //SystemParameters.analysisOutput.appendString("Value of objective function: " + solver.getObjective());
          
        if(solver.getObjective>=1.0E30) {//I don't know way, but lpsolve returns 1.0E30 when minimizing an unfeasible model
          SystemParameters.analysisOutput.appendString("The system is unfeasible, so we cannot infer access permissions for the given program");
          return null;
        }
        else {
	        val variables : Array[Double]= solver.getPtrVariables();
	        assert(variables.size==vars.size);
	        var result : Map[SymbolicValue, (Double, Int)] = Map.empty;
	        if(Settings.permissionType.epsilon) {
            val epsilon = this.extractEpsilon(vars, variables)
            for (i <- 0 to variables.length-1) {
              if(variables(i)>0) {
                val (intPart, nEpsilons)=stringWithEpsilon(variables(i), epsilon)
                SystemParameters.analysisOutput.appendString("Value of " + vars.apply(i) + " = " + intPart + " + "+nEpsilons+"epsilon (float value:"+variables(i)+")");
                result=result+((vars.apply(i), (intPart, nEpsilons)));
               }
            }
          }
          else
            for (i <- 0 to variables.length-1) {
              if(variables(i)>0) {
                SystemParameters.analysisOutput.appendString("Value of " + vars.apply(i) + " = " + clean(variables(i)));
                result=result+((vars.apply(i), (variables(i), 0)));
               }
            }
	        // delete the problem and free memory
	        solver.deleteLp();
	        return result;
        }
    }
    catch {
      case e => e.printStackTrace(); return null;
    }
    
  }

  //Clean imprecision due to floating approximation in LP solving
  private def clean(d : Double) : Double = {
     if(d % Settings.lowestValue != 0) {
       if(d % Settings.lowestValue>Settings.lowestValue/2)
         return d+(Settings.lowestValue-d % Settings.lowestValue)
       else return d-d % Settings.lowestValue
     }
    else return d;
  }
  
  def stringWithEpsilon(value : Double, epsilon : Double) : (Int, Int) = {
	  var intPart : Int = value.toInt;
	  var floatPart : Double = value-(intPart.toDouble);
	  var nEpsilons : Int =0;
	  if(floatPart>0.5) {
	 	  nEpsilons=0-getNEpsilons(1-floatPart, epsilon);
	 	  intPart=1+intPart;
	  }
	  else {  
	   nEpsilons=getNEpsilons(floatPart, epsilon);
	  }
	  return (intPart, nEpsilons);
  }
  
  private def getNEpsilons(v : Double, epsilon : Double) : Int = {
	   var nEpsilons : Int = (v/epsilon).toInt;
	   var approximation : Double = v-(nEpsilons.toFloat*epsilon) 
	   if(Math.abs(approximation)>epsilon/2) {
	 	  if(approximation>0)
	 	 	  nEpsilons=nEpsilons+1
	      else nEpsilons=nEpsilons-1
	   }
	   return nEpsilons;
  }
  
  private def extractEpsilon(vars : List[SymbolicValue], variables : Array[Double]) : Double = {
	     for(i <- 0 to vars.size-1)
        	if(vars.apply(i).equals(Epsilon))
        		return variables.apply(i);
	     return 0;
  }
      
  private def produceString(i : Int, size : Int) : String = {
    var result : String = "";
    for(j <- 0 to size) 
      if(j==i) result=result+"1 ";
      else result=result+"0 ";
    return result;
  }
  
  private def addConstraints(constraints : Set[Constraint], variables : List[SymbolicValue], solver : LpSolve) = {
    for(c <- constraints) {
      val (s, i, op) = this.extractConstraint(c, variables);
      solver.strAddConstraint(s, op, i);
    } 
  }

  private def addObjectiveFunction(variables : List[SymbolicValue], solver : LpSolve) = {
    var s : String = "";
    for(v <- variables) {
      v match {
        case x : SymbolicMonitorInvariant => s=s+Settings.priorityInvariants.toString()+" ";
        case x : SymbolicAbstractPredicates => s=s+Settings.priorityPredicates.toString()+" ";
        case x : SymbolicPreCondition => s=s+Settings.priorityContracts.toString()+" ";
        case x : SymbolicPostCondition => s=s+Settings.priorityContracts.toString()+" ";
        case x if x.equals(Epsilon) => s=s+"-2 ";
      }
    } 
    solver.strSetObjFn(s);
  }
  
  private def addEpsilonConstraint(variables : List[SymbolicValue], solver : LpSolve) = {
    var s : String = "";
    for(i <- 0 to variables.length-1) {
      variables.apply(i) match {
        case x if x.equals(Epsilon) => {
        	//We impose that (2*maxIndex(Epsilon)+1)*Epsilon<=1
        	//We take 2* to understand if we have to add or subtract epsilons, +1 to impose < instead of <= 
        	SystemParameters.analysisOutput.appendString("Massimo epsilon "+this.getMax(i+1, solver));
        	val ind=2*this.getMax(i+1, solver)+1; 
        	s=s+ind.toString()+" ";
        	}
        case _ => s=s+"0 ";
      }
    } 
    solver.strAddConstraint(s, LpSolve.LE, 1);
  }
  
  private def getMax(column : Int, solver : LpSolve) : Int = {
	  var max = 0;
	  for(i <- 0 to solver.getNrows()) {
	 	  max=Math.max(max, Math.abs(solver.getMat(i+1, column).toInt));
	  }
	  return max;
  }

  private def extractConstraint(constraint : Constraint, variables : List[SymbolicValue]) : (String, Double, Int) = constraint match {
    case Greater(left, right) =>
      val (leftvars, lefti) = extractExpressions(left, variables);
      val (rightvars, righti) = extractExpressions(right, variables);
      var resultingarray = subtractArrays(leftvars, rightvars);
      return (arrayToString(resultingarray), righti-lefti+Settings.lowestValue, LpSolve.GE)//TODO: This is wrong!
    case Geq(left, right) =>
      val (leftvars, lefti) = extractExpressions(left, variables);
      val (rightvars, righti) = extractExpressions(right, variables);
      var resultingarray = subtractArrays(leftvars, rightvars);
      return (arrayToString(resultingarray), righti-lefti, LpSolve.GE)
    case Eq(left, right) =>
      val (leftvars, lefti) = extractExpressions(left, variables);
      val (rightvars, righti) = extractExpressions(right, variables);
      var resultingarray = subtractArrays(leftvars, rightvars);
      return (arrayToString(resultingarray), righti-lefti, LpSolve.EQ)
  }
  
  private def arrayToString[T](array : Array[T]) : String = {
    var result : String = "";
    for(i <- 0 to array.size-1)
      result=result+array(i).toString()+" ";
    return result;
  }
  
  private def extractExpressions(a : ArithmeticExpression, vars : List[SymbolicValue]) : (Array[Int], Int) = a match {
    case Add(left, right) =>
      val (a1, i1) = this.extractExpressions(left, vars);
      val (a2, i2) = this.extractExpressions(right, vars);
      return (addArrays(a1, a2), i1-i2);
    case SimpleVal(value) => return (new Array[Int](vars.size), value)
    case Multiply(mul, right) => 
      val result = new Array[Int](vars.size);
      result(vars.indexOf(right))=mul;
      return (result, 0);
  }

  private def addArrays(a1 : Array[Int], a2 : Array[Int]) : Array[Int]= {
    assert(a1.size==a2.size);
    val result = new Array[Int](a1.size);
    for(i <- 0 to a1.size-1)
      result(i)=a1(i)+a2(i);
    return result;
  }
  
  private def subtractArrays(a1 : Array[Int], a2 : Array[Int]) : Array[Int]= {
    assert(a1.size==a2.size);
    val result = new Array[Int](a1.size);
    for(i <- 0 to a1.size-1)
      result(i)=a1(i)-a2(i);
    return result;
  }
  
  private def extractVariables(set : Set[Constraint]) : Set[SymbolicValue] = {
    var variables = Set.empty[SymbolicValue];
    for(c <- set)
      variables=variables++extractVariables(c);
    return variables
  }
  
  private def extractVariables(c : Constraint) : Set[SymbolicValue] = c match {
    case Eq(left, right) => return extractVariables(left)++extractVariables(right)
    case Geq(left, right) => return extractVariables(left)++extractVariables(right)
    case Greater(left, right) => return extractVariables(left)++extractVariables(right)
  }
  
  private def extractVariables(a : ArithmeticExpression) : Set[SymbolicValue] = a match {
    case Add(left, right) => return extractVariables(left)++extractVariables(right)
    case SimpleVal(value) => Set.empty[SymbolicValue]
    case Multiply(mul, right) => Set.empty[SymbolicValue]+right
  }
  
  
  def giveLoopInvariants(exs : Iterator[ControlFlowGraphExecution[State]], substitution : Map[SymbolicValue, (Double, Int)]) : Map[ProgramPoint, Map[Statement, (Double, Int)]] = {
    var result : Map[ProgramPoint, Map[Statement, (Double, Int)]] = Map.empty;
    for(ex <- exs) {                                                                                        
	    for(i <- 0 to ex.cfg.nodes.length-1) {
	      if(ex.cfg.initialBlockInLoop(i)) {
	        val state = ex.nodes.apply(i).first;
	        val pp = ex.cfg.nodes.apply(i).first.getPC();
	        result=result+((pp, inferPermissions(state, substitution)));
	      }
	    }
    }
    return result;
  }
  
  def inferPermissions(state : State, substitution : Map[SymbolicValue, (Double, Int)]) : Map[Statement, (Double, Int)] = {
    val ownedPermissions : Permissions=state._1._1;
    val environment =state._1._2._1
    val store =state._1._2._2
    
    var result : Map[Statement, (Double, Int)]=Map.empty;
    for(el <- ownedPermissions.value.keySet) 
      reach(el, environment, store) match {
        case Some( (x : Variable) :: Nil) => result=result+((x, this.symbolicPermissionToInt(ownedPermissions.get(el), substitution)));
        case Some(x) => 
          for(field <- selectFieldAccesses(x)) {
	        if(result.keys.iterator.contains(field)) {
	        	val (r1, r2)=this.symbolicPermissionToInt(ownedPermissions.get(el), substitution)
	        	val (v1, v2)=result.get(field).get
	        	result=result+((field, (Math.max(r1, v1), Math.max(r2, v2))));
	        }
	        else result=result+((field, this.symbolicPermissionToInt(ownedPermissions.get(el), substitution)));
	       }
        case None =>
      }
    return result;
  }
  
  def symbolicPermissionToInt(level : SymbolicLevelPermission, substitution : Map[SymbolicValue, (Double, Int)]) : (Double, Int) = {
    var result1 : Double = 0;
    var result2 : Int = 0;
    for(el <- level.value) {
      if(el.n==Top) throw new PermissionsException("I can't compute the loop invariant with top values");
      if(el.s==null) {
    	  result1=result1+el.n.asInstanceOf[WrappedInt].i;
    	  result2=result2+el.n.asInstanceOf[WrappedInt].i;
      }
      	else if(substitution.keySet.contains(el.s)) {
      			result1=result1+el.n.asInstanceOf[WrappedInt].i*substitution(el.s)._1;
      			result2=result2+el.n.asInstanceOf[WrappedInt].i*substitution(el.s)._2;
      	}
    }
    return (result1, result2);
  }
  
  
  private def selectFieldAccesses( s : List[Statement]) : List[FieldAccess] = s match {
    case (x : FieldAccess) :: x1 => return x :: selectFieldAccesses(x1)
    case x :: x1 => return selectFieldAccesses(x1)
    case Nil => return Nil
  }
  
    /**
   * Given an id (usually a set of heap identifiers), the state of the environment and of the heap, it returns None if the id is not
   * reachable, or a list field accesses that can be used to reach it otherwise
   */
  def reach(id : Identifier, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) : Option[List[Statement]]= id match {
    case x : VariableIdentifier => Some(new Variable(null, x) :: Nil)
    case x : HeapIdAndSetDomain[ProgramPointHeapIdentifier] => 
      var result : List[Statement] = Nil;
      var something : Boolean=false;
      for(v <- x.value)
        v match {
          case FieldAndProgramPoint(pp, field, typ) => reach1(pp, env, store) match {
            case Some(x) => something=true; result=FieldAccess(null, x :: Nil, field, typ) :: result;
            case None =>
          }
          case v1 : SimpleProgramPointHeapIdentifier => reach1(v1, env, store) match {
            case Some(x) => something=true; result=x :: result;
            case None =>
          }
          case _ =>
        }
      if(something) 
        Some(result); 
      else None;
    case x : ProgramPointHeapIdentifier => 
      var result : List[Statement] = Nil;
      var something : Boolean=false;
      x match {
        case FieldAndProgramPoint(pp, field, typ) => reach1(pp, env, store) match {
          case Some(x) => something=true; result=FieldAccess(null, x :: Nil, field, typ) :: result;
          case None =>
        }
        case v1 : SimpleProgramPointHeapIdentifier => reach1(v1, env, store) match {
          case Some(x) => something=true; result=x :: result;
          case None =>
        }
        case _ =>
      }
      if(something) 
        Some(result); 
      else None;
  }
  
  /**
   * Given an heap identifier, it returns a field access that can be used to reach it
   */
  def reach1(id : ProgramPointHeapIdentifier, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) : Option[Statement]= {
    for(k <- env.value.keySet)
      if(env.get(k).value.contains(id)) 
        return Some(new Variable(null, k));
    for(k <- store.value.keySet)
      if(store.get(k).equals(id)) 
        k match {
          case x : SimpleProgramPointHeapIdentifier => 
            return reach1(x, env, store);
          case FieldAndProgramPoint(pp, field, typ) => reach1(pp, env, store) match {
            case Some(x) => return Some(new FieldAccess(null, x :: Nil, field, null));
            case None => 
        }
    }    
    return None;
  }
  
}
