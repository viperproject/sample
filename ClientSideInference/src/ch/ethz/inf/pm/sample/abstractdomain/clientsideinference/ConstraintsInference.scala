package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._;
import ch.ethz.inf.pm.sample.abstractdomain._;
import lpsolve._;

sealed trait Constraint[S <: SymbolicValue[S]]

case class Eq[S <: SymbolicValue[S]](left : ArithmeticExpression[S], right : ArithmeticExpression[S]) extends Constraint[S] {
  override def toString()=left.toString()+"="+right.toString();
}

case class Geq[S <: SymbolicValue[S]](left : ArithmeticExpression[S], right : ArithmeticExpression[S]) extends Constraint[S] {
  override def toString()=left.toString()+">="+right.toString();
}

case class Greater[S <: SymbolicValue[S]](left : ArithmeticExpression[S], right : ArithmeticExpression[S]) extends Constraint[S] {
  override def toString()=left.toString()+">"+right.toString();
}


sealed abstract trait ArithmeticExpression[S <: SymbolicValue[S]]

case class Add[S <: SymbolicValue[S]](left : ArithmeticExpression[S], right : ArithmeticExpression[S]) extends Constraint[S] with ArithmeticExpression[S] {
  override def toString()=left.toString()+"+"+right.toString();
}

case class SimpleVal[S <: SymbolicValue[S]](value : Int) extends Constraint[S] with ArithmeticExpression[S] {
  override def toString()=value.toString();
}

case class Multiply[S <: SymbolicValue[S]](mul : ArithmeticExpression[S], right : S) extends Constraint[S] with ArithmeticExpression[S] {
  override def toString()=mul.toString()+"*"+right.toString();
}

class ConstraintsInference[S <: SymbolicValue[S], T <: SymbolicInt[T, S]] {
  private var constraints : Set[Constraint[S]] = Set.empty[Constraint[S]];

  def emptyConstraints() = constraints=Set.empty[Constraint[S]] 
  
  def getConstraints() = constraints;
  
  def addConstraint(c : Constraint[S]) = {
      constraints=constraints+c;
  }

  
  def printConstraints() = {
    SystemParameters.analysisOutput.appendString("INFERRED CONSTRAINTS\n---------------------\n\n");
    for(c <- constraints)
      SystemParameters.analysisOutput.appendString(c.toString());
  }
  
  def printConstraints(constraints : Set[Constraint[S]]) = {
    SystemParameters.analysisOutput.appendString("INFERRED CONSTRAINTS\n---------------------\n\n");
    for(c <- constraints)
      SystemParameters.analysisOutput.appendString(c.toString());
  }
/*
  def solve(constraints : Set[Constraint[S]]) : Map[SymbolicValue[S], Double] = {
      try {
        System.out.println(constraints.mkString("\n"))
        val vars : List[SymbolicValue[S]] = this.extractVariables(constraints).toList;
        val solver : LpSolve= LpSolve.makeLp(0, vars.size);

        this.addConstraints(constraints, vars, solver);

        this.addObjectiveFunction(vars, solver);

        solver.setMinim();
        //this.printSolverConstraints(solver, vars);
        // solve the problem
        solver.solve();
        val obj=solver.getObjective();
        // print solution
        //SystemParameters.analysisOutput.appendString("Value of objective function: " + solver.getObjective());

        if(solver.getObjective>=1.0E30) {//I don't know why, but lpsolve returns 1.0E30 when minimizing an unfeasible model
          SystemParameters.analysisOutput.appendString("The system is unfeasible, so we cannot infer access permissions for the given program");
          return null;
        }
        else {
	        val variables : Array[Double]= solver.getPtrVariables();
	        assert(variables.size==vars.size);
	        var result : Map[S, Double] = Map.empty;
          for (i <- 0 to variables.length-1) {
              if(variables(i)>0)
                result=result+((vars.apply(i), variables(i)));
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

  def printSolverConstraints(solver : LpSolve, vars : List[SymbolicValue[S]]) = {
    var arr : Array[Double] = new Array[Double](solver.getNcolumns()+1);
    val j : Int = 0;
    for(j <- 0 to solver.getNrows) {
      solver.getRow(j, arr)
      val i : Int = 0;
      for(i <- 1 to arr.length-1) {
        if(arr.apply(i)!= 0.0)
          System.out.print(arr.apply(i)+"*"+vars.apply(i-1));
      };
      solver.getConstrType(j) match {
        case LpSolve.GE => System.out.print(">=")
        case LpSolve.LE => System.out.print("<=")
        case LpSolve.EQ => System.out.print("==")
        case x => System.out.print("<UNKNOW OPERATOR "+x+">")
      }
      System.out.print(solver.getRh(j)+"\n");
    }
    //for(c <- solver.getConstraints())

  }

  //Clean imprecision due to floating approximation in LP solving
  def clean(d : Double) : Double = {
     if(d % Settings.lowestApproximation != 0) {
       if(d % Settings.lowestApproximation>Settings.lowestApproximation/2)
         return d+(Settings.lowestApproximation-d % Settings.lowestApproximation)
       else return d-d % Settings.lowestApproximation
     }
    else return d;
  }




  private def addConstraints(constraints : Set[Constraint[S]], variables : List[SymbolicValue[S]], solver : LpSolve) = {
    val l = constraints;
    for(c <- constraints) {
      val (s, i, op) = this.extractConstraint(c, variables);
      solver.strAddConstraint(s, op, i);
    }
  }

  private def addObjectiveFunction(variables : List[SymbolicValue[S]], solver : LpSolve) = {
    var s : String = "";
    for(v <- variables) {
      v match {
        case x => s=s+"-100000 ";
      }
    }
    solver.strSetObjFn(s);
  }

  private def getMax(column : Int, solver : LpSolve) : Int = {
	  var max = 0;
	  for(i <- 0 to solver.getNrows()) {
	 	  max=Math.max(max, Math.abs(solver.getMat(i+1, column).toInt));
	  }
	  return max;
  }

  private def extractConstraint(constraint : Constraint[S], variables : List[SymbolicValue[S]]) : (String, Double, Int) = constraint match {
    case Greater(left, right) =>
      val (leftvars, lefti) = extractExpressions(left, variables);
      val (rightvars, righti) = extractExpressions(right, variables);
      var resultingarray = subtractArrays(leftvars, rightvars);
      return (arrayToString(resultingarray), righti-lefti+Settings.lowestApproximation, LpSolve.GE)//TODO: This is wrong!
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

  private def extractExpressions(a : ArithmeticExpression[S], vars : List[SymbolicValue[S]]) : (Array[Int], Double) = a match {
    case Add(left, right) =>
      val (a1, i1) = this.extractExpressions(left, vars);
      val (a2, i2) = this.extractExpressions(right, vars);
      return (addArrays(a1, a2), i1+i2);
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

  private def extractVariables(set : Set[Constraint[S]]) : Set[SymbolicValue[S]] = {
    var variables = Set.empty[SymbolicValue[S]];
    for(c <- set)
      variables=variables++extractVariables(c);
    return variables
  }

  private def extractVariables(c : Constraint[S]) : Set[SymbolicValue[S]] = c match {
    case Eq(left, right) => return extractVariables(left)++extractVariables(right)
    case Geq(left, right) => return extractVariables(left)++extractVariables(right)
    case Greater(left, right) => return extractVariables(left)++extractVariables(right)
  }

  private def extractVariables(a : ArithmeticExpression[S]) : Set[SymbolicValue[S]] = a match {
    case Add(left, right) => return extractVariables(left)++extractVariables(right)
    case SimpleVal(value) => Set.empty[SymbolicValue[S]]
    case Multiply(mul, right) => Set.empty[SymbolicValue[S]]+right
  }
*/

}
