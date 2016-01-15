package ch.ethz.inf.pm.sample.permissionanalysis

import breeze.linalg.SparseVector
import breeze.optimize.linear._

/** Arithmetic expressions.
  *
  * @author Caterina Urban
  */
sealed trait ArithmeticExpression

/** Simple numerical value. */
case class Value(value : Double) extends ArithmeticExpression {
  override def toString: String = value.toString
}

/** Multiplication of a symbolic value. */
case class Mul(mul : Int, right : SymbolicValue) extends ArithmeticExpression {
  override def toString: String = mul.toString + "*" + right.toString
}

/** Addition of arithmetic expressions. */
case class Add(left : ArithmeticExpression, right : ArithmeticExpression) extends ArithmeticExpression {
  override def toString: String = left.toString + "+" + right.toString
}

/** Constraints.
  *
  * @author Caterina Urban
  */
sealed trait Constraint

/** Equality constraint. */
case class Eq(left : ArithmeticExpression, right : ArithmeticExpression) extends Constraint {
  override def toString = left.toString + "=" + right.toString
}

/** Greater than or equal to constraint. */
case class Geq(left : ArithmeticExpression, right : ArithmeticExpression) extends Constraint {
  override def toString = left.toString + ">=" + right.toString
}

/** Greater than constraint. */
case class Grt(left : ArithmeticExpression, right : ArithmeticExpression) extends Constraint {
  override def toString = left.toString + ">" + right.toString
}

/** Access permissions.
  *
  * @author Caterina Urban
  */
sealed trait PermissionsType {
  def ensureWrite(level : ArithmeticExpression) : Constraint = new Eq(level, new Value(maxLevel))
  def ensureRead(level : ArithmeticExpression) : Constraint = new Grt(level, new Value(minLevel))
  def maxLevel : Double
  def minLevel : Double
  def permissionToString(value : Double) : String = value.toString
}

/** Fractional permissions. */
case object FractionalPermission extends PermissionsType {
  override def maxLevel: Double = 1
  override def minLevel: Double = 0
}

/** Access permission constraints solver.
  *
  * @author Caterina Urban
  */
object PermissionSolver {

  /** Considered access permissions. */
  var permissionType : PermissionsType = FractionalPermission
  /** Smallest permission value. */
  private val epsilon : Double = 0.1

  /** Set of constraints to be solved. */
  private var constraints: Set[Constraint] = Set[Constraint]()
  /** Gets the current set of constraints. */
  def getConstraints : Set[Constraint] = constraints
  /** Adds a constraint to the current set of constraints. */
  def addConstraint(c : Constraint) = {
      constraints = constraints + c
  }

  /** Converts a SymbolicPermission into an ArithmeticExpression. */
  def convertSymbolicPermission(s: SymbolicPermission) : ArithmeticExpression =
    s.value.foldLeft(new Value(0) : ArithmeticExpression)(
      (exp, v) => new Add(exp, convertCountedSymbolicValue(v))
    )
  /** Converts a CountedSymbolicValue into an ArithmeticExpression. */
  def convertCountedSymbolicValue(s: CountedSymbolicValue) : ArithmeticExpression = {
    if (s.s == null) {
      new Value(s.n)
    } else {
      new Mul(s.n.toInt, s.s)
    }
  }

  /** Collects the SymbolicValues within an ArithmeticExpression */
  private def extractSymbolicValues(a: ArithmeticExpression): Set[SymbolicValue] = a match {
    case Value(value) => Set[SymbolicValue]()
    case Mul(mul, right) => Set[SymbolicValue](right)
    case Add(left, right) => extractSymbolicValues(left) ++ extractSymbolicValues(right)
  }
  /** Collects the SymbolicValues within a Constraint */
  private def extractSymbolicValues(c: Constraint): Set[SymbolicValue] = c match {
    case Eq(left, right) => extractSymbolicValues(left) ++ extractSymbolicValues(right)
    case Geq(left, right) => extractSymbolicValues(left) ++ extractSymbolicValues(right)
    case Grt(left, right) => extractSymbolicValues(left) ++ extractSymbolicValues(right)
  }
  /** Collects the SymbolicValues within a set of Constraints */
  private def extractSymbolicValues(set: Set[Constraint]): Set[SymbolicValue] = {
    var variables = Set[SymbolicValue]()
    for(c : Constraint <- set)
      variables = variables ++ extractSymbolicValues(c)
    variables
  }

  /** Converts an ArithmeticExpression into a LinearProgram#Expression (accepted by Breeze) */
  private def extractExpression(lp: LinearProgram)(vars:  Map[SymbolicValue,lp.Variable], a: ArithmeticExpression) : lp.Expression = a match {
    case Value(value) => throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
    case Mul(mul, right) => vars(right) * mul
    case Add(left, right) => (left, right) match {
      case (l: Value, r: Value) =>
        throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
      case (l: Value, r: ArithmeticExpression) => extractExpression(lp)(vars,r).asInstanceOf[lp.Expression] + l.value
      case (l: ArithmeticExpression, r: Value) => extractExpression(lp)(vars,l).asInstanceOf[lp.Expression] + r.value
      case _ => extractExpression(lp)(vars,left).asInstanceOf[lp.Expression] + extractExpression(lp)(vars,right).asInstanceOf[lp.Expression]
    }
  }
  /** Converts a Constraint into a LinearProgram#Constraint (accepted by Breeze) */
  private def extractConstraint(lp: LinearProgram)(vars: Map[SymbolicValue,lp.Variable], c: Constraint) : Set[lp.Constraint] = c match {
    case Eq(left, right) => (left, right) match {
      case (l: Value, r: Value) =>
        throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
      case (l: Value, r: ArithmeticExpression) =>
        val e = extractExpression(lp)(vars,r)
        Set[lp.Constraint]((e >= l.value).asInstanceOf[lp.Constraint], (e <= l.value).asInstanceOf[lp.Constraint])
      case (l: ArithmeticExpression, r: Value) =>
        val e = extractExpression(lp)(vars,l)
        Set[lp.Constraint]((e >= r.value).asInstanceOf[lp.Constraint], (e <= r.value).asInstanceOf[lp.Constraint])
      case _ =>
        val l = extractExpression(lp)(vars,left)
        val r = extractExpression(lp)(vars,right)
        Set[lp.Constraint]((l >= r).asInstanceOf[lp.Constraint], (l <= r).asInstanceOf[lp.Constraint])
    }
    case Geq(left, right) => (left, right) match {
      case (l: Value, r: Value) =>
        throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
      case (l: Value, r: ArithmeticExpression) =>
        val e = extractExpression(lp)(vars,r)
        Set[lp.Constraint]((e >= l.value).asInstanceOf[lp.Constraint])
      case (l: ArithmeticExpression, r: Value) =>
        val e = extractExpression(lp)(vars,l)
        Set[lp.Constraint]((e >= r.value).asInstanceOf[lp.Constraint])
      case _ =>
        val l = extractExpression(lp)(vars,left)
        val r = extractExpression(lp)(vars,right)
        Set[lp.Constraint]((l >= r).asInstanceOf[lp.Constraint])
    }
    case Grt(left, right) => (left, right) match {
      case (l: Value, r: Value) =>
        throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
      case (l: Value, r: ArithmeticExpression) =>
        val e = extractExpression(lp)(vars,r)
        Set[lp.Constraint]((e - l.value + epsilon <= 0).asInstanceOf[lp.Constraint])
      case (l: ArithmeticExpression, r: Value) =>
        val e = extractExpression(lp)(vars,l)
        Set[lp.Constraint]((e - r.value >= epsilon).asInstanceOf[lp.Constraint])
      case _ =>
        val l = extractExpression(lp)(vars,left)
        val r = extractExpression(lp)(vars,right)
        Set[lp.Constraint]((l - r >= epsilon).asInstanceOf[lp.Constraint])
    }
  }

  /** Solves a set of constraints. */
  def solve(constraints: Set[Constraint]) : Map[SymbolicValue,Double] = {

    val lp = new LinearProgram()  // linear program
    var result = Map[SymbolicValue, Double]() // result

    // array of symbolic values
    val sym : Array[SymbolicValue] = this.extractSymbolicValues(constraints).toArray
    // map from symbolic values to corresponding LinearProgram#Variable (accepted by Breeze)
    val symToVars : Map[SymbolicValue,lp.Variable] = sym.foldLeft(Map[SymbolicValue,lp.Variable]())(
      (m, s) => m + (s -> lp.Real(s.toString))
    )

    // list of values corresponding to symbolic variables
    val vars = symToVars.values.toList
    if (vars.length > 0) {
      // objective function: the sum of the variables
      val obj = vars.drop(1).foldLeft(vars.head: lp.Expression)(
        (exp: lp.Expression, v: lp.Variable) => exp + v
      )

      // lp problem
      var prob = obj.subjectTo()
      // adding variable bounds
      for (v <- vars) {
        prob = prob.subjectTo(v >= 0).subjectTo(v <= 1)
      }
      // adding constraints
      for (c <- constraints) {
        for (s <- extractConstraint(lp)(symToVars, c)) {
          prob = prob.subjectTo(s.asInstanceOf[lp.Constraint])
        }
      }
      // solving lp problem
      val res = lp.minimize(prob).result

      // computing the result
      for (i <- 1 to sym.length) {
        result = result + (sym(i - 1) -> res.valueAt(i - 1))
      }; result
    } else result
  }

  /** Converts a Double to a rational number string representation. */
  def doubleToRational(d: Double) : String = {

    def gcd(a: Int,b: Int): Int = if (b == 0) a else gcd(b, a % b)

    val s = d.toString
    val n = s.length - 1 - s.indexOf('.')
    var r = d
    var den = 1
    for (i <- 1 to n) {
      r = r * 10
      den = den * 10
    }
    var num = Math.round(r).toInt

    var g = gcd(num,den)
    while (g != 1) {
      num = num / g
      den = den / g
      g = gcd(num,den)
    }

    if (den != 1)
      num.toString + "/" + den.toString
    else
      num.toString
  }

  // Temporary main method (used to experiment).
  def main(args:Array[String]) = {

    val lp = new LinearProgram()

    val x = lp.Real("x")
    val y = lp.Real("y")

    val obj : lp.Expression = x + y // objective function
    var prob = obj.subjectTo() // problem
    prob = prob.subjectTo(x + y >= 2 : lp.Constraint) // adding constraint
    prob = prob.subjectTo(x >= 0) // adding constraint
    prob = prob.subjectTo(x <= 1) // adding constraint
    prob = prob.subjectTo(y >= 0) // adding constraint
    prob = prob.subjectTo(y <= 1) // adding constraint

    val res = lp.minimize(prob).result // solving

    println("Result:")
    println(x.name + ": " + res.valueAt(0))
    println(y.name + ": " + res.valueAt(1))

    println("\nDone.")

    val z = new SymbolicPrecondition(new Path(List("x")))
    val w = new SymbolicPrecondition(new Path(List("y")))
    val cz = Grt(Mul(1,z),Value(0))
    val cw = Grt(Mul(1,w),Value(0))
    val c = Grt(Add(Mul(1,z),Mul(1,w)),Value(0))
    val s = Set[Constraint](c,cz,cw)
    val result = solve(s)
    println(result.mapValues((d) => doubleToRational(d)))
  }

}
