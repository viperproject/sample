package ch.ethz.inf.pm.sample.permissionanalysis

import breeze.optimize.linear._


/** Arithmetic expressions. */
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


/** Constraints. */
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


/** Access permissions. */
sealed trait PermissionsType {
  def ensureWrite(level : ArithmeticExpression) : Constraint
  def ensureRead(level : ArithmeticExpression) : Constraint
  def maxLevel : Double
  def minLevel : Double
  def float : Boolean
  def permissionToString(value : Double) : String
}

/** Fractional permissions. */
case object FractionalPermission extends PermissionsType {
  override def ensureWrite(level : ArithmeticExpression): Constraint = new Eq(level, new Value(1))
  override def ensureRead(level : ArithmeticExpression): Constraint = new Grt(level, new Value(0))
  override def maxLevel: Double = 1
  override def minLevel: Double = 0
  override def float: Boolean = true
  override def permissionToString(value : Double): String = value.toString
}


/** Access permission constraints solver. */
object PermissionSolver {

  /** Considered access permissions. */
  var permissionType : PermissionsType = FractionalPermission

  /** Set of constraints to be solved. */
  private var constraints: Set[Constraint] = Set[Constraint]()

  /** Gets the current set of constraints. */
  def getConstraints() : Set[Constraint] = constraints

  /** Adds a constraint to the current set of constraints. */
  def addConstraint(c : Constraint) = {
      constraints = constraints + c
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
  private def convertExpression(lp: LinearProgram)(vars:  Map[SymbolicValue,lp.Variable], a: ArithmeticExpression) : lp.Expression = a match {
    case Value(value) => throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
    case Mul(mul, right) => vars(right) * mul
    case Add(left, right) => (left, right) match {
      case (l: Value, r: Value) =>
        throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
      case (l: Value, r: ArithmeticExpression) => convertExpression(lp)(vars,r) + l.value
      case (l: ArithmeticExpression, r: Value) => convertExpression(lp)(vars,l) + r.value
      case _ => convertExpression(lp)(vars,left) + convertExpression(lp)(vars,right)
    }
  }

  /** Converts a Constraint into a LinearProgram#Constraint (accepted by Breeze) */
  private def convertConstraint(lp: LinearProgram)(vars: Map[SymbolicValue,lp.Variable], c: Constraint) : Set[lp.Constraint] = c match {
    case Eq(left, right) => (left, right) match {
      case (l: Value, r: Value) =>
        throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
      case (l: Value, r: ArithmeticExpression) =>
        val e = convertExpression(lp)(vars,r)
        Set[lp.Constraint](e >= l.value, e <= l.value)
      case (l: ArithmeticExpression, r: Value) =>
        val e = convertExpression(lp)(vars,l)
        Set[lp.Constraint](e >= r.value, e <= r.value)
      case _ =>
        val l = convertExpression(lp)(vars,left)
        var r = convertExpression(lp)(vars,right)
        Set[lp.Constraint](l >= r, l <= r)
    }
    case Geq(left, right) => (left, right) match {
      case (l: Value, r: Value) =>
        throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
      case (l: Value, r: ArithmeticExpression) =>
        val e = convertExpression(lp)(vars,r)
        Set[lp.Constraint](e >= l.value)
      case (l: ArithmeticExpression, r: Value) =>
        val e = convertExpression(lp)(vars,l)
        Set[lp.Constraint](e >= r.value)
      case _ =>
        val l = convertExpression(lp)(vars,left)
        var r = convertExpression(lp)(vars,right)
        Set[lp.Constraint](l >= r)
    }
    case Grt(left, right) => (left, right) match {
      case (l: Value, r: Value) =>
        throw new IllegalArgumentException("Cannot convert a Value into a LinearProgram#Expression.")
      case (l: Value, r: ArithmeticExpression) =>
        val e = convertExpression(lp)(vars,r)
        Set[lp.Constraint](e - l.value <= -0.0001)
      case (l: ArithmeticExpression, r: Value) =>
        val e = convertExpression(lp)(vars,l)
        Set[lp.Constraint](e - r.value >= 0.0001)
      case _ =>
        val l = convertExpression(lp)(vars,left)
        var r = convertExpression(lp)(vars,right)
        Set[lp.Constraint](l - r >= 0.0001)
    }
  }

  /** Solves a set of constraints. */
  def solve(constraints: Set[Constraint]) : Map[SymbolicValue,Double] = {

    val lp = new LinearProgram()  // linear program

    // array of symbolic values
    val sym : Array[SymbolicValue] = this.extractSymbolicValues(constraints).toArray
    // map from symbolic values to corresponding LinearProgram#Variable (accepted by Breeze)
    val symToVars : Map[SymbolicValue,lp.Variable] = sym.foldLeft(Map[SymbolicValue,lp.Variable]())(
      (m, s) => m + (s -> lp.Real(s.toString))
    )

    // list of values corresponding to symbolic variables
    val vars = symToVars.values.toList
    // objective function: the sum of the variables
    val obj = vars.drop(1).foldLeft(vars.head : lp.Expression)(
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
      for (s <- convertConstraint(lp)(symToVars,c)) {
        prob = prob.subjectTo(s)
      }
    }
    // solving lp problem
    val res = lp.minimize(prob).result

    // returning the result
    var result = Map[SymbolicValue,Double]()
    for (i <- 1 to sym.length) {
      result = result + (sym(i-1) -> res.valueAt(i-1))
    }
    result
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
      d.toString + "->" + num.toString + "/" + den.toString
    else
      d.toString + "->" + num.toString
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

    val z = new SymbolicPermissionPredicate(new Path(List("x")))
    val w = new SymbolicPermissionPredicate(new Path(List("y")))
    val cz = Grt(Mul(1,z),Value(0))
    val cw = Grt(Mul(1,w),Value(0))
    val c = Grt(Add(Mul(1,z),Mul(1,w)),Value(0))
    val s = Set[Constraint](c,cz,cw)
    val result = solve(s)
    println(result.mapValues((d) => doubleToRational(d)))
  }

}
