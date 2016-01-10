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

  private var constraints: Set[Constraint] = Set[Constraint]() // set of constraints

  def main(args:Array[String]) = {

    val lp = new LinearProgram()
    import lp._

    val x = Real("x")
    val y = Real("y")

    val obj : lp.Expression = x+y // objective function
    var prob = obj.subjectTo() // problem
    prob = prob.subjectTo(x + y >= 4 : lp.Constraint) // adding constraint
    prob = prob.subjectTo(y >= 1) // adding constraint
    prob = prob.subjectTo(x >= 1) // adding constraint



    val res = minimize(prob).result // solving



    println("Result:")
    println(x.name + ": " + res.valueAt(0))
    println(y.name + ": " + res.valueAt(1))

    println("\nDone.")
  }

}
