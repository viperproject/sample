package ch.ethz.inf.pm.sample.permissionanalysis

import breeze.optimize.linear._

object PermissionSolver {

  def main(args:Array[String]) = {

    val lp = new LinearProgram()
    import lp._

    val x = Real("x")
    val y = Real("y")

    val obj = y // objective function
    var prob = obj.subjectTo() // problem
    prob = prob.subjectTo(y >= x + 1) // adding constraint
    prob = prob.subjectTo(x >= 3) // adding constraint
    val res = minimize(prob).result // solving

    println("Result:")
    println(x.name + ": " + res.data.apply(0).toInt)
    println(y.name + ": " + res.data.apply(1).toInt)

    println("\nDone.")
  }

}
