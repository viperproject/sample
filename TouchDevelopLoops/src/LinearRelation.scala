package ch.ethz.inf.pm.sample.td.cost.loops


/*
    Offers static methods for class LinearRelation.
 */
object LinearRelationHelper {

  def isValidOperator (op: String) : Boolean = {
    op == "<" || op == ">" || op == "<=" || op == ">=" || op == "=" || op == "!="
  }

  // operator to be used for the initial value of the iteration variable in the PUBS equations
  def initialOperator (op: String) : String = {
    op match {
      case "<" => ">="
      case "<=" => ">="
      case ">" => "=<"
      case ">=" => "=<"
      case _ => "="   // todo
    }
  }

  def inverseOperator (op: String) : String = {
    op match {
      case "<" => ">="
      case "<=" => ">"
      case ">" => "=<"
      case ">=" => "<"
      case "=" => "!="
      case _ => "="
    }
  }

}

/*
     A linear relation as it is used by the loop cost analysis.
 */
class LinearRelation(val lhs: LinearExpression, val operator : String, val rhs: LinearExpression) {

  def variables = lhs.variables.union(rhs.variables)

  override def toString : String = {
    operator match {
      case "<"  => { lhs.toString + " +1 =< " + rhs.toString }
      case "<=" | "=<" => { lhs.toString + " =< " + rhs.toString }
      case ">"  => { lhs.toString + " >= " + rhs.toString + " +1" }
      case ">=" => { lhs.toString + " >= " + rhs.toString }
      case "="  => { lhs.toString + " = " + rhs.toString }
      case _ => { "0 = 1" } // unknown operator => bottom
    }
  }

   def toStringWithValues : String = {
    operator match {
      case "<"  => { lhs.toString + " +1 =< " + rhs.toStringWithValues }
      case "<=" | "=<" => { lhs.toString + " =< " + rhs.toStringWithValues }
      case ">"  => { lhs.toString + " >= " + rhs.toStringWithValues + " +1" }
      case ">=" => { lhs.toString + " >= " + rhs.toStringWithValues }
      case "="  => { lhs.toString + " = " + rhs.toStringWithValues }
      case _ => { "0 = 1" } // unknown operator => bottom
    }
  }


}