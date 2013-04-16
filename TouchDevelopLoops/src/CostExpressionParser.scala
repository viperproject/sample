package ch.ethz.inf.pm.sample.td.cost.loops

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

// used for testing only
object TestC {
  def main(args: Array[String]){
    val v = CostExpressionParser("nat(A-1)*(nat(C)+nat(D))")
    println(v)
  }
}

object CostExpressionParser extends RegexParsers with PackratParsers {

  /**
   *  linear expressions
   */

  lazy val variable: Parser[VariableExpr] = """[A-Z](\w*)""".r ^^ { case name => VariableExpr(name) }
  lazy val  integer:  Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  lazy val  operator: Parser[String] = """[\+\-\*\/]""".r

  lazy val  rational:  Parser[RationalNumber] =  integer~"/"~integer ^^ { case i1~over~i2 => RationalNumber(i1, i2) } |
    integer ^^ { case i => RationalNumber(i, 1) }

  lazy val addexpr:  Parser[LinearExpr] =
    "("~>addexpr<~")" |
    multexpr~"+"~addexpr ^^ { case e1~op~e2 => CompositeLinearExpr(e1, op, e2) }  |
    multexpr~"-"~addexpr ^^ { case e1~op~e2 => CompositeLinearExpr(e1, op, e2) }  |
      multexpr

  lazy val multexpr:  Parser[LinearExpr] =
    "("~>multexpr<~")" |
    "-" ~ multexpr ^^ { case minus~e => CompositeLinearExpr(new RationalNumber(-1,1), "*", e) }  |
    terminal~"*"~multexpr ^^ { case e1~op~e2 => CompositeLinearExpr(e1, op, e2) }  |
    terminal~"/"~multexpr ^^ { case e1~op~e2 => CompositeLinearExpr(e1, op, e2) }  |
    terminal

  lazy val  terminal:  Parser[LinearExpr] =   variable | rational

  lazy val linexpr:  Parser[LinearExpr] =   addexpr

  /**
   *  cost expressions
   */

  lazy val  costexpr:  Parser[CostExpr] =   addcostexprR | addcostexprL

  /**
   *  right recursive cost expressions
   */

  lazy val addcostexprR:  Parser[CostExpr] =
    "("~>addcostexprR<~")" |
      multcostexprR~"+"~addcostexprR ^^ { case e1~op~e2 => CompositeCostExpr(e1, op, e2) }  |
      multcostexprR~"-"~addcostexprR ^^ { case e1~op~e2 => CompositeCostExpr(e1, op, e2) }  |
  multcostexprR

  lazy val multcostexprR:  Parser[CostExpr] =
    "("~>multcostexprR<~")" |
      functionR~"*"~addcostexprR ^^ { case e1~op~e2 => CompositeCostExpr(e1, op, e2) }  |
      functionR~"/"~addcostexprR ^^ { case e1~op~e2 => CompositeCostExpr(e1, op, e2) }  |
  functionR

  lazy val functionR:  Parser[CostExpr] =
    "("~>functionR<~")" |
      "nat("~linexpr~")" ^^ { case nat~e~cb => NatExpr(e) }  |
      "pow("~rational~","~addcostexprR~")" ^^ { case pow~n~c~e~cb => PowExpr(n, e) }  |
      "log("~rational~","~addcostexprR~")" ^^ { case pow~n~c~e~cb => LogExpr(n, e) }  |
      rational

  /**
   *  left recursive cost expressions
   */

  lazy val addcostexprL:  Parser[CostExpr] =
    "("~>addcostexprL<~")" |
      addcostexprL~"+"~multcostexprL ^^ { case e1~op~e2 => CompositeCostExpr(e1, op, e2) }  |
      addcostexprL~"-"~multcostexprL ^^ { case e1~op~e2 => CompositeCostExpr(e1, op, e2) }  |
      multcostexprL

  lazy val multcostexprL:  Parser[CostExpr] =
    "("~>multcostexprL<~")" |
      addcostexprL~"*"~functionL ^^ { case e1~op~e2 => CompositeCostExpr(e1, op, e2) }  |
      addcostexprL~"/"~functionL ^^ { case e1~op~e2 => CompositeCostExpr(e1, op, e2) }  |
      functionL

  lazy val functionL:  Parser[CostExpr] =
    "("~>functionL<~")" |
      "nat("~linexpr~")" ^^ { case nat~e~cb => NatExpr(e) }  |
      "pow("~rational~","~addcostexprL~")" ^^ { case pow~n~c~e~cb => PowExpr(n, e) }  |
      "log("~rational~","~addcostexprL~")" ^^ { case pow~n~c~e~cb => LogExpr(n, e) }  |
      rational

  def apply(input: String): CostExpr = parseAll(costexpr, input) match {
    case Success(result, _) => result
    //case failure : NoSuccess => scala.sys.error(failure.msg)
    case failure : NoSuccess => UnparsedCostExpr(input)
  }
}

abstract trait LinearExpr

case class VariableExpr(var name: String) extends LinearExpr {
  override def toString : String = name
}

case class RationalNumber(numerator : Int, denominator : Int) extends LinearExpr with CostExpr {
  override def toString : String = if (denominator == 1) numerator.toString else  numerator + "/" + denominator
}
case class CompositeLinearExpr(left : LinearExpr, operator: String, right : LinearExpr) extends LinearExpr {
  override def toString : String = "(" + left + operator + right + ")"
}

abstract trait CostExpr

case class NatExpr(expr: LinearExpr) extends CostExpr {
  override def toString : String = "nat(" + expr + ")"
}

case class PowExpr(n: RationalNumber, expr: CostExpr) extends CostExpr {
  override def toString : String = "pow(" + n + "," + expr + ")"
}

case class LogExpr(n: RationalNumber, expr: CostExpr) extends CostExpr {
  override def toString : String = "log(" + n + "," + expr + ")"
}

case class CompositeCostExpr(left : CostExpr, operator: String, right : CostExpr) extends CostExpr {
  override def toString : String = "(" + left + operator + right + ")"
}

// used for all strings that we could not successfully parse
case class UnparsedCostExpr(var text: String) extends CostExpr {
  override def toString : String = "Unparsed Cost Expression : "+text
}

