package Examples

import scala.util.parsing.combinator._

abstract class Expression
case class ArithmeticalExpression(left : Expression, right : Expression, op : String) extends Expression
case class Constant(c : String) extends Expression
case class ParenthesisExpression(expr : Expression) extends Expression

class Evaluation {
	def eval(exp : Expression) : Int = exp match {
	  case Constant(i) => i.toInt
	  case ParenthesisExpression(exp) => eval(exp)
	  case ArithmeticalExpression(left, right, op) => op match {
	    case "+" => eval(left)+eval(right)
	    case "-" => eval(left)-eval(right)
	    case "*" => eval(left)*eval(right)
	    case "/" => eval(left)/eval(right)
        case _ => throw new Exception()
	  }
	}
 
	def convert(exp : Any) : Expression = exp match {
	  case Constant(x) :: y :: z => ArithmeticalExpression(Constant(x), convert(z), y.toString)
	  //case "(" :: exp :: ")" => ParenthesisExpression(eval(exp))
	  case Constant(x) => Constant(x)
	}
}

object ParserExpression extends JavaTokenParsers {
  def arithexpr : Parser[Any] =  (const | parexpr) ~ opt(("+" | "-" | "*" | "/")~arithexpr)
  def const : Parser[Any] = decimalNumber ^^ {x => Constant(x toString)}
  def parexpr : Parser[Any] = "("~arithexpr~")"
  //^^ {case left~(op :: expr) => ArithmeticalExpression(left, parseExpr(expr), op )}

  def main(args : Array[String]) {
    val toBeAnalyzed = args(0)
    println("Expression: " + toBeAnalyzed)
    val expr : ParseResult[Any] =parseAll(arithexpr, toBeAnalyzed)
    val result : Int = new Evaluation().eval(new Evaluation().convert(expr))
    println("Result: "+result)
  }
  
}
