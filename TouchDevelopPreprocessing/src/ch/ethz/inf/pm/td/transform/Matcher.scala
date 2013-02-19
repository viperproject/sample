package ch.ethz.inf.pm.td.transform

import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.parser.AssignStatement
import ch.ethz.inf.pm.td.parser.While
import ch.ethz.inf.pm.td.parser.Foreach
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.parser.Access
import ch.ethz.inf.pm.td.parser.ActionDefinition
import ch.ethz.inf.pm.td.parser.ExpressionStatement
import ch.ethz.inf.pm.td.parser.For
import ch.ethz.inf.pm.td.parser.If

/**
 * User: lucas
 * Date: 2/18/13
 * Time: 5:27 PM
 */
object Matcher {

  def apply(scr:Script)(implicit onDeclaration : Declaration => Unit, onStatement : Statement => Unit, onExpression: Expression => Unit) {
    apply(scr.declarations)
  }

  def apply(decls:List[Declaration])(implicit onDeclaration : Declaration => Unit, onStatement : Statement => Unit, onExpression: Expression => Unit) {
    decls foreach (apply _)
  }

  def apply(decl:Declaration)(implicit onDeclaration : Declaration => Unit, onStatement : Statement => Unit, onExpression: Expression => Unit) {
    decl match {
      case ActionDefinition(_,_,_,bd,_) => bd.foreach(apply _)
      case _ => ()
    }
  }

  def apply(stmts:List[Statement])(implicit onStatement : Statement => Unit, onExpression: Expression => Unit) {
    stmts foreach (apply _)
  }

  def apply(stmt:Statement)(implicit onStatement : Statement => Unit, onExpression: Expression => Unit) {
    onStatement(stmt)
    stmt match {
      case For(loc,up,body) => apply(up); apply(body)
      case While(cond,body) => apply(cond); apply(body)
      case Foreach(loc,coll,guards,body) => apply(coll); apply(guards); apply(body)
      case If(cond,then,els) => apply(cond); apply(then); apply(els)
      case Box(body) => apply(body)
      case AssignStatement(left,right) => apply(left); apply(right)
      case ExpressionStatement(expr) => apply(expr)
      case _ => ()
    }
  }

  def apply(exprs:List[Expression])(implicit onExpression: Expression => Unit) {
    exprs foreach (apply _)
  }

  def apply(expr:Expression)(implicit onExpression: Expression => Unit) {
    onExpression(expr)
    expr match {
      case Access(subj,_,args) => apply(subj); apply(args)
      case _ => ()
    }
  }

}
