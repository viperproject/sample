/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.transform

import ch.ethz.inf.pm.td.parser._

/**
 * User: lucas
 * Date: 2/18/13
 * Time: 5:27 PM
 */
object Rewriter {

//  def apply(scr:Script)(implicit onDeclaration : Declaration => Declaration, onStatement : Statement => Statement, onExpression: Expression => Expression):Script = {
//    scr.copy(declarations = apply(scr.declarations)).copyPos(scr)
//  }
//
//  def apply(decls:List[Declaration])(implicit onDeclaration : Declaration => Declaration, onStatement : Statement => Statement, onExpression: Expression => Expression):List[Declaration] = {
//    decls map apply
//  }
//
//  def apply(decl:Declaration)(implicit onDeclaration : Declaration => Declaration, onStatement : Statement => Statement, onExpression: Expression => Expression):Declaration = {
//    onDeclaration(decl match {
//      case a:ActionDefinition => a.copy(body = apply(a.body))
//      case x:Declaration => x
//    }).copyPos(decl)
//  }
//
//  def apply(stmts:List[Statement])(implicit onStatement : Statement => Statement, onExpression: Expression => Statement):List[Statement] = {
//    stmts map apply
//  }
//
//  def apply(stmt:Statement)(implicit onStatement : Statement => Statement, onExpression: Expression => Expression):Statement = {
//    onStatement(stmt match {
//      case For(loc,up,body) => For(loc,apply(up),apply(body))
//      case While(cond,body) => While(apply(cond),apply(body))
//      case Foreach(loc,coll,guards,body) => Foreach(loc,apply(coll),apply(guards),apply(body))
//      case If(cond,then,els) => If(apply(cond),apply(then),apply(els))
//      case Box(body) => Box(apply(body))
//      case WhereStatement(expr,handlers,optParam) => WhereStatement(apply(expr),handlers map apply, optParam map apply)
//      case ExpressionStatement(expr) => ExpressionStatement(apply(expr))
//      case x:Statement => x
//    }).copyPos(stmt)
//  }

  def apply(exprs:List[Expression])(implicit onExpression: Expression => Expression):List[Expression] = {
    exprs map (apply(_)(onExpression))
  }

  def apply(expr:Expression)(implicit onExpression: Expression => Expression):Expression = {
    onExpression(expr match {
      case Access(subj,property,args) => Access(apply(subj)(onExpression),property,apply(args)(onExpression))
      case x:Expression => x
    }).copyPos(expr).copyType(expr)
  }
//
//  def apply(handler:InlineAction)(implicit onStatement : Statement => Statement, onExpression: Expression => Expression):InlineAction = {
//    handler.copy(body = apply(handler.body)).copyPos(handler)
//  }
//
//  def apply(opt:OptionalParameter)(implicit onStatement : Statement => Statement, onExpression: Expression => Expression):OptionalParameter = {
//    opt.copy(expr = apply(opt.expr)).copyPos(opt)
//  }

}
