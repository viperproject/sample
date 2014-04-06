package ch.ethz.inf.pm.td.transform

import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.parser.While
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.parser.Access
import ch.ethz.inf.pm.td.parser.ActionDefinition
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.parser.Statement
import ch.ethz.inf.pm.td.parser.ExpressionStatement
import ch.ethz.inf.pm.td.parser.SingletonReference
import ch.ethz.inf.pm.td.parser.Identifier
import ch.ethz.inf.pm.td.parser.Access
import ch.ethz.inf.pm.td.parser.While
import ch.ethz.inf.pm.td.parser.If
import ch.ethz.inf.pm.td.parser.Box
import ch.ethz.inf.pm.td.parser.WhereStatement
import ch.ethz.inf.pm.td.parser.InlineAction
import ch.ethz.inf.pm.td.parser.Declaration
import ch.ethz.inf.pm.td.parser.ActionDefinition
import ch.ethz.inf.pm.td.parser.PageDefinition
import scala.Predef._
import ch.ethz.inf.pm.td.parser.LocalReference
import ch.ethz.inf.pm.td.parser.PageDefinition
import ch.ethz.inf.pm.td.parser.While
import ch.ethz.inf.pm.td.parser.InlineAction
import ch.ethz.inf.pm.td.parser.WhereStatement
import ch.ethz.inf.pm.td.parser.Identifier
import ch.ethz.inf.pm.td.parser.Box
import ch.ethz.inf.pm.td.parser.SingletonReference
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.parser.Access
import ch.ethz.inf.pm.td.parser.ActionDefinition
import ch.ethz.inf.pm.td.parser.ExpressionStatement
import ch.ethz.inf.pm.td.parser.If

/**
 * Another rewriting at the syntax level that tries to
 * rewrite complex calls like  a->foo->bar into a sequence of assignments
 * and method calls, e.g.  temp1 = a->foo; temp1->bar;
 *
 * This circumvents some imprecisions in the backward analysis due to obscure Sample
 * bugs/implementation difficulties, but may create other problems, so it is disabled by default.
 */
object ChainedCallRewriter {
  val specialOps = Set(":=", "=", "≠", "<", "≤", ">", "≥", "∥", "+", "-", "*", "/")

  def isSpecialOp(op: Identifier): Boolean = specialOps.contains(op.ident)

  private case class RewrittenExpr(expr: Expression, helperStmts: List[Statement] = Nil)

  def rewriteScript(s: Script): Script = {
    var tempIdCount = 0

    def tempName(): String = {
      val count = tempIdCount
      tempIdCount += 1
      "assignTemp" + count
    }

    def rewriteAccessChains(expr: Expression): RewrittenExpr = {

      expr match {
        case outer@Access(target: Access, property, args) =>
          val newTarget: RewrittenExpr =
            if (!isSpecialOp(property)) {
              // recurse first, then rewrite inner access as temporary assignment
              val newInner = rewriteAccessChains(target)
              val name = tempName()
              val tempAssignVar = LocalReference(name)
              tempAssignVar.copyPos(target).appendIdComponent(name)
              val tempAssignStmt = ExpressionStatement(Access(tempAssignVar,
                Identifier(":="), List(newInner.expr)))
              tempAssignStmt.copyPos(target).appendIdComponent(name)
              RewrittenExpr(tempAssignVar,
                helperStmts = newInner.helperStmts ++ List(tempAssignStmt))
            } else {
              // only recurse
              val newInner = rewriteAccessChains(target)
              RewrittenExpr(newInner.expr, newInner.helperStmts)
            }

          val newArgs = args map rewriteAccessChains

          val newAccess = Access(newTarget.expr, property, newArgs.map(_.expr))
          newAccess.copyPos(outer)
          val helperStmts = newTarget.helperStmts ++ newArgs.flatMap(_.helperStmts)
          RewrittenExpr(newAccess, helperStmts)

        case _ =>
          RewrittenExpr(expr)
      }
    }

    def rewriteStatement(s: Statement): List[Statement] = {
      s match {
        case es@ExpressionStatement(expr) =>
          val rewritten = rewriteAccessChains(expr)
          val newExprStmt = ExpressionStatement(rewritten.expr).copyPos(es)
          rewritten.helperStmts ++ List(newExprStmt)

        case w@While(cond, body) =>
          val rewrittenBody = (body map rewriteStatement).flatten
          val transformed = While(cond, rewrittenBody)
          transformed.copyPos(w)
          List(transformed)
        case i@If(cond, thenBody, elseBody) =>
          val transformed = If(cond, (thenBody map rewriteStatement).flatten, (elseBody map rewriteStatement).flatten)
          transformed.copyPos(i)
          List(transformed)
        case w@Box(body) =>
          val transformed = Box((body map rewriteStatement).flatten)
          transformed.copyPos(w)
          List(transformed)
        case w@WhereStatement(expr, handlers) =>
          val transformed = WhereStatement(expr, handlers map rewriteInlineAction)
          transformed.copyPos(w)
          List(transformed)
        case _ =>
          List(s)
      }
    }

    def rewriteInlineAction(inlineAction: InlineAction): InlineAction = {
      implicit val defPos = inlineAction
      val transformed = InlineAction(inlineAction.handlerName, inlineAction.inParameters,
        inlineAction.outParameters, (inlineAction.body map rewriteStatement).flatten)
      transformed.copyPos(inlineAction)
    }

    def rewriteDeclaration(d: Declaration): Declaration = {
      d match {
        case a: ActionDefinition => a.copy(body = (a.body map rewriteStatement).flatten).copyPos(a)
        case a: PageDefinition => a.copy(initBody = (a.initBody map rewriteStatement).flatten,
          displayBody = (a.displayBody map rewriteStatement).flatten).copyPos(a)
        case _ => d
      }
    }

    Script(s.declarations map rewriteDeclaration, s.isLibrary)
  }
}

