/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.transform

import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.parser.While
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.parser.Access
import ch.ethz.inf.pm.td.parser.ActionDefinition
import ch.ethz.inf.pm.td.parser.For

/**
 * Syntactic loop unrolling by a given number i of iterations
 *
 * To get unique program points, first i unrolled  iterations get a program point
 * with a suffix "it{i}", the remaining loop iterations have the suffix "itA"
 *
 * The unrolling is useful to introduce different non-deterministic identifiers
 * for values produces in the first iteratioins, which increases precision
 * of the backward analysis
 */
object LoopUnroller {

  def unroll(s: Script, unrollings: Int): Script = {

    if (unrollings == 0) return s

    def unrollStatement(s: Statement): List[Statement] = {
      s match {
        case w@While(cond, body) =>
          val unrolledBody = (body map unrollStatement).flatten
          val prependedIfs = for (i <- (1 to unrollings).toList) yield {
            val posSuffix = "it" + i
            val c = renameExpressionPos(cond, posSuffix)
            val b = unrolledBody map (s => renameStatementPos(s, posSuffix))
            If(c, b, Nil).copyPos(w).appendIdComponent(posSuffix)
          }
          val posSuffix = "itA"
          val reposCond = renameExpressionPos(cond, posSuffix)
          val reposUnrolledBody = unrolledBody map (s => renameStatementPos(s, posSuffix))
          val repositionedWhile = While(reposCond, reposUnrolledBody).copyPos(w).appendIdComponent(posSuffix)
          prependedIfs ::: List(repositionedWhile)

        case i@If(cond, thenBody, elseBody) =>
          val transformed = If(cond, (thenBody map unrollStatement).flatten, (elseBody map unrollStatement).flatten)
          transformed.copyPos(i)
          List(transformed)
        case w@Box(body) =>
          val transformed = Box((body map unrollStatement).flatten)
          transformed.copyPos(w)
          List(transformed)
        case w@WhereStatement(expr, handlers,optParam) =>
          val transformed = WhereStatement(expr, handlers map unrollInlineAction,optParam)
          List(transformed)
        case _ =>
          List(s)
      }
    }

    def unrollInlineAction(inlineAction: InlineAction): InlineAction = {
      implicit val defPos = inlineAction
      val transformed = InlineAction(inlineAction.handlerName, inlineAction.inParameters,
        inlineAction.outParameters, inlineAction.body flatMap unrollStatement,inlineAction.typ)
      transformed.copyPos(inlineAction)
    }

    def unrollDeclaration(d: Declaration): Declaration = {
      d match {
        case a: ActionDefinition => a.copy(body = a.body flatMap unrollStatement).copyPos(a)
        case a: PageDefinition => a.copy(initBody = a.initBody flatMap unrollStatement, displayBody = a.displayBody flatMap unrollStatement).copyPos(a)
        case _ => d
      }
    }


    Script(s.declarations map unrollDeclaration, s.isLibrary)
  }


  def renameStatementPos(s: Statement, suffix: String): Statement = {
    val renameS = renameStatementPos(_: Statement, suffix)
    val renameE = renameExpressionPos(_: Expression, suffix)
    val renameIA = renameInlineActionPos(_: InlineAction, suffix)
    val transformed: Statement = s match {
      case f@For(idx, bnd, body) =>
        For(idx, renameE(bnd), body map renameS)
      case Foreach(elem, coll, guards, body) =>
        Foreach(elem, renameE(coll), guards map renameE, body map renameS)
      case If(cond, thenBody, elseBody) =>
        If(renameE(cond), thenBody map renameS, elseBody map renameS)
      case While(cond, body) =>
        While(renameE(cond), body map renameS)
      case Box(body) =>
        Box(body map renameS)
      case WhereStatement(expr, handlers, optParam) =>
        WhereStatement(renameE(expr), handlers map renameIA, optParam.map(renameOptParamPos(_,suffix)))
      case ExpressionStatement(expr) =>
        ExpressionStatement(renameE(expr))
      case _ => s
    }
    transformed.copyPos(s).appendIdComponent(suffix)
  }

  def renameOptParamPos(inlineAction: OptionalParameter, suffix: String): OptionalParameter = {
    OptionalParameter(inlineAction.name, renameExpressionPos(inlineAction.expr,suffix))
  }

  def renameInlineActionPos(inlineAction: InlineAction, suffix: String): InlineAction = {
    val renameS = renameStatementPos(_: Statement, suffix)
    InlineAction(inlineAction.handlerName, inlineAction.inParameters, inlineAction.outParameters,
      inlineAction.body map renameS,inlineAction.typ)
  }

  def renameExpressionPos(e: Expression, suffix: String): Expression = {
    val renameE = renameExpressionPos(_: Expression, suffix)
    val transformed = e match {
      case Access(subject, property, args) =>
        val newProperty = property.copy().copyPos(property).appendIdComponent(suffix)
        Access(renameE(subject), newProperty, args map renameE)
      case e: Literal => e.copy()
      case e: SingletonReference => e.copy()
      case e: LocalReference => e.copy()
      case e: Placeholder => e.copy()
    }
    transformed.copyPos(e).appendIdComponent(suffix)
  }

}