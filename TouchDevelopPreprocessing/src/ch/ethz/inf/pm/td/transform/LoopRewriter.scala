/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.transform

import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.parser.Foreach
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.parser.LocalReference
import ch.ethz.inf.pm.td.parser.While
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.parser.Access
import ch.ethz.inf.pm.td.parser.Literal
import ch.ethz.inf.pm.td.parser.ActionDefinition
import ch.ethz.inf.pm.td.parser.For

/**
 *
 * Transforms for-loops and for-each loops
 *
 * @author Lucas Brutschy
 *
 */

object LoopRewriter {

  def apply(s: Script): Script = Script(s.declarations map apply, s.isLibrary)

  def apply(d: Declaration): Declaration = {
    d match {
      case a: ActionDefinition => a.copy(body = a.body flatMap apply).copyPos(a)
      case a: PageDefinition => a.copy(initBody = a.initBody flatMap apply, displayBody = a.displayBody flatMap apply).copyPos(a)
      case _ => d
    }
  }

  def replace(inlineAction: InlineAction, from: Expression, to: Expression): InlineAction = {
    implicit val defPos = inlineAction
    pos(InlineAction(inlineAction.handlerName, inlineAction.inParameters, inlineAction.outParameters, inlineAction.body.map(replace(_, from, to)), inlineAction.typ))
  }

  def replace(optParam: OptionalParameter, from: Expression, to: Expression): OptionalParameter = {
    implicit val defPos = optParam
    pos(OptionalParameter(optParam.name, replace(optParam.expr, from, to)))
  }

  def replace(e: Expression, from: Expression, to: Expression): Expression = e match {
    case Access(subject, property, args) =>
      Access(replace(subject, from, to), property, args.map(replace(_, from, to)))
    case _ =>
      if (e.equals(from))
        to
      else
        e
  }

  def replace(s: Statement, from: Expression, to: Expression): Statement = {
    implicit val defPos = s
    s match {
      case For(idx, bnd, body) =>
        pos(For(idx, replace(bnd, from, to), body.map(replace(_, from, to))))
      case Foreach(elem, coll, guards, body) =>
        pos(Foreach(elem, replace(coll, from, to), guards.map(replace(_, from, to)), body.map(replace(_, from, to))))
      case If(cond, thenBody, elseBody) =>
        pos(If(replace(cond, from, to), thenBody.map(replace(_, from, to)), elseBody.map(replace(_, from, to))))
      case While(cond, body) =>
        pos(While(replace(cond, from, to), body.map(replace(_, from, to))))
      case Box(body) =>
        pos(Box(body.map(replace(_, from, to))))
      case WhereStatement(expr, handlers, optParams) =>
        pos(WhereStatement(replace(expr, from, to), handlers.map(replace(_, from, to)), optParams.map(replace(_, from, to))))
      case ExpressionStatement(expr) =>
        pos(ExpressionStatement(replace(expr, from, to)))
      case _ => s
    }
  }

  def apply(s: Statement): List[Statement] = {
    implicit val defPos = s
    s match {

      case For(idx, bnd, body) =>

        bnd match {

          case Literal(numTyp, value) if TouchAnalysisParameters.get.unrollForLoopsUpTo >= Math.round(value.toDouble).toInt =>

            val bodyNew = body flatMap apply
            val idxOld = pos(LocalReference(idx))
            (for (i <- 0 to Math.round(value.toDouble).toInt - 1) yield {
              if (TouchAnalysisParameters.get.renameForLoopUnrollings) {
                val posSuffix = "it" + i
                val idxNew = pos(Literal(numTyp, i.toString))
                bodyNew.map(x => LoopUnroller.renameStatementPos(replace(x, idxOld, idxNew), posSuffix))
              } else {
                val idxNew = pos(Literal(numTyp, i.toString))
                bodyNew.map(x => replace(x, idxOld, idxNew))
              }
            }).toList.flatten

          case _ =>

            // We have the following for loop:
            //   for(0 <= idx < bnd) { body }

            // We generate the following while loop:
            // var idx = 0;
            // var __idx_bound__ = bnd;
            // while (idx < __idx_bound__) {
            //   body;
            //   __idx_bound__ = __idx__bound__ + 1;
            // }

            val idxExp = pos(LocalReference(idx))
            val storedBound = pos(LocalReference(annotateName(idx, "bound")))
            val indexInit = pos(ExpressionStatement(pos(Access(idxExp, Identifier(":="), List(pos(Literal(pos(TypeName("Number")), "0")))))))
            val upperBoundStore = pos(ExpressionStatement(pos(Access(storedBound, Identifier(":="), List(bnd)))))
            val condition = pos(Access(idxExp, pos(Identifier("<")), List(storedBound)))
            val bodyPostfix = pos(ExpressionStatement(pos(Access(idxExp, Identifier(":="), List(pos(Access(idxExp, pos(Identifier("+")), List(pos(Literal(pos(TypeName("Number")), "1"))))))))))
            indexInit :: upperBoundStore :: While(condition, (body flatMap apply) ::: bodyPostfix :: Nil) :: Nil

        }


      case f@Foreach(elem, coll, guards, body) =>

        // We have the following foreach loop:
        //   foreach ( elem in coll ) where guard1 where guard2 ... { body }

        // We generate the following while loop:
        //   var __elem_index__ = 0;                  // Define a counter
        //   var __elem_collection__ = coll->copy();  // Take a snapshot of the collection
        //   while ( __elem_index__ < __elem_collection__->count ) {
        //     // every occurence of elem inside the loop body and inside the guard expressions
        //     // is replaced with __elem_collection__->at_index(__elem_index__); (at_index is like at, but always integer indexed)
        //     if (guard1 and guard2 and ... )  {
        //       body;
        //     }
        //     __elem_index__ = __elem_index__ + 1;
        //   }

        val idxExp = pos(LocalReference(annotateName(elem, "index")))
        val storedCollection = pos(LocalReference(annotateName(elem, "collection")))
        val elemExp = pos(LocalReference(elem))
        val indexInit = pos(ExpressionStatement(pos(Access(idxExp, Identifier(":="), List(pos(Literal(pos(TypeName("Number")), "0")))))))
        val collectionStore = pos(ExpressionStatement(pos(Access(storedCollection, Identifier(":="), List(pos(Access(coll, pos(Identifier("copy")), Nil)))))))
        val bodyPostfix = pos(ExpressionStatement(pos(Access(idxExp, Identifier(":="), List(pos(Access(idxExp, pos(Identifier("+")), List(pos(Literal(pos(TypeName("Number")), "1"))))))))))
        val atIndexExpr = pos(Access(storedCollection, pos(Identifier("at index")), List(idxExp)))
        val rewrittenBody = (body flatMap apply).map(replace(_, elemExp, atIndexExpr))

        val conditionalBody = guards match {
          case Literal(_, "true") :: Nil => rewrittenBody
          case head :: tail =>
            val guardCondition = tail.foldLeft(head)((left: Expression, right: Expression) => pos(Access(left, pos(Identifier("and")), List(right))))
            val guardConditionReplaced = replace(guardCondition, elemExp, atIndexExpr)
            List(pos(If(guardConditionReplaced, rewrittenBody, Nil)))
          case Nil => rewrittenBody
        }
        val countMinusOne = Access(pos(Access(storedCollection, pos(Identifier("count")), Nil)), pos(Identifier("-")), List(pos(Literal(pos(TypeName("Number")), "1"))))
        val condition = pos(Access(idxExp, pos(Identifier("≤")), List(countMinusOne))) // Less-Equal count -1 is tighter than Less count, since
      val whileLoop = pos(While(condition, conditionalBody ::: bodyPostfix :: Nil))
        indexInit :: collectionStore :: whileLoop :: Nil

      case i@If(cond, thenBody, elseBody) =>
        List(pos(If(cond, thenBody flatMap apply, elseBody flatMap apply)))
      case w@While(cond, body) =>
        List(pos(While(cond, body flatMap apply)))
      case w@Box(body) =>
        List(pos(Box(body flatMap apply)))
      case w@WhereStatement(expr, handlers, optParam) =>
        List(pos(WhereStatement(expr, handlers map apply, optParam)))
      case _ =>
        List(s)
    }
  }

  def apply(inlineAction: InlineAction): InlineAction = {
    implicit val defPos = inlineAction
    pos(InlineAction(inlineAction.handlerName, inlineAction.inParameters, inlineAction.outParameters, inlineAction.body flatMap apply, inlineAction.typ))
  }

  def annotateName(s1: String, s2: String) = "__" + s1 + "_" + s2

  def pos[T <: IdPositional](posNew: T)(implicit defPos: IdPositional): T = {
    posNew.pos = defPos.pos
    posNew.appendIdComponent("[" + id + "]")
    id = id + 1
    posNew
  }

  var id = 0

}

