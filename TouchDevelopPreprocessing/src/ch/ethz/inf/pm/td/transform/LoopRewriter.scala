package ch.ethz.inf.pm.td.transform

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
import util.parsing.input.Positional

/**
 *
 * Transforms for-loops and for-each loops
 *
 * Lucas Brutschy
 * Date: 8/23/12
 * Time: 4:55 PM
 *
 */

object LoopRewriter {

  def apply(s: Script): Script = Script(s.declarations map (apply _),s.isLibrary)

  def apply(d: Declaration): Declaration = {
    d match {
      case a@ActionDefinition(_, _, _, _, _, _) => a.copy(body = (a.body map (apply _)).flatten).copyPos(a)
      case _ => d
    }
  }

  def apply(s: Statement): List[Statement] = {
    implicit val defPos = s
    s match {

      case For(idx, bnd, body) =>

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
        val storedBound = pos(LocalReference(annotateName(idx,"bound")))
        val indexInit = pos(ExpressionStatement(pos(Access(idxExp, Identifier(":="), List(pos(Literal(pos(TypeName("Number")), "0")))))))
        val upperBoundStore = pos(ExpressionStatement(pos(Access(storedBound, Identifier(":="), List(bnd)))))
        val condition = pos(Access(idxExp, pos(Identifier("<")), List(storedBound)))
        val bodyPostfix = pos(ExpressionStatement(pos(Access(idxExp, Identifier(":="), List(pos(Access(idxExp, pos(Identifier("+")), List(pos(Literal(pos(TypeName("Number")), "1"))))))))))
        indexInit :: upperBoundStore :: While(condition, (body map (apply _)).flatten ::: bodyPostfix :: Nil) :: Nil

      case f@Foreach(elem, coll, guards, body) =>

        // We have the following foreach loop:
        //   foreach ( elem in coll ) where guard1 where guard2 ... { body }

        // We generate the following while loop:
        //   var __elem_index__ = 0;                  // Define a counter
        //   var __elem_collection__ = coll->copy();  // Take a snapshot of the collection
        //   while ( __elem_index__ < __elem_collection__->count ) {
        //     elem = __elem_collection__->at_index(__elem_index__); // at_index is like at, but always integer indexed
        //     if (guard1 and guard2 and ... )  {
        //       body;
        //     }
        //     __elem_index__ = __elem_index__ + 1;
        //   }

        val idxExp = pos(LocalReference(annotateName(elem,"index")))
        val storedCollection = pos(LocalReference(annotateName(elem,"collection")))
        val elemExp = pos(LocalReference(elem))
        val indexInit = pos(ExpressionStatement(pos(Access(idxExp, Identifier(":="), List(pos(Literal(pos(TypeName("Number")), "0")))))))
        val collectionStore = pos(ExpressionStatement(pos(Access(storedCollection, Identifier(":="), List(pos(Access(coll,pos(Identifier("copy")),Nil)))))))
        val bodyPrefix = pos(ExpressionStatement(pos(Access(elemExp, Identifier(":="), List(pos(Access(storedCollection, pos(Identifier("at index")), List(idxExp))))))))
        val bodyPostfix = pos(ExpressionStatement(pos(Access(idxExp, Identifier(":="), List(pos(Access(idxExp, pos(Identifier("+")), List(pos(Literal(pos(TypeName("Number")), "1"))))))))))
        val rewrittenBody = (body map (apply _)).flatten
        val conditionalBody = guards match {
          case head::tail =>
            val guardCondition = tail.foldLeft(head)((left:Expression,right:Expression) => pos(Access(left,pos(Identifier("and")),List(right))))
            List(pos(If(guardCondition,rewrittenBody,Nil)))
          case Nil => rewrittenBody
        }
        val condition = pos(Access(idxExp, pos(Identifier("<")), List(pos(Access(storedCollection, pos(Identifier("count")), Nil)))))
        val whileLoop = pos(While(condition, bodyPrefix :: conditionalBody ::: bodyPostfix :: Nil))
        indexInit :: collectionStore :: whileLoop :: Nil

      case i@If(cond,thenBody,elseBody) =>
        List(pos(If(cond,(thenBody map (apply _)).flatten,(elseBody map (apply _)).flatten)))
      case w@While(cond,body) =>
        List(pos(While(cond,(body map (apply _)).flatten)))
      case w@Box(body) =>
        List(pos(Box((body map (apply _)).flatten)))
      case w@WhereStatement(expr,handlers) =>
        List(pos(WhereStatement(expr,handlers map (apply _))))
      case _ =>
        List(s)
    }
  }

  def apply(inlineAction:InlineAction):InlineAction = {
    implicit val defPos = inlineAction
    pos(InlineAction(inlineAction.handlerName,inlineAction.inParameters,inlineAction.outParameters,(inlineAction.body map (apply _)).flatten))
  }

  def annotateName(s1:String,s2:String) = "__"+s1+"_"+s2

  def pos[T <: IdPositional](posNew:T)(implicit defPos:IdPositional):T = {
    posNew.pos = defPos.pos
    posNew.setOptionalId(defPos.getId)
    posNew
  }

}

