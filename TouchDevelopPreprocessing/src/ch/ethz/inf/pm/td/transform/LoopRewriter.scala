package ch.ethz.inf.pm.td.transform

import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.parser.Foreach
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.parser.LocalReference
import ch.ethz.inf.pm.td.parser.AssignStatement
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

  def apply(s: Script): Script = Script(s.declarations map (apply _))

  def apply(d: Declaration): Declaration = {
    d match {
      case a@ActionDefinition(_, _, _, _, _) => a.copy(body = (a.body map (apply _)).flatten).copyPos(a)
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
        val indexInit = pos(AssignStatement(List(idxExp), pos(Literal(pos(TypeName("Number")), "0"))))
        val upperBoundStore = pos(AssignStatement(List(storedBound), bnd))
        val condition = pos(Access(idxExp, "<", List(storedBound)))
        val bodyPostfix = pos(AssignStatement(List(idxExp), pos(Access(idxExp, "+", List(pos(Literal(pos(TypeName("Number")), "1")))))))
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
        val indexInit = pos(AssignStatement(List(idxExp), pos(Literal(pos(TypeName("Number")), "0"))))
        val collectionStore = pos(AssignStatement(List(storedCollection), pos(Access(coll,"copy",Nil))))
        val bodyPrefix = pos(AssignStatement(List(elemExp), pos(Access(storedCollection, "at_index", List(idxExp)))))
        val bodyPostfix = pos(AssignStatement(List(idxExp), pos(Access(idxExp, "+", List(pos(Literal(pos(TypeName("Number")), "1")))))))
        val rewrittenBody = (body map (apply _)).flatten
        val conditionalBody = guards match {
          case head::tail =>
            val guardCondition = tail.foldLeft(head)((left:Expression,right:Expression) => pos(Access(left,"and",List(right))))
            List(pos(If(guardCondition,rewrittenBody,Nil)))
          case Nil => rewrittenBody
        }
        val condition = pos(Access(idxExp, "<", List(pos(Access(storedCollection, "count", Nil)))))
        val whileLoop = pos(While(condition, bodyPrefix :: conditionalBody ::: bodyPostfix :: Nil))
        indexInit :: collectionStore :: whileLoop :: Nil

      case i@If(cond,thenBody,elseBody) =>
        List(pos(If(cond,(thenBody map (apply _)).flatten,(elseBody map (apply _)).flatten)))
      case w@While(cond,body) =>
        List(pos(While(cond,(body map (apply _)).flatten)))
      case _ =>
        List(s)
    }
  }

  def annotateName(s1:String,s2:String) = "__"+s1+"_"+s2+"__"
  def pos[T <: Positional](posNew:T)(implicit defPos:Positional):T = { posNew.pos = defPos.pos; posNew }

}

