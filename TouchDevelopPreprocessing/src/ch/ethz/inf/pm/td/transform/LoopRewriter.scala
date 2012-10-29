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

/**
 *
 * Transforms in-expression assignments, for-loops and for-each loops
 *
 * for(0 <= a < b) c --> a=0; while(a < b) { c; a = a + 1 }
 * foreach ( a in b ) c --> a_ind=0; while(a_ind < b.count) { a = b.at[a_ind]; c; a = a + 1 }
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
      case a@ActionDefinition(_, _, _, _) => a.copy(body = (a.body map (apply _)).flatten)
      case _ => d
    }
  }

  def apply(s: Statement): List[Statement] = {
    s match {
      case For(idx, bnd, body) =>
        val idxExp = LocalReference(idx)
        val condition = Access(idxExp, "<", List(bnd))
        val loopPrefix = AssignStatement(List(idxExp), Literal(TypeName("Number"), "0"))
        val bodyPostfix = AssignStatement(List(idxExp), Access(idxExp, "+", List(Literal(TypeName("Number"), "1"))))
        loopPrefix :: While(condition, (body map (apply _)).flatten ::: bodyPostfix :: Nil) :: Nil
      case Foreach(elem, coll, _, body) => // TODO: guards
        val idxExp = LocalReference("__" + elem + "_index__")
        val elemExp = LocalReference(elem)
        val condition = Access(idxExp, "<", List(Access(coll, "count", Nil)))
        val loopPrefix = AssignStatement(List(idxExp), Literal(TypeName("Number"), "0"))
        val bodyPrefix = AssignStatement(List(elemExp), Access(coll, "at", List(idxExp)))
        val bodyPostfix = AssignStatement(List(idxExp), Access(idxExp, "+", List(Literal(TypeName("Number"), "1"))))
        loopPrefix :: While(condition, bodyPrefix :: (body map (apply _)).flatten ::: bodyPostfix :: Nil) :: Nil
      case _ => List(s)
    }
  }

}

