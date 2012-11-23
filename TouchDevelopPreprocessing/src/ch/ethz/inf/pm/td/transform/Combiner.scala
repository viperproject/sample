//package ch.ethz.inf.pm.td.transform
//
//import ch.ethz.inf.pm.td.parser._
//import ch.ethz.inf.pm.td.parser.Foreach
//import ch.ethz.inf.pm.td.parser.TypeName
//import ch.ethz.inf.pm.td.parser.LocalReference
//import ch.ethz.inf.pm.td.parser.AssignStatement
//import ch.ethz.inf.pm.td.parser.While
//import ch.ethz.inf.pm.td.parser.Script
//import ch.ethz.inf.pm.td.parser.Access
//import ch.ethz.inf.pm.td.parser.Literal
//import ch.ethz.inf.pm.td.parser.ActionDefinition
//import ch.ethz.inf.pm.td.parser.For
//
///**
// *
// * Combines events and the effects of multiple runs into a single script.
// *
// * Source:
// *
// * data X
// * action main MAIN_CODE
// * event A
// * event B
// *
// * Result:
// *
// * action main {
// *   X = invalid;
// *   while(true) {
// *     (Reset non-persistent state)
// *     MAIN_CODE
// *     while(ND) {
// *       if(canA && ND) A()
// *       if(canB && ND) B()
// *     }
// *   }
// * }
// *
// */
//
//object Combiner {
//
//  def apply(s: Script): Script = {
//    val varDefs = s.declarations flatMap { case VariableDefinition(v) => Some(v); case _ => None }
//
//    Script(s.declarations map (apply _))
//  }
//
//  def apply(d: Declaration): Declaration = {
//    d match {
//      case a@ActionDefinition(_, _, _, _) => a.copy(body = (a.body map (apply _)).flatten)
//      case _ => d
//    }
//  }
//
//  def apply(s: Statement): List[Statement] = {
//    s match {
//      case For(idx, bnd, body) =>
//        val idxExp = LocalReference(idx)
//        val condition = Access(idxExp, "<", List(bnd))
//        val loopPrefix = AssignStatement(List(idxExp), Literal(TypeName("Number"), "0"))
//        val bodyPostfix = AssignStatement(List(idxExp), Access(idxExp, "+", List(Literal(TypeName("Number"), "1"))))
//        loopPrefix :: While(condition, (body map (apply _)).flatten ::: bodyPostfix :: Nil) :: Nil
//      case Foreach(elem, coll, _, body) => // TODO: guards
//        val idxExp = LocalReference("__" + elem + "_index__")
//        val elemExp = LocalReference(elem)
//        val condition = Access(idxExp, "<", List(Access(coll, "count", Nil)))
//        val loopPrefix = AssignStatement(List(idxExp), Literal(TypeName("Number"), "0"))
//        val bodyPrefix = AssignStatement(List(elemExp), Access(coll, "at", List(idxExp)))
//        val bodyPostfix = AssignStatement(List(idxExp), Access(idxExp, "+", List(Literal(TypeName("Number"), "1"))))
//        loopPrefix :: While(condition, bodyPrefix :: (body map (apply _)).flatten ::: bodyPostfix :: Nil) :: Nil
//      case i@If(cond,thenBody,elseBody) => List(i.copy(
//        thenBody = (thenBody map (apply _)).flatten,
//        elseBody = (elseBody map (apply _)).flatten))
//      case _ => List(s)
//    }
//  }
//
//}
//
