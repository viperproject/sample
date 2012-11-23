package ch.ethz.inf.pm.td.transform

import scala.collection.immutable
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.compiler.TouchException

/**
 *
 * Lucas Brutschy
 * Date: 9/19/12
 * Time: 5:09 PM
 *
 */
object Inliner {

  var counter = 0
  var callableFunctions = immutable.Map.empty[String,ActionDefinition]

  def inline(scr:Script):Script = {
    for(decl <- scr.declarations) {
      decl match {
        case a@ActionDefinition(ident,_,_,_,_) => callableFunctions = callableFunctions + (ident -> a)
        case _ => ()
      }
    }
    Script(scr.declarations map (inline _))
  }

  def inline(decl:Declaration):Declaration = {
    decl match {
      case a@ActionDefinition(ident,in,out,body,_) => a.copy(body = body flatMap (inline _))
      case x => x
    }
  }

  def inline(smt:Statement):List[Statement] = {
    smt match {
      case ExpressionStatement(expr) =>
        val (newExpr,newStmts) = inline(expr)
        newStmts ::: List(ExpressionStatement(newExpr))
      case AssignStatement(left,right) =>
        val (newExpr,newStmts) = inline(right)
        newStmts ::: List(AssignStatement(left,newExpr))
      case For(loc,up,body) =>
        //val (newExpr,newStmts) = inline(up)
        //newStmts ::: List(For(loc,newExpr,body flatMap(inline _)))
        throw new TouchException("Inliner requires loop rewriter")
      case While(cond,body) =>
        val (newExpr,newStmts) = inline(cond)
        newStmts ::: List(While(newExpr,body flatMap(inline _)))
      case Foreach(loc,coll,guards,body) =>
        //val (newExpr,newStmts) = inline(coll)
        //val (guardExprs,guardStmts) = (guards map (inline _)).unzip
        //newStmts ::: (guardStmts flatten) ::: List(Foreach(loc,newExpr,guardExprs,body flatMap(inline _)))
        throw new TouchException("Inliner requires loop rewriter")
      case If(cond,then,els) =>
        val (newExpr,newStmts) = inline(cond)
        newStmts ::: List(If(newExpr,then flatMap(inline _),els flatMap(inline _)))
      case x => List(x)
    }
  }

  def inline(expr:Expression):(Expression,List[Statement]) = {
    expr match {
      case Access(SingletonReference("code"),prop,args) =>
        // Here is were the main inlining is done. For...
        //    action inlinable(in:Something) returns out:Something { out = in }
        //    action caller() { inlinable(1)->post_to_wall }
        // We generate the following
        // action caller() {
        //    __in_1 := 1
        //    __out_1 := __in_1
        //    __out_1->post_to_wall
        // }
        counter += 1
        callableFunctions.get(prop) match {
          case Some(ActionDefinition(_,in,out,body,_)) =>
            if (out.length > 1) throw TouchException("Inliner does not support multiple return values for now")

            // First all renaming... inline might change counter
            val resultExpression =
              if (out.length == 1) LocalReference(rename(out.head.ident))
              else Literal(TypeName("Number"),"0") // Ugly hack that represents an empty expression
            val renamedBody = body map (rename _)
            val renamedArguments = in map (x => rename(x.ident))

            // Now inline argument expressions (functions called in arguments), inline body create arg assignments
            val (newArgExpr,argSmts) = (args map(inline _)).unzip
            val argAssignments = (renamedArguments zip newArgExpr) map
              (x => AssignStatement(List(LocalReference(x._1)),x._2))
            val inlinedBody = renamedBody flatMap (inline _)
            val resultingStmts = (argSmts flatten) ::: argAssignments ::: inlinedBody

            (resultExpression,resultingStmts)
          case None => throw TouchException("Call to unknown function")
        }
      case Access(subj,prop,args) =>
        val (newSubjExpr,subjSmts) = inline(subj)
        val (newArgExpr,argSmts) = (args map(inline _)).unzip
        (Access(newSubjExpr,prop,newArgExpr),subjSmts:::(argSmts flatten))
      case x => (x,Nil)
    }
  }

  def rename(smt:Statement):Statement = {
    smt match {
      case ExpressionStatement(expr) => ExpressionStatement(rename(expr))
      case AssignStatement(left,right) => AssignStatement((left map (rename _)).asInstanceOf[List[LValue]],rename(right))
      case For(loc,up,body) => For(rename(loc),rename(up),body map(rename _))
      case While(cond,body) => While(rename(cond),body map(rename _))
      case Foreach(loc,coll,guards,body) => Foreach(rename(loc),rename(coll),guards map(rename _),body map(rename _))
      case If(cond,then,els) => If(rename(cond),then map(rename _),els map(rename _))
      case x => x
    }
  }

  def rename(expr:Expression):Expression = {
    expr match {
      case LocalReference(id) => LocalReference(rename(id))
      case Access(subj,prop,args) => Access(rename(subj),prop,args map (rename _))
      case x => x
    }
  }

  def rename(string:String):String = {
    "__"+string+"_"+counter
  }

}
