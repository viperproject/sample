package ch.ethz.inf.pm.td.transform

import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.parser.While
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.parser.Access
import ch.ethz.inf.pm.td.parser.ActionDefinition


/**
 * An implementation crudge that introduces calls to a "cleanup" function
 * between statements. This gives us a chance to remove temporaries variables
 * there.
 *
 * (Not recommended, dont use if not needed)
 */
object CleanupInserter {

  def insertCleanupStatements(s: Script): Script = {

    def rewriteStatement(s: Statement): List[Statement] = {
      s match {
        case es: ExpressionStatement =>
          val typName = "Helpers"
          val ppPostfix = "_cleanup"
          val target = SingletonReference(typName.toLowerCase, typName).copyPos(es).appendIdComponent(ppPostfix)
          val method = Identifier("cleanup").copyPos(es).appendIdComponent(ppPostfix)
          val cleanupStmt = ExpressionStatement(Access(target, method, Nil)).copyPos(es).appendIdComponent(ppPostfix)
          List(es, cleanupStmt)

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

