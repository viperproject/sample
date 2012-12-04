package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation._
import io.Source
import ch.ethz.inf.pm.td.parser.{Declaration, ScriptParser}
import ch.ethz.inf.pm.td.symbols.Typer
import ch.ethz.inf.pm.td.webapi.Scripts
import ch.ethz.inf.pm.td.transform.LoopRewriter
import ch.ethz.inf.pm.td.semantics._
import ch.ethz.inf.pm.td.parser.LibraryDefinition
import scala.Some
import ch.ethz.inf.pm.td.parser.Script

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:50 PM
 *
 */
class SimpleTouchCompiler extends TouchCompiler {

  override def getLabel(): String = "SimpleTouchDevelop"

  override def getMethod(name: String, classType: Type, parameters: List[Type]): Option[(MethodDeclaration, Type)] = {
    getCalledMethod(name,parameters) match {
      case Some(x) => Some(x,classType)
      case None => None
    }
  }

}
