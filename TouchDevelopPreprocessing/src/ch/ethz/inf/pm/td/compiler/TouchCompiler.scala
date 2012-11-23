package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation._
import io.Source
import ch.ethz.inf.pm.td.parser.{Declaration, LibraryDefinition, Script, ScriptParser}
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
class TouchCompiler extends ch.ethz.inf.pm.sample.oorepresentation.Compiler {

  var parsedIDs : Set[String] = Set[String]()
  var parsedScripts : List[ClassDefinition] = Nil

  /**
   *
   * A runnable method is a method that can be executed directly by the user.
   * It is the set of methods that should be analyzed.
   *
   */
  type RunnableMethods = Map[ClassDefinition,Set[RunnableMethodDeclaration]]
  var runnableMethods : RunnableMethods = Map.empty

  /**
   Takes a path OR a URL
   */
  def compileFile(path: String): List[ClassDefinition] = {
    val (source,pubID) =
      if (path.startsWith("http")) (Source.fromURL(path),Scripts.pubIDfromURL(path))
      else (Source.fromFile(path),Scripts.pubIDfromFilename(path))
    compileString(source.getLines().mkString("\n"),pubID)
  }

  def compileString(scriptStr:String, pubID:String): List[ClassDefinition] = {
    val script = LoopRewriter(ScriptParser(scriptStr))
    Typer.processScript(script)
    var cfgs = List(CFGGenerator.process(script,pubID))
    parsedScripts = cfgs ::: parsedScripts
    parsedIDs = parsedIDs + pubID
    val libIDs = discoverRequiredLibraries(script)
    for (id <- libIDs; if (!parsedIDs.contains(id))) {
       cfgs = cfgs ::: compileFile(Scripts.codeURLfromPubID(id))
    }
    cfgs
  }

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = {
    List(
      new SAssert(),
      new SBazaar(),
      new TBoard(),
      new TBoolean(),
      new SCode(this),
      new TColor(),
      new SColors(),
      new SInvalid(),
      new SMath(),
      new SMedia(),
      new TNumber(),
      new TPicture(),
      new TSprite(),
      new SSenses(),
      new SWall(),
      new TVector3()
    )
  }

  def extensions(): List[String] = List("td")

  def getLabel(): String = "TouchDevelop"


  /**
   * Discovers all libraries required by a script
   * @param script The AST of a Script
   * @return A list of PublicIDs for the required libraries
   */
  private def discoverRequiredLibraries(script:Script):List[String] = {
    script.declarations.foldLeft(List[String]())( (ids:List[String],dec:Declaration) => dec match {
      case LibraryDefinition(_,pubID,_,_) => pubID :: ids
      case _ => ids
    })
  }

  /**
   * USING THIS METHOD, YOU GET THE SEMANTICS FOR A FUNCTION THAT IS CALLED FROM ANOTHER FUNCTION
   */
  def getCalledMethod(name: String, parameters: List[Type]): Option[MethodDeclaration] = {
    val methods = parsedScripts.map(_.methods).flatten.filter
      {x:MethodDeclaration => x.name.toString.equals(name) && x.arguments.apply(0).size==parameters.size}
    if (methods.length == 1) {
      val m = methods.head
      var ok : Boolean = true
      if(m.arguments.size!=1) throw new TouchException("Not yet supported")
      for(i <- 0 to m.arguments.apply(0).size-1) {
        if(! parameters.apply(i).lessEqual(m.arguments.apply(0).apply(i).typ))
          ok=false
      }
      if(ok) return new Some(m)
    }
    None
  }

  /**
   * USING THIS METHOD, YOU GET THE SEMANTICS FOR A RUNNABLE FUNCTION (WITH EVENT LOOP AFTERWARDS)
   */
  def getMethod(name: String, classType: Type, parameters: List[Type]): Option[(MethodDeclaration, Type)] = {
    getClassDeclaration(classType) match {
      case Some(classe) =>
        for(m <- runnableMethods(classe))
          if(m.name.toString.equals(name) && m.arguments.apply(0).size==parameters.size) {
            var ok : Boolean = true
            if(m.arguments.size!=1) throw new TouchException("Not yet supported")
            for(i <- 0 to m.arguments.apply(0).size-1) {
              if(! parameters.apply(i).lessEqual(m.arguments.apply(0).apply(i).typ))
                ok=false
            }
            if(ok) return new Some[(MethodDeclaration, Type)]((m, classType))
          }
        None
      case None => None
    }
  }

  private def getClassDeclaration(t : Type) : Option[ClassDefinition] = {
    for(c <- parsedScripts)
      if(c.typ.equals(t))
        return Some(c)
    None
  }

}
