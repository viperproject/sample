package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation._
import io.Source
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.typecheck.Typer
import ch.ethz.inf.pm.td.webapi.{WebASTImporter, ScriptQuery}
import ch.ethz.inf.pm.td.transform.LoopRewriter
import ch.ethz.inf.pm.td.semantics._
import scala.Some
import ch.ethz.inf.pm.td.parser.LibraryDefinition
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.sample.SystemParameters

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:50 PM
 *
 */
class TouchCompiler extends ch.ethz.inf.pm.sample.oorepresentation.Compiler {


  var main: ClassDefinition = null
  var mainID: String = null
  var parsedNames: List[String] = Nil

  /**
   * A list of scripts in "Simple" representation
   */
  var parsedScripts: List[ClassDefinition] = Nil

  /**
   * A map from public ID to TouchDevelop ASTs. This includes the main script and all libraries
   */
  var parsedTouchScripts: Map[String, Script] = Map.empty

  var publicMethods: Set[MethodDeclaration] = Set.empty
  var privateMethods: Set[MethodDeclaration] = Set.empty
  var events: Set[MethodDeclaration] = Set.empty
  var globalData: Set[FieldDeclaration] = Set.empty
  var relevantLibraryFields: Set[String] = Set.empty
  var userTypes: Map[String, AAny] = Map.empty

  var isInLibraryMode = false

  val cfgGenerator = new CFGGenerator(this)

  /**
   * This takes one of the following arguments:
   *
   * http://www.touchdevelop.com/api/[pubID]/... Some URL to a script
   * https://www.touchdevelop.com/api/[pubID]/... Some URL to a script
   * td://[pubID] Some PubID in uri form
   * Some path to a local file with extension .td for source code.
   * Some path to a local file with extension .json for a cached json representation
   *
   * It uses either the WebAST importer or the script parser to get the corresponding
   * TouchDevelop AST. If a URL or a pubID is provided, we may use the local cache
   *
   */
  def retrieveScript(path: String): (Script, String) = {
    if (path.startsWith("http://"))
      (ScriptCache.get(ScriptQuery.pubIDfromURL(path)), ScriptQuery.pubIDfromURL(path))
    else if (path.startsWith("https://"))
      (ScriptCache.get(ScriptQuery.pubIDfromURL(path)), ScriptQuery.pubIDfromURL(path))
    else if (path.startsWith("td://"))
      (ScriptCache.get(path.substring(5)), path.substring(5))
    else if (path.toLowerCase.endsWith(".td"))
      (ScriptParser(Source.fromFile(path).getLines().mkString("\n")), ScriptQuery.pubIDfromFilename(path))
    else if (path.toLowerCase.endsWith(".json"))
      (WebASTImporter.convertFromString(Source.fromFile(path).getLines().mkString("\n")), ScriptQuery.pubIDfromFilename(path))
    else throw TouchException("Unrecognized path " + path)
  }


  def compileFile(path: String): List[ClassDefinition] = {

    val (script, pubID) = retrieveScript(path)

    // Compile
    main = compileScriptRecursive(script, pubID)
    mainID = pubID

    val allTouchMethods = main.methods.toSet

    // We analyze public methods from the main class, events from the main class but globalData from all files (library)
    publicMethods = allTouchMethods filter {
      tm =>
        !tm.name.asInstanceOf[TouchMethodIdentifier].isPrivate && !tm.name.asInstanceOf[TouchMethodIdentifier].isEvent
    }

    privateMethods = allTouchMethods filter {
      tm =>
        tm.name.asInstanceOf[TouchMethodIdentifier].isPrivate
    }

    events = allTouchMethods filter {
      tm =>
        tm.name.asInstanceOf[TouchMethodIdentifier].isEvent
    }

    globalData = Set.empty
    for (c <- parsedScripts) {
      globalData ++= c.fields
    }

    parsedScripts

  }

  def compileScriptRecursive(script: Script, pubID: String, libDef: Option[LibraryDefinition] = None): ClassDefinition = {

    val rewrittenScript = LoopRewriter(script)
    Typer.processScript(rewrittenScript)

    // update fields
    libDef match {
      case Some(LibraryDefinition(name, _, _, _)) => parsedNames = parsedNames ::: List(name)
      case None => parsedNames = parsedNames ::: List(pubID)
    }
    parsedTouchScripts += ((pubID, rewrittenScript))

    // recursive for libs
    val libDefs = discoverRequiredLibraries(rewrittenScript)
    // FIXME: This should actually be checking for parsed names not parsed ids, right?
    for (lib <- libDefs; if !parsedNames.contains(lib.name) && !lib.pubID.isEmpty) {
      val (libScript, libPubID) = retrieveScript("td://" + lib.pubID)
      compileScriptRecursive(libScript, libPubID, Some(lib))
    }

    val newCFG = cfgGenerator.process(rewrittenScript, pubID, libDef)
    parsedScripts = parsedScripts ::: List(newCFG)


    isInLibraryMode = script.isLibrary

    newCFG
  }

  def getSourceCode(path: String): String = {
    val (script, pubID) = retrieveScript(path)
    var parsed = Set(pubID)

    def getSourceCodeRecursive(pubID: String): String = {
      if (!parsed.contains(pubID) && !pubID.isEmpty) {
        val (libScript, libPubID) = retrieveScript("td://" + pubID)
        parsed = parsed + libPubID
        val libDef = discoverRequiredLibraries(libScript)
        PrettyPrinter(libScript) + (for (lib <- libDef) yield {
          getSourceCodeRecursive(lib.pubID)
        }).mkString("\n")
      } else ""
    }

    val libDef = discoverRequiredLibraries(script)
    PrettyPrinter(script) + (for (lib <- libDef) yield {
      getSourceCodeRecursive(lib.pubID)
    }).mkString("\n")
  }

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = {
    (new Libraries() :: TypeList.types.values.toList) ::: userTypes.values.toList
  }

  def getSemantics(name: String): AAny = {
    TypeList.types.get(name) match {
      case Some(x) => x
      case None =>
        userTypes.get(name) match {
          case Some(x) => x
          case None =>
            throw new TouchException("Could not find type " + name)
        }
    }
  }

  def extensions(): List[String] = List("td", "json")

  def getLabel(): String = "TouchDevelop"


  /**
   * Discovers all libraries required by a script
   * @param script The AST of a Script
   * @return A list of PublicIDs for the required libraries
   */
  private def discoverRequiredLibraries(script: Script): List[LibraryDefinition] = {
    script.declarations.foldLeft(List[LibraryDefinition]())((libs: List[LibraryDefinition], dec: Declaration) => dec match {
      case l@LibraryDefinition(_, _, _, _) => l :: libs
      case _ => libs
    })
  }

  def getMethod(name: String, parameters: List[Type]): Option[MethodDeclaration] = {
    for (clazz <- parsedScripts; method <- clazz.methods) yield {
      if (method.name.toString.equals(name) && method.arguments.apply(0).size == parameters.size) {
        var ok: Boolean = true
        for (i <- 0 to method.arguments(0).size - 1) {
          if (!parameters(i).lessEqual(method.arguments(0)(i).typ))
            ok = false
        }
        if (ok) return Some(method)
      }
    }
    return None
  }

  def getMethod(name: String, classType: Type, parameters: List[Type]): Option[(MethodDeclaration, Type)] = {
    getMethodWithClassDefinition(name, classType, parameters) match {
      case Some(mdecl) => Some((mdecl, mdecl.ownerType))
      case None => None
    }
  }

  def getMethodWithClassDefinition(name: String, classType: Type, parameters: List[Type]): Option[MethodDeclaration] = {
    val matches = (for (clazz <- parsedScripts; if (clazz.typ.name == classType.name); method <- clazz.methods) yield {
      if (method.name.toString.equals(name) && method.arguments.apply(0).size == parameters.size) {
        var ok: Boolean = true
        for (i <- 0 to method.arguments(0).size - 1) {
          if (!parameters(i).lessEqual(method.arguments(0)(i).typ))
            ok = false
        }
        if (ok) return Some(method)
        else None
      } else None
    }).flatten

    if (matches.length == 1)
      matches.head
    else if (matches.length == 0)
      None
    else throw new TouchException("Local or library call may resolve to multiple methods.")
  }

  def getPublicMethods: Set[MethodDeclaration] = publicMethods

  def getPrivateMethods: Set[MethodDeclaration] = privateMethods

  def getMethods(name: String): List[(ClassDefinition, MethodDeclaration)] =
    for (mdecl <- publicMethods.toList if mdecl.name.toString == name)
    yield (mdecl.classDef, mdecl)

  def reset() {
    main = null
    publicMethods = Set.empty
    privateMethods = Set.empty
    events = Set.empty
    globalData = Set.empty
    parsedNames = Nil
    relevantLibraryFields = Set.empty
    parsedScripts = Nil
    parsedTouchScripts = Map.empty
    userTypes = Map.empty
    isInLibraryMode = false
    SRecords.reset()
  }

  def generateTopType() {
    SystemParameters.typ = DefaultTouchType("__TMP__").top()
  }

  override def allMethods: List[MethodDeclaration] = ???
}
