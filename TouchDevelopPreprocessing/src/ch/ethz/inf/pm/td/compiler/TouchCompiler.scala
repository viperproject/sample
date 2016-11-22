/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.compiler

import java.util.NoSuchElementException

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, Dispatcher}
import ch.ethz.inf.pm.td.parser.{LibraryDefinition, Script, _}
import ch.ethz.inf.pm.td.semantics._
import ch.ethz.inf.pm.td.transform.{LoopUnroller, LoopRewriter}
import ch.ethz.inf.pm.td.typecheck.Typer
import ch.ethz.inf.pm.td.webapi.{ScriptQuery, WebASTImporter}

import scala.io.Source

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

  var mainPublicMethods: Set[MethodDeclaration] = Set.empty
  var mainPrivateMethods: Set[MethodDeclaration] = Set.empty
  var events: List[MethodDeclaration] = Nil
  var globalData: Set[FieldDeclaration] = Set.empty
  var relevantLibraryFields: Set[String] = Set.empty

  var isInLibraryMode = false

  val cfgGenerator = new CFGGenerator(this)

  override def compile(compilable: Compilable): List[ClassDefinition] = {
    compilable match {
      case Compilable.Identifier(id) =>
        val ((script,_), pubID) = ScriptRetriever.getPath(id)
        compileScript(script,pubID)
      case Compilable.Path(path) =>
        val ((script,_), pubID) = ScriptRetriever.getPath(path.toAbsolutePath.toString)
        compileScript(script,pubID)
      case Compilable.Code(label,code) =>
        val script = ScriptParser(code)
        compileScript(script,label)
    }
  }

  private def compileScript(script:Script, pubID:String): List[ClassDefinition]= {

    // Compile
    main = compileScriptRecursive(script, pubID)
    mainID = pubID

    val mainTouchMethods = main.methods.toSet
    val allTouchMethods = parsedScripts.flatMap(_.methods)

    // We analyze public methods from the main class, events from the main class but globalData from all files (library)
    mainPublicMethods = mainTouchMethods filter {
      tm =>
        !tm.name.asInstanceOf[TouchMethodIdentifier].isPrivate && !tm.name.asInstanceOf[TouchMethodIdentifier].isEvent
    }

    mainPrivateMethods = mainTouchMethods filter {
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

  private def compileScriptRecursive(script: Script, pubID: String, libDef: Option[LibraryDefinition] = None): ClassDefinition = {

    // update fields
    libDef match {
      case Some(LibraryDefinition(name, _, _, _, _, _, _, _)) => parsedNames = parsedNames ::: List(name)
      case None => parsedNames = parsedNames ::: List(pubID)
    }

    // recursive for libs
    val libDefs = discoverRequiredLibraries(script)
    // FIXME: This should actually be checking for parsed names not parsed ids, right?
    for (lib <- libDefs; if !parsedNames.contains(lib.name) && !lib.libIdentifier.isEmpty) {
      val ((libScript,_), libPubID) = ScriptRetriever.getPath("td://" + lib.libIdentifier)
      compileScriptRecursive(libScript, libPubID, Some(lib))
    }

    val rewrittenScript = LoopUnroller.unroll(LoopRewriter(script),TouchAnalysisParameters.get.numberOfUnrollings)
    Typer.processScript(rewrittenScript)
    parsedTouchScripts += ((pubID, rewrittenScript))
    val newCFG = cfgGenerator.process(rewrittenScript, pubID, libDef)
    parsedScripts = parsedScripts ::: List(newCFG)

    isInLibraryMode = script.isLibrary

    newCFG
  }

  def getSourceCode(path: String): String = {
    val ((script,_), pubID) = ScriptRetriever.getPath(path)
    var parsed = Set(pubID)

    def getSourceCodeRecursive(pubID: String): String = {
      if (!parsed.contains(pubID) && !pubID.isEmpty) {
        val ((libScript,_), libPubID) = ScriptRetriever.getPath("td://" + pubID)
        parsed = parsed + libPubID
        val libDef = discoverRequiredLibraries(libScript)
        PrettyPrinter(libScript) + (for (lib <- libDef) yield {
          getSourceCodeRecursive(lib.libIdentifier)
        }).mkString("\n")
      } else ""
    }

    val libDef = discoverRequiredLibraries(script)
    PrettyPrinter(script) + (for (lib <- libDef) yield {
      getSourceCodeRecursive(lib.libIdentifier)
    }).mkString("\n")
  }

  def getNativeMethodsSemantics: List[NativeMethodSemantics] = List(Dispatcher)

  def extensions(): List[String] = List("td", "json")

  def label: String = "TouchDevelop"

  /**
   * Discovers all libraries required by a script
   * @param script The AST of a Script
   * @return A list of PublicIDs for the required libraries
   */
  private def discoverRequiredLibraries(script: Script): List[LibraryDefinition] = {
    script.declarations.foldLeft(List[LibraryDefinition]())((libs: List[LibraryDefinition], dec: Declaration) => dec match {
      case l@LibraryDefinition(_, _, _, _, _, _, _, _) => l :: libs
      case _ => libs
    })
  }

  def getMethod(name: String, parameters: List[Type]): Option[MethodDeclaration] = {
    for (clazz <- parsedScripts; method <- clazz.methods) yield {
      if (method.name.toString.equals(name) && method.arguments.head.size == parameters.size) {
        var ok: Boolean = true
        for (i <- method.arguments.head.indices) {
          if (!parameters(i).lessEqual(method.arguments.head(i).typ))
            ok = false
        }
        if (ok) return Some(method)
      }
    }
    None
  }

  def getMethod(name: String, classType: Type, parameters: List[Type]): Option[(MethodDeclaration, Type)] = {
    getMethodWithClassDefinition(name, classType, parameters) match {
      case Some(mdecl) => Some((mdecl, mdecl.ownerType))
      case None => None
    }
  }

  def getMethodWithClassDefinition(name: String, classType: Type, parameters: List[Type]): Option[MethodDeclaration] = {
    val matches = (for (clazz <- parsedScripts; if clazz.typ.name == classType.name; method <- clazz.methods) yield {
      if (method.name.toString.equals(name) && method.arguments.head.size == parameters.size) {
        var ok: Boolean = true
        for (i <- method.arguments.head.indices) {
          if (!parameters(i).lessEqual(method.arguments.head(i).typ))
            ok = false
        }
        if (ok) return Some(method)
        else None
      } else None
    }).flatten

    if (matches.length == 1)
      matches.head
    else if (matches.isEmpty)
      None
    else throw TouchException("Local or library call may resolve to multiple methods.")
  }

  def getPublicMethods: Set[MethodDeclaration] = mainPublicMethods

  def getPrivateMethods: Set[MethodDeclaration] = mainPrivateMethods

  def getMethods(name: String): List[(ClassDefinition, MethodDeclaration)] =
    for (mdecl <- mainPublicMethods.toList if mdecl.name.toString == name)
    yield (mdecl.classDef, mdecl)

  def reset() {
    TypeList.reset()
    main = null
    mainPublicMethods = Set.empty
    mainPrivateMethods = Set.empty
    events = Nil
    globalData = Set.empty
    parsedNames = Nil
    relevantLibraryFields = Set.empty
    parsedScripts = Nil
    parsedTouchScripts = Map.empty
    isInLibraryMode = false
  }

  def generateTopType() {
    SystemParameters.typ = TNothing.top()
  }

  override def allMethods: List[MethodDeclaration] = (mainPublicMethods ++ mainPrivateMethods ++ events).toList

}
