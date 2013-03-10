package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation._
import io.Source
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.symbols.Typer
import ch.ethz.inf.pm.td.webapi.Scripts
import ch.ethz.inf.pm.td.transform.{Matcher, LoopRewriter}
import ch.ethz.inf.pm.td.semantics._
import scala.Some
import ch.ethz.inf.pm.td.parser.LibraryDefinition
import ch.ethz.inf.pm.td.parser.Script
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:50 PM
 *
 */
class TouchCompiler extends ch.ethz.inf.pm.sample.oorepresentation.Compiler {

  var main : ClassDefinition = null
  var mainID : String = null
  var parsedNames : List[String] = Nil
  var parsedScripts : List[ClassDefinition] = Nil
  var parsedSourceStrings : Map[String,String] = Map.empty
  var publicMethods : Set[(ClassDefinition,MethodDeclaration)] = Set.empty
  var events : Set[(ClassDefinition,MethodDeclaration)] = Set.empty
  var globalData : Set[FieldDeclaration] = Set.empty
  var relevantLibraryFields : Set[String] = Set.empty

  var types : Map[String,AAny] = Map(
    SAssert.typName -> new SAssert(),
    SBazaar.typName -> new SBazaar(),
    SBox.typName -> new SBox(),
    SCode.typName -> new SCode(),
    SCollections.typName -> new SCollections(),
    SColors.typName -> new SColors(),
    SHome.typName -> new SHome(),
    SInvalid.typName -> new SInvalid(),
    SLanguages.typName -> new SLanguages(),
    SLocations.typName -> new SLocations(),
    SMaps.typName -> new SMaps(),
    SMath.typName -> new SMath(),
    SMedia.typName -> new SMedia(),
    SPhone.typName -> new SPhone(),
    SPlayer.typName -> new SPlayer(),
    SRadio.typName -> new SRadio(),
    SRecords.typName -> new SRecords(),
    SSenses.typName -> new SSenses(),
    SSocial.typName -> new SSocial(),
    STags.typName -> new STags(),
    STime.typName -> new STime(),
    SWall.typName -> new SWall(),
    SWeb.typName -> new SWeb(),
    TAppointment_Collection.typName -> new TAppointment_Collection(),
    TAppointment.typName -> new TAppointment(),
    TBoard.typName -> new TBoard(),
    TBoolean.typName -> new TBoolean(),
    TCamera.typName -> new TCamera(),
    TColor.typName -> new TColor(),
    TContact_Collection.typName -> new TContact_Collection(),
    TContact.typName -> new TContact(),
    TDateTime.typName -> new TDateTime(),
    TDevice_Collection.typName -> new TDevice_Collection(),
    TDevice.typName -> new TDevice(),
    TJson_Object.typName -> new TJson_Object(),
    TLink_Collection.typName -> new TLink_Collection(),
    TLink.typName -> new TLink(),
    TLocation_Collection.typName -> new TLocation_Collection(),
    TLocation.typName -> new TLocation(),
    TMap.typName -> new TMap(),
    TMedia_Link_Collection.typName -> new TMedia_Link_Collection(),
    TMedia_Link.typName -> new TMedia_Link(),
    TMedia_Player_Collection.typName -> new TMedia_Player_Collection(),
    TMedia_Player.typName -> new TMedia_Player(),
    TMedia_Server_Collection.typName -> new TMedia_Server_Collection(),
    TMedia_Server.typName -> new TMedia_Server(),
    TMessage_Collection.typName -> new TMessage_Collection(),
    TMessage.typName -> new TMessage(),
    TMotion.typName -> new TMotion(),
    TNothing.typName -> new TNothing(),
    TNumber_Collection.typName -> new TNumber_Collection(),
    TNumber_Map.typName -> new TNumber_Map(),
    TNumber.typName -> new TNumber(),
    TPage_Button.typName -> new TPage_Button(),
    TPage_Collection.typName -> new TPage_Collection(),
    TPage.typName -> new TPage(),
    TPicture_Album.typName -> new TPicture_Album(),
    TPicture_Albums.typName -> new TPicture_Albums(),
    TPicture.typName -> new TPicture(),
    TPictures.typName -> new TPictures(),
    TPlace_Collection.typName -> new TPlace_Collection(),
    TPlace.typName -> new TPlace(),
    TPlaylist.typName -> new TPlaylist(),
    TPlaylists.typName -> new TPlaylists(),
    TPrinter_Collection.typName -> new TPrinter_Collection(),
    TPrinter.typName -> new TPrinter(),
    TSong_Album.typName -> new TSong_Album(),
    TSong_Albums.typName -> new TSong_Albums(),
    TSong.typName -> new TSong(),
    TSongs.typName -> new TSongs(),
    TSound.typName -> new TSound(),
    TSprite.typName -> new TSprite(),
    TSprite_Set.typName -> new TSprite_Set(),
    TString_Collection.typName -> new TString_Collection(),
    TString_Map.typName -> new TString_Map(),
    TString.typName -> new TString(),
    TTextBox.typName -> new TTextBox(),
    TTile.typName -> new TTile(),
    TVector3.typName -> new TVector3(),
    TWeb_Request.typName -> new TWeb_Request(),
    TWeb_Response.typName -> new TWeb_Response(),
    TXml_Object.typName -> new TXml_Object()
  )

  /**
  Takes a path OR a URL
    */
  def compileFile(path: String): List[ClassDefinition] = {
    val (source,pubID) =
      if (path.startsWith("http")) (Source.fromURL(path),Scripts.pubIDfromURL(path))
      else (Source.fromFile(path),Scripts.pubIDfromFilename(path))
    compileString(source.getLines().mkString("\n"),pubID)
  }

  def compileStringRecursive(scriptStr:String, pubID:String, libDef:Option[LibraryDefinition] = None): ClassDefinition = {

    // compile
    val script = LoopRewriter(ScriptParser(scriptStr))
    Typer.processScript(script)
    var newCFG =  CFGGenerator.process(script,pubID,libDef)
    var cfgs = List(newCFG)

    // update fields
    parsedScripts = parsedScripts ::: List(newCFG)
    libDef match {
      case Some(LibraryDefinition(name,_,_,_)) => parsedNames = parsedNames ::: List(name)
      case None => parsedNames = parsedNames ::: List(pubID)
    }
    parsedSourceStrings += ((pubID,scriptStr))

    // recursive for libs
    val libDefs = discoverRequiredLibraries(script)
    // FIXME: This should actually be checking for parsed names not parsed ids, right?
    for (lib <- libDefs; if (!parsedNames.contains(lib.name))) {
      compileStringRecursive(Source.fromURL(Scripts.codeURLfromPubID(lib.pubID)).getLines().mkString("\n"),lib.pubID,Some(lib))
    }

    newCFG
  }

  def compileString(scriptStr:String, pubID:String): List[ClassDefinition] = {

    // Compile
    main = compileStringRecursive(scriptStr,pubID)
    mainID = pubID

    // We analyze public methods from the main class, events from the main class but globalData from all files (library)
    publicMethods = (main.methods filter {
      m:MethodDeclaration =>
        !m.name.asInstanceOf[TouchMethodIdentifier].isPrivate && !m.name.asInstanceOf[TouchMethodIdentifier].isEvent
    }).map((main,_)).toSet

    events = (main.methods filter {
      m:MethodDeclaration =>
        m.name.asInstanceOf[TouchMethodIdentifier].isEvent
    }).map((main,_)).toSet

    globalData = Set.empty
    for (c <- parsedScripts) {
      globalData ++= c.fields
    }

    parsedScripts
  }

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = {
    new Libraries() :: types.values.toList
  }

  def extensions(): List[String] = List("td")

  def getLabel(): String = "TouchDevelop"


  /**
   * Discovers all libraries required by a script
   * @param script The AST of a Script
   * @return A list of PublicIDs for the required libraries
   */
  private def discoverRequiredLibraries(script:Script):List[LibraryDefinition] = {
    script.declarations.foldLeft(List[LibraryDefinition]())( (libs:List[LibraryDefinition],dec:Declaration) => dec match {
      case l@LibraryDefinition(_,_,_,_) => l :: libs
      case _ => libs
    })
  }

  def getMethod(name: String, classType: Type, parameters: List[Type]): Option[(MethodDeclaration, Type)] = {
    val methods = parsedScripts.map(_.methods).flatten.filter
    {x:MethodDeclaration => x.name.toString.equals(name) && x.arguments.apply(0).size==parameters.size}
    if (methods.length == 1) {
      val m = methods.head
      var ok : Boolean = true
      for(i <- 0 to m.arguments(0).size-1) {
        if(! parameters(i).lessEqual(m.arguments(0)(i).typ))
          ok=false
      }
      if(ok) return new Some((m,classType))
    }
    None
  }

  def getMethodWithClassDefinition(name: String, classType: Type, parameters: List[Type]): Option[(ClassDefinition, MethodDeclaration)] = {
    val matches = (for (clazz <- parsedScripts; if (clazz.typ.getName().equals(classType.getName())); method <- clazz.methods) yield {
      if (method.name.toString.equals(name) && method.arguments.apply(0).size==parameters.size) {
        var ok : Boolean = true
        for(i <- 0 to method.arguments(0).size-1) {
          if(! parameters(i).lessEqual(method.arguments(0)(i).typ))
            ok=false
        }
        if(ok) return Some((clazz,method))
        else None
      } else None
    }).flatten

    if (matches.length == 1)
      matches.head
    else if (matches.length == 0)
      None
    else throw new TouchException("Local or library call may resolve to multiple methods.")
  }

  def getMethod(name: String, parameters: List[Type]): Option[(ClassDefinition, MethodDeclaration)] = {
    val matches = (for (clazz <- parsedScripts) yield getMethodWithClassDefinition(name,clazz.typ,parameters)).flatten
    if (matches.length == 1) Some(matches.head)
    else if (matches.length == 0) None
    else throw new TouchException("Local or library call may resolve to multiple methods.")
  }

  def getPublicMethods: Set[(ClassDefinition,MethodDeclaration)] = publicMethods

  def getMethods(name:String): List[(ClassDefinition,MethodDeclaration)] =
    (publicMethods filter (_._2.name.toString == name)).toList

  def reset() {
    main = null
    publicMethods = Set.empty
    events = Set.empty
    globalData = Set.empty
    parsedNames = Nil
    relevantLibraryFields = Set.empty
    parsedScripts = Nil
    parsedSourceStrings = Map.empty
  }


}
