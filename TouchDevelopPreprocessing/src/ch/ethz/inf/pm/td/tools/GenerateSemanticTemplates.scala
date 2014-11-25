import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.webapi
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import java.io.{File, PrintWriter}
import ch.ethz.inf.pm.td.webapi.WebASTImporter
import net.liftweb.json._
import net.liftweb.json.JsonAST.{JArray, JField, JObject}

/**
 * Parses the new JSON based API description an generates semantic templates
 */

object GenerateSemanticTemplates {
  
  val API_URL = "https://www.touchdevelop.com/api/language/apis"
  val GEN_DIR = "TouchDevelopPreprocessing/gen/"

  def argListToString(l: List[String]) = {
    l.map({
      x: String => x.replace(" ", "_")
    }).mkString(",")
  }

  def typeNameToScalaType(t:TypeName):String = {
    (if (t.arguments.nonEmpty) "G"
    else "T") +
      t.ident.replace(" ","_") +
      (if (t.arguments.nonEmpty) {
        "("+t.arguments.map(typeNameToScalaType).mkString(", ")+")"
      } else "")
  }

  def typeNameToScalaCreationCode(t:TypeName):String = {
    "TypeName(\""+t.ident+"\"" +
      (if (t.arguments.nonEmpty) ", List("+t.arguments.map{typeNameToScalaType(_)+".typeName"}.mkString(", ")+")"
      else "") +
      ")"
  }

  def main(args: Array[String]) {

    val json =
      if (args.isEmpty) parse(fetchFile(API_URL))
      else parse(fetchLocalFile(args.head))

    // save for diffing
    var i = 0
    var file = new File(GEN_DIR+"API."+i+".json")
    while (file.exists()) {
      i = i + 1
      file = new File(GEN_DIR+"API."+i+".json")
    }
    val p = new PrintWriter(file)
    p.print(pretty(render(json)))
    p.close()


    for (
      JObject(root) <- json;
      JField("types", JArray(types)) <- root;
      typ <- types
    ) {

      // Type Properties
      val JString(name) = typ \ "name"
      val JString(help) = typ \ "help"
      val isData = typ \ "isData" match { case JBool(x) => x ; case _ => false }

      var superClass = if (!isData) {
        "ASingleton"
      } else {
        "AAny"
      }

      var myTypeName = TypeName(name)
      val JArray(properties) = typ \ "properties"
      // if we are a generic type, we have to determine the correct type name from a "this" parameter
      if (properties.nonEmpty) {
        for (
          JField("parameters", JArray(parameters)) <- properties.head;
          parameter <- parameters
        ) yield {
          val JString(paramName) = parameter \ "name"
          val JString(paramType) = parameter \ "type"
          if (paramName == "this") {
            myTypeName =  WebASTImporter.makeTypeName(paramType)
          }
        }
      }

      val scalaTypeName = typeNameToScalaCreationCode(myTypeName)

      val className =
        if (isData && myTypeName.arguments.nonEmpty) "G" + name.replace(" ","_")
        else if (isData) "T" + name.replace(" ","_")
        else "S" + name.replace(" ","_")

      val scalaDecl =
        if (isData && myTypeName.arguments.nonEmpty)
          s"case class $className (" +  myTypeName.arguments.map{typeNameToScalaType(_) + ":AAny"}.mkString(", ") + ")"
        else s"object $className"

      // Dump the code
      val p = new PrintWriter(new File(GEN_DIR + className + ".scala"))

      try {


        p.println(
          s"""
            |package ch.ethz.inf.pm.td.semantics
            |
            |import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
            |import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
            |import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
            |import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, TouchType, ApiMember}
            |import ch.ethz.inf.pm.td.parser.TypeName
            |import ch.ethz.inf.pm.td.analysis.ApiField
            |
            |/**
            | * Specifies the abstract semantics of $name
            | *
            | * $help
            | *
            | * @author Lucas Brutschy
            | */
          """.stripMargin
        )

        // ============================================================
        /// DETERMINE SUBTYPING
        // ============================================================

        var keyType:Option[String] = None
        var valueType:Option[String] = None
        var actionArgs:List[String] = Nil
        className match {
          case "TSongs" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TSong")
          case "TPictures" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TPicture")
          case "TXml_Object" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TXml_Object")
          case "TAppointment_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TAppointment")
          case "TPicture_Albums" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TPicture_Album")
          case "TMedia_Player_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TMedia_Player")
          case "TPage_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TPage")
          case "TPlaylists" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TPlaylist")
          case "TPrinter_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TPrinter")
          case "TSong_Albums" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TSong_Album")
          case "TMedia_Link_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TMedia_Link")
          case "TContact_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TContact")
          case "TDevice_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TDevice")
          case "TBuffer" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TNumber")
          case "TBoard_Background_Scene" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TBoard_Background_Layer")
          case "TJson_Object" => superClass = "AMap"; keyType = Some("TNumber"); valueType = Some("TJson_Object")
          case "TJson_Builder" => superClass = "AMap"; keyType = Some("TString"); valueType = Some("TJson_Builder")
          case "TString_Map" => superClass = "AMap"; keyType = Some("TString"); valueType = Some("TString")
          case "TNumber_Map" => superClass = "AMap"; keyType = Some("TNumber"); valueType = Some("TNumber")
          case "TLink_Collection" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TLink")
          case "TMatrix" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TNumber")
          case "TBoard" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TSprite")
          case "TNumber_Collection" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TNumber")
          case "TMessage_Collection" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TMessage")
          case "TSprite_Set" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TSprite")
          case "TPlace_Collection" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TPlace")
          case "TString_Collection" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TString")
          case "TLocation_Collection" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TLocation")
          case "GCollection" => superClass = "AMutable_Collection"; keyType = Some("TNumber"); valueType = Some("TT")
          case "GAction1" => superClass = "AAction"; actionArgs = List("TT")
          case "GAtomic_Action1" => superClass = "AAction"; actionArgs = List("TT")
          case "TAction" => superClass = "AAction"; actionArgs = List()
          case "TBoolean_Action" => superClass = "AAction"; actionArgs = List("TBoolean_Action")
          case "TJson_Action" => superClass = "AAction"; actionArgs = List("TJson_Action")
          case "TMessage_Collection_Action" => superClass = "AAction"; actionArgs = List("TMessage_Collection")
          case "TNumber_Action" => superClass = "AAction"; actionArgs = List("TNumber")
          case "TPosition_Action" => superClass = "AAction"; actionArgs = List("TNumber","TNumber")
          case "TSprite_Action" => superClass = "AAction"; actionArgs = List("TSprite_Action")
          case "TSprite_Set_Action" => superClass = "AAction"; actionArgs = List("TSprite_Set")
          case "TText_Action" => superClass = "AAction"; actionArgs = List("TString")
          case "TWeb_Response_Action" => superClass = "AAction"; actionArgs = List("TWeb_Response")
          case _ => ()
        }

        var filter = Set("is invalid", "post to wall")
        superClass match {
          case "AAction" => filter = filter + "run"
          case "ACollection" => filter = filter ++ Set("count","copy","to json","from json")
          case "ALinearCollection" => filter = filter ++  Set("count","copy","to json","from json","at","random","rand")
          case "AMap" => filter = filter ++ Set("count","copy","to json","from json","at","random","rand","set at","set many",
            "remove","keys")
          case "AMutable_Collection" => filter = filter ++ Set("count","copy","to json","from json","at","random","rand","set at",
            "set many","remove","keys","add","add many","clear","index of","insert at","remove","remove at","reverse",
            "sort","contains")
          case _ => ()
        }

        p.println(
          s"""
            |$scalaDecl extends $superClass {
            |
            |  lazy val typeName = $scalaTypeName
          """.stripMargin
        )

        if (keyType.isDefined) p.println("  def keyTypeName = "+keyType.get+".typeName\n")
        if (valueType.isDefined) p.println("  def valueTypeName = "+valueType.get+".typeName\n")
        if (superClass == "AAction") p.println("  def actionArguments = List("+actionArgs.map("ApiParam("+_+")").mkString(",")+")\n")

        // ============================================================
        // PRINT DECLARATIONS.
        // ============================================================

        var fieldsDecl = List[String]()
        var fields = List[String]()

        val members = (for (
          JField("properties", JArray(properties)) <- typ;
          property <- properties.sortBy(_.toString)
        ) yield {
          val JString(propName) = property \ "name"
          val JString(propHelp) = property \ "help"
          val JString(propJsName) = property \ "jsName"
          val JInt(usageCount) = property \ "usage_count"
          val JString(resultTyp) = property \ "result" \ "type"

          if (!filter.contains(propName)) {

            var thisType = ""
            var isPure = true

            val parameters = (for (
              JField("parameters", JArray(parameters)) <- property;
              parameter <- parameters
            ) yield {
              val JString(paramName) = parameter \ "name"
              val JString(paramType) = parameter \ "type"
              val mutatedString = parameter \ "writesMutable" match {
                case JBool(x) if x => isPure = false; ",isMutated=true"
                case _ => ""
              }
              if (paramName != "this") {
                Some("ApiParam("+typeNameToScalaType(WebASTImporter.makeTypeName(paramType))+mutatedString+")")
              } else {
                thisType = "ApiParam(this"+mutatedString+")"
                None
              }
            }).flatten.mkString(", ")

            assert (thisType != "")

            val ret = typeNameToScalaType(WebASTImporter.makeTypeName(resultTyp))

            val propFieldName = "member_"+propJsName
            val fieldFieldName = "field_"+propJsName

            if (ret != "TNothing" && parameters.isEmpty && isPure) {
              fieldsDecl ::= s"""lazy val $fieldFieldName = ApiField("$propName",$ret.typeName)"""
              fields ::= s"""$fieldFieldName"""
            }

            val semantics = "DefaultSemantics"

            val countText = if (usageCount == 0) {
                              "Never used"
                            } else if (usageCount < 10) {
                              "Rarely used"
                            } else if (usageCount < 100) {
                              "Sometimes used"
                            } else if (usageCount < 1000) {
                              "Frequently used"
                            } else {
                              "Very frequently used"
                            }

            p.println(s"""  /** $countText: $propHelp */""")
            p.println(
              s"""  lazy val $propFieldName = new ApiMember(
                 |    name = "$propName",
                 |    paramTypes = List($parameters),
                 |    thisType = $thisType,
                 |    returnType = $ret
                 |  ) with $semantics""".stripMargin+"\n")
            Some(s""""$propName" -> $propFieldName""")

          } else None

        }).flatten.mkString(",\n    ")

        if (members.nonEmpty) {
          p.println(
            s"""
            |  override lazy val declarations:Map[String,ApiMember] = super.declarations ++ Map(
            |    $members
            |  )
            """.stripMargin
          )
        }

        if (fields.nonEmpty) {
          val fieldDeclString = fieldsDecl.mkString("\n//  ")
          p.println(
            s"""
                |//  $fieldDeclString
                """.stripMargin
          )
          val fieldListString = fields.mkString(",\n//    ")
          p.println(
            s"""
                |//  override lazy val possibleFields = super.possibleFields ++ Set(
                |//    $fieldListString
                |//  )
                """.stripMargin
            )
        }

        p.println(
          s"""
          |}
          """.stripMargin
        )

      } finally {
        p.close()
      }

    }
  }
}
