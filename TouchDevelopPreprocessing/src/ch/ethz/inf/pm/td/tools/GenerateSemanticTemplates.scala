/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi.WebASTImporter
import net.liftweb.json.JsonAST.{JArray, JField, JObject}
import net.liftweb.json._

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

  def typeNameToScalaType(t: TypeName): String = {
    (if (t.arguments.nonEmpty) "G"
    else "T") +
      t.ident.replace(" ", "_") +
      (if (t.arguments.nonEmpty) {
        "(" + t.arguments.map(typeNameToScalaType).mkString(", ") + ")"
      } else "")
  }

  def typeNameToScalaCreationCode(t: TypeName): String = {
    "TypeName(\"" + t.ident + "\"" +
      (if (t.arguments.nonEmpty) ", List(" + t.arguments.map {
        typeNameToScalaType(_) + ".typeName"
      }.mkString(", ") + ")"
      else "") +
      (if (t.isSingleton) ", isSingleton = true" else "") +
      ")"
  }

  def main(args: Array[String]) {

    val json =
      if (args.isEmpty) parse(fetchFile(API_URL))
      else parse(fetchLocalFile(args.head))

    // save for diffing
    var i = 0
    var file = new File(GEN_DIR + "API." + i + ".json")
    while (file.exists()) {
      i = i + 1
      file = new File(GEN_DIR + "API." + i + ".json")
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
      val isData = typ \ "isData" match {
        case JBool(x) => x;
        case _ => false
      }

      var superClass = if (!isData) {
        "ASingleton"
      } else {
        "AAny"
      }

      var myTypeName = TypeName(name, isSingleton = !isData)
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
            myTypeName = WebASTImporter.makeTypeName(paramType, isSingleton = !isData)
          }
        }
      }

      val scalaTypeName = typeNameToScalaCreationCode(myTypeName)

      val className =
        if (isData && myTypeName.arguments.nonEmpty) "G" + name.replace(" ", "_")
        else if (isData) "T" + name.replace(" ", "_")
        else "S" + name.replace(" ", "_")

      // ============================================================
      /// DETERMINE SUBTYPING
      // ============================================================

      var keyType: Option[String] = None
      var valueType: Option[String] = None
      var actionArgs: List[String] = Nil
      className match {
        case "TSongs" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TSong")
        case "TPictures" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TPicture")
        case "TXml_Object" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TXml_Object")
        case "TAppointment_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TAppointment")
        case "TPicture_Albums" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TPicture_Album")
        case "TMedia_Player_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TMedia_Player")
        case "TMedia_Server_Collection" => superClass = "ALinearCollection"; keyType = Some("TNumber"); valueType = Some("TMedia_Server")
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
        case "TCollection_Message_Action" => superClass = "AAction"; actionArgs = List("GCollection(TMessage)")
        case "TNumber_Action" => superClass = "AAction"; actionArgs = List("TNumber")
        case "TPosition_Action" => superClass = "AAction"; actionArgs = List("TNumber", "TNumber")
        case "TVector_Action" => superClass = "AAction"; actionArgs = List("TNumber", "TNumber", "TNumber", "TNumber")
        case "TSprite_Action" => superClass = "AAction"; actionArgs = List("TSprite_Action")
        case "TSprite_Set_Action" => superClass = "AAction"; actionArgs = List("TSprite_Set")
        case "TText_Action" => superClass = "AAction"; actionArgs = List("TString")
        case "TWeb_Response_Action" => superClass = "AAction"; actionArgs = List("TWeb_Response")
        case "TVector_Action" => superClass = "AAction"; actionArgs = List("TNumber", "TNumber", "TNumber", "TNumber")
        case "GComparison" => ??? // update to have superclass AAction
        case "GConverter" => ??? // update to have superclass AAction
        case "GNumber_Converter" => ??? // update to have superclass AAction
        case "GPredicate" => ??? // update to have superclass AAction
        case "GString_Converter" => ??? // update to have superclass AAction
        case "TAtomic_Action" => ??? // update to have superclass AAction
        case _ => ()
      }

      var filter = Set("is invalid", "post to wall", ",", ":", ":=", "async", "equals")
      superClass match {
        case "AAction" => filter = filter + "run"
        case "ACollection" => filter = filter ++ Set("count", "copy", "to json", "from json", "at index")
        case "ALinearCollection" => filter = filter ++ Set("count", "copy", "to json", "from json", "at", "random", "rand")
        case "AMap" => filter = filter ++ Set("count", "copy", "to json", "from json", "at", "set at", "set many",
          "remove", "clear", "keys")
        case "AMutable_Collection" => filter = filter ++ Set("count", "copy", "to json", "from json", "at", "random", "rand", "set at",
          "remove", "add", "add many", "clear", "index of", "insert at", "remove", "remove at", "reverse",
          "sort", "contains")
        case _ => ()
      }

      // ============================================================
      /// CONSTRUCT HEADERS
      // ============================================================

      val abstractDecl =
        if (isData && myTypeName.arguments.nonEmpty) {
          val defs = myTypeName.arguments.map {
            "def " + typeNameToScalaType(_) + ":AAny"
          }.mkString("\n  ")
          s"""trait Default_$className extends $superClass {
              |
             |  $defs
           """.stripMargin
        } else s"trait Default_$className extends $superClass {"

      val scalaDecl =
        if (isData && myTypeName.arguments.nonEmpty) {
          val defs = myTypeName.arguments.map {
            typeNameToScalaType(_) + ":AAny"
          }.mkString(", ")
          s"case class $className ($defs) extends Default_$className {"
        } else s"object $className extends Default_$className {"


      var fieldsDecl = List[String]()
      var fields = List[String]()

      // ============================================================
      /// DUMP DEFAULT
      // ============================================================

      val p = new PrintWriter(new File(GEN_DIR + "Default_" + className + ".scala"))

      try {

        p.println(
          s"""
             |package ch.ethz.inf.pm.td.defsemantics
             |
            |import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
             |import ch.ethz.inf.pm.td.parser.TypeName
             |import ch.ethz.inf.pm.td.semantics._
             |
            |/**
             | * Specifies the abstract semantics of $name
             | *
             | * $help
             | *
             | * @author Lucas Brutschy
             | */
             |
            |$abstractDecl
             |
            |  lazy val typeName = $scalaTypeName
          """.stripMargin
        )

        if (keyType.isDefined) p.println("  def keyType = " + keyType.get + "\n")
        if (valueType.isDefined) p.println("  def valueType = " + valueType.get + "\n")
        if (superClass == "AAction") p.println("  def actionArguments = List(" + actionArgs.map("ApiParam(" + _ + ")").mkString(",") + ")\n")

        // ============================================================
        // PRINT DECLARATIONS.
        // ============================================================

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
                Some("ApiParam(" + typeNameToScalaType(WebASTImporter.makeTypeName(paramType)) + mutatedString + ")")
              } else {
                thisType = "ApiParam(this" + mutatedString + ")"
                None
              }
            }).flatten.mkString(", ")

            assert(thisType != "")

            val ret = typeNameToScalaType(WebASTImporter.makeTypeName(resultTyp))

            val propFieldName = "member_" + propJsName
            val fieldFieldName = "field_" + propJsName

            if (ret != "TNothing" && parameters.isEmpty && isPure) {
              fieldsDecl ::= s"""lazy val $fieldFieldName = ApiField("$propName",$ret)"""
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
              s"""  def $propFieldName = ApiMember(
                  |    name = "$propName",
                  |    paramTypes = List($parameters),
                  |    thisType = $thisType,
                  |    returnType = $ret,
                  |    semantics = $semantics
                  |  )""".stripMargin + "\n")
            Some(s""""$propName" -> $propFieldName""")

          } else None

        }).flatten.mkString(",\n    ")

        if (members.nonEmpty) {
          p.println(
            s"""
               |  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
               |    $members
               |  )
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

      // ============================================================
      /// DUMP DEFAULT
      // ============================================================


      val p2 = new PrintWriter(new File(GEN_DIR + className + ".scala"))

      try {

        p2.println(
          s"""
             |package ch.ethz.inf.pm.td.semantics
             |
            |import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
             |import ch.ethz.inf.pm.td.parser.TypeName
             |import ch.ethz.inf.pm.td.defsemantics.Default_$className
             |
            |/**
             | * Customizes the abstract semantics of $name
             | *
             | * $help
             | *
             | * @author Lucas Brutschy
             | */
             |
            |$scalaDecl
          """.stripMargin
        )

        if (fields.nonEmpty) {
          val fieldDeclString = fieldsDecl.mkString("\n//  ")
          p2.println(
            s"""
               |//  $fieldDeclString
                  """.stripMargin
          )
          val fieldListString = fields.mkString(",\n//    ")
          p2.println(
            s"""
               |//  override lazy val possibleFields = super.possibleFields ++ Set(
               |//    $fieldListString
               |//  )
                  """.stripMargin
          )
        }

        p2.println(
          s"""
             |}
          """.stripMargin
        )

      } finally {
        p2.close()
      }


    }
  }
}
