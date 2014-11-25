import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import java.io.{File, PrintWriter}
import ch.ethz.inf.pm.td.webapi.WebASTImporter
import net.liftweb.json._
import net.liftweb.json.JsonAST.{JArray, JField, JObject}

/**
 * Parses the new JSON based API description an generates type information
 */

object GenerateTypeInformation {

  def main(args: Array[String]) {
    val url = "https://www.touchdevelop.com/api/language/apis"
    val json = parse(fetchFile(url))

    val p = new PrintWriter(new File("TouchDevelopPreprocessing/gen/StdLib.scala"))

    try {

      p.println(
        """package ch.ethz.inf.pm.td.typecheck
          |
          |import GenericTypes._
          |
          |/**
          | *
          | * AUTOGENERATED BY generate_type_information.scala
          | *
          | * Lucas Brutschy
          | *
          | */
          |
          |trait StdLib extends AbstractSymbolTable {
          |
          |
        """.stripMargin)

      for (
        JObject(root) <- json;
        JField("types", JArray(types)) <- root;
        typ <- types
      ) {
        val JString(name) = typ \ "name"
        val JString(help) = typ \ "help"
        val isData = typ \ "isData" match {
          case JBool(x) => x;
          case _ => false
        }

        val fields = (for (
          JField("properties", JArray(properties)) <- typ;
          property <- properties
        ) yield {
          val JString(propName) = property \ "name"
          val JString(propHelp) = property \ "help"
          val JString(resultTyp) = property \ "result" \ "type"

          val paramList = (for (
            JField("parameters", JArray(parameters)) <- property;
            parameter <- parameters
          ) yield {

            val JString(paramName) = parameter \ "name"
            val JString(paramType) = parameter \ "type"

            if (paramName != "this")
              Some(WebASTImporter.makeTypeName(paramType).makeCode)
            else
              None

          }).flatten

          // For collections, we also generate "at index" and "copy", which are required to implement foreach loops
          val additional =
            if (propName == "at") {
              List(
                "    Member(\"at index\", List(\"Number\"), \"" + resultTyp + "\") /* ONLY INSIDE SAMPLE: an accessor which is always number based */",
                "    Member(\"copy\", \"" + name + "\") /* ONLY INSIDE SAMPLE: cloning a collection*/"
              )
            } else {
              Nil
            }


          if (paramList.size > 0) {
            additional ::: List("    Member(\"" + propName + "\", List(" + paramList.mkString(", ") + "), " + WebASTImporter.makeTypeName(resultTyp).makeCode + ") /* " + propHelp + " */")
          } else {
            additional ::: List("    Member(\"" + propName + "\", " + WebASTImporter.makeTypeName(resultTyp).makeCode + ") /* " + propHelp + " */")
          }

        }).flatten

        p.println("  // " + help)
        if (!isData) {
          p.println("  addSingleton(\"" + name + "\", gAlsoSingletons(TypeName(\"" + name + "\")) ::: List(")
        } else {
          p.println("  addType(\"" + name + "\", gAny(TypeName(\"" + name + "\")) ::: List(")
        }
        p.println(fields.mkString(" ,\n"))
        p.println("  ))")
        p.println("")

      }

      p.println(
        """
          |}
        """.stripMargin)


    } finally {
      p.close()
    }
  }
}
