import ch.ethz.inf.pm.td.webapi.URLFetcher._
import java.io.{File, PrintWriter}
import net.liftweb.json._
import net.liftweb.json.JsonAST.{JArray, JField, JObject}

/**
 * Parses the new JSON based API description an generates semantic templates
 */

val url = "https://www.touchdevelop.com/api/language/apis?releaseid=2520367591921199337-1b0ee8a7.10c7.4d91.a1b2.b6159b9db272-42803"
val json = parse(fetchFile(url))

// save for diffing
var i = 0
var file = new File("TouchDevelopPreprocessing/gen/API."+i+".json")
while (file.exists()) {
  i = i + 1
  file = new File("TouchDevelopPreprocessing/gen/API."+i+".json")
}
val p = new PrintWriter(file)
p.print(pretty(render(json)))
p.close()

def argListToString(l: List[String]) = {
  l.map({
    x: String => x.replace(" ", "_")
  }).mkString(",")
}

for (
  JObject(root) <- json;
  JField("types", JArray(types)) <- root;
  typ <- types
) {
  val JString(name) = typ \ "name"
  val JString(help) = typ \ "help"
  val isData = (typ \ "isData") match { case JBool(x) => x ; case _ => false }

  val className =
    if (isData) "T" + name.replace(" ","_")
    else "S" + name.replace(" ","_")

  val p = new PrintWriter(new File("TouchDevelopPreprocessing/gen/" + className + ".scala"))

  try {

    p.println(
      """
        |package ch.ethz.inf.pm.td.semantics
        |
        |import ch.ethz.inf.pm.td.compiler.TouchType
        |import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
        |import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
        |
        |/**
        | * Specifies the abstract semantics of XXX
        | *
        | * YYY
        | *
        | * @author Lucas Brutschy
        | */ """.stripMargin.replace("XXX", name).replace("YYY", help)
    )

    p.println(
      """
        |object XXX {
        |
        |  val typName = "YYY"
        |  val typ = new TouchType(typName,isSingleton = true)
        |
        |}
        |
        |class XXX extends AAny {
        |
        |  def getTyp = XXX.typ
        |
        |  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
        |                                     (implicit pp:ProgramPoint,state:S):S = method match {
      """.stripMargin.replace("XXX", className).replace("YYY", name))

    var fields : List[String] = Nil
    for (
      JField("properties", JArray(properties)) <- typ;
      property <- properties.sortBy(_.toString)
    ) {

      val JString(propName) = property \ "name"
      val JString(propHelp) = property \ "help"
      val JString(resultTyp) = property \ "result" \ "type"

      if (propName != "is invalid" && propName != "post to wall") {

        val paramList = (for (
          JField("parameters", JArray(parameters)) <- property;
          parameter <- parameters
        ) yield {

          val JString(paramName) = parameter \ "name"
          val JString(paramType) = parameter \ "type"

          if (paramName != "this")
            Some(paramName, paramType)
          else
            None

        }).flatten

        val (argList, typList) = paramList.unzip

        p.println("    /** " + propHelp.trim + " */")
        p.println("    // case \"" + propName + "\" => ")
        p.println("    //   val List(" + argListToString(argList) + ") = parameters // " + argListToString(typList))
        if (resultTyp != "Nothing") {
          p.println("    //   TopWithInvalid[S](T" + resultTyp.replace(" ", "_") + ".typ)")
        } else {
          p.println("    //   Skip")
        }

        // Is this just a getter for a field?
        if (args.isEmpty && resultTyp != "Nothing") {
          p.println("    // DECLARATION AS FIELD: ")
          p.println("    //   /** " + propHelp.trim + " */")
          p.println("    //   val field_" + propName.replace(" ", "_") + " = new TouchField(\"" + propName + "\",T" + resultTyp.replace(" ", "_") + ".typ)")
          fields = fields ::: List("field_" + propName.replace(" ", "_"))
        }

        p.println()
      }

    }

    p.println("    // FIELDS: " + fields.mkString(", ") + "\n")

    p.println(
      """    case _ =>
        |      super.forwardSemantics(this0,method,parameters,returnedType)
        |
        |  }
        |}
      """.stripMargin)

  } finally {
    p.close()
  }

}
