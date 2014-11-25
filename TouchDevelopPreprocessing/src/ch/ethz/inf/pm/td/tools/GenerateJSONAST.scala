import ch.ethz.inf.pm.td.webapi.URLFetcher._


object GenerateJSONAST {

  def main(args: Array[String]) {
    val url = "https://www.touchdevelop.com/api/language/webast?releaseid=2520367591921199337-1b0ee8a7.10c7.4d91.a1b2.b6159b9db272-42803"
    val description = fetchFile(url)

    var current = description

    current = current.replaceAll( """([^\w])type([^\w])""", "$1`type`$2")
    current = current.replaceAll( """bool""", "Boolean")
    current = current.replaceAll( """string""", "String")
    current = current.replaceAll( """number""", "Int")
    current = current.replaceAll( """(\w+)\[\]""", "Array[$1]")
    current = current.replaceAll( """\?: (\w+)""", ": Option[$1]")
    current = current.replaceAll( """;""", ",")
    current = current.replaceAll( """(?m)(?s)/\*abstract\*/ export interface (\w+)(\s+)\{([^\}]*)\}""", "class $1 ($3)")
    current = current.replaceAll( """(?m)(?s)/\*abstract\*/ export interface (\w+) extends (\w+)(\s+)\{([^\}]*)\}""", "class $1($4) extends $2")
    current = current.replaceAll( """(?m)(?s)export interface (\w+) extends (\w+)(\s+)\{([^\}]*)\}""", "case class $1($4) extends $2")
    current = current.replaceAll( """(?m)(?s)export interface (\w+)(\s+)\{([^\}]*)\}""", "case class $1($3)")
    current = current.replaceAll( """(?m)(?s),(\s*)\)""", "$1)")

    println(current)
    println("List(" + (for (m <- """case class (\w+)""".r.findAllIn(current).matchData) yield {
      "classOf[" + m.group(1) + "]"
    }).mkString(", ") + ")")
  }
}

