import ch.ethz.inf.pm.td.webapi.{URLFetcher, ScriptQuery}

object CountScripts {

  def main(args:Array[String]) {

    var totalCount = 0
    var libraryCount = 0
    var rootCount = 0
    var hiddenCount = 0

    println("Counting scripts:")

    for (s <- new ScriptQuery) {
      if (s.rootid == s.id) rootCount += 1
      if (s.islibrary) libraryCount += 1
      if (s.ishidden) hiddenCount += 1
      totalCount += 1
      if (totalCount % 100 == 0) println("Currently at... " + totalCount)
    }

    println("totalCount " + totalCount)
    println("libraryCount " + libraryCount)
    println("rootCount " + rootCount)
    println("hiddenCount " + hiddenCount)

    println("Official report:")
    println(URLFetcher.fetchFile("http://www.touchdevelop.com/api/stats"))

  }

}