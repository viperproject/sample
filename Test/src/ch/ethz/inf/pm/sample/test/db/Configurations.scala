package ch.ethz.inf.pm.sample.test.db

import ch.ethz.inf.pm.td.webapi._

case class Configuration(label: String,
                         compiler: String,
                         analysis: String,
                         parameters: Map[String, String],
                         heapAnalysis: String,
                         heapParameters: Map[String, String],
                         property: String)

object Configurations {

  def defaultTouchDevelopConfiguration: Configuration =
    Configuration("TDdefault(may,must,oct)", "TouchDevelop", "TouchDevelop/APRON", Map("Domain" -> "ApronOctagons"),
      "MayAndMustDomain(Program point)", Map.empty, "All checks")

  val configs: List[Configuration] = List(defaultTouchDevelopConfiguration)

}

object Iterators {

  val allScripts = new ScriptQuery
  val topScripts = new ScriptQuery with ErrorFilter with TopService
  val newScripts = new ScriptQuery with ErrorFilter with NewService
  val rootScripts = new ScriptQuery with ErrorFilter with RootFilter with LibraryFilter
  val veryRecentScripts = {
    val x = new ScriptQuery with ErrorFilter with RootFilter with NewService
    x.setLimit(20)
    x
  }

  val scriptsIn2013 = {
    val x = new ScriptQuery with ErrorFilter with RootFilter with NewService with DateFilter
    x.setLimit(20)
    x.setDate("01/01/2013")
    x
  }

  val iterators = List(allScripts, topScripts, newScripts, veryRecentScripts, rootScripts, scriptsIn2013).toArray[IteratorOverPrograms]

}