import ch.ethz.inf.pm.td.webapi.{TopService, ScriptQuery}

val list = (for (s <- new ScriptQuery with TopService) yield {
  println(s.id)
  (s.id, s.installations, s.runs, s.rootid == s.id)
}).toList

println("sorted")
for ((id, install, runs, isRoot) <- list.sortBy(_._3).reverse) {
  println(id + "\t" + runs + "\t" + install + "\t" + isRoot)
}