import ch.ethz.inf.pm.td.webapi.{URLFetcher, ScriptQuery}

var totalCount = 0
var noErrorCount = 0
var libraryCount = 0
var rootCount = 0
var noErrorRootCount = 0
var hiddenCount = 0

println("Counting scripts:")

for (s <- new ScriptQuery) {
  if (!s.haserrors) noErrorCount += 1
  if (s.rootid == s.id) rootCount += 1
  if (!s.haserrors && s.rootid == s.id) noErrorRootCount += 1
  if (s.islibrary) libraryCount += 1
  if (s.ishidden) hiddenCount += 1
  totalCount += 1
  if (totalCount % 100 == 0) println("Currently at... " + totalCount)
}

println("totalCount " + totalCount)
println("noErrorCount " + noErrorCount)
println("libraryCount " + libraryCount)
println("rootCount " + rootCount)
println("noErrorRootCount " + noErrorRootCount)
println("hiddenCount " + hiddenCount)

println("Official report:")
println(URLFetcher.fetchFile("http://www.touchdevelop.com/api/stats"))

