import ch.ethz.inf.pm.td.analysis.TestRunner

val toAnalyze =
  """aabda
    |aabja
    |aacgb
    |aacpivoy
    |aacv
    |aaczyknp
    |aads
    |aaeha
    |aaeq
    |aagca
    |aagf
    |aagya
    |aaib
    |aaisa
    |aajcb
    |aakfhcrj
    |aalra
    |aanpa
    |aany
    |aaoma
    |aaoweirj
    |aapq
    |aapx
    |aapzacrw
    |aarh
    |aasza
    |aatk
    |aaub
    |aauh
    |aavbvjbh
    |aavj
    |aavna
    |aawlauxh
    |aawt
    |aaxdb
    |aaxd
    |aayr
    |abap
    |abaua
    |abbbmzhy
    |abdva
    |abes
    |abfe
    |abfq
    |abfz
    |abgeurfl
    |abgrrcyn
    |abhdvpey
    |abhe
    |abilwiun
  """.stripMargin.split("[\n,]").map(_.trim)

def analyzer(id:String) {
  if(!id.isEmpty)
    TestRunner.runIdWithApron(id)
}

for(a <- toAnalyze) {
  analyzer(a)
}
