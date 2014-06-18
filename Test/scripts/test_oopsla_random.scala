import ch.ethz.inf.pm.td.analysis.TestRunner

val toAnalyze =
  """aaajb
    |aabda
    |aabja
    |aacgb
    |aacpivoy
    |aacv
    |aaczyknp
    |aadc
    |aads
    |aaeha
    |aaeq
    |aafp
    |aagca
    |aagf
    |aagya
    |aaib
    |aaihdntr
    |aaisa
    |aajcb
    |aakfhcrj
    |aalra
    |aanja
    |aanpa
    |aanrnxyy
    |aany
    |aaoma
    |aaoweirj
    |aapla
    |aapq
    |aapx
    |aapzacrw
    |aarh
    |aaslb
    |aaspvizp
    |aasya
    |aasza
    |aatk
    |aaub
    |aauh
    |aavbvjbh
    |aavj
    |aavna
    |aawlauxh
    |aawt
    |aawx
    |aaxd
    |aaxdb
    |aaxt
    |aayea
    |aayp
    |aayr
  """.stripMargin.split("[\n,]").map(_.trim)

def analyzer(id: String) {
  if (!id.isEmpty)
    TestRunner.runIdWithApron(id)
}

for (a <- toAnalyze) {
  analyzer(a)
}
