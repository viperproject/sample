import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi._

val skipSet =
  """wbnx
    |ezpja
    |sernxzmj
    |cetl
    |nzckjwoh
    |xwpx
    |rzcq
    |yggfa
    |zusbb
    |lepua
    |qdaca
    |aedm
    |qwqub
    |llnma
    |kpzca
    |pslj
    |gknyb
    |xpfib
    |zgrh
    |lpqulmbr
    |ehkdudjl
    |ejjveskm
  """.stripMargin.split("\n").map(_.trim).toSet
var i = 0

// SLOW/ DOES NOT WORK: lpqulmbr ehkdudjl ejjveskm


def analyzer(id: String) {
  if (!skipSet.contains(id)) {
    println(i + " " + id)
    i = i + 1
    TestRunner.runIdWithApron(id)
  }
}

//analyzer("mfoea")
TestRunner(new NewScripts(), 100, analyzer)


