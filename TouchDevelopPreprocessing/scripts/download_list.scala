import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi.{Scripts, Script, DowncasedTypeHints}
import java.io.{File, PrintWriter}
import net.liftweb.json._

/**
 * Downloads all scripts of a specific user to a local directory.
 *
 * Set the userid below (e.g. pboj for the TouchDevelop Samples user)
 */

//val dirs = "pietrosanalbugs"
//
//val pubIDs =
//  """tnrl
//    |zzqe
//    |exgg
//    |gjgbnarq
//    |qzrc
//    |jsnbazzf
//    |txct
//    |xmdj
//    |jsqo
//    |bgdy
//    |uxurzxoq
//    |ccxkqeym
//    |uhfb
//    |aypvivip
//    |evvvcftn
//    |hakfviol
//    |skzfpehx
//    |iwporbjy
//    |voiz
//    |qemx
//    |blok
//    |qkzbwoqk
//    |wvnp
//    |apzl
//    |vonhbnaz
//    |atzq
//    |nukpa
//    |wjod
//    |tbdj
//    |xxusa
//    |mjoz
//    |xmrsbtjg
//    |ivoq
//    |xgmt
//    |stnv
//    |ksbbguhv
//  """.stripMargin.split("\n")

//val dirs = "nullpointerbug"
//val pubIDs = "ipjk, mzjd, hlpw, cmxz, xxaw, fadz, xrkp, hxzk, zihkb, odle, tdex, qzke, zvos, bflyfahe, sdzbwros, opal, fqhi, klys, nqpn, cvwt, browzzgw, ezlh, egsx, umaxa, ytuj, eqokucjr, qvsm, hhnqa, ejyc, xdhs, jpyh, lalh, wbny, fmaixptt, eoydkuck, fwip, nqglbrax".split(", ")

val dirs = "nonterminating"
val pubIDs = List("htre", "vexbxpjs", "kijx")

for (pubID <- pubIDs) {

  val dir = new File("TouchDevelopPreprocessing/testfiles/"+dirs)

  if (dir.isDirectory || dir.mkdir()) {
    val p = new PrintWriter(new File(dir+"/"+pubID+".td"))
    try {
      p.println(fetchFile(Scripts.codeURLfromPubID(pubID)))
    } finally { p.close() }
  } else { println("could not create dir") }

}

