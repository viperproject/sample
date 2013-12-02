import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi.ScriptListings
import java.io.{File, PrintWriter}

/**
 * Downloads all scripts from a list to a local directory in JSON format
 */

val dirName = "pldi_random"
val pubIDs =
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
  """.stripMargin.split("\n").map(_.split(",")).flatten.map(_.trim).distinct


for (pubID <- pubIDs) {

  if (!pubID.isEmpty) {
    val dir = new File("Test/test/TouchDevelop/"+dirName)

    if (dir.isDirectory || dir.mkdir()) {
      val p = new PrintWriter(new File(dir+ "/" +pubID+".json"))
      try {
        p.println(fetchFile(ScriptListings.webastURLfromPubID(pubID)))
      } finally { p.close() }

    } else { println("could not create dir") }
  }

}

