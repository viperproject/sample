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

//val dirs = "nonterminating"
//val pubIDs = List("htre", "vexbxpjs", "kijx")

val dirs = "compilerErrors2"
val pubIDs = "tpef, hbft, yslk, xfmua, pogi, yhlba, nbdv, uvtj, eivm, oqxd, dbuk, laogsfab, xcrx, wizx, isry, pbje, ozdlhigg, xydl, gvrt, fogn, shxx, ffag, owiw, jaef, uswy, vtzo, oajw, jxop, nlfk, rztvztkq, vzdsuwnd, xnegtvnu, jgzlsjeh, okje, rqvba, dbarlscj, nlpowdcw, xlfw, kbuy, fmks, xrnj, gqxz, oeok, rjxu, heoq, jtdd, wgtt, jeow, wrsx, xuki, dcgk, xycn, bvhu, buqt, tjpy, gfhh, dddwa, kjiu, nivua, lxpz, cyks, uadla, tooga, acyi, jjbw, xweva, kdrv, qhsz, cnsu, xkvn, huqv, frzd, auvya, kwtc, duara, vxcga, ntbn, lhxxb, bqnm, ledg, wrkq, xgmu, jywla, kwkra, ywaw, vvzk, gxus, shlk, lhht, upzpa, ovix, tdnj, nhcp, ykio, dmqt, kzvu, hjbb, xrfj, arwba, mnvmalwg, rgakphwg, xoopbbgb, akhz, bgdx, zyzh, dfmc, nome, lcto, rwby, lmsd, jklv, uime, dzdba, fqxv, isvn, uhed, rxlqb, bkzw, nszi, uhmz, dkjd, dwih, vgmy, epae, nufm, jqyp, gaig, mgeu, txkm, nanj, frnu, vsns, yave, keod, arok, aixo, ihxt, xtrt, ilod, tsht, vpbf, zjnra, ufmq, sdjw, ytnu, cbgoa, ysnu, oqze, yonn, thpz, cnuo, jlleglgd, dksc, qexb, iflz, tgdw, sxxgtlco, xcls, sezw, dskr, ilcm, gxeq, vgsq, wool, oasz, qssc, qanh, vjuv, cpfp, patj, sngp, txuv, qfhi, jtfn, rfwv, gpvx, umao, ttiw, sbob, tsmt, cbos, jvzu, cfjx, qfdb, dylx, lwhx, fkqq, bwtj, iush, bihw, sbog, txih, tizp, rqdr, tgdi, ejke, tbiv, noaha, ghin, sxek, ggmy, xqud, nvlj, ipet, urgo, eutj, ooxo, efkk, gxhj, bzjc, aagm, miph, osjc, azmwb, gzgl, imfv, ujyb, bthu, xqvja, dnoo, wyjc, zhac, upes, trur, dusp, jpmf, xhmh, qptr, hbkx, zmeq, ojvu, uaah, kbzk, olbj, luum, nqwn, zgab, gwgl, sacj, ggjs, uivv, giph, jile, rrpc, rkyk, hhkz, amaha, piyz, waiq, kmaj, tktv, hbei, tenl".split(", ")


for (pubID <- pubIDs) {

  val dir = new File("TouchDevelopPreprocessing/testfiles/"+dirs)

  if (dir.isDirectory || dir.mkdir()) {
    val p = new PrintWriter(new File(dir+"/"+pubID+".td"))
    try {
      p.println(fetchFile(Scripts.codeURLfromPubID(pubID)))
    } finally { p.close() }
  } else { println("could not create dir") }

}

