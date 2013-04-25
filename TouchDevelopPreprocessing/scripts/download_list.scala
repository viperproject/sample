import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi.Scripts
import java.io.{File, PrintWriter}

/**
 * Downloads all scripts of a specific user to a local directory.
 *
 * Set the userid below (e.g. pboj for the TouchDevelop Samples user)
 */

val dirs = "compilerErrors3"
val pubIDs =
  """hugma, enora, tldhapwn, ekkna, wrgz, tlme, qrek, lvyppebo, mgnq, gqwe, wklw, mpffa, virza, adeha, cpvsa, jlqja,
    |oweyarhk, fhkm, ozumdrpv, hmuongtj, xdbjphhk, esog, nnbbqjck, iilq,
    |lytu, ppwy, oktlfosb, afnwjekd, zwpmbosd, oyjfaijo, khuv,
    |enwxa, anvhb, bxccplly, kyom, uqpf,
    |rxomxiyt, wzlb, iolgmvcb, gobhqpmo, qwismoze,
    |incja, auxza, bgtb,
    |ndzt, fnxmwacp, vfrk,
    |fmhpmyqz, dvfjicyh,
    |coapaeau, wkjo,
    |nmhpqduo, scywnioi,
    |aufr, dbbexceu,
    |mtrsvchw, jyef,
    |adsza, abaua,
    |etjn, wuera,
    |eskd, hpcob,
    |xsslxinx, emeuoybx,
    |fqae, ghlm,
    |soac,
    |yhrva,
    |akfw,
    |xfoz,
    |nfif,
    |vlmb,
    |aufm,
    |rxkfonss,
    |uayuwhgu,
    |jherb,
    |jkfzgrhv,
    |omwibijk,
    |drxca,
    |thwy,
    |lcnxuumc,
    |hpupmcas,
    |yuvzwkiw,
    |iiippuvc,
    |npff,
    |abfq,
    |ctuha,
    |znvcpwqv,
    |qzge,
    |ridd,
    |sojl,
    |brxzatui,
    |buwb,
    |cqzwoxdw,
    |qgjh,
    |qayr,
    |lmff,
    |jrmnxzrx,
    |bgbl,
    |mclr,
    |cbleb,
    |herz,
    |ftcc,
    |unbca,
    |mbjxa,
    |cvyt,
    |bfhx,
    |eksha,
    |cjrj,
    |tvty,
    |gvnga,
    |anffpsen,
    |aaruaypx,
    |yknc,
    |vxshzilu,
    |xkfiudvt,
    |sjdn,
    |ssvoxnxq,
    |ahcra,
    |uzlx,
    |vnldzfjg,
    |caiulpth,
    |gxozpckd,
    |jnroa,
    |zpbb,
    |tzridlbn,
    |esyuvxdd,
    |zdiwifsf,
    |berxavwm,
    |ygmh,
    |kyiqvfel,
    |yactb,
    |yjsvhwnx,
    |vvdv,
    |wkdyljnu,
    |xbqsa,
    |memrzzhh,
    |xhaf,
    |yawf,
    |jfwf,
    |tury,
    |ymlxrwlh,
    |lbioa,
    |dqls,
    |mnvua,
    |vdhwa,
    |ldyy,
  """.stripMargin.split("\n").map(_.split(",")).flatten.map(_.trim).distinct


for (pubID <- pubIDs) {

  if (!pubID.isEmpty) {
    val dir = new File("TouchDevelopPreprocessing/testfiles/"+dirs)

    if (dir.isDirectory || dir.mkdir()) {
      val p = new PrintWriter(new File(dir+"/"+pubID+".td"))
      try {
        p.println(fetchFile(Scripts.codeURLfromPubID(pubID)))
      } finally { p.close() }

    } else { println("could not create dir") }
  }

}

