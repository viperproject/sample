/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.tools

import ch.ethz.inf.pm.td.webapi.{ScriptQuery, URLFetcher}

/**
  * Finds all scripts using a specific construct
  */
object VersionsUsersLines {

  val usedScripts =
    """#kqfnc
      |#ornb
      |#uvjba
      |#qnpge
      |#uvlma
      |#whpgc
      |#qwidc
      |#wbuei
      |#gcane
      |#qzeua
      |#ruef
      |#wkvhc
      |#padg
      |#kzwue
      |wccqepeb
      |kmac
      |cvuz
      |qzju
      |cavke
      |gbtxe
      |eddm
      |kjxzcgcv
      |ohgxa
      |fqaba
      |etww
      |eijba
      |oxhs
      |ycxbc
      |nggfa
      |blqz
      |nvoha
      |sxjua""".stripMargin.split("\n").filterNot(_.startsWith("#"))

  def main(args: Array[String]) {

    for (arg <- usedScripts) {

      val record = ScriptQuery.getScriptRecord(arg)
      var users = Set(record.userid)
      var versions = Set(record.id)
      var codes = Map(record.id -> URLFetcher.fetchFile(record.getCodeURL).split("\n").length)

      up(record.baseid)
      down(record.updateid)
      FindConstruct.getLibs(arg).foreach(lib)

      def up(id: String): Unit = {
        if (!id.isEmpty && !versions.contains(id)) {
          versions = versions + id
          val record = ScriptQuery.getScriptRecord(id)
          users = users + record.userid
          up(record.baseid)
        }
      }

      def down(id: String): Unit = {
        if (!id.isEmpty && !versions.contains(id)) {
          versions = versions + id
          val record = ScriptQuery.getScriptRecord(id)
          users = users + record.userid
          down(record.updateid)
        }
      }

      def lib(id: String): Unit = {
        if (!id.isEmpty && !codes.contains(id)) {
          val record = ScriptQuery.getScriptRecord(id)
          codes = codes + (record.id -> URLFetcher.fetchFile(record.getCodeURL).split("\n").length)
          FindConstruct.getLibs(id).foreach(lib)
        }
      }

      val userC = users.size
      val versionC = versions.size
      val codeC = codes.values.sum
      println(s"$arg\t$userC\t$versionC\t$codeC")

    }
  }


}
