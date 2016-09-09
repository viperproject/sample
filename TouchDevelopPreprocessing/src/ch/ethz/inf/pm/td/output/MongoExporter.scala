/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleError}
import ch.ethz.inf.pm.sample.util.AccumulatingTimer
import ch.ethz.inf.pm.td.compiler.{SpaceSavingProgramPoint, TouchCompiler, TouchProgramPoint}
import com.mongodb.MongoException
import com.mongodb.casbah.Imports._


object MongoExporter {

  lazy val client = {
    MongoClient()("tb")("analysisJobs")
  }

}

class MongoExporter extends ErrorExporter {

  def setDebugInformation(s: String) {

    try {
      MongoExporter.client.update(MongoDBObject("jobID" -> Exporters.jobID), $set("debug" -> s))
    } catch {
      case x: MongoException => Exporters.exportToMongo = false // Mongo disabled
      case x: java.net.ConnectException => Exporters.exportToMongo = false // Mongo disabled
    }

  }

  def setStatus(s: String) {

    try {
      MongoExporter.client.update(MongoDBObject("jobID" -> Exporters.jobID), $set("status" -> s))
    } catch {
      case x: MongoException => Exporters.exportToMongo = false // Mongo disabled
      case x: java.net.ConnectException => Exporters.exportToMongo = false // Mongo disabled
    }

  }

  def exportWarnings(compiler: TouchCompiler) {

    try {
      def getPP(pp: ProgramPoint): String = pp match {
        case touchPP: SpaceSavingProgramPoint => touchPP.fullPosString
        case touchPP: TouchProgramPoint => touchPP.fullPosString
        case _ => ""
      }

      val result = for (SampleError(id, message, pp, causes) <- Reporter.assertionViolations) yield {
        MongoDBObject(
          "id" -> id,
          "message" -> message,
          "pp" -> getPP(pp),
          "causes" -> causes.map { x => MongoDBObject("message" -> x._1, "pp" -> getPP(x._2))}
        )
      }

      val dummies = for (m <- Reporter.impreciseSemantics) yield {
        MongoDBObject(
          "message" -> m.message,
          "pp" -> getPP(m.pp)
        )
      }

      val bottoms = for (m <- Reporter.unreachableCode) yield {
        MongoDBObject(
          "message" -> m.message,
          "pp" -> getPP(m.pp)
        )
      }

      val libs = for (id <- compiler.parsedTouchScripts.keySet - compiler.mainID) yield {
        MongoDBObject(
          "id" -> id
        )
      }

      val timings = for ((str,tm) <- AccumulatingTimer.times) yield {
        MongoDBObject(
          str.replaceAll("[^A-Za-z]+","") -> tm.sum
        )
      }

      val html = new HTMLExporter().export(compiler.parsedTouchScripts)

      MongoExporter.client.update(MongoDBObject("jobID" -> Exporters.jobID), $set(
        "result" -> result,
        "html" -> html,
        "dummies" -> dummies,
        "bottoms" -> bottoms,
        "libs" -> libs,
        "timings" -> timings
      ))

    } catch {
      case x: MongoException => Exporters.exportToMongo = false // Mongo disabled
      case x: java.net.ConnectException => Exporters.exportToMongo = false // Mongo disabled
    }


  }

}
