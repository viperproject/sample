/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.tools

import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
  * @author Lucas Brutschy
  */
object AnalyzeRecords {

  case class Session(id: String, numberOfEvents: Int, firstEventTime: Int, programID: String)

  def getSessions: List[Session] = {

    val settings = TouchAnalysisParameters.get
    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient(settings.mongoServer, settings.mongoPort)
    val collection = mongoClient(settings.mongoDatabase)("records")
    val result = collection.aggregate(
      List(
        MongoDBObject(
          "$group" -> MongoDBObject(
            "_id" -> "$sessionID",
            "sessionID" -> MongoDBObject("$first" -> "$sessionID"),
            "numberOfEvents" -> MongoDBObject("$sum" -> 1),
            "firstEventTime" -> MongoDBObject("$min" -> "$retrievedAt"),
            "programID" -> MongoDBObject("$first" -> "$programID")
          )
        )
      )
    )

    (for (doc <- result.results) yield {
      Session(
        doc.getAsOrElse[String]("sessionID", ""),
        doc.getAsOrElse[Int]("numberOfEvents", 0),
        doc.getAsOrElse[Int]("firstEventTime", 0),
        doc.getAsOrElse[String]("programID", "")
      )
    }).toList

  }

  def analyzeSession(id: String) = ???

  def main(args: Array[String]) {

    val Session = """-session=([\w+])""".r

    for (arg <- args) {

      arg match {

        case Session(session) =>

          analyzeSession(session)

        case "-list" =>

          println(getSessions.sortBy(_.firstEventTime).mkString("\n"))
          1
        case "-interactive" =>

          println(getSessions.sortBy(_.firstEventTime).zipWithIndex.mkString("\n"))
          while (true) {
            val index = try {
              Some(scala.io.StdIn.readLine().toInt)
            } catch {
              case x: NumberFormatException => None
            }
            val validIndex = index.filter(a => a < getSessions.length && a >= 0)
            validIndex.foreach { x =>
              analyzeSession(getSessions(x).id)
            }
          }

        case "-all" =>

          for (s <- getSessions) {
            analyzeSession(s.id)
          }

      }

    }

  }

}
