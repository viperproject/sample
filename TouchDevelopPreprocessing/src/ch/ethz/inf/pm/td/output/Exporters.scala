/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.execution.AnalysisResult
import ch.ethz.inf.pm.td.compiler.TouchCompiler

import scala.collection.immutable.ListSet

/**
  * Exports results into various formats
  *
  * @author Lucas Brutschy
  */
object Exporters {


  var jobID: String = System.nanoTime().toString

  var resultExporters: ListSet[ResultExporter] =
    ListSet(
      HTMLExporter,
      //TSVExporter,
      //JSONExporter,
      //MongoExporter,
      LabeledGraphExporter
    )

  var statusExporters: ListSet[StatusExporter] =
    ListSet(
      //MongoExporter
    )

  def exportResults(compiler: TouchCompiler, results: List[AnalysisResult]) {
    resultExporters.foreach(_.exportResults(compiler, results))
  }

  def setStatus(s: String): Unit = {
    statusExporters.foreach(_.setStatus(s))
  }

  def setDebugInformation(s: String): Unit = {
    statusExporters.foreach(_.setDebugInformation(s))
  }

  def disable(exporter: Exporter): Unit = {
    resultExporters = resultExporters.filter(_ != exporter)
    statusExporters = statusExporters.filter(_ != exporter)
  }

  def enable(exporter: Exporter): Unit = {
    exporter match {
      case x: ResultExporter =>
        resultExporters = resultExporters + x
      case _ => ()
    }
    exporter match {
      case x: StatusExporter =>
        statusExporters = statusExporters + x
      case _ => ()
    }
  }

}

trait Exporter

/**
  * Exports the analysis result to a string of some format (HTML, TSV, JSON etc.)
  *
  * @author Lucas Brutschy
  */
trait ResultExporter extends Exporter {

  def exportResults(compiler: TouchCompiler, results: List[AnalysisResult])

}


/**
  * Exports the current status
  *
  * @author Lucas Brutschy
  */
trait StatusExporter extends Exporter {

  def setDebugInformation(s: String)

  def setStatus(s: String)

}