/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.inference.SilverExporter
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.json.prettyRender
import viper.silver.ast.pretty.FastPrettyPrinter.{pretty => prettyPrint}
import viper.silver.{ast => sil}

/**
  * Exports the changes to an extended program as json. For programs that did already contain some specifications
  * we do not export new specifications but report an error (in json format).
  * All changes and errors are accompanied by a Position in the original silver program.
  *
  * See section 5.2.3 in Ruben K&auml;lin's thesis for the definition of the JSON output.
  *
  * @tparam S The type of the state.
  * @author Flurin Rindisbacher
  */
trait SilverJsonExporter[S <: State[S]]
  extends SilverExporter[S] {

  def extractSpecifications(other: SilverJsonExporter[_]): Unit = {
    preconditions = other.preconditions
    postconditions = other.postconditions
    invariants = other.invariants
  }

  private var preconditions: Map[sil.Position, (Seq[sil.Exp], Seq[sil.Exp])] = Map.empty
  private var postconditions: Map[sil.Position, (Seq[sil.Exp], Seq[sil.Exp])] = Map.empty
  private var invariants: Map[sil.Position, (Seq[sil.Exp], Seq[sil.Exp])] = Map.empty

  override def exportPreconditions(method: sil.Method, inferred: Seq[sil.Exp]): Unit = {
    val position = method.body.pos
    preconditions.get(position) match {
      case Some(entry) => preconditions = preconditions + (position -> (method.pres, entry._2 ++ inferred))
      case None => preconditions = preconditions + (position -> (method.pres, inferred))
    }
  }

  override def exportPostconditions(method: sil.Method, inferred: Seq[sil.Exp]): Unit = {
    val position = method.body.pos
    postconditions.get(position) match {
      case Some(entry) => postconditions = postconditions + (position -> (method.posts, entry._2 ++ inferred))
      case None => postconditions = postconditions + (position -> (method.posts, inferred))
    }
  }

  override def exportInvariants(loop: sil.While, inferred: Seq[sil.Exp]): Unit = {
    val position = loop.body.pos
    invariants.get(position) match {
      case Some(entry) => invariants = invariants + (position -> (loop.invs, entry._2 ++ inferred))
      case None => invariants = invariants + (position -> (loop.invs, inferred))
    }
  }

  // For debugging/development you may want to enable prettyRender to have a look at the JSON
  //private def render = prettyRender _
  private def render = prettyRender _

  private val ErrorTag = "Error"
  private val SampleInferenceTag = "SpecificationInference"
  private val SampleInferenceErrorTag = "sample.error.inferenceOmitted"
  private val JsonKeyPreconditions = "preconditions"
  private val JsonKeyPostconditions = "postconditions"
  private val JsonKeyInvariants = "invariants"

  /**
    * Convert Seq[Exp] to the JValue for json export.
    * arg consists of (position, (old specifications, new specifications))
    *
    * @param keyword They keyword to prefix the specs. Usually "invariant", "requires" or "ensures"
    * @param arg     (position, (old specifications, new specifications))
    * @return A JValue representing the list of specifications
    */
  private def specToJsonDSL(keyword: String)(arg: (sil.Position, (Seq[sil.Exp], Seq[sil.Exp]))): JValue = arg match {
    case (pos: sil.Position, (_, specifications: Seq[sil.Exp])) =>
      val (start, _) = formatPosition(pos)
      ("position" -> start) ~
        ("specifications" -> specifications.map(spec => s"$keyword ${prettyPrint(spec)}"))
  }

  /**
    * Formats a Position to the format the IDE expects. The format is LINE_NO:COLUMN_NO.
    * If they are not known &lt;unknown line&gt; and &lt;unknown column&gt; are used.
    * See section 5.2.3 in Ruben K&auml;lin's thesis.
    *
    * @param pos The position to format
    * @return A tuple containing the formatted start and end locations as a string.
    */
  private def formatPosition(pos: sil.Position): (String, String) = pos match {
    case sil.SourcePosition(_, start, end) =>
      (s"${start.line}:${start.column}",
        if (end.isDefined)
          s"${end.get.line}:${end.get.column}"
        else
          "<unknown line>:<unknown column>")

    case s: sil.HasLineColumn => (s"${s.line}:${s.column}", "<unknown line>:<unknown column>")
    case _ => ("<unknown line>:<unknown column>", "<unknown line>:<unknown column>")
  }

  /**
    * Exports the specifications for the extended program or an error message. Both in JSON format.
    *
    * @param file The path to the file for which the JSON is exported
    * @return A JSON-string according to the VIPER IDE protocol
    */
  def specificationsAsJson(file: String): String = {
    val existing = preconditions.values.map(_._1).find(_.nonEmpty)
      .orElse(postconditions.values.map(_._1).find(_.nonEmpty))
      .orElse(invariants.values.map(_._1).find(_.nonEmpty))

    existing match {
      case Some(e) =>
        val (start, end) = formatPosition(e.head.pos)
        render(
          ("type" -> ErrorTag) ~
            ("file" -> file) ~
            ("errors" ->
              List(
                ("start" -> start) ~
                  ("end" -> end) ~
                  ("tag" -> SampleInferenceErrorTag) ~
                  ("message" -> "Inference omitted since some specifications already exist.")
              ))
        )
      case None =>
        render(
          ("type" -> SampleInferenceTag) ~
            ("file" -> file) ~
            (JsonKeyPreconditions -> preconditions.map(specToJsonDSL("requires"))) ~
            (JsonKeyPostconditions -> postconditions.map(specToJsonDSL("ensures"))) ~
            (JsonKeyInvariants -> invariants.map(specToJsonDSL("invariant")))
        )
    }
  }
}