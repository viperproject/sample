/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.{BlockPosition, CfgResult}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.json.compactRender
import viper.silver.ast.pretty.FastPrettyPrinter.{pretty => prettyPrint}
import viper.silver.ast.{HasLineColumn, Position, SourcePosition}
import viper.silver.{ast => sil}

/**
  * Mixin to collect how a program has been extended. Afterwards getSpecifications() can be used to get all changes
  * as a map from Position to (previous-specifications, new-specifications)
  *
  * This trait assumes that for EVERY extended program a new SilverExtender() with SpecificationsExporter is created.
  * (since it stores the changes to the program)
  *
  * @tparam T The type of the inferred specification.
  * @tparam S The type of the state.
  */
trait SpecificationsExporter[T, S <: State[S] with SilverSpecification[T]]
  extends SilverExtender[T, S] {

  val Pre = "preconditions"
  val Post = "postconditions"
  val Inv = "invariants"

  private var specifications: Map[String, Map[sil.Position, (Seq[sil.Exp], Seq[sil.Exp])]] = Map.empty.withDefault(_ => Map.empty)

  private def extendAndSaveResult(typeOfExtension: String, position: sil.Position, existing: Seq[sil.Exp], fun: () => Seq[sil.Exp]): Seq[sil.Exp] = {
    val inferred = fun()
    // We don't expect to see multiple changes to the same position and type (pre/post/inv)
    assert(!(specifications(typeOfExtension) contains position))
    if (inferred.nonEmpty)
      specifications += (typeOfExtension -> (specifications(typeOfExtension) + (position -> (existing, inferred))))
    inferred
  }

  abstract override def preconditions(method: sil.Method, existing: Seq[sil.Exp], position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp] = {
    extendAndSaveResult(Pre, method.pos, existing, () => super.preconditions(method, existing, position, result))
  }

  abstract override def postconditions(method: sil.Method, existing: Seq[sil.Exp], position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp] = {
    extendAndSaveResult(Post, method.pos, existing, () => super.postconditions(method, existing, position, result))
  }

  abstract override def invariants(loop: sil.While, existing: Seq[sil.Exp], position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp] = {
    extendAndSaveResult(Inv, loop.pos, existing, () => super.invariants(loop, existing, position, result))
  }

  /**
    * Returns a Map of the collected specifications. For pre-/postconditions and invariants
    * a Map from Position -> Tuple() is returned. The first element in the tuple are the "old" specifications
    * in the original program. The second element in the tuple are the inferred specifications with which
    * the program has been extended.
    *
    * @return
    */
  protected def getSpecifications: Map[String, Map[sil.Position, (Seq[sil.Exp], Seq[sil.Exp])]] = {
    specifications
  }
}

/**
  * Exports the changes to an extended program as json. For programs that did already contain some specifications
  * we do not export new specifications but report an error (in json format).
  * All changes and errors are accompanied by a Position in the original silver program.
  *
  * See section 5.2.3 in Ruben K&auml;lin's thesis for the definition of the JSON output.
  *
  * @tparam T The type of the inferred specification.
  * @tparam S The type of the state.
  */
trait SpecificationsJsonExporter[T, S <: State[S] with SilverSpecification[T]] extends SpecificationsExporter[T, S] {

  // For debugging/development you may want to enable prettyRender to have a look at the JSON
  //private def render = prettyRender _
  private def render = compactRender _

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
  private def formatPosition(pos: Position): (String, String) = pos match {
    case sourcePos@SourcePosition(_, start, end) =>
      (s"${start.line}:${start.column}",
        if (end.isDefined)
          s"${end.get.line}:${end.get.column}"
        else
          "<unknown line>:<unknown column>")

    case s: HasLineColumn => (s"${s.line}:${s.column}", "<unknown line>:<unknown column>")
    case _ => ("<unknown line>:<unknown column>", "<unknown line>:<unknown column>")
  }

  /**
    * Exports the specifications for the extended program or an error message. Both in JSON format.
    *
    * @param file The path to the file for which the JSON is exported
    * @return A JSON-string according to the VIPER IDE protocol
    */
  def specificationsAsJson(file: String): String = {
    val specs = getSpecifications
    // check whether the original program contained specifications
    val existingSpec = specs.values.find(_.values.exists(_._1.nonEmpty))
    if (existingSpec.isDefined) {
      // Some specifications exist. Just point the user to one of them
      val errorPosition = existingSpec.get.find {
        case (_, (existingSpecs, _)) => existingSpecs.nonEmpty
      }.get._1
      val (start, end) = formatPosition(errorPosition)
      render(
        ("type" -> ErrorTag) ~
          ("file" -> file) ~
          ("errors" ->
            List(
              ("start" -> start) ~
                ("end" -> end) ~
                ("tag" -> SampleInferenceErrorTag) ~
                ("message" -> "Inference omitted since some already exist.")
            ))
      )
    } else {
      render(
        ("type" -> SampleInferenceTag) ~
          ("file" -> file) ~
          (JsonKeyPreconditions -> specs(Pre).map(specToJsonDSL("requires"))) ~
          (JsonKeyPostconditions -> specs(Post).map(specToJsonDSL("ensures"))) ~
          (JsonKeyInvariants -> specs(Inv).map(specToJsonDSL("invariant")))
      )
    }
  }
}