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
import net.liftweb.json.prettyRender
import viper.silver.ast.pretty.FastPrettyPrinter.{pretty => prettyPrint}
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
  * Exports the changes to an extended program as json. For programs that did already contains some specifications
  * we do not export new specifications but report an error (in json format).
  * All changes and errors are accompanied by a SourcePosition in the original silver program.
  *
  * @tparam T The type of the inferred specification.
  * @tparam S The type of the state.
  */
trait SpecificationsJsonExporter[T, S <: State[S] with SilverSpecification[T]] extends SpecificationsExporter[T, S] {

  //TODO prettyRender is for humans. For an actual IDE integration you may want to change this to compactRender
  private def render = prettyRender _

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
      pos.toString -> specifications.map(spec => s"$keyword ${prettyPrint(spec)}")
  }

  /**
    * Exports the specifications for the extended program or an error message. Both in JSON format.
    *
    * @return A JSON-string containing at least the key "error".
    *         error == true => The keys errorMessage and errorPosition will be available
    *         error == false => The JSON will contain the keys "preconditions", "invariants" and "postconditions"
    */
  def specificationsAsJson: String = {
    val specs = getSpecifications
    // check whether the original program contained specifications
    val existingSpec = specs.values.find(_.values.exists(_._1.nonEmpty))
    if (existingSpec.isDefined) {
      // Some specifications exist. Just point the user to one of them
      val pos = existingSpec.get.find {
        case (_, (existingSpecs, _)) => existingSpecs.nonEmpty
      }.get._1
      render(
        ("error" -> true) ~
          ("errorMessage" -> "Program already contains specifications!") ~
          ("errorPosition" -> pos.toString)
      )
    } else {
      render(
        ("error" -> false) ~
          (Pre -> specs(Pre).map(specToJsonDSL("requires"))) ~
          (Post -> specs(Post).map(specToJsonDSL("ensures"))) ~
          (Inv -> specs(Inv).map(specToJsonDSL("invariant")))
      )
    }
  }
}