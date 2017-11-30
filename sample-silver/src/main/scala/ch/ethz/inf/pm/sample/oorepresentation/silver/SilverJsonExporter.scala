/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.ProgramResult
import ch.ethz.inf.pm.sample.inference.SilverExporter
import net.liftweb.json.JsonAST.{JNothing, JValue}
import net.liftweb.json.JsonDSL._
import net.liftweb.json.prettyRender
import viper.silver.ast.Program
import viper.silver.ast.pretty.FastPrettyPrinter.{pretty => prettyPrint}
import viper.silver.{ast => sil}


/**
  * Infers specifications for a Viper program and exports them as JSON.
  *
  * @tparam S The type of the state.
  * @author Jerome Dohrau
  * @author Flurin Rindisbacher
  */
trait SilverJsonExporter[S <: State[S]]
  extends SilverExporter[S] {

  private var parameters: JValue = JNothing
  private var preconditions: JValue = JNothing
  private var postconditions: JValue = JNothing
  private var invariants: JValue = JNothing
  private var replacements: JValue = JNothing
  private var errors: JValue = JNothing

  def getJson(file: String): JValue = errors match {
    case JNothing =>
      ("type" -> "inference") ~
        ("file" -> file) ~
        ("parameters" -> parameters) ~
        ("preconditions" -> preconditions) ~
        ("postconditions" -> postconditions) ~
        ("invariants" -> invariants) ~
        ("replacements" -> replacements)
    case _ =>
      ("type" -> "error") ~
        ("file" -> file) ~
        ("errors" -> errors)
  }

  def getString(file: String): String = prettyRender(getJson(file))

  override protected def exportProgram(program: Program, results: ProgramResult[S]): Unit = {
    // reset
    parameters = JNothing
    preconditions = JNothing
    postconditions = JNothing
    invariants = JNothing
    replacements = JNothing
    errors = JNothing

    super.exportProgram(program, results)
  }

  protected override def exportParameters(method: sil.Method, inferred: Seq[sil.LocalVarDecl]): Unit =
    parameters = parameters ++ generate(method.pos, inferred, method.formalArgs)

  protected override def exportPreconditions(method: sil.Method, inferred: Seq[sil.Exp]): Unit =
    preconditions = preconditions ++ generate(method.pos, inferred, method.pres)

  protected override def exportPostconditions(method: sil.Method, inferred: Seq[sil.Exp]): Unit =
    postconditions = postconditions ++ generate(method.pos, inferred, method.posts)

  protected override def exportInvariants(loop: sil.While, inferred: Seq[sil.Exp]): Unit =
    invariants = invariants ++ generate(loop.pos, inferred, loop.invs)

  protected def generate[T <: sil.Node with sil.Positioned](position: sil.Position, inferred: Seq[T], existing: Seq[T]): JValue = {
    val (insertions, additions, deletions) = difference(inferred, existing)
    // format insertions
    val formattedInsertions = insertions
      .groupBy { case (_, position) => position }
      .map { case (position, list) =>
        val specifications = list.map { case (node, _) => prettyPrint(node) }
        ("action" -> "insert") ~ ("position" -> format(position)) ~ ("specifications" -> specifications)
      }
    // format additions
    val formattedAdditions = if (additions.isEmpty) JNothing else {
      val specifications = additions.map { node => prettyPrint(node) }
      ("action" -> "append") ~ ("position" -> format(position)) ~ ("specifications" -> specifications)
    }
    // format deletions
    val formattedDeletions = deletions.map { node => ("action" -> "delete") ~ ("position" -> format(node.pos)) }

    JNothing ++ formattedInsertions ++ formattedAdditions ++ formattedDeletions
  }

  /**
    * Given a list of inferred specifications and a list of existing specifications, this method tries to determine a
    * set of inserting, appending and deletion actions that turn the existing specifications into the inferred ones.
    *
    * @param inferred The list of inferred specifications.
    * @param existing The list of existing specifications.
    * @tparam T The type of the specifications.
    * @return (insert, append, delete)
    */
  protected def difference[T <: sil.Positioned](inferred: Seq[T], existing: Seq[T]): (Seq[(T, sil.Position)], Seq[T], Seq[T]) =
    if (existing.isEmpty) (Seq.empty, inferred, Seq.empty)
    else if (inferred.isEmpty) (Seq.empty, Seq.empty, existing)
    else {
      val node = inferred.head
      val index = existing.indexOf(node)
      if (index == -1) {
        // the element does not exist
        val a = Seq((node, existing.head.pos))
        val (insert, append, delete) = difference(inferred.tail, existing)
        (a ++ insert, append, delete)
      } else {
        // the element exists
        val (before, rest) = existing.splitAt(index)
        val (insert, append, delete) = difference(inferred.tail, rest.tail)
        (insert, append, before ++ delete)
      }
    }

  protected def exportReplacement(node: sil.Node with sil.Positioned, replacement: sil.Node): Unit = {
    val json = ("position" -> format(node.pos)) ~ ("replacement" -> prettyPrint(replacement))
    replacements = replacements ++ json
  }

  protected def format(position: sil.Position): JValue = position match {
    case sil.SourcePosition(_, start, end) => end match {
      case Some(existing) => ("start" -> format(start)) ~ ("end" -> format(existing))
      case None => format(start)
    }
    case position: sil.HasLineColumn => ("line" -> position.line) ~ ("column" -> position.column)
    case _ => "unknown"
  }
}
