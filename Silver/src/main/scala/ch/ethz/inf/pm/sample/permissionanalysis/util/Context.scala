/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis.util

import ch.ethz.inf.pm.sample.execution.CfgResult
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverProgramDeclaration
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.FieldDeclaration
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState

/**
  * A context object for the permission inference that stores the result of the
  * alias analysis.
  *
  * @author Jerome Dohrau
  */
object Context {
  /**
    * The program currently being analyzed.
    */
  private var program: Option[SilverProgramDeclaration] = None

  /**
    * Stores the result of the alias analysis.
    */
  private var aliases: Option[CfgResult[_]] = None

  /**
    * Sets the program being analyzed to the given program.
    *
    * @param program The program being analyzed.
    */
  def setProgram(program: SilverProgramDeclaration): Unit =
    this.program = Some(program)

  /**
    * Sets the result of the alias analysis.
    *
    * @param aliases The result of the alias analysis to set.
    * @tparam A The type of the alias analysis.
    */
  def setAliases[A <: AliasAnalysisState[A]](aliases: CfgResult[A]): Unit = {
    this.aliases = Some(aliases)
  }

  /**
    * Returns the fields of the program currently being analyzed.
    *
    * @return The fields of the program currently being analyzed.
    */
  def getFields(): Seq[FieldDeclaration] =
    program.get.fields

  /**
    * Returns the reference fields of the program currently being analyzed.
    *
    * @return The reference fields of the program currently being analyzed.
    */
  def getReferenceFields(): Seq[FieldDeclaration] =
    getFields().filter(_.typ.isObject)

  /**
    * Returns the field with the given name of the program currently being
    * analyzed.
    *
    * @param name The name of the field.
    * @return The field with the given name.
    */
  def getField(name: String): Option[FieldDeclaration] =
    getFields().find(_.variable.getName == name)

  /**
    * Clears the result of the alias analysis.
    */
  def clearAliases(): Unit = {
    this.aliases = None
  }

  /**
    * Returns the state of the alias analysis before the given program point.
    *
    * @param pp The program point.
    * @tparam A The type of the alias analysis.
    * @return The state of the alias analysis before the given program point.
    */
  def preAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A =
    aliases.get.preStateAt(pp).asInstanceOf[A]

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @tparam A The type fo the alias analysis.
    * @return The state of the alias analysis after the given program point.
    */
  def postAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A =
    aliases.get.postStateAt(pp).asInstanceOf[A]
}
