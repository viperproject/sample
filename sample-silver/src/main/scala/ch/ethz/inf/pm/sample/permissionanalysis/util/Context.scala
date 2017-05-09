/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis.util

import ch.ethz.inf.pm.sample.execution.{CfgResult, DefaultProgramResult, ProgramResult}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverIdentifier, SilverMethodDeclaration, SilverProgramDeclaration}
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
    * The method currently being analyzed.
    */
  private var method: Option[SilverIdentifier] = None

  /**
    * Stores the result of the alias analysis.
    */
  private var aliases: Option[ProgramResult[_]] = None

  /**
    * Sets the program being analyzed to the given program.
    *
    * @param program The program being analyzed.
    */
  def setProgram(program: SilverProgramDeclaration): Unit = {
    this.program = Some(program)
    this.aliases = Some(DefaultProgramResult(program))
  }

  /**
    * Sets the method being analyzed to the given method.
    *
    * @param method The method being analyzed.
    */
  def setMethod(method: SilverIdentifier): Unit = {
    this.method = Some(method)
  }

  /**
    * Sets the result of the alias analysis.
    *
    * @param aliases The result of the alias analysis to set.
    * @tparam A The type of the alias analysis state.
    */
  def setAliases[A <: AliasAnalysisState[A]](aliases: CfgResult[A]): Unit = {
    val method = this.method.get
    val results = this.aliases.get.asInstanceOf[ProgramResult[A]]
    results.setResult(method, aliases)
  }

  /**
    * Returns the result of the alias analysis.
    *
    * @tparam A The type of the alias analysis state.
    * @return The result of the alias analysis.
    */
  def getAliases[A <: AliasAnalysisState[A]]: CfgResult[A] = {
    val method = this.method.get
    val results = this.aliases.get.asInstanceOf[ProgramResult[A]]
    results.getResult(method)
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
}
