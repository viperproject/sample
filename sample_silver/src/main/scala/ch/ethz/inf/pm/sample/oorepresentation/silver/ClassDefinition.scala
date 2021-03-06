/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.execution.SampleCfg
import ch.ethz.inf.pm.sample.oorepresentation._
import viper.silver.cfg._

case class SilverIdentifier(name: String) {
  override def toString: String = name
}

class SilverProgramDeclaration(val fields: Seq[FieldDeclaration],
                               val functions: Seq[SilverFunctionDeclaration],
                               val methods: Seq[SilverMethodDeclaration])

class SilverFunctionDeclaration(val programPoint: ProgramPoint,
                                val name: SilverIdentifier,
                                val parameters: List[VariableDeclaration],
                                val returnType: Type,
                                val body: Option[Statement])

class SilverMethodDeclaration(val programPoint: ProgramPoint,
                              val name: SilverIdentifier,
                              val arguments: List[VariableDeclaration],
                              val returns: List[VariableDeclaration],
                              val returnType: List[Type],
                              val body: SampleCfg) {

  /**
    * Returns the precondition of the method.
    *
    * @return The precondition of the method.
    */
  def precondition(): Seq[Statement] = body.entry match {
    case PreconditionBlock(pres) => pres
    case _ => Seq.empty
  }

  /**
    * Returns the postcondition of the method.
    *
    * TODO: Make the postcondition (and also precondition) a field of the method.
    *
    * @return The postcondition of the method.
    */
  def postcondition(): Seq[Statement] = body.exit match {
    case Some(PostconditionBlock(posts)) => posts
    case _ => Seq.empty
  }
}