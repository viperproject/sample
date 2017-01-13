/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.{BlockPosition, CfgPosition, CfgResult}
import viper.silver.{ast => sil}

/**
  * Silver specification.
  *
  * @tparam T The type of the specifications.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
trait SilverSpecification[T] {
  /**
    * Returns the inferred specifications.
    *
    * @return The inferred specifications.
    */
  def specifications: T
}

/**
  *
  * @tparam T The type of the specification.
  * @tparam S The type of the state.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
trait SilverExtender[T, S <: State[S] with SilverSpecification[T]] {
  /**
    * Modifies the list of preconditions using the given specifications.
    *
    * @param existing       The list of existing preconditions.
    * @param specifications The specifications.
    * @return The modified list of preconditions.
    */
  def preconditions(existing: Seq[sil.Exp], specifications: T): Seq[sil.Exp]

  /**
    * Modifies the list of postcondition using the given specifications.
    *
    * @param existing       The list of existing postconditions.
    * @param specifications The specifications.
    * @return The modified list of postconditions.
    */
  def postconditions(existing: Seq[sil.Exp], specifications: T): Seq[sil.Exp]

  /**
    * Modifies the list of invariants using the given specifications.
    *
    * @param existing       The list of existing invariants.
    * @param specifications The specifications.
    * @return The modified list of postconditions.
    */
  def invariants(existing: Seq[sil.Exp], specifications: T): Seq[sil.Exp]

  /**
    * Modifies the list of fields of a new statement using the given
    * specification.
    *
    * @param existing      The existing list of fields.
    * @param specification The specification.
    * @return The modified list of fields.
    */
  def fields(existing: Seq[sil.Field], specification: T): Seq[sil.Field]

  /**
    * Extends the given program using the given results of the analysis.
    *
    * @param program    The program to extend.
    * @param cfgResults The result of the analysis.
    * @return The
    */
  def extendProgram(program: sil.Program, cfgResults: Map[SilverIdentifier, CfgResult[S]]): sil.Program = {
    // extend methods
    val extendedMethods = program.methods.map { method =>
      cfgResults.get(SilverIdentifier(method.name)) match {
        case Some(cfgResult) => extendMethod(method, cfgResult)
        case None => method
      }
    }

    // return extended program
    program.copy(methods = extendedMethods)(program.pos, program.info)
  }

  /**
    * Extends the given method using the given result of the analysis.
    *
    * @param method    The method to extend.
    * @param cfgResult The result of the analysis.
    * @return The extended program.
    */
  def extendMethod(method: sil.Method, cfgResult: CfgResult[S]): sil.Method = {

    val entrySpecifications = cfgResult.entryState().specifications
    val exitSpecifications = cfgResult.exitState().specifications

    val extendedPreconditions = preconditions(method.pres, entrySpecifications)
    val extendedPostconditions = postconditions(method.posts, exitSpecifications)
    val extendedBody = extendStatement(method.body, cfgResult)

    // TODO: Handle arguments.

    // return extended method
    method.copy(
      _pres = extendedPreconditions,
      _posts = extendedPostconditions
    )(method.pos, method.info)
  }

  /**
    * Extends the given statement using the given result of the analysis.
    *
    * @param statement The statement to extend.
    * @param cfgResult The result of the analysis.
    * @return The extended statement.
    */
  def extendStatement(statement: sil.Stmt, cfgResult: CfgResult[S]): sil.Stmt = statement match {
    case sil.Seqn(originalStatements) =>
      // recursively extend all statements of the sequence
      val extendedStatements = originalStatements.map(statement => extendStatement(statement, cfgResult))
      sil.Seqn(extendedStatements)(statement.pos, statement.info)
    case sil.If(condition, originalThen, originalElse) =>
      // recursively extend then and else branch of if statement
      val extendedThen = extendStatement(originalThen, cfgResult)
      val extendedElse = extendStatement(originalElse, cfgResult)
      sil.If(condition, extendedThen, extendedElse)(statement.pos, statement.info)
    case loop@sil.While(condition, originalInvariants, locals, originalBody) =>
      // get specifications
      val position = getLoopPosition(loop, cfgResult)
      val specifications = cfgResult.preStateAt(position).specifications
      // extend while loop
      val extendedInvariants = invariants(originalInvariants, specifications)
      val extendedBody = extendStatement(originalBody, cfgResult)
      sil.While(condition, extendedInvariants, locals, extendedBody)(statement.pos, statement.info)
    case sil.NewStmt(lhs, originalFields) =>
      // get specifications
      val position = getPosition(statement, cfgResult)
      val specifications = cfgResult.preStateAt(position).specifications
      // extend new statement
      val extendedFields = fields(originalFields, specifications)
      sil.NewStmt(lhs, extendedFields)(statement.pos, statement.info)
    case sil.Constraining(vars, originalBody) =>
      // recursively extend body of constraining statement
      val extendedBody = extendStatement(originalBody, cfgResult)
      sil.Constraining(vars, extendedBody)(statement.pos, statement.info)
  }

  def getPosition(statement: sil.Stmt, cfgResult: CfgResult[S]): CfgPosition = {
    val cfg = cfgResult.cfg
    val pp = DefaultSilverConverter.convert(statement.pos)
    cfg.getPosition(pp)
  }

  def getLoopPosition(loop: sil.While, cfgResult: CfgResult[S]): CfgPosition = {
    val cfg = cfgResult.cfg
    val pp = DefaultSilverConverter.convert(loop.cond.pos)
    val pos = cfg.getEdgePosition(pp)
    BlockPosition(pos.edge.source, 0)
  }
}
