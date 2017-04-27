/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleBlock
import ch.ethz.inf.pm.sample.execution.{BlockPosition, CfgPosition, CfgResult, SampleCfg}
import viper.silver.{ast => sil}

/**
  * A trait used to mix into a states in order to provide inferred
  * specifications for the program extender.
  *
  * @tparam T The type of the inferred specifications.
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
  * A program extender that modifies a program using inferred specifications.
  *
  * @tparam T The type of the inferred specification.
  * @tparam S The type of the state.
  * @author Jerome Dohrau
  * @author Caterina Urban
  */
trait SilverExtender[T, S <: State[S] with SilverSpecification[T]] {
  /**
    * Extends the given program using the given results of the analysis.
    *
    * @param program The program to extend.
    * @param results The result of the analysis.
    * @return The
    */
  def extendProgram(program: sil.Program, results: Map[SilverIdentifier, CfgResult[S]]): sil.Program = {
    // extend methods
    val extendedMethods = program.methods.map { method =>
      results.get(SilverIdentifier(method.name)) match {
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
    * @param method The method to extend.
    * @param result The result of the analysis.
    * @return The extended program.
    */
  def extendMethod(method: sil.Method, result: CfgResult[S]): sil.Method = {
    // TODO: Handle CFGs without exit blocks?
    val entry = firstPosition(result.cfg.entry)
    val exit = lastPosition(result.cfg.exit.get)

    val extendedPreconditions = preconditions(method.pres, entry, result)
    val extendedPostconditions = postconditions(method.posts, exit, result)
    val extendedBody = extendStatement(method.body, result)

    // TODO: Handle arguments.

    // return extended method
    method.copy(
      _pres = extendedPreconditions,
      _posts = extendedPostconditions,
      _body = extendedBody
    )(method.pos, method.info)
  }

  /**
    * Extends the given statement using the given result of the analysis.
    *
    * @param statement The statement to extend.
    * @param results   The result of the analysis.
    * @return The extended statement.
    */
  def extendStatement(statement: sil.Stmt, results: CfgResult[S]): sil.Stmt = statement match {
    case sil.Seqn(originalStatements) =>
      // recursively extend all statements of the sequence
      val extendedStatements = originalStatements.map(statement => extendStatement(statement, results))
      sil.Seqn(extendedStatements)(statement.pos, statement.info)
    case sil.If(condition, originalThen, originalElse) =>
      // recursively extend then and else branch of if statement
      val extendedThen = extendStatement(originalThen, results)
      val extendedElse = extendStatement(originalElse, results)
      sil.If(condition, extendedThen, extendedElse)(statement.pos, statement.info)
    case loop@sil.While(condition, originalInvariants, locals, originalBody) =>
      // get the position of the loop
      val position = getLoopPosition(loop, results.cfg)
      // extend while loop
      val extendedInvariants = invariants(originalInvariants, position, results)
      val extendedBody = extendStatement(originalBody, results)
      sil.While(condition, extendedInvariants, locals, extendedBody)(statement.pos, statement.info)
    case sil.NewStmt(lhs, originalFields) =>
      // get the position of the new statement
      val position = getPosition(statement, results.cfg).asInstanceOf[BlockPosition]
      // extend new statement
      val extendedFields = fields(originalFields, position, results)
      sil.NewStmt(lhs, extendedFields)(statement.pos, statement.info)
    case sil.Constraining(vars, originalBody) =>
      // recursively extend body of constraining statement
      val extendedBody = extendStatement(originalBody, results)
      sil.Constraining(vars, extendedBody)(statement.pos, statement.info)
    case _ =>
      // do nothing
      statement
  }

  /* ------------------------------------------------------------------------- *
   * Abstract Methods
   */

  /**
    * Modifies the list of preconditions using the given analysis result.
    *
    * @param existing The list of existing preconditions.
    * @param position The position of the first precondition.
    * @param result   The analysis result.
    * @return The modified list of preconditions.
    */
  def preconditions(existing: Seq[sil.Exp], position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp]

  /**
    * Modifies the list of postconditions using the given analysis result.
    *
    * @param existing The list of existing postconditions.
    * @param position The position of the last postcondition.
    * @param result   The analysis result.
    * @return The modified list of postconditions.
    */
  def postconditions(existing: Seq[sil.Exp], position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp]

  /**
    * Modifies the list of invariants using the given analysis result.
    *
    * @param existing The list of existing invariants.
    * @param position The position of the first invariant.
    * @param result   The analysis result.
    * @return The modified list of invariants.
    */
  def invariants(existing: Seq[sil.Exp], position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp]

  /**
    * Modifies the list of fields of a new statement using the given analysis result.
    *
    * @param existing The list of existing fields.
    * @param position The position of the new statement.
    * @param result   The analysis result.
    * @return The modified list of fields.
    */
  def fields(existing: Seq[sil.Field], position: BlockPosition, result: CfgResult[S]): Seq[sil.Field]

  /* ------------------------------------------------------------------------- *
   * Helper Functions
   */

  private def firstPosition(block: SampleBlock): BlockPosition =
    BlockPosition(block, 0)

  private def lastPosition(block: SampleBlock): BlockPosition =
    BlockPosition(block, block.elements.length - 1)

  /**
    * Returns the position of the given statement in the given control flow
    * graph.
    *
    * @param statement The statement.
    * @param cfg       The control flow graph.
    * @return The position of the given statement in the given control flow
    *         graph.
    */
  private def getPosition(statement: sil.Stmt, cfg: SampleCfg): CfgPosition = {
    val pp = DefaultSilverConverter.convert(statement.pos)
    cfg.getPosition(pp)
  }

  /**
    * Returns the position of the given loop in the given control flow graph.
    *
    * @param loop The loop.
    * @param cfg  The control flow graph.
    * @return The position of the given loop in the given control flow graph.
    */
  private def getLoopPosition(loop: sil.While, cfg: SampleCfg): BlockPosition = {
    val pp = DefaultSilverConverter.convert(loop.cond.pos)
    val pos = cfg.getEdgePosition(pp)
    BlockPosition(pos.edge.source, 0)
  }
}
