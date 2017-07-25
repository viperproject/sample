/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleBlock
import ch.ethz.inf.pm.sample.execution._
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
  def extendProgram(program: sil.Program, results: ProgramResult[S]): sil.Program = {
    // extend methods
    val extendedMethods = program.methods.map { method =>
      val identifier = SilverIdentifier(method.name)
      val result = results.getResult(identifier)
      extendMethod(method, result)
    }

    // return extended program
    program.copy(methods = extendedMethods)(program.pos, program.info, program.errT)
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
    
    val extendedPreconditions = preconditions(method, entry, result)
    val extendedPostconditions = postconditions(method, exit, result)
    val extendedBody = extendBody(method.body, result)

    // TODO: Handle arguments.

    // return extended method
    method.copy(
      pres = extendedPreconditions,
      posts = extendedPostconditions,
      body = extendedBody
    )(method.pos, method.info, method.errT)
  }

  def extendBody(body: sil.Seqn, results: CfgResult[S]): sil.Seqn = extendStatement(body, results) match {
    case seqn: sil.Seqn => seqn
    case _ => ???
  }

  /**
    * Extends the given statement using the given result of the analysis.
    *
    * @param statement The statement to extend.
    * @param results   The result of the analysis.
    * @return The extended statement.
    */
  def extendStatement(statement: sil.Stmt, results: CfgResult[S]): sil.Stmt = statement match {
    case sil.Seqn(originalStatements, scopedDecls) =>
      // recursively extend all statements of the sequence
      val extendedStatements = originalStatements.map(statement => extendStatement(statement, results))
      sil.Seqn(extendedStatements, scopedDecls)(statement.pos, statement.info)
    case sil.If(condition, originalThen, originalElse) =>
      // recursively extend then and else branch of if statement
      val extendedThen = extendBody(originalThen, results)
      val extendedElse = extendBody(originalElse, results)
      sil.If(condition, extendedThen, extendedElse)(statement.pos, statement.info)
    case loop@sil.While(condition, _, originalBody) =>
      // get the position of the loop
      val position = getLoopPosition(loop, results.cfg)
      // extend while loop
      val extendedInvariants = invariants(loop, position, results)
      val extendedBody = extendBody(originalBody, results)
      sil.While(condition, extendedInvariants, extendedBody)(statement.pos, statement.info)
    case newStmt@sil.NewStmt(lhs, originalFields) =>
      // get the position of the new statement
      val position = getPosition(statement, results.cfg).asInstanceOf[BlockPosition]
      // extend new statement
      val extendedFields = fields(newStmt, position, results)
      sil.NewStmt(lhs, extendedFields)(statement.pos, statement.info)
    case sil.Constraining(vars, originalBody) =>
      // recursively extend body of constraining statement
      val extendedBody = extendBody(originalBody, results)
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
    * @param method   The method that should be extended.
    * @param position The position of the first precondition.
    * @param result   The analysis result.
    * @return The modified list of preconditions.
    */
  def preconditions(method: sil.Method, position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp]

  /**
    * Modifies the list of postconditions using the given analysis result.
    *
    * @param method   The method that should be extended.
    * @param position The position of the last postcondition.
    * @param result   The analysis result.
    * @return The modified list of postconditions.
    */
  def postconditions(method: sil.Method, position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp]

  /**
    * Modifies the list of invariants using the given analysis result.
    *
    * @param loop     The while statement for which invariants are modified
    * @param position The position of the first invariant.
    * @param result   The analysis result.
    * @return The modified list of invariants.
    */
  def invariants(loop: sil.While, position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp]

  /**
    * Modifies the list of fields of a new statement using the given analysis result.
    *
    * @param newStmt The sil.NewStmt for which fields should be modified
    * @param position The position of the new statement.
    * @param result   The analysis result.
    * @return The modified list of fields.
    */
  def fields(newStmt: sil.NewStmt, position: BlockPosition, result: CfgResult[S]): Seq[sil.Field]

  /* ------------------------------------------------------------------------- *
   * Helper Functions
   */

  /**
    * Returns the position before the first element of the given block.
    *
    * @param block The block.
    * @return The position before the first element of the given block.
    */
  protected def firstPosition(block: SampleBlock): BlockPosition =
    BlockPosition(block, 0)

  /**
    * Returns the position after the last element of the given block.
    *
    * @param block The block.
    * @return The position after the last element.
    */
  protected def lastPosition(block: SampleBlock): BlockPosition =
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
  protected def getPosition(statement: sil.Stmt, cfg: SampleCfg): CfgPosition = {
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
  protected def getLoopPosition(loop: sil.While, cfg: SampleCfg): BlockPosition = {
    val pp = DefaultSilverConverter.convert(loop.cond.pos)
    val pos = cfg.getEdgePosition(pp)
    BlockPosition(pos.edge.source, 0)
  }
}
