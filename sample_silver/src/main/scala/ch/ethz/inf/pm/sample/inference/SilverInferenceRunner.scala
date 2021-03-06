/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.inference

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleBlock
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSilverConverter, SilverAnalysisRunner, SilverIdentifier}
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
  * A specification inference runner for silver programs.
  *
  * @tparam S The type of the state.
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait SilverInferenceRunner[S <: State[S]] extends
  SilverAnalysisRunner[S] {

  /**
    * Infers a list of parameters for the given method using the given analysis
    * result.
    *
    * @param method The method for which the parameters are inferred.
    * @param result The analysis result.
    * @return The inferred parameters.
    */
  def inferParameters(method: sil.Method, result: CfgResult[S]): Seq[sil.LocalVarDecl] = method.formalArgs

  /**
    * Infers a list of preconditions for the given method using the given
    * analysis result.
    *
    * @param method The method for which the preconditions are inferred.
    * @param result The analysis result.
    * @return The inferred preconditions.
    */
  def inferPreconditions(method: sil.Method, result: CfgResult[S]): Seq[sil.Exp] = method.pres

  /**
    * Infers a list of postconditions for the given method using the given
    * analysis result.
    *
    * @param method The method for which the postconditions are inferred.
    * @param result The analysis result.
    * @return The inferred postconditions.
    */
  def inferPostconditions(method: sil.Method, result: CfgResult[S]): Seq[sil.Exp] = method.posts

  /**
    * Infers a list of invariants for the given while loop using the given
    * analysis result.
    *
    * @param loop   The while loop for which the invariants are inferred.
    * @param result The analysis result.
    * @return The inferred invariants.
    */
  def inferInvariants(loop: sil.While, result: CfgResult[S]): Seq[sil.Exp] = loop.invs

  /**
    * Infers a list of fields for a new statement using the given analysis
    * result.
    *
    * @param newStmt  The new statement for with the fields are inferred.
    * @param position The position of the new statement.
    * @param result   The analysis result.
    * @return The inferred fields.
    */
  def inferFields(newStmt: sil.NewStmt, position: BlockPosition, result: CfgResult[S]): Seq[sil.Field] = newStmt.fields

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

/**
  * A program extender that modifies a program using inferred specifications.
  *
  * This trait is intended to mix into a silver inference runner
  * [[SilverInferenceRunner]] in order to add the functionality to extend a
  * silver program. This trait is compatible with the silver exporter
  * [[SilverExporter]].
  *
  * @tparam S The type of the state.
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait SilverExtender[S <: State[S]] extends SilverInferenceRunner[S] {
  /**
    * The main method that runs the analysis with the given arguments.
    *
    * @param arguments The arguments.
    */
  override def main(arguments: Array[String]): Unit = {
    require(arguments.nonEmpty, "No file specified")
    val extended = extend(arguments)
    printExtended(extended)
  }

  def extend(arguments: Array[String]): sil.Program = {
    val file = new File(arguments(0))
    extend(file)
  }

  def extend(file: File): sil.Program = {
    val path = file.toPath
    val compilable = Compilable.Path(path)
    val program = compile(compilable)
    extend(program)
  }

  def extend(program: sil.Program): sil.Program = {
    val results = run(program)
    extendProgram(program, results)
  }

  /**
    * Extends the given program using the given results of the analysis.
    *
    * @param program The program to extend.
    * @param results The result of the analysis.
    * @return The extended program.
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
    * @return The extended method.
    */
  def extendMethod(method: sil.Method, result: CfgResult[S]): sil.Method = {
    val entry = firstPosition(result.cfg.entry)
    val exit = lastPosition(result.cfg.exit.get)
    // return extended method
    method.copy(
      formalArgs = inferParameters(method, result),
      pres = inferPreconditions(method, result),
      posts = inferPostconditions(method, result),
      body = method.body.map(extendBody(_, result))
    )(method.pos, method.info, method.errT)
  }

  /**
    * Extends the body of a method or a while loop.
    *
    * @param body    The body to extend.
    * @param results The result of the analysis.
    * @return The extended body.
    */
  def extendBody(body: sil.Seqn, results: CfgResult[S]): sil.Seqn = extendStatement(body, results) match {
    case seqn: sil.Seqn => seqn
    case _ => ???
  }

  /**
    * Extends the given statement using the given result of the analysis.
    *
    * @param statement The statement to extend.
    * @param result    The result of the analysis.
    * @return The extended statement.
    */
  def extendStatement(statement: sil.Stmt, result: CfgResult[S]): sil.Stmt = statement match {
    case sil.Seqn(originalStatements, scopedDecls) =>
      // recursively extend all statements of the sequence
      val extendedStatements = originalStatements.map(statement => extendStatement(statement, result))
      sil.Seqn(extendedStatements, scopedDecls)(statement.pos, statement.info)
    case sil.If(condition, originalThen, originalElse) =>
      // recursively extend then and else branch of if statement
      val extendedThen = extendBody(originalThen, result)
      val extendedElse = extendBody(originalElse, result)
      sil.If(condition, extendedThen, extendedElse)(statement.pos, statement.info)
    case loop@sil.While(condition, _, originalBody) =>
      // extend while loop
      val extendedInvariants = inferInvariants(loop, result)
      val extendedBody = extendBody(originalBody, result)
      sil.While(condition, extendedInvariants, extendedBody)(statement.pos, statement.info)
    case newStmt: sil.NewStmt =>
      // get the position of the new statement
      val position = getPosition(statement, result.cfg).asInstanceOf[BlockPosition]
      // extend new statement
      val extendedFields = inferFields(newStmt, position, result)
      sil.NewStmt(newStmt.lhs, extendedFields)(statement.pos, statement.info)
    case _ =>
      // do nothing
      statement
  }

  def printExtended(program: sil.Program): Unit = {
    printHeader("Extended Program")
    println(program)
  }
}

/**
  * A specification exporter that exports inferred specifications for a silver
  * program.
  *
  * This trait is intended to mix into a silver inference runner
  * [[SilverInferenceRunner]] in order to add the functionality to export the
  * inferred specifications of a silver program. This trait is compatible with
  * the silver extender [[SilverExtender]].
  *
  * @tparam S The type of the state.
  * @author Jerome Dohrau
  */
trait SilverExporter[S <: State[S]] extends SilverInferenceRunner[S] {

  def export(arguments: Array[String]): Unit = {
    val compilable = Compilable.Path(new File(arguments(0)).toPath)
    val program = compile(compilable)
    val results = run(program)
    exportProgram(program, results)
  }

  /**
    * Exports the inferred parameters of the given method.
    *
    * @param method   The method.
    * @param inferred The inferred parameters.
    */
  protected def exportParameters(method: sil.Method, inferred: Seq[sil.LocalVarDecl]): Unit = {}

  /**
    * Exports the inferred preconditions of the given method.
    *
    * @param method   The method.
    * @param inferred The inferred preconditions.
    */
  protected def exportPreconditions(method: sil.Method, inferred: Seq[sil.Exp]): Unit = {}

  /**
    * Exports the inferred postconditions of the given method.
    *
    * @param method   The method.
    * @param inferred The inferred postconditions.
    */
  protected def exportPostconditions(method: sil.Method, inferred: Seq[sil.Exp]): Unit = {}

  /**
    * Exports the inferred invariants of the given while loop.
    *
    * @param loop     The while loop.
    * @param inferred The inferred invariants.
    */
  protected def exportInvariants(loop: sil.While, inferred: Seq[sil.Exp]): Unit = {}

  /**
    * Infers and exports specifications for the given program using the given analysis result.
    *
    * @param program The program.
    * @param results The analysis result.
    */
  protected def exportProgram(program: sil.Program, results: ProgramResult[S]): Unit = {
    program.methods.foreach { method =>
      val identifier = SilverIdentifier(method.name)
      val result = results.getResult(identifier)
      exportMethod(method, result)
    }
  }

  /**
    * Infers and exports specifications for the given method using the given analysis results.
    *
    * @param method The method.
    * @param result The analysis result.
    */
  protected def exportMethod(method: sil.Method, result: CfgResult[S]): Unit = {
    exportParameters(method, inferParameters(method, result))
    exportPreconditions(method, inferPreconditions(method, result))
    exportPostconditions(method, inferPostconditions(method, result))
    method.body.foreach { statement => exportStatement(statement, result) }
  }

  /**
    * Recursively infers and exports specifications for the given statement using the given analysis result.
    *
    * @param statement The statement.
    * @param result    The analysis result.
    */
  protected def exportStatement(statement: sil.Stmt, result: CfgResult[S]): Unit = statement match {
    case sil.Seqn(statements, _) =>
      statements.foreach { statement => exportStatement(statement, result) }
    case sil.If(_, statement1, statement2) =>
      exportStatement(statement1, result)
      exportStatement(statement2, result)
    case loop: sil.While =>
      exportInvariants(loop, inferInvariants(loop, result))
      exportStatement(loop.body, result)
    case _ =>
  }

}
