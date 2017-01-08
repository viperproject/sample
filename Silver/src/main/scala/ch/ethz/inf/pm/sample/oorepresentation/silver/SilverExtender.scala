/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.{BlockPosition, CfgPosition, CfgResult, EdgePosition}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPointUtils
import viper.silver.cfg.{ConditionalEdge, LoopHeadBlock, StatementBlock}
import viper.silver.{ast => sil}

/**
  * Silver Specification
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait SilverSpecification {
  /** Modifies the list of formal arguments using information stored in the
    * current state.
    *
    * @param existing The list of existing formal arguments.
    * @return The modified list of formal arguments
    */
  def formalArguments(existing: Seq[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = existing

  /**
    * Modifies the list of preconditions using information stored in the current
    * state.
    *
    * @param existing The list of existing preconditions.
    * @return The modified list of preconditions.
    */
  def preconditions(existing: Seq[sil.Exp]): Seq[sil.Exp] = existing

  /**
    * Modifies the list of invariants using information stored in the current
    * state.
    *
    * @param existing The list of existing invariants.
    * @return The modified list of invariants.
    */
  def invariants(existing: Seq[sil.Exp]): Seq[sil.Exp] = existing

  /**
    * Modifies the list of postconditions using information stored in the
    * current state.
    *
    * @param existing The list of existing postconditions.
    * @return The modified list of postconditions.
    */
  def postconditions(existing: Seq[sil.Exp]): Seq[sil.Exp] = existing

  /**
    * Modifies the list of fields of a new statement using information stored in
    * the current state.
    *
    * @param existing The list of existing fields.
    * @return The modified list of fields.
    */
  def fields(existing: Seq[sil.Field]): Seq[sil.Field] = existing
}

/**
  * Silver Program Extender
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait SilverExtender[S <: State[S] with SilverSpecification] {
  /**
    * Extends a sil.Program with inferred specifications.
    **/
  def extendProgram(prog: sil.Program, results: Map[SilverIdentifier, CfgResult[S]]): sil.Program = {
    // map of method names to control flow graphs
    //val methodNameToCfgState = results.map(result => result.method.name.toString -> result.cfgState).toMap
    // extending program methods
    val extMethods = prog.methods.map(
      method => results.get(SilverIdentifier(method.name)) match {
        case Some(cfgResult) => extendMethod(method, cfgResult)
        case None => method
      }
    )
    // building the extended program
    prog.copy(methods = extMethods)(prog.pos, prog.info)
  }

  /**
    * Extends a sil.Method with inferred specifications.
    **/
  def extendMethod(method: sil.Method, cfgResult: CfgResult[S]): sil.Method = {
    // retrieve the result of the analysis at the method entry and exit
    val entry = cfgResult.entryState()
    val exit = cfgResult.exitState()

    // update the formal arguments, precondition, postcondition and method body
    val entryArgs = entry.formalArguments(method.formalArgs)
    val exitArgs = exit.formalArguments(method.formalArgs)
    val formalArguments = collectFormalArguments(method.body, exitArgs, cfgResult)
    var precondition = entry.preconditions(method.pres)
    val body = extendStmt(method.body, cfgResult)
    val postcondition = exit.postconditions(method.posts)

    // TODO: get rid of this hack
    val paramExists = formalArguments.exists {
      case sil.LocalVarDecl(name, _) => name == "read"
      case _ => false
    }
    val condExists = precondition.exists {
      case sil.PermGtCmp(_, _) => true
      case _ => false
    }
    if (paramExists && !condExists) {
      val read = sil.LocalVar("read")(sil.Perm)
      val cond = Seq(sil.And(sil.PermGtCmp(read, sil.NoPerm()())(), sil.PermLtCmp(read, sil.FullPerm()())())())
      precondition = cond ++ precondition
    }

    // return updated method
    method.copy(formalArgs = formalArguments, _pres = precondition, _body = body, _posts = postcondition)(method.pos, method.info)
  }

  /**
    * Extends a sil.Stmt with inferred specifications.
    **/
  def extendStmt(stmt: sil.Stmt, cfgResult: CfgResult[S]): sil.Stmt = stmt match {

    case stmt: sil.Constraining =>
      sil.Constraining(vars = stmt.vars, body = extendStmt(stmt.body, cfgResult))(stmt.pos, stmt.info)

    case stmt: sil.If =>
      val thn = extendStmt(stmt.thn, cfgResult)
      val els = extendStmt(stmt.els, cfgResult)
      sil.If(cond = stmt.cond, thn = thn, els = els)(stmt.pos, stmt.info)

    case stmt: sil.NewStmt =>
      // retrieve the result of the analysis at the new statement
      // TODO: this and others
      val position = getPosition(stmt, cfgResult)
      val pre = cfgResult.preStateAt(position)
      // update the new statement
      val fields = pre.fields(stmt.fields)
      sil.NewStmt(stmt.lhs, fields)(stmt.pos, stmt.info)

    case stmt: sil.Seqn =>
      val seq: Seq[sil.Stmt] = stmt.ss.foldRight(Seq[sil.Stmt]())(
        (s: sil.Stmt, ss: Seq[sil.Stmt]) => ss.+:(extendStmt(s, cfgResult))
      )
      sil.Seqn(seq)(stmt.pos, stmt.info)

    case loop: sil.While =>
      // retrieve the result of the analysis at the loop head
      val position = getLoopPosition(loop, cfgResult)
      val pre = cfgResult.preStateAt(position)
      // update the method loop invariants
      val invariants = pre.invariants(loop.invs)
      sil.While(loop.cond, invs = invariants, loop.locals, body = extendStmt(loop.body, cfgResult))(loop.pos, loop.info)

    case _ => stmt
  }

  def collectFormalArguments(stmt: sil.Stmt,
                             args: Seq[sil.LocalVarDecl],
                             cfgResult: CfgResult[S]): Seq[sil.LocalVarDecl] =
    stmt match {
      case stmt: sil.Seqn =>
        stmt.ss.foldLeft(args) { case (currArgs, currStmt) =>
          collectFormalArguments(currStmt, currArgs, cfgResult)
        }
      case loop: sil.While =>
        // retrieve the block index and the statement index within the block of the loop head
        val position = getLoopPosition(loop, cfgResult)
        val pre: S = cfgResult.preStateAt(position)
        //cfgResult.preStateAt(BlockPosition(position.block, 0))
        val whileArgs = pre.formalArguments(args)
        collectFormalArguments(loop.body, whileArgs, cfgResult)
      case _ => args
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

