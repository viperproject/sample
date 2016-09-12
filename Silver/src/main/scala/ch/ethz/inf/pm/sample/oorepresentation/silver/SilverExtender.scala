/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.{AbstractCFGState, AnalysisResult}
import ch.ethz.inf.pm.sample.oorepresentation.CFGPosition
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.ProgramPoint
import viper.silver.{ast => sil}

/** Silver Specification
  *
  * @author Caterina Urban, Jerome Dohrau
  */
trait SilverSpecification {

  /**
    * Generates a list of additional formal arguments for the method
    *
    * @return a sequence of sil.LocalVarDecl
    */
  def formalArguments(args: Seq[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = args

  /** Generates a Silver precondition from the current state
    *
    * @return a sequence of sil.Exp
    */
  def precondition(): Seq[sil.Exp] = Seq.empty

  /** Generates a Silver invariant from the current state
    *
    * @return a sequence of sil.Exp
    */
  def invariant(): Seq[sil.Exp] = Seq.empty

  /** Generates a Silver postcondition from the current state
    *
    * @return a sequence of sil.Exp
    */
  def postcondition(): Seq[sil.Exp] = Seq.empty

}

/** Silver Program Extender
  *
  * @author Caterina Urban, Jerome Dohrau
  */
trait SilverExtender[S <: State[S] with SilverSpecification] {

  /** Extends a sil.Program with inferred specifications. */
  def extendProgram(prog: sil.Program, results: List[AnalysisResult[S]]): sil.Program = {
    // map of method names to control flow graphs
    val methodNameToCfgState = results.map(result => result.method.name.toString -> result.cfgState).toMap
    // extending program methods
    val extMethods = prog.methods.map(
      method => methodNameToCfgState.get(method.name) match {
        case Some(cfgState) => extendMethod(method, cfgState)
        case None => method
      }
    )
    // building the extended program
    prog.copy(methods = extMethods)(prog.pos, prog.info)
  }

  /** Extends a sil.Method with inferred specifications. */
  def extendMethod(method: sil.Method, cfgState: AbstractCFGState[S]): sil.Method = {
    // retrieve the result of the analysis at the method entry and exit
    val entry = cfgState.entryState()
    val exit = cfgState.exitState()

    // update the formal arguments, precondition, postcondition and method body
    val entryArgs = entry.formalArguments(method.formalArgs)
    val formalArguments = collectFormalArguments(method.body, entryArgs, cfgState)
    var precondition = entry.precondition ++ method.pres
    val body = extendStmt(method.body, cfgState)
    val postcondition = exit.postcondition ++ method.posts

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
      val cond = Seq(sil.And(sil.PermGtCmp(read, sil.NoPerm()())(),sil.PermLtCmp(read, sil.FullPerm()())())())
      precondition = cond ++ precondition
    }

    // return updated method
    method.copy(formalArgs = formalArguments, _pres = precondition, _body = body, _posts = postcondition)(method.pos, method.info)
  }

  /** Extends a sil.Stmt with inferred specifications. */
  def extendStmt(stmt: sil.Stmt, cfgState: AbstractCFGState[S]): sil.Stmt = stmt match {

    case stmt: sil.Constraining =>
      sil.Constraining(vars = stmt.vars, body = extendStmt(stmt.body, cfgState))(stmt.pos, stmt.info)

    case stmt: sil.If =>
      val thn = extendStmt(stmt.thn, cfgState)
      val els = extendStmt(stmt.els, cfgState)
      sil.If(cond = stmt.cond, thn = thn, els = els)(stmt.pos, stmt.info)

    case stmt: sil.NewStmt => stmt

    case stmt: sil.Seqn =>
      val seq: Seq[sil.Stmt] = stmt.ss.foldRight(Seq[sil.Stmt]())(
        (s: sil.Stmt, ss: Seq[sil.Stmt]) => ss.+:(extendStmt(s, cfgState))
      )
      sil.Seqn(seq)(stmt.pos, stmt.info)

    case stmt: sil.While =>
      // retrieve the position of the loop head
      val pos: ProgramPoint = DefaultSilverConverter.convert(stmt.cond.pos)
      // retrieve the block index and the statement index within the block of the loop head
      val cfgPositions: List[CFGPosition] = cfgState.cfg.nodes.zipWithIndex.flatMap({
        case (stmts, blockIdx) => stmts.zipWithIndex.flatMap({
          case (stmt, stmtIdx) =>
            if (stmt.getPC() == pos) Some(CFGPosition(blockIdx, stmtIdx)) else None
        })
      })
      // retrieve the result of the analysis at the loop head
      val pre: S = cfgState.preStateAt(CFGPosition(cfgPositions.head.blockIdx, 0))
      // update the method loop invariants
      val invariants: Seq[sil.Exp] = pre.invariant ++ stmt.invs
      sil.While(stmt.cond, invs = invariants, stmt.locals, body = extendStmt(stmt.body, cfgState))(stmt.pos, stmt.info)

    case _ => stmt
  }

  def collectFormalArguments(stmt: sil.Stmt,
                             args: Seq[sil.LocalVarDecl],
                             cfgState: AbstractCFGState[S]): Seq[sil.LocalVarDecl] = stmt match {
    case stmt: sil.Seqn =>
      stmt.ss.foldLeft(args) { case (currArgs, currStmt) =>
        collectFormalArguments(currStmt, currArgs, cfgState)
      }
    case stmt: sil.While =>
      // retrieve the position of the loop head
      val pos: ProgramPoint = DefaultSilverConverter.convert(stmt.cond.pos)
      // retrieve the block index and the statement index within the block of the loop head
      val cfgPositions: List[CFGPosition] = cfgState.cfg.nodes.zipWithIndex.flatMap({
        case (stmts, blockIdx) => stmts.zipWithIndex.flatMap({
          case (stmt, stmtIdx) =>
            if (stmt.getPC() == pos) Some(CFGPosition(blockIdx, stmtIdx)) else None
        })
      })
      val pre: S = cfgState.preStateAt(CFGPosition(cfgPositions.head.blockIdx, 0))
      val whileArgs = pre.formalArguments(args)
      collectFormalArguments(stmt.body, whileArgs, cfgState)
    case _ => args
  }
}

