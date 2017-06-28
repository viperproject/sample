/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverMethodDeclaration
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Expression
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, MethodCall}
import ch.ethz.inf.pm.sample.permissionanalysis._

/**
  * A state providing transformers for inhale statements, exhale statements,
  * preconditions, postconditions, invariants, entering loops, and leaving
  * loops.
  *
  * @tparam S the self-type of the state
  * @author Jerome Dohrau
  */
trait SilverState[S <: SilverState[S]]
  extends SimpleState[S] {
  this: S =>

  import InterproceduralSilverInterpreter.{ArgumentPrefix, ReturnPrefix}

  /** Executes the given command.
    *
    * @param cmd The command to execute.
    * @return The abstract state after the execution of the given command.
    */
  override def command(cmd: Command): S = cmd match {
    case cmd: SilverCommand => cmd match {
      case InhaleCommand(expression) => inhale(expression)
      case ExhaleCommand(expression) => exhale(expression)
      case PreconditionCommand(expression) => precondition(expression)
      case PostconditionCommand(expression) => postcondition(expression)
      case InvariantCommand(expression) => invariant(expression)
      case EnterLoopCommand() => enterLoop()
      case LeaveLoopCommand() => leaveLoop()
      case cmd: ReturnFromMethodCommand[S] => returnFromMethod(cmd.methodDeclaration, cmd.methodCall, cmd.targetExpressions, cmd.exitState)
      case cmd: CallMethodBackwardsCommand[S] => callMethodBackwards(cmd.methodDeclaration, cmd.methodCall, cmd.parameterExpressions, cmd.entryState)
    }
    case _ => super.command(cmd)
  }

  /**
    * Inhales the given expression.
    *
    * @param expression The expression to inhale.
    * @return The state after inhaling the given expression.
    */
  def inhale(expression: Expression): S

  /**
    * Inhales the given expression.
    *
    * @param expression The expression to inhale.
    * @return The state after inhaling the given expression.
    */
  def inhale(expression: ExpressionSet): S = {
    // return this, not bottom, when the set of expression set is empty
    if (isBottom || expression.isBottom || expression.isTop || expression.expressions.isTop) this
    else Lattice.bigLub(expression.toSetOrFail.map(inhale))
  }

  /**
    * Exhales the given expression.
    *
    * @param expression The expression to inhale.
    * @return The state after exhaling the given expression.
    */
  def exhale(expression: Expression): S

  /**
    * Exhales the given expression.
    *
    * @param expression The expression to exhale.
    * @return The state after exhaling the given expression.
    */
  def exhale(expression: ExpressionSet): S = {
    // TODO: Do we want to report if an exhale failed?
    // return this, not bottom, when the set of expression set is empty
    if (isBottom || expression.isBottom || expression.isTop || expression.expressions.isTop) this
    else Lattice.bigLub(expression.toSetOrFail.map(exhale))
  }

  /**
    * Processes the given precondition.
    *
    * By default, the precondition is processed by inhaling the expression.
    *
    * @param expression The expression representing the precondition.
    * @return The state after processing the given precondition.
    */
  def precondition(expression: Expression): S = inhale(expression)

  /**
    * Processes the given precondition.
    *
    * By default, the precondition is processed by inhaling the expression.
    *
    * @param expression The expression representing the precondition.
    * @return The state after processing the precondition.
    */
  def precondition(expression: ExpressionSet): S = {
    // return this, not bottom, when the expression set is empty
    if (isBottom || expression.isBottom || expression.isTop || expression.expressions.isTop) this
    else Lattice.bigLub(expression.toSetOrFail.map(precondition))
  }

  /**
    * Processes the given postcondition.
    *
    * By default, the postcondition is processed by exhaling the expression.
    *
    * @param expression The expression representing the postcondition.
    * @return The state after processing the postcondition.
    */
  def postcondition(expression: Expression): S = exhale(expression)

  /**
    * Processes the given postcondition.
    *
    * By default, the postcondition is processed by exhaling the expression.
    *
    * @param expression The expression representing the postcondition.
    * @return The state after processing the postcondition.
    */
  def postcondition(expression: ExpressionSet): S = {
    // return this, not bottom, when the expression set is empty
    if (isBottom || expression.isBottom || expression.isTop || expression.expressions.isTop) this
    else Lattice.bigLub(expression.toSetOrFail.map(postcondition))
  }

  /**
    * Processes the given invariant.
    *
    * By default, the invariant is processed by first exhaling and then inhaling
    * the expression.
    *
    * @param expression The expression representing the invariant.
    * @return The state after processing the invariant.
    */
  def invariant(expression: Expression): S = exhale(expression).inhale(expression)

  /**
    * Processes the given invariant.
    *
    * By default, the invariant is processed by first exhaling and then inhaling
    * the expression.
    *
    * @param expression The expression representing the invariant.
    * @return The state after processing the invariant.
    */
  def invariant(expression: ExpressionSet): S = {
    // return this, not bottom, when the expression set is empty
    if (isBottom || expression.isBottom || expression.isTop || expression.expressions.isTop) this
    else Lattice.bigLub(expression.toSetOrFail.map(invariant))
  }

  /**
    * This method is invoked to signal the state that a loop is being entered.
    *
    * By default, nothing happens when a loop is entered.
    *
    * @return The state after entering the loop.
    */
  def enterLoop(): S = this

  /**
    * This method is invoked to signal the state that a loop is being left.
    *
    * By default, nothing happens when a loop is left.
    *
    * @return The state after leaving the loop.
    */
  def leaveLoop(): S = this

  /**
    * Returns a state where methodCall.targets gets the values of the the called methods returns.
    * methodCall.targets.head would "get" "ReturnPrefix"1 and so on
    *
    * @param methodDeclaration The method declaration of the callee
    * @param methodCall        The statement in the caller that calls the method
    * @param targetExpressions The target expressions that will receive the callee's returns
    * @param exitState         The exit state of the callee (from a previous analysis)
    * @return
    */
  def returnFromMethod(methodDeclaration: SilverMethodDeclaration, methodCall: MethodCall, targetExpressions: Seq[ExpressionSet], exitState: S): S = {
    var index = 0
    var st = exitState
    val returnVariableMapping = for ((formalRetVar, targetVar) <- methodDeclaration.returns.zip(targetExpressions)) yield {
      // formalRetVar = the variable declared in returns(...) of the method
      // targetVar = the target-expression which we'll assign to later
      val exp = ExpressionSet(VariableIdentifier(ReturnPrefix + index)(formalRetVar.typ))
      index += 1
      st = st.createVariable(exp, formalRetVar.typ, DummyProgramPoint).assignVariable(exp, ExpressionSet(formalRetVar.variable.id))
      (targetVar, exp)
    }
    st = st.ids.toSetOrFail // let's remove all non temporary-ret-variables
      .filter(id => !id.getName.startsWith(ReturnPrefix))
      .foldLeft(st)((st, ident) => st.removeVariable(ExpressionSet(ident)))
    // map return values to temp variables and remove all temporary ret variables
    val joinedState = returnVariableMapping.foldLeft(this.command(UnifyCommand(st)))((st: State[S], tuple) => (st.assignVariable _).tupled(tuple))
    returnVariableMapping.foldLeft(joinedState)((st, tuple) => st.removeVariable(tuple._2))
  }

  /**
    * Returns a state where parameterExpressions gets the values of the the called method's entry state.
    * This is used in the backward analysis and it's the dual to returnFromMethod() in the forward analysis.
    *
    *
    * @param methodDeclaration The method declaration of the callee
    * @param methodCall        The statement in the caller that calls the method
    * @param parameterExpressions The parameter expressions that will receive the callee's arguments
    * @param entryState         The entry state of the callee (from a previous analysis)
    * @return
    */
  def callMethodBackwards(methodDeclaration: SilverMethodDeclaration, methodCall: MethodCall, parameterExpressions: Seq[ExpressionSet], entryState: S): S = {
    var index = 0
    var st = entryState
    val argVariableMapping = for((formalArgVar, argVar) <- methodDeclaration.arguments.zip(parameterExpressions)) yield {
      // formalArgVar = the variable declared in arguments(...) of the method
      // argVar = the argument/parameter-expression which is assigned to the temporary variable later
      val exp = ExpressionSet(VariableIdentifier(ArgumentPrefix + index )(formalArgVar.typ))
      index += 1
      st = st.createVariable(exp, formalArgVar.typ, DummyProgramPoint).assignVariable(ExpressionSet(formalArgVar.variable.id), exp)
      (exp, argVar)
    }
    st = st.ids.toSetOrFail // let's remove all non ret_# variables
      .filter(id => ! id.getName.startsWith(ArgumentPrefix))
      .foldLeft(st)((st, ident)=> st.removeVariable(ExpressionSet(ident)))
    // map return values to temp variables and remove all temporary ret_# variables
    val joinedState = argVariableMapping.foldLeft(this.command(UnifyCommand(st)))((st: State[S], tuple) => (st.assignVariable _).tupled(tuple))
    argVariableMapping.foldLeft(joinedState)((st, tuple) => st.removeVariable(tuple._1))
  }

}