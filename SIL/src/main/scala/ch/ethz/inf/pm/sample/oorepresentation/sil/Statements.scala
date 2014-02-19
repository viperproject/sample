package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.oorepresentation._
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.SystemParameters

case class WrappedProgramPoint(pos: sil.RealPosition) extends LineColumnProgramPoint {
  def getLine: Int = pos.line

  def getColumn: Int = pos.column

  override def toString: String = description
}

/**
 * Overrides the default forward semantics of MethodCall in accordance with SIL.
 *
 * Method calls and function applications are purely static in SIL. There are no receivers.
 * Also, function and method names are globally unique.
 * Instead of FieldAccess, the translation uses a simple Variable to refer to the method or function.
 * This trait does not consider the forward semantics of such variables because they do not exist anyway.
 *
 * @todo Merge the code with Sample's MethodCall implementation.
 * @todo Parts of the code have been taken from TouchDevelop's MethodSummaries.
 */
trait ContractAwareMethodCall extends MethodCall {
  override def forwardSemantics[S <: State[S]](state: S): S = {
    // We can use the original state for executing all argument statements,
    // as all of these expressions do not have any effect on the state.
    var (paramExprSets, result) = UtilitiesOnStates.forwardExecuteListStatements[S](state, parameters)

    val inParams = callTarget.arguments(0)
    val outParams = callTarget.arguments(1)
    val params = inParams ++ outParams

    // Create a temporary variable for each in- and out-parameter.
    // callTarget.arguments is a list of lists. The first list holds all in-parameters
    // and the second one all out-parameters.
    // TODO: What if there are in-parameters and out-parameters with the same name?
    def createTmpParam(param: VariableDeclaration): VariableIdentifier = {
      val tmpParam = VariableIdentifier(makeTempVariableName(param.variable.id.toString()), ProgramPointScopeIdentifier(callTarget.programpoint))(param.typ, pp)
      val expr = new ExpressionSet(tmpParam.getType).add(tmpParam)
      result = result.createVariable(expr, tmpParam.getType, callTarget.programpoint)
      tmpParam
    }

    val tmpInParams = inParams.map(createTmpParam)
    val tmpOutParams = outParams.map(createTmpParam)
    val tmpParams = tmpInParams ++ tmpOutParams

    // Assign the actual argument expression sets to the temporary in-parameter variables
    for ((tmpInParam, paramExprSet) <- tmpInParams.zip(paramExprSets)) {
      val expr = new ExpressionSet(tmpInParam.getType).add(tmpInParam)
      result = result.assignVariable(expr, paramExprSet)
    }

    // Set the temporary out-parameter variables to top
    // Not necessary with the new heap analysis
    /* for (tmpOutParam <- tmpOutParams) {
      val expr = new ExpressionSet(tmpOutParam.getType()).add(tmpOutParam)
      val topExpr = new ExpressionSet(tmpOutParam.getType()).add(new Constant("valid", tmpOutParam.getType(), pp))
      result = result.assignVariable(expr, topExpr)
    } */

    // Create replacement for variables in pre- and pots-conditions with temporary variables
    val replacement = new Replacement(isPureRenaming = true)
    for ((param, tmpParam) <- params.zip(tmpParams)) {
      replacement.value += (Set[Identifier](param.variable.id) -> Set[Identifier](tmpParam))
    }

    // Assert the method precondition on the in-parameters
    val preCondExprSet = callTarget.precond.forwardSemantics(result).getExpression.merge(replacement)
    result = result.setExpression(preCondExprSet)
    if (!result.testFalse().lessEqual(state.bottom())) {
      Reporter.reportError("Possible pre-condition violation", pp)
    }
    result = result.testTrue()

    // Assume the method post-condition (could involve old expressions).
    // There is no need to to set the in-parameter variables to top (they cannot be reassigned in SIL).
    val postCondExprSet = callTarget.postcond.forwardSemantics(result).getExpression.merge(replacement)
    result = result.assume(postCondExprSet)

    // Remove the temporary in-parameter variables
    // TODO: Not supported by the heap analysis
    /* tmpInParams.foreach(tmpInParam =>
      result = result.removeVariable(new ExpressionSet(tmpInParam.typ).add(tmpInParam))
    ) */

    forwardSemanticsOutParams(result, tmpOutParams)
  }

  /**
   * Builds the resulting state using the temporary out-parameters.
   * The post-condition has already been assumed on the out-parameters at this point.
   */
  protected def forwardSemanticsOutParams[S <: State[S]](state: S, tmpOutParams: List[VariableIdentifier]): S

  /**
   * Generates the name of a temporary variable.
   * The percent prefix ensures that it does not clash with valid SIL identifiers.
   */
  protected def makeTempVariableName(id: String): String = "%tmp_" + id

  /** Returns whether a given identifier refers to a temporary variable. */
  protected def isTempVariableName(id: String): Boolean = id.startsWith("%tmp_")

  /** Whether this method call is pure (i.e. does not change the heap in any way */
  protected lazy val isPure: Boolean = callTarget.modifiers.contains(PureModifier)

  /** Returns the declaration corresponding to the called method. */
  protected lazy val callTarget: MethodDeclaration = method match {
    case Variable(programPoint, VariableIdentifier(name, _)) =>
      // In SIL, function and method names must be globally unique.
      // TODO: Accessing the compiler through the global variable is incredibly ugly
      SystemParameters.compiler.asInstanceOf[SilCompiler].getMethods(name)(0)._2
    case _ => throw new IllegalStateException("expected method to be of type Variable, got " + method.getClass.toString)
  }

  /** Returns whether this method call may trigger recursive method calls */
  protected lazy val isRecursive: Boolean = {
    // It is not strictly necessary to add the current method to the set of visited methods
    // It's just a performance optimization
    // TODO: Needs update
    /* var visitedMethods = Set(SystemParameters.currentMethod)
    def isRecursiveStmt(stmt: Statement): Boolean = stmt match {
      case c: ContractAwareMethodCall if isRecursiveMethod(c.callTarget) => true
      case _ => stmt.getChildren.exists(isRecursiveStmt)
    }
    def isRecursiveMethod(method: MethodDeclaration): Boolean = {
      if (visitedMethods.contains(method.name.toString)) return true
      visitedMethods += method.name.toString
      method.body.getChildren.exists(isRecursiveStmt)
    }
    isRecursiveMethod(callTarget) */
    true
  }
}

class SilFunctionCall(
    override val pp: ProgramPoint,
    override val method: Statement,
    override val parametricTypes: List[Type],
    override val parameters: List[Statement],
    override val returnedType: Type)
  extends MethodCall(pp, method, parametricTypes, parameters, returnedType) with ContractAwareMethodCall {

  /**
   * Add a result variable access to the state's expression set.
   *
   * We may not remove the temporary result out-parameter variable.
   * It is not clear at this point what the result value will be used for.
   */
  protected def forwardSemanticsOutParams[S <: State[S]](state: S, tmpOutParams: List[VariableIdentifier]): S =
    new Variable(pp, tmpOutParams(0)).forwardSemantics(state)
}

class SilMethodCall(
    override val pp: ProgramPoint,
    override val method: Statement,
    override val parametricTypes: List[Type],
    override val parameters: List[Statement],
    targets: List[Variable])
  extends MethodCall(pp, method, parametricTypes, parameters, returnedType = null) with ContractAwareMethodCall {

  /**
   * Directly assign the temporary out-parameter variables to the given target variables
   * and remove the temporary out-parameter variables.
   */
  protected def forwardSemanticsOutParams[S <: State[S]](state: S, tmpOutParams: List[VariableIdentifier]): S = {
    var result = state
    for ((target, tmpOutParam) <- targets.zip(tmpOutParams)) {
      val assign = new Assignment(target.getPC(), target, new Variable(target.getPC(), tmpOutParam))
      result = assign.forwardSemantics(result)
      // TODO: Not supported by heap analysis
      // result = result.removeVariable(new ExpressionSet(tmpOutParam.typ).add(tmpOutParam))
    }
    result
  }
}