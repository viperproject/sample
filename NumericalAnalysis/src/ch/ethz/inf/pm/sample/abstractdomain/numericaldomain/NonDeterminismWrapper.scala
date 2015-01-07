package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._

/**
 *
 * Gives Non-Deterministic expression support to a relational domain.
 *
 * (A non-deterministic expression is something like 5 to 100)
 *
 * @author Lucas Brutschy
 */
case class NonDeterminismWrapper[X <: RelationalNumericalDomain[X]](wrapped:X)
  extends RelationalNumericalDomain[NonDeterminismWrapper[X]]
  with RelationalNumericalDomainWrapper[X,NonDeterminismWrapper[X]] {

  override def assign(variable: Identifier, expr: Expression): NonDeterminismWrapper[X] = {
    wrapperFactory(nondeterminismWrapper(expr, wrapped, (someExpr, someState) => {
      someState.assign(variable,someExpr)
    }))
  }

  override def assume(expr: Expression): NonDeterminismWrapper[X] = {
    wrapperFactory(nondeterminismWrapper(expr, wrapped, (someExpr, someState) => {
      someState.assume(someExpr)
    }))
  }

  override def backwardAssign(oldPre: NonDeterminismWrapper[X], variable: Identifier, expr: Expression): NonDeterminismWrapper[X] = {

    // for non-deterministic expressions, we fall back to simply forgetting the variable
    if (!isDeterministicExpr(expr)) {
      return this.setToTop(variable)
    }

    super.backwardAssign(oldPre,variable,expr)

  }

  protected def nondeterminismWrapper(expr: Expression, state: X, someFunc: (Expression, X) => X): X = {

    // Extract all non-deterministic expressions and store them in temporary variables
    var newState = state
    val (newExpr, tempAssigns) = removeNondeterminism("tmp", expr)

    // Add all temporary variables
    for ((id, _) <- tempAssigns) {
      newState = newState.createVariable(id)
    }

    for ((id, ndExpr) <- tempAssigns) {
      ndExpr.op match {
        case NondeterministicOperator.or =>
          val newStateLeft = newState.assign(id, ndExpr.left)
          val newStateRight = newState.assign(id, ndExpr.right)
          newState = newStateLeft.lub(newStateRight)
        case NondeterministicOperator.to =>
          newState = newState.
            assume(BinaryArithmeticExpression(id, ndExpr.left, ArithmeticOperator.>=, ndExpr.typ)).
            assume(BinaryArithmeticExpression(id, ndExpr.right, ArithmeticOperator.<=, ndExpr.typ))
      }
    }

    newState = someFunc(newExpr, newState)

    // Remove all temporary variables
    for ((id, _) <- tempAssigns) {
      newState = newState.removeVariable(id)
    }

    newState
  }

  private def removeNondeterminism(label: String, expr: Expression): (Expression, List[(Identifier, BinaryNondeterministicExpression)]) = {
    expr match {
      case BinaryArithmeticExpression(left, right, op, typ) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (BinaryArithmeticExpression(expL, expR, op, typ), varL ::: varR)
      case BinaryBooleanExpression(left, right, op, typ) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (BinaryBooleanExpression(expL, expR, op, typ), varL ::: varR)
      case ReferenceComparisonExpression(left, right, op, typ) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (ReferenceComparisonExpression(expL, expR, op, typ), varL ::: varR)
      case NegatedBooleanExpression(left) =>
        val (expL, varL) = removeNondeterminism(label, left)
        (NegatedBooleanExpression(expL), varL)
      case UnaryArithmeticExpression(left, op, ret) =>
        val (expL, varL) = removeNondeterminism(label, left)
        (UnaryArithmeticExpression(expL, op, ret), varL)
      case BinaryNondeterministicExpression(left, right, op, returnType) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        val identifier = new VariableIdentifier(label)(expr.typ, expr.pp)
        (identifier, varL ::: varR ::: List((identifier, BinaryNondeterministicExpression(expL, expR, op, returnType))))
      case x: Expression => (x, Nil)
    }
  }

  private def isDeterministicExpr(expr: Expression): Boolean = {
    expr match {
      case BinaryArithmeticExpression(left, right, op, typ) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case BinaryBooleanExpression(left, right, op, typ) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case ReferenceComparisonExpression(left, right, op, typ) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case NegatedBooleanExpression(left) =>
        isDeterministicExpr(left)
      case UnaryArithmeticExpression(left, op, ret) =>
        isDeterministicExpr(left)
      case BinaryNondeterministicExpression(left, right, op, returnType) =>
        false
      case x: Expression => true
    }
  }

  override def wrapperFactory(wrapped: X): NonDeterminismWrapper[X] = NonDeterminismWrapper(wrapped)
}
