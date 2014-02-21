package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._

object Utilities {

  val BinaryBoolArithmeticOperators = Set(ArithmeticOperator.>,
                                    ArithmeticOperator.>=,
                                    ArithmeticOperator.<,
                                    ArithmeticOperator.!=,
                                    ArithmeticOperator.==,
                                    ArithmeticOperator.<=)
  val BinaryBoolOperators = Set(BooleanOperator.||, BooleanOperator.&&)

  def negateExpression(exp: Expression): Expression = exp match {
    // TODO(severinh): Code is similar to ApronInterface.assume. Code sharing may be possible.
    case v: VariableIdentifier => NegatedBooleanExpression(v)
    case NegatedBooleanExpression(e) => e
    case BinaryArithmeticExpression(l, r, o, t) =>
      new BinaryArithmeticExpression(l, r, ArithmeticOperator.negate(o), t)
    case BinaryBooleanExpression(l, r, o, t) =>
      new BinaryBooleanExpression(negateExpression(l), negateExpression(r), negateBoolOperator(o), t)
    case Constant("true", typ, pp) =>
      Constant("false", typ, pp)
    case Constant("false", typ, pp) =>
      Constant("true", typ, pp)
    case ReferenceComparisonExpression(l, r, o, t) =>
      new ReferenceComparisonExpression(l, r, ArithmeticOperator.negate(o), t)
  }

  private def negateBoolOperator(op: BooleanOperator.Value): BooleanOperator.Value = op match {
    case BooleanOperator.&& => BooleanOperator.||
    case BooleanOperator.|| => BooleanOperator.&&
    case _ => throw new Exception("Invalid boolean operator.")
  }

  def removeAccessPathIdentifiers[S <: SemanticDomain[S]](state : S) : S = {
    val idsToRemove = state.getIds().filter(id => id.isInstanceOf[AccessPathIdentifier]).toSet[Identifier]
    state.removeVariables(idsToRemove)
  }

  /** Returns the GLB of two states, but takes the union of their identifiers. */
  def glbPreserveIds[S <: SemanticDomain[S]](left: S, right: S): S = {
    val newRightIds = edgeLocalAndAccessPathIds(left) diff right.getIds()
    val newLeftIds = edgeLocalAndAccessPathIds(right) diff left.getIds()
    val newLeft = left.createVariables(newLeftIds)
    val newRight = right.createVariables(newRightIds)
    newLeft.glb(newRight)
  }

  /** Returns all edge-local identifiers in a state. */
  def edgeLocalIds[S <: SemanticDomain[S]](state: S) =
    state.getIds().collect({ case id: EdgeLocalIdentifier => id })

  /** Returns all access path identifiers in a state. */
  def accPathIds[S <: SemanticDomain[S]](state: S) =
    state.getIds().collect({ case id: AccessPathIdentifier => id })

  /** Returns all edge-local and access path identifiers in a state. */
  def edgeLocalAndAccessPathIds[S <: SemanticDomain[S]](state: S) =
    state.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] ||
      id.isInstanceOf[AccessPathIdentifier])

  /**
   * Replaces all non-numerical `VariableIdentifier`s in the given expression
   * with a corresponding `AccessPathIdentifier`.
   *
   * @todo replace all `VariableIdentifiers`, including numerical ones
   */
  def normalizeExpression(exp: Expression): Expression =
    exp.transform({
      case v: VariableIdentifier if v.typ.isObject => AccessPathIdentifier(v)
      case e => e
    })
}
