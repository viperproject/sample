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

  /** Add various helper methods to `SemanticDomain` objects. */
  implicit class ExtendedSemanticDomain[S <: SemanticDomain[S]](state: S) {
    /** Returns all edge-local identifiers in the state. */
    def edgeLocalIds: Set[EdgeLocalIdentifier] =
      state.ids.collect({ case id: EdgeLocalIdentifier => id })

    /** Returns all access path identifiers in th state. */
    def accPathIds: Set[AccessPathIdentifier] =
      state.ids.collect({ case id: AccessPathIdentifier => id })

    /** Returns all edge-local and access path identifiers in the state. */
    def edgeLocalAndAccessPathIds: Set[Identifier] =
      state.ids.filter(id => id.isInstanceOf[EdgeLocalIdentifier] ||
        id.isInstanceOf[AccessPathIdentifier])

    /** Returns the GLB of this and another state, but takes the union
      * of their identifiers.
      */
    def glbPreserveIds(right: S): S = {
      val newRightIds = state.edgeLocalAndAccessPathIds diff right.ids
      val newLeftIds = right.edgeLocalAndAccessPathIds diff state.ids
      val newLeft = state.createVariables(newLeftIds)
      val newRight = right.createVariables(newRightIds)
      newLeft.glb(newRight)
    }

    /** Removes all access path identifiers from the state. */
    def removeAccessPathIds(): S =
      state.removeVariables(accPathIds)
  }
}
