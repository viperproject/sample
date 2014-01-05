package graph

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
   * Pairwise GLB between states in <code>leftConds</code> and <code>rightConds</code>preserving
   * EdgeLocalIdentifiers and AccessPathIdentifiers.
   *
   * @param leftConds
   * @param rightConds
   * @return
   *
   * @author Milos Novacek
   */
  def applyConditions[S <: SemanticDomain[S]](leftConds : Set[S], rightConds : Set[S]) : Set[S] = {
    val resultingConds = scala.collection.mutable.Set.empty[S]
    for (lCond <- leftConds)
      for (rCond <- rightConds) {
        resultingConds += glbPreserveIds(lCond, rCond)
      }
    resultingConds.toSet[S]
  }

  def removeAccessPathIdentifiers[S <: SemanticDomain[S]](state : S) : S = {
    val idsToRemove = state.getIds().filter(id => id.isInstanceOf[AccessPathIdentifier]).toSet[Identifier]
    state.removeVariables(idsToRemove)
  }

  /** Returns the GLB of two states, but takes the union of their identifiers. */
  def glbPreserveIds[S <: SemanticDomain[S]](left: S, right: S): S = {
    val newRightIds = edgeLocalAndAccessPathIds(left) diff right.getIds()
    val newLeftIds = edgeLocalAndAccessPathIds(right) diff left.getIds()
    val newLeft = left.createVariables(newLeftIds.toSet[Identifier])
    val newRight = right.createVariables(newRightIds.toSet[Identifier])
    newLeft.glb(newRight)
  }

  /** Returns all edge-local and access path identifiers in a state. */
  def edgeLocalAndAccessPathIds[S <: SemanticDomain[S]](state: S) =
    state.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] ||
      id.isInstanceOf[AccessPathIdentifier])
}
