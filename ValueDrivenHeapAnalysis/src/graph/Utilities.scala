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

  def removeVariablesFromState[S <: SemanticDomain[S]](state: S, ids: Set[Identifier]): S = {
    var result = state
    for (id <- ids) {
      result = result.removeVariable(id)
    }
    return result
  }

  def createVariablesForState[S <: SemanticDomain[S]](state: S, ids: Set[Identifier]): S = {
    var result = state
    for (id <- ids -- state.getIds())
      result = result.createVariable(id, id.getType())
    return result
  }

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
        val idsToAddRight = lCond.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] || id.isInstanceOf[AccessPathIdentifier]) diff rCond.getIds()
        val idsToAddLeft = rCond.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] || id.isInstanceOf[AccessPathIdentifier]) diff lCond.getIds()
        val newLeftCond = Utilities.createVariablesForState(lCond, idsToAddLeft.toSet[Identifier])
        val newRightCond = Utilities.createVariablesForState(rCond, idsToAddRight.toSet[Identifier])
        resultingConds += lCond.glb(newLeftCond, newRightCond)
      }
    resultingConds.toSet[S]
  }

  def removeAccessPathIdentifiers[S <: SemanticDomain[S]](state : S) : S = {
    val idsToRemove = state.getIds().filter(id => id.isInstanceOf[AccessPathIdentifier]).toSet[Identifier]
    removeVariablesFromState(state, idsToRemove)
  }

}
