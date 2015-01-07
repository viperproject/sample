package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._

/**
 *
 * Makes a non-relational domain sound for summary nodes
 *
 * This corresponds to the summarization technique
 * Gopan et al. "Numeric Domains with Summarized Dimensions"
 *
 * @author Lucas Brutschy
 */
case class SummaryNodeWrapper[X <: RelationalNumericalDomain[X]](wrapped:X)
  extends RelationalNumericalDomain[SummaryNodeWrapper[X]]
  with RelationalNumericalDomainWrapper[X,SummaryNodeWrapper[X]] {

  override def assign(variable: Identifier, expr: Expression): SummaryNodeWrapper[X] = {

    // Handling of summary nodes on the right side -> Materialize
    val res =
      if (expr.ids.filter(x => !x.representsSingleVariable).nonEmpty) {
        materializeSummaryNodes(expr, wrapped, (someExpr, someState) => {
          someState.assign(variable, someExpr)
        })
      } else {
        wrapped.assign(variable, expr)
      }

    // Handling of summary nodes on the left side
    // If variable is a summary node, perform weak update by computing S[x<-v] |_| S
    if (!variable.representsSingleVariable) {
      wrapperFactory(res.lub(wrapped))
    } else wrapperFactory(res)
  }

  override def assume(expr: Expression): SummaryNodeWrapper[X] = {

    // Handling of summary nodes on the right side -> Materialize
    if (expr.ids.filter(x => !x.representsSingleVariable).nonEmpty) {
      wrapperFactory(materializeSummaryNodes(expr, wrapped, (someExpr, someState) => {
        someState.assume(someExpr)
      }))
    } else {
      wrapperFactory(wrapped.assume(expr))
    }

  }

  /**
   * Materializes all summary nodes in Expression. Then calls someFunc.
   * Then, performs a clean-up.
   *
   * @param expr The expression to be expanded
   * @param state The current state
   * @param someFunc The function to be executed
   * @return The modified state, the set of temporary variables
   */
  private def materializeSummaryNodes(expr: Expression, state: X, someFunc: (Expression, X) => X): X = {

    // We have a summary node.
    var temporaryCounter = 0
    val expandTemporaryVariables: Replacement = new Replacement(isPureExpanding = true)
    val removeTemporaryVariables: Replacement = new Replacement(isPureRemoving = true)

    val transformedExpression = expr.transform({
      case x: Identifier =>
        if (!x.representsSingleVariable) {
          val newIdentifier = SimpleNumericalIdentifier(x.getName + "__TMP" + temporaryCounter, summary = false, x.typ, x.pp)
          temporaryCounter = temporaryCounter + 1
          expandTemporaryVariables.value(Set(x)) =
            expandTemporaryVariables.value.get(Set(x)) match {
              case Some(s) => s ++ Set(x, newIdentifier)
              case None => Set(x, newIdentifier)
            }
          removeTemporaryVariables.value += (Set(newIdentifier.asInstanceOf[Identifier]) -> Set.empty[Identifier])
          newIdentifier
        } else x
      case x: Expression => x
    })
    val preState = state.merge(expandTemporaryVariables)
    val postState = someFunc(transformedExpression, preState)
    val cleanPostState = postState.merge(removeTemporaryVariables)

    cleanPostState

  }

  override def wrapperFactory(wrapped: X): SummaryNodeWrapper[X] = SummaryNodeWrapper(wrapped)

}
