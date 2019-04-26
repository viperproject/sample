/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

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
case class SummaryNodeWrapper[X <: NumericalDomain.Relational[X]](wrapped:X)
  extends NumericalDomain.Relational[SummaryNodeWrapper[X]]
  with NumericalDomain.Relational.Wrapper[X,SummaryNodeWrapper[X]] {

  override def assign(variable: Identifier, expr: Expression): SummaryNodeWrapper[X] = {

    if (expr.ids.isTop) return setToTop(variable)

    // Handling of summary nodes on the right side -> Materialize
    val res =
      if (expr.ids.getNonTop.exists(x => !x.representsSingleVariable)) {
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

    if (expr.ids.isTop) return this

    // Handling of summary nodes on the right side -> Materialize
    if (expr.ids.getNonTop.exists(x => !x.representsSingleVariable)) {
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
          val newIdentifier = VariableIdentifier(x.getName + "__TMP" + temporaryCounter)(x.typ, x.pp)
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

  override def wrapperFactory(newWrapped: X): SummaryNodeWrapper[X] = SummaryNodeWrapper(newWrapped)

}
