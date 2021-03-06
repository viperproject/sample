/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.{Replacement, Identifier, SemanticDomain}

/** Subscribes to ghost operations performed by `PredicateDrivenHeapState`. */
trait GhostOpSubscriber[S <: SemanticDomain[S]] {
  def notify(state: PredicateDrivenHeapState[S], event: GhostOpEvent)
}

/** Subscriber that collects all ghost operation events. */
case class GhostOpCollector[S <: SemanticDomain[S]]() extends GhostOpSubscriber[S] {
  private[this] var _ghostOps: Seq[GhostOpEvent] = Seq.empty

  def ghostOps = _ghostOps

  /** Returns all collected unfold ghost operations. */
  def unfoldGhostOps = ghostOps.collect({ case e: UnfoldGhostOpEvent => e })

  /** Returns all collected fold ghost operations. */
  def foldGhostOps = ghostOps.collect({ case e: FoldGhostOpEvent => e })

  /** Returns all collected predicate merge ghost operations. */
  def predIdMergeGhostOps = ghostOps.collect({ case e: PredicateIdentifierMergeEvent => e })

  override def notify(state: PredicateDrivenHeapState[S], event: GhostOpEvent) = {
    _ghostOps = _ghostOps :+ event
  }
}

/** Represents a ghost operation performed by the `PredicateDrivenHeapState`. */
trait GhostOpEvent {
}

/** Represents an unfold performed by the `PredicateDrivenHeapState`. */
final case class UnfoldGhostOpEvent(
    variable: Identifier,
    predicateId: PredicateIdentifier)
  extends GhostOpEvent {
}

/** Represents a fold performed by the `PredicateDrivenHeapState`. */
final case class FoldGhostOpEvent(
    variable: Identifier,
    predicateId: PredicateIdentifier)
  extends GhostOpEvent {
}

/** Represents a merge of predicate IDs by the `PredicateDrivenHeapState`. */
final case class PredicateIdentifierMergeEvent(predIdMerge: PredicateIdentifierMerge)
  extends GhostOpEvent {
}