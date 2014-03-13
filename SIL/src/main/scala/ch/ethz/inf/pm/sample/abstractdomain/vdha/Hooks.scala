package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.{Replacement, Identifier, SemanticDomain}

/** Interface using which `PredicateDrivenHeapState` can let interested
  * parties know when ghost operations take place.
  */
trait GhostOpHook[S <: SemanticDomain[S]] {
  def handleUnfold(unfold: UnfoldGhostOp[S])
  def handleFold(fold: FoldGhostOp[S])
  def handlePredMerge(merge: PredMergeGhostOp[S])
}

/** Does nothing when a ghost operation is performed. */
case class DummyGhostOpHook[S <: SemanticDomain[S]]() extends GhostOpHook[S] {
  def handleUnfold(unfold: UnfoldGhostOp[S]) = {}
  def handleFold(fold: FoldGhostOp[S]) = {}
  def handlePredMerge(merge: PredMergeGhostOp[S]): Unit = {}
}

/** Collects all ghost operations. */
case class CollectingGhostOpHook[S <: SemanticDomain[S]]() extends GhostOpHook[S] {
  private[this] var _unfolds: Seq[UnfoldGhostOp[S]] = Seq.empty
  private[this] var _folds: Seq[FoldGhostOp[S]] = Seq.empty
  private[this] var _predMerges: Seq[PredMergeGhostOp[S]] = Seq.empty

  def unfolds = _unfolds
  def folds = _folds
  def predMerges = _predMerges

  def handleUnfold(unfold: UnfoldGhostOp[S]) = {
    _unfolds = _unfolds :+ unfold
  }

  def handleFold(fold: FoldGhostOp[S]) = {
    _folds = _folds :+ fold
  }

  def handlePredMerge(predMerge: PredMergeGhostOp[S]) = {
    _predMerges = _predMerges :+ predMerge
  }
}


/** Represents a ghost operation performed by the `PredicateDrivenHeapState` */
trait GhostOp[S <: SemanticDomain[S]] {
  def state: PredicateDrivenHeapState[S]
}

/** Represents an unfold performed by the `PredicateDrivenHeapState` */
final case class UnfoldGhostOp[S <: SemanticDomain[S]](
    state: PredicateDrivenHeapState[S],
    variable: Identifier,
    predicateId: Identifier)
  extends GhostOp[S] {
}

/** Represents a fold performed by the `PredicateDrivenHeapState` */
final case class FoldGhostOp[S <: SemanticDomain[S]](
    state: PredicateDrivenHeapState[S],
    variable: Identifier,
    predicateId: Identifier)
  extends GhostOp[S] {
}

/** Represents a merge of predicates by the `PredicateDrivenHeapState`. */
final case class PredMergeGhostOp[S <: SemanticDomain[S]](
    state: PredicateDrivenHeapState[S],
    repl: Replacement)
  extends GhostOp[S] {
}