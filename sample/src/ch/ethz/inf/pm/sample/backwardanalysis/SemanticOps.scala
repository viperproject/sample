package ch.ethz.inf.pm.sample.backwardanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionFactory, Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.oorepresentation._

/**
 * The basic unit of execution histories.
 *
 * Basically, it captures an operation invoked on AbstractState, together
 * with the forward and backward transformers.
 */
trait SemanticOp[S <: State[S]] {
  def forward(pre: S): S
  def backward(post: S, oldPre: S): S
}

case class StatementOp[S <: State[S]](stmt: Statement) extends SemanticOp[S] {
  override def forward(pre: S): S = stmt.forwardSemantics(pre)
  override def backward(post: S, oldPre: S): S = stmt.backwardSemantics(post, oldPre)
}

case class AssignVarOp[S <: State[S]](lhsExpr: ExpressionSet, rhsExpr: ExpressionSet) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.assignVariable(lhsExpr, rhsExpr)
  override def backward(post: S, oldPre: S): S = post.backwardAssignVariable(oldPre, lhsExpr, rhsExpr)
}

case class AssignFieldOp[S <: State[S]](obj: ExpressionSet, field: String, rhsExpr: ExpressionSet) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.assignField(obj, field, rhsExpr)
  override def backward(post: S, oldPre: S): S = post.backwardAssignField(oldPre, obj, field, rhsExpr)
}

case class NativeMethodOp[S <: State[S]](callTarget: ExpressionSet, calledMethod: String, parametersExprs: List[ExpressionSet], parametricTypes: List[Type], returnType: Type, pp: ProgramPoint) extends SemanticOp[S] {

  override def forward(pre: S): S = {
    // collect results of applicable native semantics  (lazy, not evaluated yet)
    val nativeSemanticsResults = (
      for(sem <- SystemParameters.nativeMethodsSemantics.view) yield
        sem.applyForwardNativeSemantics[S](callTarget, calledMethod, parametersExprs, parametricTypes, returnType, pp, pre)
      ).flatten

    // return first successful application (applies all semantics until first successful one, if any)
    val firstSuccess = nativeSemanticsResults.headOption
    firstSuccess match {
      case Some(s) => s
      case None =>
        pre.top()
    }
  }

  override def backward(post: S, oldPre: S): S = {
    // collect results of applicable native semantics  (lazy, not evaluated yet)
    val nativeSemanticsResults = (
      for(sem <- SystemParameters.nativeMethodsSemantics.view) yield
        sem.applyBackwardNativeSemantics[S](callTarget, calledMethod, parametersExprs, parametricTypes, returnType, pp, post, oldPre)
      ).flatten

    // return first successful application (applies all semantics until first successful one, if any)
    val firstSuccess = nativeSemanticsResults.headOption
    firstSuccess match {
      case Some(s) => s
      case None =>
        Reporter.reportImprecision("Native backward semantics for type " + callTarget.getType() + " with method " + calledMethod + " not implemented", pp)
        post.top()
    }
  }
}

case class CreateObjOp[S <: State[S]](typ: Type, pp: ProgramPoint, fields: Option[Set[Identifier]]) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.createObject(typ, pp, fields)
  override def backward(post: S, oldPre: S): S = post.removeObject(oldPre, post.expr, fields)
}

case class CreateVarOp[S <: State[S]](x: ExpressionSet, typ: Type, pp: ProgramPoint) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.createVariable(x, typ, pp)

  override def backward(post: S, oldPre: S): S = post
}

case class SetExprOp[S <: State[S]](expr: ExpressionSet) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.setExpression(expr)
  override def backward(post: S, oldPre: S): S = post.setExpression(ExpressionFactory.unitExpr)
}

case class EmptyOp[S <: State[S]](expr: ExpressionSet) extends SemanticOp[S] {
  override def forward(pre: S): S = pre

  override def backward(post: S, oldPre: S): S = post
}

case class OptimizeSummaryOp[S <: State[S]]() extends SemanticOp[S] {
  override def forward(pre: S): S = pre.optimizeSummaryNodes()

  // TODO: not sound/correct?
  override def backward(post: S, oldPre: S): S = post
}

case class PruneVarOp[S <: State[S]](filter: Identifier => Boolean) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.pruneVariables(filter)
  override def backward(post: S, oldPre: S): S = post.undoPruneVariables(oldPre, filter)
}

case class CollectionLengthOp[S <: State[S]](collectionSet: ExpressionSet) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.getCollectionLength(collectionSet)
  override def backward(post: S, oldPre: S): S = post // I assume this operation has no side effects on the state.
}

case class CollectionCopyOp[S <: State[S]](fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.copyCollection(fromCollectionSet, toCollectionSet)

  override def backward(post: S, oldPre: S): S = {
    post.setCollectionToTop(toCollectionSet)
  }
}

case class CreateCollectionOp[S <: State[S]](collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type,
                                             keyCollectionTyp: Option[Type], tpp: ProgramPoint,
                                             fields: Option[Set[Identifier]]) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.createCollection(collTyp, keyTyp, valueTyp, lengthTyp, keyCollectionTyp, tpp, fields)

  override def backward(post: S, oldPre: S): S = post.pruneUnreachableHeap()
}


case class InsertCollectionTopElemOp[S <: State[S]](collectionSet: ExpressionSet, keyTop: ExpressionSet,
                                                    valueTop: ExpressionSet, pp: ProgramPoint) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.insertCollectionTopElement(collectionSet, keyTop, valueTop, pp)

  override def backward(post: S, oldPre: S): S = post.setCollectionToTop(collectionSet)
}

case class InsertCollectionElemOp[S <: State[S]](collectionSet: ExpressionSet, key: ExpressionSet,
                                                    value: ExpressionSet, pp: ProgramPoint) extends SemanticOp[S] {
  override def forward(pre: S): S = pre.insertCollectionElement(collectionSet, key, value, pp)

  override def backward(post: S, oldPre: S): S = post.setCollectionToTop(collectionSet)
}

case class CollectionContainsValueOp[S <: State[S]](collectionSet: ExpressionSet, valueSet: ExpressionSet,
                                               booleanTyp: Type, pp: ProgramPoint) extends SemanticOp[S] {

  override def forward(pre: S): S = pre.collectionContainsValue(collectionSet, valueSet, booleanTyp, pp)

  // Assumes the heap is not modified by this operation.
  override def backward(post: S, oldPre: S): S = post.setExpression(oldPre.expr)
}

case class CollectionContainsKeyOp[S <: State[S]](collectionSet: ExpressionSet, valueSet: ExpressionSet,
                                               booleanTyp: Type, pp: ProgramPoint) extends SemanticOp[S] {

  override def forward(pre: S): S = pre.collectionContainsKey(collectionSet, valueSet, booleanTyp, pp)

  // Assumes the heap is not modified by this operation.
  override def backward(post: S, oldPre: S): S = post.setExpression(oldPre.expr)
}



case class RemoveFirstCollectionValueByValueOp[S <: State[S]](collectionSet: ExpressionSet,
                                                              valueSet: ExpressionSet) extends SemanticOp[S] {

  override def forward(pre: S): S = pre.removeCollectionValueByKey(collectionSet, valueSet)

  override def backward(post: S, oldPre: S): S = post.setCollectionToTop(collectionSet)
}

case class AssignAllCollectionKeysOp[S <: State[S]](collectionSet: ExpressionSet,
                                                    valueSet: ExpressionSet) extends SemanticOp[S] {

  override def forward(pre: S): S = pre.assignAllCollectionKeys(collectionSet, valueSet)

  override def backward(post: S, oldPre: S): S = post.setCollectionToTop(collectionSet)
}

case class GetOriginalCollectionOp[S <: State[S]](collectionSet: ExpressionSet) extends SemanticOp[S] {

  override def forward(pre: S): S = pre.getOriginalCollection(collectionSet)

  // correct??? what does this do?
  override def backward(post: S, oldPre: S): S = oldPre.getOriginalCollection(collectionSet)
}