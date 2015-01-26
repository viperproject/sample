package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import util.HeapIdSetFunctionalLifting

object ExpressionFactory {

  def createVariable(variable: Variable, ty: Type, pp: ProgramPoint): ExpressionSet = {
    var result = new ExpressionSet(ty)
    result = result.add(new VariableIdentifier(variable.getName, variable.id.scope)(ty, pp))
    result
  }

  def createBinaryExpression(left: ExpressionSet, right: ExpressionSet, op: ArithmeticOperator.Value, ty: Type): ExpressionSet = {
    var result = new ExpressionSet(ty)
    for (expleft <- left.getSetOfExpressions)
      for (expright <- right.getSetOfExpressions)
        result = result.add(new BinaryArithmeticExpression(expleft, expright, op, ty))
    result
  }

  def createReferenceComparisonExpression(left: ExpressionSet, right: ExpressionSet, op: ArithmeticOperator.Value, ty: Type): ExpressionSet = {
    var result = new ExpressionSet(ty)
    for (expleft <- left.getSetOfExpressions)
      for (expright <- right.getSetOfExpressions)
        result = result.add(new ReferenceComparisonExpression(expleft, expright, op, ty))
    result
  }

  def createBooleanBinaryExpression(left: ExpressionSet, right: ExpressionSet, op: BooleanOperator.Value, ty: Type): ExpressionSet = {
    var result = new ExpressionSet(ty)
    for (expleft <- left.getSetOfExpressions)
      for (expright <- right.getSetOfExpressions)
        result = result.add(new BinaryBooleanExpression(expleft, expright, op, ty))
    result
  }

  def createNondeterministicBinaryExpression(left: ExpressionSet, right: ExpressionSet, op: NondeterministicOperator.Value, ty: Type): ExpressionSet = {
    var result = new ExpressionSet(ty)
    for (expleft <- left.getSetOfExpressions)
      for (expright <- right.getSetOfExpressions)
        result = result.add(new BinaryNondeterministicExpression(expleft, expright, op, ty))
    result
  }

  def createUnaryExpression(v: ExpressionSet, op: ArithmeticOperator.Value, ty: Type): ExpressionSet = {
    var result = new ExpressionSet(ty)
    for (expleft <- v.getSetOfExpressions)
      result = result.add(new UnaryArithmeticExpression(expleft, op, ty))
    result
  }

  def createNegatedBooleanExpression(v: ExpressionSet): ExpressionSet = {
    var result = new ExpressionSet(v.getType())
    for (expleft <- v.getSetOfExpressions)
      result = result.add(new NegatedBooleanExpression(expleft))
    result
  }

  def createAbstractOperator(thisExpr: ExpressionSet, parameters: List[ExpressionSet], typeParameters: List[Type], op: AbstractOperatorIdentifiers.Value, ty: Type): ExpressionSet = {
    var result = new ExpressionSet(ty)
    val combinations = combineListValue(parameters)
    for (thisexp <- thisExpr.getSetOfExpressions)
      for (combination <- combinations)
        result = result.add(new AbstractOperator(thisexp, combination, typeParameters, op, ty))
    result
  }

  lazy val unitExpr = ExpressionSet(new UnitExpression(SystemParameters.typ.top(), DummyProgramPoint))

  def createUnitExpression(pp: ProgramPoint): ExpressionSet = {
    ExpressionSet(new UnitExpression(SystemParameters.typ.top(), pp))
  }

  private def combineListValue(list: List[ExpressionSet]): Set[List[Expression]] = list match {
    case Nil => Set(Nil)
    case x :: xs =>
      val previous: Set[List[Expression]] = combineListValue(xs)
      var result: Set[List[Expression]] = Set.empty
      for (expr <- x.getSetOfExpressions)
        for (l <- previous)
          result = result + (expr :: l)
      result
  }

}

case class ExpressionSet(
                          initialTyp: Type,
                          s: SetDomain.Default[Expression] = SetDomain.Default())
  extends CartesianProductDomain[Type, SetDomain.Default[Expression], ExpressionSet] {

  def _1 = initialTyp

  def _2 = s

  override def factory(): ExpressionSet =
    new ExpressionSet(
      if (getType() == null) {
        if (SystemParameters.typ != null) SystemParameters.typ.top(); else null
      } else getType().top(),
      SetDomain.Default()
    )

  def factory(a: Type, b: SetDomain.Default[Expression]) = new ExpressionSet(a, b)

  // TODO: rf: this method is highly suspicious. Why should _1 ever be inconsistent with the computed type?
  def getType(): Type = this._1.glb(this.computeType())

  def getSetOfExpressions = this._2.value

  override def isTop = this._2.isTop

  override def isBottom = {
    // `SetDomain` objects may currently have an empty set, while both
    // `isTop` and `isBottom` are set to `false`, which is inconsistent.
    // Unfortunately, we cannot easily forbid such inconsistent `SetDomain`
    // objects, since there is quite some code in Sample that produces such
    // inconsistent objects, especially for `ExpressionSet`s.

    // Quite some methods in `AbstractState` already make the
    // the assumption that when the set is empty and `isTop` is `false`,
    // then this means that the `SetDomain` object is actually bottom.

    // Thus, make that assumption here as well, until the whole `ExpressionSet`
    // mess is cleaned up properly.
    _2.isBottom || (_2.value.isEmpty && !_2.isTop)
  }

  private def computeType(): Type = {
    if (this._2.isTop) return SystemParameters.typ.top()
    var typ: Type = null
    for (t <- this.getSetOfExpressions)
      typ = if (typ == null) t.typ else typ.lub(t.typ)
    if (typ == null) SystemParameters.typ.top()
    else typ
  }

  def add(exp: Expression): ExpressionSet = {
    val v2 = this._2.add(exp)
    val typ = this._1.glb(exp.typ)
    new ExpressionSet(typ, v2)
  }

  def add(expr: ExpressionSet): ExpressionSet = {
    var set = this._2
    for (exprVal <- expr.getSetOfExpressions) set = set.add(exprVal)
    val typ = this._1.glb(expr.getType())
    new ExpressionSet(typ, set)
  }

  def not(): ExpressionSet = {
    var result = this._2.factory()
    for (key <- getSetOfExpressions)
      result = result.add(new NegatedBooleanExpression(key))
    new ExpressionSet(getType(), result)
  }

  override def toString: String = "Type " + _1.toString + ": " + _2.toString

  def merge(r: Replacement): ExpressionSet = {

    if (r.isEmpty()) return this

    var newSet = this._2.value

    for ((froms, tos) <- r.value; from <- froms) {
      // HACK: replace does not work with replacements that remove variables ({h1, h2} -> {})
      if (!tos.isEmpty)
        newSet = (for (to <- tos) yield {
          newSet.map(_.replace(from, to))
        }).flatten.toSet
    }

    new ExpressionSet(getType(), SetDomain.Default[Expression](newSet))

  }

  def isUnitExprSet: Boolean = this == ExpressionFactory.unitExpr
}

object ExpressionSet {
  /** Creates a singleton `ExpressionSet` from a given expression. */
  def apply(exp: Expression): ExpressionSet =
    apply(Seq(exp))

  def apply(exprs: Seq[Expression]): ExpressionSet = {
    require(!exprs.isEmpty)

    var es = ExpressionSet()
    for (exp <- exprs) {
      es = es.add(exp)
    }
    es
  }

  def flatten(exprSets: Seq[ExpressionSet]): ExpressionSet = {
    require(!exprSets.isEmpty)

    var es = ExpressionSet()
    for (set <- exprSets) {
      es = es.add(set)
    }
    es
  }

  /** Creates an empty `ExpressionSet` whose type is top. */
  def apply(): ExpressionSet =
    new ExpressionSet(SystemParameters.typ.top())

}

case class AbstractState[
N <: SemanticDomain[N],
H <: HeapDomain[H, I],
I <: HeapIdentifier[I]](
                         domain: HeapAndAnotherDomain[N, H, I],
                         expr: ExpressionSet)
  extends CartesianProductDomain[HeapAndAnotherDomain[N, H, I], ExpressionSet, AbstractState[N, H, I]]
  with SimpleState[AbstractState[N, H, I]]
  with SingleLineRepresentation
  with LatticeWithReplacement[AbstractState[N, H, I]] {

  def _1 = domain

  def _2 = expr

  def factory(a: HeapAndAnotherDomain[N, H, I], b: ExpressionSet) = AbstractState(a, b)

  def getStringOfId(id: Identifier): String = domain.getStringOfId(id)

  def getSemanticDomain = domain.semantic

  def getHeapDomain = domain.heap

  def before(pp: ProgramPoint) = this

  def createObject(typ: Type, pp: ProgramPoint): AbstractState[N, H, I] = {

    if (isBottom) return this

    // It discharges on the heap analysis the creation of the object and its fields
    val (createdLocation, newHeap, rep) = domain.heap.createObject(typ, pp)
    var result = domain.factory(domain.semantic.merge(rep), newHeap)

    // Create all variables involved representing the object
    result = HeapIdSetFunctionalLifting.applyToSetHeapId(result, createdLocation, result.createVariable(_, typ))
    var result2 = result
    for (field <- typ.possibleFields) {
      val (ids, state, rep2) = HeapIdSetFunctionalLifting.applyGetFieldId(createdLocation, result2, result2.heap.getFieldIdentifier(_, field.getName, field.typ, field.pp))
      result2 = HeapIdSetFunctionalLifting.applyToSetHeapId(result2, ids, domain.factory(result2.semantic.merge(rep2), state).createVariable(_, field.typ))
    }

    setExpression(new ExpressionSet(typ).add(createdLocation)).setState(result2)

  }

  def removeExpression(): AbstractState[N, H, I] = {
    if (isBottom) return factory(domain, expr.bottom())
    factory(domain, ExpressionFactory.unitExpr)
  }

  def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): AbstractState[N, H, I] =
    factory(domain.createVariable(variable, typ), expr)

  def createVariableForArgument(variable: VariableIdentifier, typ: Type): AbstractState[N, H, I] = {
    val (newDomain, _) = domain.createVariableForArgument(variable, typ, Nil)
    factory(newDomain, expr)
  }

  def assignVariable(left: Expression, right: Expression): AbstractState[N, H, I] = {
    left match {
      case variable: Assignable =>
        factory(domain.assign(variable, right), expr)
      case ids: HeapIdSetDomain[I] =>
        factory(HeapIdSetFunctionalLifting.applyToSetHeapId(domain, ids, domain.assign(_, right)), expr)
      case _ =>
        sys.error("I can assign only variables here")
    }
  }

  def assignField(obj: Expression, field: String, right: Expression): AbstractState[N, H, I] = {
    obj match {
      case variable: Identifier =>
        factory(domain.assignField(variable, field, right, right.typ, variable.pp), expr)
      case heapid: HeapIdSetDomain[I] =>
        factory(HeapIdSetFunctionalLifting.applyToSetHeapId(domain, heapid, domain.assignField(_, field, right, right.typ, heapid.pp)), expr)
      case _ =>
        bottom()
    }
  }

  def backwardAssignField(oldPreState: AbstractState[N, H, I], obj: Expression, field: String, right: Expression): AbstractState[N, H, I] = {
    obj match {
      case variable: Identifier =>
        factory(domain.backwardAssignField(oldPreState.domain, variable, field, right, right.typ, variable.pp), expr).setUnitExpression()
      case heapId: HeapIdSetDomain[I] =>
        val newInner = HeapIdSetFunctionalLifting.applyToSetHeapId(domain, heapId, domain.backwardAssignField(oldPreState.domain, _, field, right, right.typ, heapId.pp))
        factory(newInner, expr)
      case _ =>
        bottom()
    }
  }

  def backwardAssignVariable(oldPreState: AbstractState[N, H, I], lhs: Expression,
                             rhs: Expression): AbstractState[N, H, I] = {
    lhs match {
      case variable: Assignable => factory(domain.backwardAssign(oldPreState.domain, variable, rhs), expr)
      case ids: HeapIdSetDomain[I] =>
        val factoryState = domain
        val newInner = HeapIdSetFunctionalLifting.applyToSetHeapId(factoryState, ids, domain.backwardAssign(oldPreState.domain, _, rhs))
        factory(newInner, expr)
      case _ => bottom()
    }
  }

  override def setArgument(x: ExpressionSet, right: ExpressionSet): AbstractState[N, H, I] = {
    if (isBottom) return this
    if (right.isTop) return top()
    var result: AbstractState[N, H, I] = bottom()
    for (el <- x.getSetOfExpressions) {

      //For each parameter that is set, it computes its semantics and it considers the upper bound
      el match {
        case variable: Assignable =>
          for (assigned <- right.getSetOfExpressions) {
            val done = factory(domain.setArgument(variable, assigned), expr)
            result = result.lub(done)
            result = result.setExpression(ExpressionSet(new UnitExpression(variable.typ.top(), variable.pp)))
          }

        case _ => throw new SymbolicSemanticException("I can assign only variables")
      }
    }
    result
  }

  def removeVariable(varExpr: VariableIdentifier): AbstractState[N, H, I] = {
    factory(domain.removeVariable(varExpr), expr)
  }

  override def removeObject(oldPreState: AbstractState[N, H, I], x: ExpressionSet, fields: Option[Set[Identifier]]): AbstractState[N, H, I] = {
    if (isBottom) return this

    // for all exprs in x, do the following, then take lub
    var result: AbstractState[N, H, I] = bottom()
    for (e <- x.getSetOfExpressions) {
      e match {
        case objectIds: HeapIdSetDomain[I] =>
          // remove field variable identifiers from .domain
          var heapAndOther = domain
          for (field <- fields.getOrElse(Set.empty[Identifier])) {
            val (fieldIdSet, _, _) = HeapIdSetFunctionalLifting.applyGetFieldId(objectIds, heapAndOther, heapAndOther.heap.getFieldIdentifier(_, field.getName, field.typ, field.pp))
            heapAndOther = HeapIdSetFunctionalLifting.applyToSetHeapId(heapAndOther, fieldIdSet, heapAndOther.removeVariable)
          }
          // remove variable for created heap object itself
          heapAndOther = HeapIdSetFunctionalLifting.applyToSetHeapId(heapAndOther, objectIds, heapAndOther.removeVariable)

          // remove heap object
          val newHeap = HeapIdSetFunctionalLifting.applyToSetHeapId(heapAndOther.heap, objectIds, heapAndOther.heap.removeObject(_)._1)
          heapAndOther = domain.factory(heapAndOther.semantic, newHeap)

          val newState = new AbstractState[N, H, I](heapAndOther, ExpressionFactory.unitExpr)
          result = result.lub(newState)
          result = result.setExpression(ExpressionFactory.unitExpr)
      }
    }
    result
  }

  def throws(throwed: ExpressionSet): AbstractState[N, H, I] = throw new NotImplementedError

  def evalConstant(value: String, typ: Type, pp: ProgramPoint): AbstractState[N, H, I] = {
    if (isBottom) return this
    setExpression(ExpressionSet(new Constant(value, typ, pp)))
  }

  def getVariableValue(id: Assignable): AbstractState[N, H, I] = {
    if (isBottom) return this
    val state = factory(domain.access(id), removeExpression().expr)
    factory(state.domain, ExpressionSet(id.asInstanceOf[Expression]))
  }

  def backwardGetVariableValue(id: Assignable): AbstractState[N, H, I] = {
    if (isBottom) return this
    val state = factory(domain.backwardAccess(id), removeExpression().expr)
    factory(state.domain, ExpressionSet(id.asInstanceOf[Expression]))
  }

  def getType(variable: Identifier): Type = {
    //TODO: is this correct???
    variable.typ
  }

  def getFieldValue(obj: Expression, field: String, typ: Type): AbstractState[N, H, I] = {
    val (heapId, newHeap, rep) = obj match {
      case obj: Assignable =>
        domain.heap.getFieldIdentifier(obj, field, typ, obj.pp)
      case obj: HeapIdSetDomain[I] =>
        HeapIdSetFunctionalLifting.applyGetFieldId(obj, domain,
          domain.heap.getFieldIdentifier(_, field, typ, obj.pp))
      // Ignore other expressions like InvalidExpression
      case _ => return bottom()
    }

    val result = domain.factory(domain.semantic.merge(rep), newHeap)
    val accessed = if (heapId.isTop) result.top()
    else HeapIdSetFunctionalLifting.applyToSetHeapId(result, heapId, result.access)
    factory(accessed, new ExpressionSet(typ).add(heapId))
  }

  def backwardGetFieldValue(obj: ExpressionSet, field: String, typ: Type): AbstractState[N, H, I] = {
    if (isBottom) return this
    var result: AbstractState[N, H, I] = bottom()
    for (exprVal <- obj.getSetOfExpressions) {
      if (!exprVal.isInstanceOf[Assignable]) throw new SymbolicSemanticException("Only assignable objects should be here")
      val (heapid, newHeap, rep) = domain.heap.getFieldIdentifier(expr.asInstanceOf[Assignable], field, typ, exprVal.pp)
      val result2 = domain.factory(domain.semantic.merge(rep), newHeap)
      val accessed = HeapIdSetFunctionalLifting.applyToSetHeapId(result2, heapid, result2.backwardAccess)
      val state = factory(accessed, new ExpressionSet(typ).add(heapid))
      result = result.lub(state)
    }
    result
  }

  def setVariableToTop(varExpr: Expression): AbstractState[N, H, I] = {
    varExpr match {
      case variable: Assignable =>
        factory(domain.setToTop(variable), expr)
      case ids: HeapIdSetDomain[I] =>
        val newInner = HeapIdSetFunctionalLifting.applyToSetHeapId(domain, ids, domain.setToTop)
        factory(newInner, expr)
      case _ =>
        sys.error("Something assignable expected here")
    }
  }

  def assume(cond: Expression): AbstractState[N, H, I] = {
    val (dom, replacement) = domain.assume(cond)
    factory(dom, expr.merge(replacement))
  }

  def setExpression(value: ExpressionSet): AbstractState[N, H, I] = {
    if (isBottom) return this
    factory(domain, value)
  }

  def setState(value: HeapAndAnotherDomain[N, H, I]): AbstractState[N, H, I] = {
    if (isBottom) return this
    factory(value, expr)
  }

  override def toSingleLineString(): String = {
    if (isBottom) "⊥"
    else domain.toString + ";\nExpr.: " + expr.toString
  }

  override def toString: String = {
    if (isBottom) "⊥"
    else domain.toString + "\nExpression: " + expr.toString
  }

  override def explainError(expr: Expression): Set[(String, ProgramPoint)] = _1.explainError(expr)

  /**
   * Removes all variables satisfying filter
   */
  def pruneVariables(filter: Identifier => Boolean): AbstractState[N, H, I] = {

    var curState = domain
    for (id <- domain.ids) {
      id match {

        case va: Identifier =>
          if (filter(va)) {
            curState = curState.removeVariable(id)
          }

        case _ => ()

      }
    }

    factory(curState, expr)
  }

  override def undoPruneVariables(unprunedPreState: AbstractState[N, H, I], filter: Identifier => Boolean): AbstractState[N, H, I] = {
    var curState = domain
    for (id <- unprunedPreState.domain.ids) {
      id match {
        case va: Identifier =>
          if (filter(va)) {
            curState = curState.createVariable(id, id.typ)
            curState = curState.setToTop(id)
          }
        case _ => ()

      }
    }

    factory(curState, expr)
  }

  override def undoPruneUnreachableHeap(preState: AbstractState[N, H, I]): AbstractState[N, H, I] = {
    val unreachable = preState.domain.heap.getUnreachableHeap
    val oldInnerState = domain
    val newInnerState =
      unreachable.foldLeft(oldInnerState) {
        case (curInnerState, hId) =>
          curInnerState.createVariable(hId, hId.typ)
            .setToTop(hId)
      }
    factory(newInnerState, expr)
  }


  /**
   * Performs abstract garbage collection
   */
  def pruneUnreachableHeap(): AbstractState[N, H, I] = {

    val unreachable = domain.heap.getUnreachableHeap
    val pruned = pruneVariables({
      case a: I => unreachable.contains(a)
      case _ => false
    })

    pruned

  }

  override def lubWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())
    val (d, rep) = domain.lubWithReplacement(other.domain)
    val s = expr.lub(other.expr)
    val result = factory(d, s.merge(rep))
    (result, rep)
  }

  override def lub(other: AbstractState[N, H, I]): AbstractState[N, H, I] = lubWithReplacement(other)._1

  override def glbWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (isBottom || other.isBottom) return (bottom(), new Replacement())
    val (d, rep) = domain.glbWithReplacement(other.domain)
    val s = expr.glb(other.expr)
    val result = factory(d, s.merge(rep))
    (result, rep)
  }

  override def glb(other: AbstractState[N, H, I]): AbstractState[N, H, I] = glbWithReplacement(other)._1

  override def wideningWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())
    val (d, rep) = domain.wideningWithReplacement(other.domain)
    val s = expr.widening(other.expr)
    val result = factory(d, s.merge(rep))
    (result, rep)
  }

  override def widening(other: AbstractState[N, H, I]): AbstractState[N, H, I] = wideningWithReplacement(other)._1

  def createNonDeterminismSource(wrapperTyp: Type, pp: ProgramPoint, summary: Boolean): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    // Allocate non-determinism identifier in heap domain
    val (nonDetId, newHeap) = domain.heap.createNonDeterminismSource(wrapperTyp, pp, summary)
    val heapIdCreatedState = domain.factory(domain.semantic, newHeap)

    // Create a corresponding variable (in all domains)
    val varCreatedState = heapIdCreatedState.createVariable(nonDetId, wrapperTyp)

    // Ugly assumptions about wrapper type (no access to TouchType here)
    Predef.assert(wrapperTyp.possibleFields.size == 1)
    val valueField = wrapperTyp.possibleFields.head

    // Create variables for inner value field
    val (fieldIds, _, _) = varCreatedState.heap.getFieldIdentifier(nonDetId, valueField.getName, valueField.typ, valueField.pp)
    val fieldCreatedState = HeapIdSetFunctionalLifting.applyToSetHeapId(varCreatedState,
      fieldIds, varCreatedState.createVariable(_, valueField.typ))

    factory(fieldCreatedState, ExpressionSet(nonDetId))
  }

  def nonDeterminismSourceAt(pp: ProgramPoint, typ: Type): AbstractState[N, H, I] = {
    val nonDetId = domain.heap.getNonDeterminismSource(pp, typ)
    val idExpr = ExpressionSet(nonDetId)
    setExpression(idExpr)
  }
}