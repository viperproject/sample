/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.util.HeapIdSetFunctionalLifting

object ExpressionFactory {

  def BigOr(expressions: List[Expression])(implicit tm: TypeMap, pp: ProgramPoint): Expression = {
    expressions match {
      case Nil => True
      case List(head) => head
      case he :: tail => he || BigOr(tail)
    }
  }

  def BigAnd(expressions: List[Expression])(implicit tm: TypeMap, pp: ProgramPoint): Expression = {
    expressions match {
      case Nil => True
      case List(head) => head
      case he :: tail => he && BigAnd(tail)
    }
  }

  @inline def True(implicit tm: TypeMap, pp: ProgramPoint): Expression =
    Constant("true", tm.Boolean, pp)

  @inline def Var(name: String, typ: Type)(implicit pp: ProgramPoint): Expression =
    VariableIdentifier(name)(typ, pp)

  @inline def IntVar(name: String)(implicit pp: ProgramPoint, tm: TypeMap): Expression =
    VariableIdentifier(name)(tm.Int, pp)

  @inline def BoolVar(name: String)(implicit pp: ProgramPoint, tm: TypeMap): Expression =
    VariableIdentifier(name)(tm.Boolean, pp)

  @inline def StringVar(name: String)(implicit pp: ProgramPoint, tm: TypeMap): Expression =
    VariableIdentifier(name)(tm.String, pp)

  @inline def BinaryNumNum(a: Expression, b: Expression, op: ArithmeticOperator.Value): Expression = {
    assert(a.typ.isNumericalType && b.typ.isNumericalType)
    BinaryArithmeticExpression(a, b, op, a.typ.lub(b.typ))
  }

  @inline def BinaryStrBool(a: Expression, b: Expression, op: ArithmeticOperator.Value)(implicit tm: TypeMap): Expression = {
    assert(a.typ.isStringType && b.typ.isStringType)
    BinaryArithmeticExpression(a, b, op, tm.Boolean)
  }

  @inline def BinaryNumBool(a: Expression, b: Expression, op: ArithmeticOperator.Value)(implicit tm: TypeMap): Expression = {
    assert(a.typ.isNumericalType && b.typ.isNumericalType)
    BinaryArithmeticExpression(a, b, op, tm.Boolean)
  }

  @inline def BinaryBoolBool(a: Expression, b: Expression, op: BooleanOperator.Value): Expression = {
    assert(a.typ.isBooleanType && b.typ.isBooleanType && a.typ == b.typ)
    BinaryBooleanExpression(a, b, op, a.typ)
  }

  @inline def -(expr: Expression): Expression = {
    UnaryArithmeticExpression(expr, ArithmeticOperator.-, expr.typ)
  }

  @inline implicit def toRichExpression(e: Expression): RichExpression = RichExpression(e)

  @inline implicit def toExpression(e: RichExpression): Expression = e.expr

  @inline implicit def toRichExpression(e: Int)(implicit pp: ProgramPoint, tm: TypeMap): RichExpression =
    RichExpression(Constant(e.toString, tm.Int, pp))

  @inline implicit def toRichExpression(e: String)(implicit pp: ProgramPoint, tm: TypeMap): RichExpression =
    RichExpression(Constant(e.toString, tm.Int, pp))

  @inline implicit def toRichExpression(e: Boolean)(implicit pp: ProgramPoint, tm: TypeMap): RichExpression =
    RichExpression(Constant(e.toString, tm.Int, pp))

  @inline def not(expr: Expression): Expression = {
    NegatedBooleanExpression(expr)
  }

  case class TypeMap(
      Int: Type = DummyIntegerType,
      Float: Type = DummyFloatType,
      String: Type = DummyStringType,
      Boolean: Type = DummyBooleanType
  )

  final case class RichExpression(expr: Expression) {

    @inline def +(other: RichExpression): RichExpression =
      BinaryNumNum(this.expr, other.expr, ArithmeticOperator.+)

    @inline def -(other: RichExpression): RichExpression =
      BinaryNumNum(this.expr, other.expr, ArithmeticOperator.-)

    @inline def *(other: RichExpression): RichExpression =
      BinaryNumNum(this.expr, other.expr, ArithmeticOperator.*)

    @inline def /(other: RichExpression): RichExpression =
      BinaryNumNum(this.expr, other.expr, ArithmeticOperator./)

    @inline def <(other: RichExpression)(implicit tm: TypeMap): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.<)

    @inline def >(other: RichExpression)(implicit tm: TypeMap): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.>)

    @inline def >=(other: RichExpression)(implicit tm: TypeMap): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.>=)

    @inline def <=(other: RichExpression)(implicit tm: TypeMap): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.<=)

    @inline def equal(other: RichExpression)(implicit tm: TypeMap): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.==)

    @inline def unequal(other: RichExpression)(implicit tm: TypeMap): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.!=)

    @inline def &&(other: RichExpression): RichExpression =
      BinaryBoolBool(this.expr, other.expr, BooleanOperator.&&)

    @inline def ||(other: RichExpression): RichExpression =
      BinaryBoolBool(this.expr, other.expr, BooleanOperator.||)

  }


}

object ExpressionSetFactory {

  lazy val unitExpr = ExpressionSet(UnitExpression(SystemParameters.typ.top(), DummyProgramPoint))

  def createVariable(variable: Variable, ty: Type, pp: ProgramPoint): ExpressionSet = {
    var result = new ExpressionSet(ty)
    result = result.add(VariableIdentifier(variable.getName, variable.id.scope)(ty, pp))
    result
  }

  def createBinaryExpression(left: ExpressionSet, right: ExpressionSet, op: ArithmeticOperator.Value, ty: Type): ExpressionSet = {
    if (!left.isTop && !right.isTop && !left.s.isTop && !right.s.isTop) {
      var result = new ExpressionSet(ty)
      for (expleft <- left.toSetOrFail)
        for (expright <- right.toSetOrFail)
          result = result.add(BinaryArithmeticExpression(expleft, expright, op, ty))
      result
    } else left.top()
  }

  def createReferenceComparisonExpression(left: ExpressionSet, right: ExpressionSet, op: ArithmeticOperator.Value, ty: Type): ExpressionSet = {
    if (!left.isTop && !right.isTop && !left.s.isTop && !right.s.isTop) {
      var result = new ExpressionSet(ty)
      for (expleft <- left.toSetOrFail)
        for (expright <- right.toSetOrFail)
          result = result.add(ReferenceComparisonExpression(expleft, expright, op, ty))
      result
    } else left.top()
  }

  def createBooleanBinaryExpression(left: ExpressionSet, right: ExpressionSet, op: BooleanOperator.Value, ty: Type): ExpressionSet = {
    if (!left.isTop && !right.isTop && !left.s.isTop && !right.s.isTop) {
      var result = new ExpressionSet(ty)
      for (expleft <- left.toSetOrFail)
        for (expright <- right.toSetOrFail)
          result = result.add(BinaryBooleanExpression(expleft, expright, op, ty))
      result
    } else left.top()
  }

  def createNondeterministicBinaryExpression(left: ExpressionSet, right: ExpressionSet, op: NondeterministicOperator.Value, ty: Type): ExpressionSet = {
    if (!left.isTop && !right.isTop && !left.s.isTop && !right.s.isTop) {
      var result = new ExpressionSet(ty)
      for (expleft <- left.toSetOrFail)
        for (expright <- right.toSetOrFail)
          result = result.add(BinaryNondeterministicExpression(expleft, expright, op, ty))
      result
    } else left.top()
  }

  /** @author Caterina Urban */
  def createFieldAccessPredicate(ids: ExpressionSet, nums: ExpressionSet, dens: ExpressionSet, ty: Type) : ExpressionSet = {
    if (!ids.isTop && !nums.isTop && !dens.isTop) {
      var result = new ExpressionSet(ty)
      for (id <- ids.toSetOrFail)
        for (num <- nums.toSetOrFail)
          for (den <- dens.toSetOrFail)
            result = result.add(FieldAccessPredicate(id, num, den, ty))
      result
    } else ids.top()
  }

  def createCurrentPermission(ids: ExpressionSet, typ: Type): ExpressionSet = {
    if (!ids.isTop) {
      var result = new ExpressionSet(typ)
      for (id <- ids.toSetOrFail)
        result = result.add(CurrentPermission(id, typ))
      result
    } else ids.top()
  }

  def createUnaryExpression(v: ExpressionSet, op: ArithmeticOperator.Value, ty: Type): ExpressionSet = {
    if (!v.isTop) {
      var result = new ExpressionSet(ty)
      for (expleft <- v.toSetOrFail)
        result = result.add(UnaryArithmeticExpression(expleft, op, ty))
      result
    } else v.top()
  }

  def createNegatedBooleanExpression(v: ExpressionSet): ExpressionSet = {
    if (!v.isTop) {
      var result = new ExpressionSet(v.typ)
      for (expleft <- v.toSetOrFail)
        result = result.add(NegatedBooleanExpression(expleft))
      result
    } else v.top()
  }

  def createAbstractOperator(thisExpr: ExpressionSet, parameters: List[ExpressionSet], typeParameters: List[Type], op: AbstractOperatorIdentifiers.Value, ty: Type): ExpressionSet = {
    if (!thisExpr.isTop && !parameters.exists(_.isTop)) {
      var result = new ExpressionSet(ty)
      val combinations = combineListValue(parameters)
      for (thisexp <- thisExpr.toSetOrFail)
        for (combination <- combinations)
          result = result.add(AbstractOperator(thisexp, combination, typeParameters, op, ty))
      result
    } else thisExpr.top()
  }

  def createUnitExpression(pp: ProgramPoint): ExpressionSet = {
    ExpressionSet(UnitExpression(SystemParameters.typ.top(), pp))
  }

  private def combineListValue(list: List[ExpressionSet]): Set[List[Expression]] = list match {
    case Nil => Set(Nil)
    case x :: xs =>
      val previous: Set[List[Expression]] = combineListValue(xs)
      var result: Set[List[Expression]] = Set.empty
      for (expr <- x.toSetOrFail)
        for (l <- previous)
          result = result + (expr :: l)
      result
  }

}

case class ExpressionSet(
    typ: Type,
                          s: SetDomain.Default[Expression] = SetDomain.Default.Bottom())
  extends CartesianProductDomain[Type, SetDomain.Default[Expression], ExpressionSet] {

  def expressions: SetDomain.Default[Expression] = _2

  override def factory(): ExpressionSet = new ExpressionSet(typ.top(),s.top())

  def factory(a: Type, b: SetDomain.Default[Expression]) = new ExpressionSet(a, b)

  def ids: IdentifierSet = s match {
    case SetDomain.Default.Bottom() => IdentifierSet.Bottom
    case SetDomain.Default.Top() => IdentifierSet.Top
    case SetDomain.Default.Inner(x) => Lattice.bigLub(x.map(_.ids))
  }

  def add(exp: Expression): ExpressionSet = {
    val v2 = this._2.+(exp)
    val typ = this._1.glb(exp.typ)
    new ExpressionSet(typ, v2)
  }

  def add(expr: ExpressionSet): ExpressionSet = {
    var set = this._2
    for (exprVal <- expr.toSetOrFail) set = set.+(exprVal)
    val typ = this._1.glb(expr.typ)
    new ExpressionSet(typ, set)
  }

  def toSetOrFail: Set[Expression] = this._2.toSetOrFail

  def _2: SetDomain.Default[Expression] = s

  def _1: Type = typ

  def not(): ExpressionSet = {
    var result:SetDomain.Default[Expression] = this._2.bottom()
    for (key <- toSetOrFail)
      result = result.+(NegatedBooleanExpression(key))
    new ExpressionSet(typ, result)
  }

  override def toString: String = "Type " + _1.toString + ": " + _2.toString

  def merge(r: Replacement): ExpressionSet = this._2 match {
    case SetDomain.Default.Bottom() => this
    case SetDomain.Default.Top()    => this
    case SetDomain.Default.Inner(v) =>

      if (r.isEmpty()) return this

      var newSet = v

      for ((froms, tos) <- r.value; from <- froms) {
        // HACK: replace does not work with replacements that remove variables ({h1, h2} -> {})
        if (tos.nonEmpty)
          newSet = (for (to <- tos) yield {
            newSet.map(_.replace(from, to))
          }).flatten
      }

      new ExpressionSet(typ, SetDomain.Default.Inner(newSet))
  }

  def isUnitExprSet: Boolean = this == ExpressionSetFactory.unitExpr

  /** Returns a single expression iff this has exactly one expression */
  def getSingle:Option[Expression] = {
    s match {
      case SetDomain.Default.Inner(x) if x.size == 1 =>
        Some(x.head)
      case _ =>
        None
    }
  }

}

object ExpressionSet {
  /** Creates a singleton `ExpressionSet` from a given expression. */
  def apply(exp: Expression): ExpressionSet =
    apply(Seq(exp))

  def apply(exprs: Seq[Expression]): ExpressionSet = {
    require(exprs.nonEmpty)

    var es = ExpressionSet()
    for (exp <- exprs) {
      es = es.add(exp)
    }
    es
  }

  /** Creates an empty `ExpressionSet` whose type is top. */
  def apply(): ExpressionSet =
    new ExpressionSet(SystemParameters.typ.top())

  def flatten(exprSets: Seq[ExpressionSet]): ExpressionSet = {
    require(exprSets.nonEmpty)

    var es = ExpressionSet()
    for (set <- exprSets) {
      es = es.add(set)
    }
    es
  }

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

  def _2: ExpressionSet = expr

  def getStringOfId(id: Identifier): String = domain.getStringOfId(id)

  def getSemanticDomain: N = domain.semantic

  def getHeapDomain: H = domain.heap

  def before(pp: ProgramPoint): AbstractState[N, H, I] = this

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

  def setState(value: HeapAndAnotherDomain[N, H, I]): AbstractState[N, H, I] = {
    if (isBottom) return this
    factory(value, expr)
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

  def refiningAssignField(oldPreState: AbstractState[N, H, I], obj: Expression, field: String, right: Expression): AbstractState[N, H, I] = {
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

  def refiningAssignVariable(oldPreState: AbstractState[N, H, I], lhs: Expression,
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
    for (el <- x.toSetOrFail) {

      //For each parameter that is set, it computes its semantics and it considers the upper bound
      el match {
        case variable: Assignable =>
          for (assigned <- right.toSetOrFail) {
            val done = factory(domain.setArgument(variable, assigned), expr)
            result = result.lub(done)
            result = result.setExpression(ExpressionSet(UnitExpression(variable.typ.top(), variable.pp)))
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
    for (e <- x.toSetOrFail) {
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

          val newState = new AbstractState[N, H, I](heapAndOther, ExpressionSetFactory.unitExpr)
          result = result.lub(newState)
          result = result.setExpression(ExpressionSetFactory.unitExpr)
      }
    }
    result
  }

  override def lub(other: AbstractState[N, H, I]): AbstractState[N, H, I] = lubWithReplacement(other)._1

  override def lubWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())
    val (d, rep) = domain.lubWithReplacement(other.domain)
    val s = expr.lub(other.expr)
    val result = factory(d, s.merge(rep))
    (result, rep)
  }

  def throws(throwed: ExpressionSet): AbstractState[N, H, I] = throw new NotImplementedError

  def evalConstant(value: String, typ: Type, pp: ProgramPoint): AbstractState[N, H, I] = {
    if (isBottom) return this
    setExpression(ExpressionSet(Constant(value, typ, pp)))
  }

  def setExpression(value: ExpressionSet): AbstractState[N, H, I] = {
    if (isBottom) return this
    factory(domain, value)
  }

  def getVariableValue(id: Identifier): AbstractState[N, H, I] = {
    if (isBottom) return this
    val state = factory(domain, removeExpression().expr)
    factory(state.domain, ExpressionSet(id.asInstanceOf[Expression]))
  }

  def removeExpression(): AbstractState[N, H, I] = {
    if (isBottom) return factory(domain, expr.bottom())
    factory(domain, ExpressionSetFactory.unitExpr)
  }

  def factory(a: HeapAndAnotherDomain[N, H, I], b: ExpressionSet) = AbstractState(a, b)

  def refiningGetVariableValue(id: Identifier): AbstractState[N, H, I] = {
    if (isBottom) return this
    val state = factory(domain, removeExpression().expr)
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
    factory(result, new ExpressionSet(typ).add(heapId))
  }

  def refiningGetFieldValue(obj: ExpressionSet, field: String, typ: Type): AbstractState[N, H, I] = {
    if (isBottom) return this
    var result: AbstractState[N, H, I] = bottom()
    for (exprVal <- obj.toSetOrFail) {
      if (!exprVal.isInstanceOf[Assignable]) throw new SymbolicSemanticException("Only assignable objects should be here")
      val (heapid, newHeap, rep) = domain.heap.getFieldIdentifier(expr.asInstanceOf[Assignable], field, typ, exprVal.pp)
      val result2 = domain.factory(domain.semantic.merge(rep), newHeap)
      val state = factory(result2, new ExpressionSet(typ).add(heapid))
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

  override def toSingleLineString(): String = {
    if (isBottom) "⊥"
    else domain.toString + ";\nExpr.: " + expr.toString
  }

  override def toString: String = {
    if (isBottom) "⊥"
    else domain.toString + "\nExpression: " + expr.toString
  }

  override def explainError(expr: Expression): Set[(String, ProgramPoint)] = _1.explainError(expr)

  def _1: HeapAndAnotherDomain[N, H, I] = domain

  /**
   * Removes all variables satisfying filter
   */
  def pruneVariables(filter: VariableIdentifier => Boolean): AbstractState[N, H, I] = {

    var curState = domain
    for (id <- domain.ids.asInstanceOf[IdentifierSet.Inner].value) {
      id match {

        case va: VariableIdentifier =>
          if (filter(va)) {
            curState = curState.removeVariable(id)
          }

        case _ => ()

      }
    }

    factory(curState, expr)
  }

  override def undoPruneVariables(unprunedPreState: AbstractState[N, H, I], filter: VariableIdentifier => Boolean): AbstractState[N, H, I] = ???

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
  def pruneUnreachableHeap(): AbstractState[N, H, I] = ???

  override def glb(other: AbstractState[N, H, I]): AbstractState[N, H, I] = glbWithReplacement(other)._1

  override def glbWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (isBottom || other.isBottom) return (bottom(), new Replacement())
    val (d, rep) = domain.glbWithReplacement(other.domain)
    val s = expr.glb(other.expr)
    val result = factory(d, s.merge(rep))
    (result, rep)
  }

  override def widening(other: AbstractState[N, H, I]): AbstractState[N, H, I] = wideningWithReplacement(other)._1

  override def wideningWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())
    val (d, rep) = domain.wideningWithReplacement(other.domain)
    val s = expr.widening(other.expr)
    val result = factory(d, s.merge(rep))
    (result, rep)
  }

  override def ids: IdentifierSet = domain.ids lub expr.ids

}