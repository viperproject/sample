package ch.ethz.inf.pm.sample.abstractdomain


import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import util.HeapIdSetFunctionalLifting

/**
 * A domain that combines a heap and another semantic domain.
 * The intuition is that the heap domain takes care of approximating the heap structure, while the
 * semantic domain has to manage the information of its interest without taking care of field accesses
 * and object creation, but dealing only with identifiers (of variables or of heap nodes).
 *
 * TODO:
 * Rewrite most methods. Contains lots of (cosmetically cleaned up) legacy code. Especially,
 * remove the timing code as it is a separate concern (use adapter to wrap domains?)
 */
case class HeapAndAnotherDomain[
N <: SemanticDomain[N],
H <: HeapDomain[H, I],
I <: HeapIdentifier[I]](
                         semantic: N,
                         heap: H)
  extends Lattice[HeapAndAnotherDomain[N, H, I]]
  with LatticeWithReplacement[HeapAndAnotherDomain[N, H, I]] {


  type T = HeapAndAnotherDomain[N, H, I]

  override def toString: String = "Heap state:\n" +
    ToStringUtilities.indent(heap.toString) +
    "\nSemantic state:\n" +
    ToStringUtilities.indent(semantic.toString)

  def _1 = semantic

  def _2 = heap

  def getStringOfId(id: Identifier): String = semantic.getStringOfId(id)

  def isBottom = _1.isBottom || _2.isBottom

  def ids = _1.ids ++ _2.ids

  def factory(semantic: N, heap: H): T = HeapAndAnotherDomain[N, H, I](semantic, heap)

  def factory(): T = top()

  def createVariableForArgument(variable: Assignable, typ: Type, path: List[String]) = {
    SystemParameters.heapTimer.start()
    val (newHeap, ids, r) = heap.createVariableForArgument(variable, typ, path)
    SystemParameters.heapTimer.stop()
    var newSemantic = semantic
    newSemantic = applyToAssignable[N](variable, newSemantic, _.createVariableForArgument(_, typ, path)._1)
    variable match {
      case x: VariableIdentifier =>
        newSemantic = newSemantic.createVariableForArgument(x, typ, path)._1
      case x: HeapIdSetDomain[I] =>
        var first: Boolean = true
        for (singleid <- x.value)
          if (first) {
            first = false
            newSemantic = newSemantic.createVariableForArgument(singleid, typ, path)._1
          }
          else
            newSemantic = x.combinator(newSemantic, newSemantic.createVariableForArgument(singleid, typ, path)._1)
    }
    //We recursively create the entry state for all the entry abstract nodes.
    SystemParameters.domainTimer.start()
    newSemantic = newSemantic.merge(r)
    for (id <- ids.keys)
      if (!id.equals(variable))
        newSemantic = newSemantic.createVariableForArgument(id, typ, ids.apply(id))._1
    SystemParameters.domainTimer.stop()
    (factory(newSemantic, newHeap), ids)
  }

  def setToTop(variable: Assignable): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.setToTop(variable)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    var newSemantic = semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.setToTop(_))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap)
  }

  def assign(variable: Assignable, expr: Expression): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.assign(variable, expr, semantic)
    val (newHeap2, r1) = newHeap.endOfAssignment()
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    var newSemantic = semantic.merge(r).merge(r1)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.assign(_, expr))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap2)
  }

  def assignField(variable: Assignable, field: String, expr: Expression, typ: Type, pp: ProgramPoint): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r2) = heap.assignField(variable, field, expr)
    val (id, newHeap2, r1) = newHeap.getFieldIdentifier(variable, field, typ, pp)
    val (newHeap3, r3) = newHeap2.endOfAssignment()
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(r2).merge(r1).merge(r3)
    var newSemanticOpt: Option[N] = None
    if (id.isTop)
      newSemanticOpt = Some(newSemantic.top())
    else
      for (singleheapid <- id.value) {
        if (newSemanticOpt == None)
          newSemanticOpt = Some(newSemantic.assign(singleheapid, expr))
        else newSemanticOpt = Some(id.combinator(newSemanticOpt.get, newSemantic.assign(singleheapid, expr)))
      }
    val newSemanticResult =
      if (newSemanticOpt != None)
        newSemanticOpt.get //throw new SemanticException("You should assign to something")
      else newSemantic
    SystemParameters.domainTimer.stop()
    factory(newSemanticResult, newHeap3)
  }

  def backwardAssignField(oldPreState: T, variable: Assignable, field: String, expr: Expression, typ: Type, pp: ProgramPoint): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r2) = heap.backwardAssignField(oldPreState.heap, variable, field, expr)
    val (id, newHeap2, r1) = newHeap.getFieldIdentifier(variable, field, typ, pp)
    val (newHeap3, r3) = newHeap2.endOfAssignment()
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(r2).merge(r1).merge(r3)
    var newSemanticOpt: Option[N] = None
    if (id.isTop)
      newSemanticOpt = Some(newSemantic.top())
    else
      for (singleheapid <- id.value) {
        if (newSemanticOpt == None)
          newSemanticOpt = Some(newSemantic.backwardAssign(oldPreState.semantic, singleheapid, expr))
        else newSemanticOpt = Some(id.combinator(newSemanticOpt.get, newSemantic.backwardAssign(oldPreState.semantic, singleheapid, expr)))
      }
    val newSemanticResult = newSemanticOpt.getOrElse(newSemantic)
    SystemParameters.domainTimer.stop()
    factory(newSemanticResult, newHeap3)
  }


  private def assignSemanticValue(ids: HeapIdSetDomain[I], value: Expression, initial: N): N = {
    def assignValueTo(initialState: N, value: Expression)(a: Assignable) = {
      applyToAssignable[N](a, initialState, _.assign(_, value))
    }

    if (ids.isTop) {
      initial.top()
    } else {
      HeapIdSetFunctionalLifting.applyToSetHeapId(initial.factory(), ids, assignValueTo(initial, value))
    }
  }

  def setArgument(variable: Assignable, expr: Expression): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.setArgument(variable, expr)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    var newSemantic = semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.setArgument(_, expr))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap)
  }

  def assume(expr: Expression): (T, Replacement) = expr match {
    case BinaryBooleanExpression(_, _, BooleanOperator.&&, _) =>
      val binaryBoolExpr = expr.asInstanceOf[BinaryBooleanExpression]
      val (result, rep1) = this.assume(binaryBoolExpr.left)
      val (result2, rep2) = result.assume(binaryBoolExpr.right)
      (result2, rep1 ++ rep2)
    case _ =>
      SystemParameters.heapTimer.start()
      val (newHeap, r) = heap.assume(expr)
      SystemParameters.heapTimer.stop()
      SystemParameters.domainTimer.start()
      val newSemantic = semantic.merge(r).assume(expr)
      SystemParameters.domainTimer.stop()
      (factory(newSemantic, newHeap), r)
  }

  def areEqual(left: Expression, right: Expression): BooleanDomain = {
    val equalsExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.==, null)

    val (leftEqualsRight, _) = this.assume(equalsExpression)
    val (leftNotEqualsRight, _) = this.assume(NegatedBooleanExpression(equalsExpression))

    if (!leftEqualsRight.lessEqual(this.bottom()) && leftNotEqualsRight.lessEqual(this.bottom())) {
      // must be equal
      return BooleanDomain.domTrue
    } else if (leftEqualsRight.lessEqual(this.bottom())) {
      // must be not equal
      return BooleanDomain.domFalse
    }

    BooleanDomain.domTop
  }

  def createVariable(variable: Assignable, typ: Type): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.createVariable(variable, typ)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = applyToAssignable[N](variable, semantic.merge(r), _.createVariable(_, typ))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap)
  }

  def removeVariable(variable: Assignable): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.removeVariable(variable)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = applyToAssignable[N](variable, semantic.merge(r), _.removeVariable(_))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap)
  }

  def access(field: Assignable): T = {
    //Access does not change the state of the heap domain
    val newSemantic = applyToAssignable[N](field, semantic, _.access(_))
    factory(newSemantic, heap)
  }

  def backwardAccess(field: Assignable): T = {
    SystemParameters.domainTimer.start()
    val newSemantic = applyToAssignable[N](field, semantic, _.backwardAccess(_))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, heap)
  }

  def backwardAssign(oldPreState: T, variable: Assignable, expr: Expression): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.backwardAssign(oldPreState.heap, variable, expr)
    val (newHeap2, _) = oldPreState.heap.glbWithReplacement(newHeap)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    var newSemantic = semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.backwardAssign(oldPreState.semantic, _, expr))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap2)
  }

  override def top(): T = factory(semantic.top(), heap.bottom())

  override def bottom(): T = factory(semantic.bottom(), heap.bottom())

  override def lubWithReplacement(other: T): (T, Replacement) = {
    SystemParameters.heapTimer.start()
    val (newHeap, rep) = heap.lubWithReplacement(other.heap)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(rep).lub(other.semantic.merge(rep))
    SystemParameters.domainTimer.stop()
    (factory(newSemantic, newHeap), rep)
  }

  override def lub(other: T): T = lubWithReplacement(other)._1

  override def glbWithReplacement(other: T): (T, Replacement) = {
    SystemParameters.heapTimer.start()
    val (newHeap, rep) = heap.glbWithReplacement(other.heap)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(rep).glb(other.semantic.merge(rep))
    SystemParameters.domainTimer.stop()
    (factory(newSemantic, newHeap), rep)
  }

  override def glb(other: T): T = glbWithReplacement(other)._1

  override def wideningWithReplacement(other: T): (T, Replacement) = {
    SystemParameters.heapTimer.start()
    val (newHeap, rep) = heap.wideningWithReplacement(other.heap)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(rep).widening(other.semantic.merge(rep))
    SystemParameters.domainTimer.stop()
    (factory(newSemantic, newHeap), rep)
  }

  override def widening(other: T): T = wideningWithReplacement(other)._1

  override def lessEqual(other: T): Boolean = {
    if (semantic.lessEqual(semantic.bottom())) return true
    if (other.semantic.lessEqual(other.semantic.bottom())) return false
    SystemParameters.heapTimer.start()
    var b = heap.lessEqual(other.heap)
    SystemParameters.heapTimer.stop()
    if (!b) return false
    SystemParameters.domainTimer.start()
    b = semantic.lessEqual(other.semantic)
    SystemParameters.domainTimer.stop()
    b
  }

  def explainError(expr: Expression): Set[(String, ProgramPoint)] = _1.explainError(expr) ++ _2.explainError(expr)

  private def applyToAssignable[L <: Lattice[L]](variable: Assignable, state: L, functor: (L, Identifier) => L): L = {
    variable match {
      case x: VariableIdentifier => functor(state, x)
      case x: I => functor(state, x): L
    }
  }


}