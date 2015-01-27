package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation.Type

/**
 *
 * Implements a domain that wraps another domain
 * This can be used in certain situations to separate concepts.
 *
 * @author Lucas Brutschy
 *
 */
trait LatticeWrapper[X <: Lattice[X], T <: LatticeWrapper[X,T]]
  extends Lattice[T] {
  self:T =>

  def wrapped:X

  def wrapperFactory(wrapped:X):T

  override def isBottom: Boolean = wrapped.isBottom
  override def isTop: Boolean = wrapped.isTop
  override def factory(): T = wrapperFactory(wrapped.factory())
  override def bottom(): T = wrapperFactory(wrapped.bottom())
  override def widening(other: T): T = wrapperFactory(wrapped.widening(other.wrapped))
  override def lessEqual(r: T): Boolean = wrapped.lessEqual(r.wrapped)
  override def top(): T = wrapperFactory(wrapped.top())
  override def lub(other: T): T = wrapperFactory(wrapped.lub(other.wrapped))
  override def glb(other: T): T = wrapperFactory(wrapped.glb(other.wrapped))
  override def toString:String = wrapped.toString
}


/**
 *
 * Implements a domain that wraps another domain, walking the lattice in the inverted direction
 * This can be used in certain situations to separate concepts.
 *
 * @author Lucas Brutschy
 *
 */
trait InvertedLatticeWrapper[X <: Lattice[X], T <: InvertedLatticeWrapper[X,T]]
  extends LatticeWrapper[X,T] {
  self:T =>

  override def isBottom: Boolean = wrapped.isTop
  override def isTop: Boolean = wrapped.isBottom
  override def factory(): T = top()
  override def bottom(): T = wrapperFactory(wrapped.top())
  override def widening(other: T): T = wrapperFactory(wrapped glb other.wrapped) // TODO: What?
  override def lessEqual(r: T): Boolean = r.wrapped lessEqual wrapped
  override def top(): T = wrapperFactory(wrapped.bottom())
  override def lub(other: T): T = wrapperFactory(wrapped glb other.wrapped)
  override def glb(other: T): T = wrapperFactory(wrapped lub other.wrapped)

}

trait SemanticDomainWrapper[X <: SemanticDomain[X], T <: SemanticDomainWrapper[X,T]]
  extends SemanticDomain[T]
  with LatticeWrapper[X,T] {
  self:T =>

  override def merge(f: Replacement): T = wrapperFactory(wrapped.merge(f))
  override def backwardAccess(id: Identifier): T = wrapperFactory(wrapped.backwardAccess(id))
  override def setArgument(variable: Identifier, expr: Expression): T = wrapperFactory(wrapped.setArgument(variable, expr))
  override def setToTop(variable: Identifier): T = wrapperFactory(wrapped.setToTop(variable))
  override def removeVariable(id: Identifier): T = wrapperFactory(wrapped.removeVariable(id))
  override def ids: Set[Identifier] = wrapped.ids
  override def assume(expr: Expression): T = wrapperFactory(wrapped.assume(expr))
  override def createVariable(variable: Identifier, typ: Type): T = wrapperFactory(wrapped.createVariable(variable,typ))
  override def assign(variable: Identifier, expr: Expression): T = wrapperFactory(wrapped.assign(variable,expr))
  override def access(id: Identifier): T = wrapperFactory(wrapped.access(id))
  override def backwardAssign(oldPreState: T, id: Identifier, expr: Expression): T = wrapperFactory(wrapped.backwardAssign(oldPreState.wrapped,id,expr))
  override def getStringOfId(id: Identifier): String = wrapped.getStringOfId(id)

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]): (T, Map[Identifier, List[String]]) = {
    val (arg1,arg2) = wrapped.createVariableForArgument(variable,typ,path)
    (wrapperFactory(arg1),arg2)
  }
}

trait InvertedSetDomain[V,T <: InvertedSetDomain[V,T]] extends InvertedLatticeWrapper[SetDomain.Default[V],T]
  with SetDomain[V,T] {
  this:T =>
  override def factory(value: Set[V]): T = wrapperFactory(wrapped.factory(value))
  override def remove(v: V): T = wrapperFactory(wrapped.remove(v))
  override def remove(v: T): T = wrapperFactory(wrapped.remove(v.wrapped))
  override def add(v: V): T = wrapperFactory(wrapped.add(v))
  override def add(v: T): T = wrapperFactory(wrapped.add(v.wrapped))
}

object InvertedSetDomain {

  case class Default[V](wrapped:SetDomain.Default[V] = SetDomain.Default.Bottom[V]())
    extends InvertedSetDomain[V,Default[V]] {
    override def wrapperFactory(wrapped: SetDomain.Default[V]): Default[V] = Default(wrapped)
  }

}
