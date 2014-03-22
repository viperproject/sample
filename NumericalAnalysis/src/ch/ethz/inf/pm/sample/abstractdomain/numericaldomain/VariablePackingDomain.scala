package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, Identifier, Replacement}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.util.Predef._

/**
 * Given a classifier, a relational domain and a cheap (usually non-relational) domain, this domain classifies
 * identifiers into groups, and represents singleton groups using the cheap domain and larger groups using one instance
 * for each group.
 *
 * @param packer Maps identifiers to a variable pack (if not a singleton)
 * @param cheap Used to represent singleton packs
 * @param relFactory This is an empty instance of the relational domain which we keep around just for instantiating it
 * @param map Maps from VariablePacks to relational domains
 * @param pureBottom Is the domain bottom?
 * @param pureTop Is the domain top?
 * @tparam C the type of the cheap numerical domain
 * @tparam R the type of the relational numerical domain
 */
case class VariablePackingDomain[C <: NumericalDomain[C], R <: NumericalDomain[R]](
                                                                                    packer: VariablePacker,
                                                                                    cheap: C,
                                                                                    relFactory: R,
                                                                                    map: Map[VariablePack, R],
                                                                                    pureBottom: Boolean = false,
                                                                                    pureTop: Boolean = false
                                                                                    ) extends NumericalDomain[VariablePackingDomain[C, R]] {

  /** Returns all identifiers over whom the `SemanticDomain` is defined. */
  override def ids: Set[Identifier] = {
    cheap.ids ++ (map flatMap {
      x => x._2.ids
    })
  }

  /**
   * This method provides the backward semantics of assignment
   *
   * @param id the assigned id
   * @param expr the right hand side
   * @return the state before variable = expr
   */
  override def backwardAssign(oldPreState: VariablePackingDomain[C, R], id: Identifier, expr: Expression): VariablePackingDomain[C, R] = {
    if (this.pureTop || oldPreState.pureBottom) return oldPreState
    packer.classify(id) match {
      case None => this.copy(cheap = cheap.backwardAssign(oldPreState.cheap, id, expr))
      case Some(pack) => this.copy(map = map + (pack -> getDomain(pack).backwardAssign(oldPreState.getDomain(pack), id, expr)))
    }
  }

  /**
   * This method represents the backward semantics when accessing an identifier
   *
   * @param id the accessed id
   * @return the state before this action
   */
  override def backwardAccess(id: Identifier): VariablePackingDomain[C, R] = this

  /**
   * This method represents the semantics when accessing an identifier
   *
   * @param id the accessed id
   * @return the state after this action
   */
  override def access(id: Identifier): VariablePackingDomain[C, R] = this

  /**
   * This method removed a variable.
   * @param variable the variable to be removed
   * @return the state after this action
   */
  override def removeVariable(variable: Identifier): VariablePackingDomain[C, R] =
    packer.classify(variable) match {
      case None => this.copy(cheap = cheap.removeVariable(variable))
      case Some(pack) => this.copy(map = map + (pack -> getDomain(pack).removeVariable(variable)))
    }

  /**
   * This method creates a variable that is an argument of the analyzed method
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action and a map relating identifiers to the path starting with the argument
   *         to access them (this is useful for the heap domain that has to create abstract references to approximate
   *         the initial heap structure)
   *
   *         NOT IMPLEMENTED
   */
  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]): (VariablePackingDomain[C, R], Map[Identifier, List[String]]) = ???

  /**
   * This method creates a variable.
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
   */
  override def createVariable(variable: Identifier, typ: Type): VariablePackingDomain[C, R] = {
    packer.classify(variable) match {
      case None => this.copy(cheap = cheap.createVariable(variable, typ))
      case Some(pack) => this.copy(map = map + (pack -> getDomain(pack).createVariable(variable, typ)))
    }
  }

  /**
   * This method assumes that a given expression hold
   *
   * @param expr the expression to be assumed
   * @return the state after this action
   */
  override def assume(expr: Expression): VariablePackingDomain[C, R] = {
    var cur = this
    // GOES TO PURE BOTTOM IMMEDIATELY AFTER ASSUME
    for (optPack <- packer.classifyAll(expr.ids)) {
      optPack match {
        case None =>
          val res = cheap.assume(expr)
          if (res.isBottom) return bottom()
          cur = cur.copy(cheap = res)
        case Some(pack) =>
          val res = cur.getDomain(pack).assume(expr)
          if (res.isBottom) return bottom()
          cur = cur.copy(map = cur.map + (pack -> res))
      }
    }
    cur
  }

  /**
   * This method set an argument to the given expression
   *
   * @param variable the argument to set
   * @param expr the expression to set@return the state after this action
   *
   *             NOT IMPLEMENTED.
   */
  override def setArgument(variable: Identifier, expr: Expression): VariablePackingDomain[C, R] = ???

  /**
   * This method assigns a given variable to the given expression
   *
   * @param variable the variable to be assigned
   * @param expr the expression to be assigned
   * @return the state after this action
   */
  override def assign(variable: Identifier, expr: Expression): VariablePackingDomain[C, R] = {
    packer.classify(variable) match {
      case None => this.copy(cheap = cheap.assign(variable, expr))
      case Some(pack) => this.copy(map = map + (pack -> getDomain(pack).assign(variable, expr)))
    }
  }

  /**
   * This method sets to top a given variable
   *
   * @param id the variable to be set to top
   * @return the state after this action
   */
  override def setToTop(id: Identifier): VariablePackingDomain[C, R] = {
    if (pureTop || pureBottom) return this
    packer.classify(id) match {
      case None => this.copy(cheap = cheap.setToTop(id))
      case Some(pack) => this.copy(map = map + (pack -> getDomain(pack).setToTop(id)))
    }
  }

  /**
   * This method returns representing the value of the given identifier
   *
   * @param id the identifier
   * @return the string representing its state
   */
  override def getStringOfId(id: Identifier): String = {
    getDomain(id).getStringOfId(id)
  }

  /**
   * For each set of identifiers in the domain of f, this method merges these identifiers
   * into the given one.
   *
   * @param f The identifiers to merge
   * @return the state after the merge
   */
  override def merge(f: Replacement): VariablePackingDomain[C, R] = {
    if (f.isEmpty() || pureBottom || pureTop) return this
    var cur = this
    for (optPack <- packer.classifyAll(f.ids)) {
      optPack match {
        case None => cur = cur.copy(cheap = cur.cheap.merge(f))
        case Some(x) => cur = cur.copy(map = map + (x -> cur.getDomain(x).merge(f)))
      }
    }
    cur
  }

  /**
   * Returns true iff <code>this</code> is less or equal than <code>r</code>
   *
   * @param other The value to compare
   * @return true iff <code>this</code> is less or equal than <code>r</code>
   */
  override def lessEqual(other: VariablePackingDomain[C, R]): Boolean = {
    if (this.pureBottom || other.pureTop) return true
    if (this.pureTop || other.pureBottom) return false
    if (!cheap.lessEqual(other.cheap)) return false
    val bottom = relFactory.bottom()
    for (key <- this.map.keys ++ other.map.keys) {
      if (!this.map.getOrElse(key, bottom).lessEqual(other.map.getOrElse(key, bottom))) return false
    }
    return true
  }

  /**
   * Computes widening of two elements
   *
   * @param other The new value
   * @return The widening of <code>left</code> and <code>right</code>
   */
  override def widening(other: VariablePackingDomain[C, R]): VariablePackingDomain[C, R] = {
    if (this.pureBottom || other.pureTop) return other
    if (this.pureTop || other.pureBottom) return this
    val lubMap = mergeMaps[VariablePack, R](this.map, other.map, relFactory.bottom(), {
      _ glb _
    })
    VariablePackingDomain(packer, this.cheap.widening(other.cheap), relFactory, lubMap)
  }

  /**
   * Computes the greatest lower bound of two elements
   *
   * @param other The other value
   * @return The greatest upper bound, that is, an element that is less or equal than the two arguments,
   *         and greater or equal than any other lower bound of the two arguments
   */
  override def glb(other: VariablePackingDomain[C, R]): VariablePackingDomain[C, R] = {
    if (this.pureTop || other.pureBottom) return other
    if (this.pureBottom || other.pureTop) return this
    val glbMap = mergeMaps[VariablePack, R](this.map, other.map, relFactory.top(), {
      _ glb _
    })
    VariablePackingDomain(packer, this.cheap.glb(other.cheap), relFactory, glbMap)
  }

  /**
   * Computes the upper bound of two elements
   *
   * @param other The other value
   * @return The least upper bound, that is, an element that is greater or equal than the two arguments
   */
  override def lub(other: VariablePackingDomain[C, R]): VariablePackingDomain[C, R] = {
    if (this.pureBottom || other.pureTop) return other
    if (this.pureTop || other.pureBottom) return this
    val lubMap = mergeMaps[VariablePack, R](this.map, other.map, relFactory.bottom(), {
      _ lub _
    })
    VariablePackingDomain(packer, this.cheap.lub(other.cheap), relFactory, lubMap)
  }

  /**
   * Returns the bottom value of the lattice
   *
   * @return The bottom value, that is, a value x that is less or equal than any other value
   */
  override def bottom(): VariablePackingDomain[C, R] =
    VariablePackingDomain(packer, cheap, relFactory, Map.empty, pureBottom = true)

  /**
   * Returns the top value of the lattice
   *
   * @return The top value, that is, a value x that is greater or equal than any other value
   */
  override def top(): VariablePackingDomain[C, R] =
    VariablePackingDomain(packer, cheap, relFactory, Map.empty, pureTop = true)

  /**
   * Returns a new instance of the lattice
   *
   * @return A new instance of the current object
   */
  override def factory(): VariablePackingDomain[C, R] =
    VariablePackingDomain(packer, cheap.factory(), relFactory, Map.empty)

  /**
   * Returns the domain that stores the given identifier.
   *
   * @param id the identifier that is contained in the domain
   * @return a numerical domain that stores information about the identifier
   */
  def getDomain(id: Identifier): NumericalDomain[_] = {
    packer.classify(id) match {
      case None => cheap
      case Some(pack) => getDomain(pack)
    }
  }

  /**
   * Returns the domain that stores the given identifier.
   *
   * @param pack the variable pack
   * @return a numerical domain that stores information about the identifier
   */
  def getDomain(pack: VariablePack): R = {
    map.getOrElse(pack, relFactory.bottom())
  }

}

/**
 * Used to map identifiers to packs
 */
trait VariablePacker {

  def classify(id: Identifier): Option[VariablePack]

  def classifyAll(ids: Iterable[Identifier]): Set[Option[VariablePack]] = {
    (for (id <- ids) yield classify(id)).toSet
  }

}

/**
 * Used to identify a variable pack.
 */
trait VariablePack

/**
 * Stores a singleton for the dummy Pack
 */
object DummyPack {
  val pack = DummyPack()
}

/**
 * A variable pack with one possible instance
 */
case class DummyPack() extends VariablePack

/**
 * Packs everything in one big pack
 */
case class DummyPacker() extends VariablePacker {
  override def classify(id: Identifier): Option[VariablePack] = Some(DummyPack.pack)
}

/**
 * Nothing is ever represented in the relational domain
 */
case class NeverPacker() extends VariablePacker {
  override def classify(id: Identifier): Option[VariablePack] = None
}