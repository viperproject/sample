package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain.{SimplifiedSemanticDomain, Replacement, Expression, Identifier}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.util.Predef._

object PackStorage {

  def make[R <: NumericalDomain[R]](x: R): PackStorage[R] = {
    new PackStorage[R](Map.empty)
  }

}

case class PackStorage[R <: NumericalDomain[R]](map: Map[Identifier, R]) {

  def removeVariable(id: Identifier): PackStorage[R] = {
    map.get(id) match {
      case Some(x) => PackStorage(setForAllIdentifiers(x.removeVariable(id)).map - id)
      case None => this
    }
  }

  def setToTop(variable: Identifier): PackStorage[R] = {
    map.get(variable) match {
      case Some(x) => setForAllIdentifiers(x.setToTop(variable))
      case None => this
    }
  }

  def getStringOfId(id: Identifier): String = {
    map.get(id) match {
      case Some(x) => x.getStringOfId(id)
      case None => ""
    }
  }

  def empty(): PackStorage[R] = PackStorage(map map {
    x => x._1 -> x._2.factory()
  })

  def bottom(): PackStorage[R] = PackStorage(map map {
    x => x._1 -> x._2.bottom()
  })

  def top(): PackStorage[R] = PackStorage(map map {
    x => x._1 -> x._2.top()
  })

  def ids: Set[Identifier] = map.keySet

  def setForAllIdentifiers(state: R): PackStorage[R] = {
    PackStorage(map ++ (for (id <- state.ids) yield {
      id -> state
    }))
  }

}

/**
 * Given a classifier, a relational domain and a cheap (usually non-relational) domain, this domain classifies
 * identifiers into groups, and represents singleton groups using the cheap domain and larger groups using one instance
 * for each group.
 *
 * @param cheap Used to represent singleton packs
 * @param relFactory This is an empty instance of the relational domain which we keep around just for instantiating it
 * @param exp Maps from identifiers to relational domains
 * @tparam C the type of the cheap numerical domain
 * @tparam R the type of the relational numerical domain
 */
case class VariablePackingDomain[C <: NumericalDomain[C], R <: NumericalDomain[R]](
                                                                                    cheap: C,
                                                                                    relFactory: R,
                                                                                    exp: PackStorage[R]
                                                                                    )
  extends NumericalDomain[VariablePackingDomain[C, R]]
  with SimplifiedSemanticDomain[VariablePackingDomain[C, R]] {

  // INVARIANT: The cheap domain covers a superset of the identifiers in the relational domain
  if (SystemParameters.DEBUG) {
    assert(exp.ids.subsetOf(ids))
  }

  /**
   * This is the central function of the domain, lifting a set of identifiers from the cheap domain to the relational
   * domain
   * @param ids The set of identifiers to be lifted
   * @return A fresh relational domain incorporating the existing information
   */
  private def lift(ids: Set[Identifier]): R = {

    // get a list of all the relational states involved in this set of ids
    val existingStates = (for (id <- ids) yield {
      exp.map.get(id)
    }).toSet.flatten

    // lift from non-relational domain if something does not exist yet
    val uncoveredIDs = ids -- (existingStates map {
      _.ids
    }).flatten
    val extendedStates =
      if (uncoveredIDs.nonEmpty) {
        var cur = relFactory.factory().createVariables(uncoveredIDs)
        for (constraint <- cheap.getConstraints(uncoveredIDs)) {
          cur = cur.assume(constraint)
        }
        existingStates + cur
      } else existingStates

    // compute the common domain for the pack
    val joinedStates =
      if (extendedStates.size > 1) {
        extendedStates.foldLeft(relFactory.bottom()) {
          _ lub _
        }
      } else if (extendedStates.size == 1) {
        extendedStates.head
      } else {
        relFactory.bottom()
      }

    joinedStates

  }

  private def liftMergeOperation[X](other: VariablePackingDomain[C, R], mergeOp: (R, R) => R): PackStorage[R] = {

    if (other.exp.map.isEmpty) return this.exp
    if (this.exp.map.isEmpty) return other.exp

    val leftPartitioning = this.exp.map map {
      x => x._1 -> x._2.ids
    }
    val rightPartitioning = other.exp.map map {
      x => x._1 -> x._2.ids
    }
    val mergedPartitioning = mergePartitionings(leftPartitioning, rightPartitioning)

    var newMap = this.exp.empty()
    for (partition <- mergedPartitioning.values) {
      val leftS = this.lift(partition)
      val rightS = other.lift(partition)
      newMap = newMap.setForAllIdentifiers(mergeOp(leftS, rightS))
    }

    newMap

  }

  override def ids: Set[Identifier] =
    cheap.ids

  override def removeVariable(variable: Identifier): VariablePackingDomain[C, R] = {
    if (!ids.contains(variable)) return this
    this.copy(cheap = cheap.removeVariable(variable), exp = exp.removeVariable(variable))
  }

  override def createVariable(variable: Identifier, typ: Type): VariablePackingDomain[C, R] = {
    if (ids.contains(variable)) return this
    this.copy(cheap = cheap.createVariable(variable), exp = exp)
  }

  override def assume(expr: Expression): VariablePackingDomain[C, R] = {
    val assumedState = lift(expr.ids).assume(expr)
    this.copy(cheap = cheap.assume(expr), exp = exp.setForAllIdentifiers(assumedState))
  }

  override def assign(variable: Identifier, expr: Expression): VariablePackingDomain[C, R] = {
    val assignedState = lift(expr.ids + variable).assign(variable, expr)
    this.copy(cheap = cheap.assign(variable, expr), exp = exp.setForAllIdentifiers(assignedState))
  }

  override def setToTop(id: Identifier): VariablePackingDomain[C, R] = {
    this.copy(cheap = cheap.setToTop(id), exp = exp.setToTop(id))
  }

  override def getStringOfId(id: Identifier): String = {
    cheap.getStringOfId(id) + "\n" + exp.getStringOfId(id)
  }

  override def merge(f: Replacement): VariablePackingDomain[C, R] = {
    if (f.isEmpty()) return this
    val mergedState = lift(f.ids.toSet).merge(f)
    this.copy(cheap = cheap.merge(f), exp = exp.setForAllIdentifiers(mergedState))
  }

  override def isTop: Boolean = {
    if (!cheap.isTop) return false
    for (x <- exp.map.values) {
      if (!x.isTop) return false
    }
    return true
  }

  override def isBottom: Boolean = {
    if (cheap.isBottom) return true
    for (x <- exp.map.values) {
      if (x.isBottom) return true
    }
    return false
  }

  override def lessEqual(other: VariablePackingDomain[C, R]): Boolean = {

    if (this.isBottom) return true
    if (other.isBottom) return false
    if (!cheap.lessEqual(other.cheap)) return false

    val leftPartitioning = this.exp.map map {
      x => x._1 -> x._2.ids
    }
    val rightPartitioning = other.exp.map map {
      x => x._1 -> x._2.ids
    }
    val mergedPartitioning = mergePartitionings(leftPartitioning, rightPartitioning)

    for (partition <- mergedPartitioning.values) {
      val leftS = this.lift(partition)
      val rightS = other.lift(partition)
      if (!leftS.lessEqual(rightS)) {
        return false
      }
    }

    return true

  }

  override def widening(other: VariablePackingDomain[C, R]): VariablePackingDomain[C, R] = {
    if (this.isBottom) return other
    if (other.isBottom) return this
    VariablePackingDomain(this.cheap.widening(other.cheap), relFactory, liftMergeOperation(other, {
      _ widening _
    }))
  }

  override def glb(other: VariablePackingDomain[C, R]): VariablePackingDomain[C, R] = {
    if (this.isBottom) return this
    if (other.isBottom) return other
    VariablePackingDomain(this.cheap.glb(other.cheap), relFactory, liftMergeOperation(other, {
      _ glb _
    }))
  }

  override def lub(other: VariablePackingDomain[C, R]): VariablePackingDomain[C, R] = {
    if (this.isBottom) return other
    if (other.isBottom) return this
    VariablePackingDomain(this.cheap.lub(other.cheap), relFactory, liftMergeOperation(other, {
      _ lub _
    }))
  }

  override def bottom(): VariablePackingDomain[C, R] =
    VariablePackingDomain(cheap.bottom(), relFactory, exp.bottom())

  override def top(): VariablePackingDomain[C, R] =
    VariablePackingDomain(cheap.top(), relFactory, exp.top())

  override def factory(): VariablePackingDomain[C, R] =
    VariablePackingDomain(cheap.factory(), relFactory, exp.empty())

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]): (VariablePackingDomain[C, R], Map[Identifier, List[String]]) = ???

  override def setArgument(variable: Identifier, expr: Expression): VariablePackingDomain[C, R] = ???

  override def backwardAccess(id: Identifier): VariablePackingDomain[C, R] = this

  override def access(id: Identifier): VariablePackingDomain[C, R] = this

  override def backwardAssign(oldPreState: VariablePackingDomain[C, R], id: Identifier, expr: Expression): VariablePackingDomain[C, R] = ???

}
