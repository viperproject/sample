/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.util.UndirectedGraph

import scala.collection.mutable

/**
 * A <code>Replacement</code> is a map from sets of identifiers to sets of identifiers.
 * Each relation in the map represents the set of identifiers that should be
 * merged into the given set of ids.
 *
 * For instance, merge([{a, b} -> {c}]) in a state in which a->[0..0] and b->[1..1] will produce
 * a state in which c -> [0..0] lub [1..1] = [0..1] and there are no a and b
 * merge([{a} -> {b, c}]) in a state in which a->[0..0] will produce
 * a state in which b -> [0..0] and c -> [0..0] and there is no a
 *
 * The creator of a replacement can give hints to the implementor about which operations are
 * required to perform the replacements using boolean flags. This way, an efficient handling
 * of simple operations can be implemented. These flags are completely optional.
 *
 * The flags are:
 *
 * isPureRenaming == A set of replacements of form {a}->{b} which can be executed
 * sequentially (i.e. union of all left side disjoint to union of all right sides)
 *
 * isPureExpanding == A set of replacements of form {a}->{a,b} which can be executed sequentially
 *
 * isPureRemoving == A set of replacements of form {a}->{}
 *
 * @author Pietro Ferrara
 * @version 0.1
 */

class Replacement(val value: mutable.Map[Set[Identifier], Set[Identifier]] = new mutable.HashMap[Set[Identifier], Set[Identifier]](),
                  val isPureRenaming: Boolean = false,
                  val isPureExpanding: Boolean = false,
                  val isPureRemoving: Boolean = false) {

  def addedIdentifiers: Set[Identifier] = value.values.flatten.toSet -- value.keys.flatten
  def removedIdentifiers: Set[Identifier] = value.keys.flatten.toSet -- value.values.flatten

  /**
   * Compute lub of replacements. Note that this was developed with the interval domain in mind
   * (may be too specific for that case).
   * @author Raphael Fuchs
   */
  def lub(other: Replacement): Replacement = {
    type Entry = (Set[Identifier], Set[Identifier])
    def adjacent(x: Entry, y: Entry): Boolean = x != y && (x._2 intersect y._2).nonEmpty

    val entries = (value.toList ++ other.value.toList).distinct
    val g = UndirectedGraph.build(entries, adjacent)
    val lubEntries =
      for (component <- g.getComponents) yield {
        component reduceLeft {
          (l: Entry, r: Entry) => (l._1 union r._1, l._2 union r._2)
        }
      }

    new Replacement(scala.collection.mutable.HashMap(lubEntries: _*))
  }

  def glb(other: Replacement): Replacement = new Replacement(
    value.retain({
      case (a, b) => other.value.keySet.contains(a) && other.value.apply(a).equals(b);
    }
    )
  )

  def isEmpty() = value.isEmpty

  def keySet() = value.keySet

  def ids:IdentifierSet = IdentifierSet.Inner((value flatMap {
    x => x._1 ++ x._2
  }).toSet)

  def apply(k: Set[Identifier]) = value.apply(k)

  def ++(other: Replacement) =
    new Replacement(value ++ other.value,
      isPureRenaming = isPureRenaming && other.isPureRenaming,
      isPureExpanding = isPureExpanding && other.isPureExpanding,
      isPureRemoving = isPureRemoving && other.isPureRemoving
    )

  def >>(other: Replacement): Replacement = {
    // This implementation is broken. Currently no one seems to use this method
    // but if someone does the three question marks below are supposed to draw
    // his or her attention to this note.
    ???

    if (this.value.isEmpty) return other
    if (other.value.isEmpty) return this

    val rep = new Replacement

    for ((fromsLeft, tosLeft) <- this.value) {
      val newTosLeft = tosLeft.diff(other.value.keys.flatten.toSet)
      if (newTosLeft.nonEmpty || tosLeft.isEmpty)
        rep.value += (fromsLeft -> newTosLeft)
    }

    def replace(in: Set[Identifier], inverseReplacement: Replacement): (Set[Identifier], Set[Identifier]) = {
      var result = in
      var replacedIds = Set.empty[Identifier]
      for ((fromsLeft, tosLeft) <- inverseReplacement.value) {
        val replaceIds = in.intersect(tosLeft)
        if (replaceIds.nonEmpty) {
          result = result.diff(replaceIds).union(fromsLeft)
        }
        replacedIds = replacedIds ++ replaceIds
      }
      (result, replacedIds)
    }

    for ((fromsRight, tosRight) <- other.value) {
      val (newFromsRight, replacedIds) = replace(fromsRight, this)
      rep.value += (newFromsRight -> tosRight)
      if (replacedIds.nonEmpty) {
        rep.value += (replacedIds -> Set.empty[Identifier])
      }
    }

    rep
  }

  /**
   * Pretty-print replacement in the notation described above
   */
  override def toString = {
    val lines = for ((k, v) <- value) yield k.mkString("{", ",", "}") + "->" + v.mkString("{", ",", "}")
    lines.mkString("\n")
  }
}

/**
 * A <code>HeapDomain</code> is a domain aimed at tracking information
 * about the heap structure.
 *
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
// TODO: type parameters are in the wrong order - not consistent
trait HeapDomain[T <: HeapDomain[T, I], I <: HeapIdentifier[I]]
  extends Analysis
  with Lattice[T]
  with LatticeWithReplacement[T] {
  this: T =>

  /**
   * Gets the Heap Identifiers to which the provided Variable points to
   * in the Heap graph.
   *
   * @param key variable
   * @return  Heap Identifiers to which the variable points to
   */
  def get(key: VariableIdentifier): HeapIdSetDomain[I]

  /**
   * Gets the Heap Identifiers to which the provided Variable points to
   * in the Heap graph.
   *
   * @param key Heap Identifier
   * @return  Heap Identifiers to which the Heap Identifier points to
   */
  def get(key: I): HeapIdSetDomain[I]

  /**
   * This method creates an object of a given type
   *
   * @param typ The type of the object to be created
   * @param pp The point of the program that creates the reference
   * @return the identifier of the created object and the state of the heap after that
   */
  def createObject(typ: Type, pp: ProgramPoint): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method creates an array
   *
   * @param length the length of the array to be created
   * @param typ its type
   * @param pp the program point
   * @param state the state of the semantic domain at that point.
   * @return the heap id of the array, the state after this action, and the replacement caused by the creation of the array
   */
  def createArray[S <: SemanticDomain[S]](length: Expression, typ: Type, pp: ProgramPoint, state: S): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method returns an id of the length of a given array
   *
   * @param arrayId the id of the array we want to know the length
   * @return the heap id of the length of the array, the state after this action, and a replacement
   */
  def getArrayLength(arrayId: Assignable): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method returns the identifier of the field of an object
   *
   * @param objectIdentifier the identifier of the object to be accessed
   * @param name the name of the field
   * @param typ the type of the accessed field
   * @param pp the program point that accesses the field
   * @return the identifier of accessed field and the state of the heap after that
   */
  def getFieldIdentifier(objectIdentifier: Assignable, name: String, typ: Type, pp: ProgramPoint): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method is used to signal that we have ended to assign something. For instance,
   * in TVLA we could create some temporary nodes when assigning. This method signals to
   * TVLA to drop all these temporary nodes.
   * @return the state of the heap after the action and a replacement
   */
  def endOfAssignment(): (T, Replacement)

  /**
   * This method returns the identifier of the cell of an array
   * @param arrayIdentifier the identifier of the array to be accessed
   * @param index the index used to access the array
   * @param state the state of the semantic domain at that point. This could be useful to precisely
   *              analyze inside which bounds of the array the access is
   * @param typ the type of the accessed cell
   * @return the identifier of accessed cell, the state of the heap after that (since we could create new
   *         abstract ids when accessing the array in order to be more precise), and the eventual replacements (e.g.,
   *         if the heap analyzed has summarized or split some cells)
   */
  def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier: Assignable, index: Expression, state: S, typ: Type): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method sets to top a given variable
   *
   * @param variable the variable to be set to top
   * @return the state after this action
   */
  def setToTop(variable: Assignable): (T, Replacement)

  /**
   * This method assigns a given variable to the given expression
   *
   * @param variable the variable to be assigned
   * @param expr the expression to be assigned
   * @param state the state of the semantic domain at that point
   * @return the state after this action and the eventual replacements (e.g.,
   *         if the heap analyzed has summarized or split some cells)
   */
  def assign[S <: SemanticDomain[S]](variable: Assignable, expr: Expression, state: S): (T, Replacement)

  /**
   * This method assigns a given field of a given object to the given expression
   *
   * @param obj the object whose field has to be assigned
   * @param field the field to be assigned
   * @param expr the expression to be assigned
   * @return the state after this action and the eventual replacements (e.g.,
   *         if the heap analyzed has summarized or split some cells)
   */
  def assignField(obj: Assignable, field: String, expr: Expression): (T, Replacement)

  def backwardAssignField(oldPreState: T, obj: Assignable, field: String, expr: Expression): (T, Replacement) = ???


  /**
   * This method assigns a given field of a given object to the given expression
   *
   * @param obj the array whose cell has to be assigned
   * @param index the index to be assigned
   * @param expr the expression to be assigned
   * @param state the state of the semantic domain (useful to refine eventually the splitting of the array)
   * @return the state after this action and the eventual replacements (e.g.,
   *         if the heap analyzed has summarized or split some cells)
   */
  def assignArrayCell[S <: SemanticDomain[S]](obj: Assignable, index: Expression, expr: Expression, state: S): (T, Replacement)

  /**
   * This method set a parameter (usually the parameter passed to a method) to the given expression
   *
   * @param variable the variable to set
   * @param expr the expression to set
   * @return the state after this action
   */
  def setArgument(variable: Assignable, expr: Expression): (T, Replacement)

  /**
   * This method assumes that a given expression holds
   *
   * @param expr the expression to be assumed
   * @return the state after this action
   */
  def assume(expr: Expression): (T, Replacement)

  /**
   * This method creates a variable
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
   */
  def createVariable(variable: Assignable, typ: Type): (T, Replacement)

  /**
   * This method creates a variable that is a parameter of the analyzed method
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action and a map relating identifiers to the path starting with the parameter
   *         to access them (this is useful for the heap domain that has to create abstract references to approximate
   *         the initial heap structure)
   */
  def createVariableForArgument(variable: Assignable, typ: Type, path: List[String]): (T, Map[Identifier, List[String]], Replacement)

  /**
   * This method removed a variable
   * @param variable the variable to be removed
   * @return the state after this action
   */
  def removeVariable(variable: Assignable): (T, Replacement)


  def removeObject(obj: Assignable): (T, Replacement) = ???

  /**
   * This method provides the backward semantics of assignment on the post state (this)
   *
   * @param oldPreState the prestate to be refined
   * @param variable the variable to be assigned
   * @param expr RHS expression of assignment
   * @return the refined prestate before variable=expr
   */
  def backwardAssign(oldPreState: T, variable: Assignable, expr: Expression): (T, Replacement)

  /** Returns all identifiers over whom the `HeapDomain` is defined. */
  def ids: Set[Identifier]

  /**
   * Performs abstract garbage collection
   */
  def getUnreachableHeap: Set[I]

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, summary: Boolean): (I, T)

  def getNonDeterminismSource(pp: ProgramPoint, typ: Type): Identifier

  /**
   * May try to explain an error
   *
   * @param expr An error-expression that should be infeasible but exposes an error
   * @return If a cause of the error is found, it returns an explanation and the program point of the cause
   */
  def explainError(expr: Expression): Set[(String, ProgramPoint)] = Set.empty // NOT SUPPORTED BY HEAPDOMAINS YET

}

trait Assignable {
  def pp: ProgramPoint

  def typ: Type
}

trait HeapIdSetDomain[I <: HeapIdentifier[I]]
  extends SetDomain[I, HeapIdSetDomain[I]]
  with Expression {

  // Used to know if it's definite - glb - or maybe - lub.
  def combinator[S <: Lattice[S]](s1: S, s2: S): S

  def convert(add: I): HeapIdSetDomain[I]

  @Deprecated
  def value:Set[I]

  def heapCombinator[H <: LatticeWithReplacement[H], S <: SemanticDomain[S]](h1: H, h2: H, s1: S, s2: S): (H, Replacement)

  def merge(rep: Replacement): HeapIdSetDomain[I]
}

object HeapIdSetDomain {

  trait Top[I <: HeapIdentifier[I]] extends HeapIdSetDomain[I] with SetDomain.Top[I, HeapIdSetDomain[I]] {

    def ids = IdentifierSet.Bottom

    def typ = SystemParameters.tm.Top

    override def transform(f: (Expression => Expression)): Expression = this

    def pp = DummyProgramPoint // TODO: Not well-defined

    def contains(f: (Expression => Boolean)): Boolean = f(this)

    def value = Set.empty // TODO: Not well-defined

    def merge(rep: Replacement): HeapIdSetDomain[I] = this

  }

  trait Bottom[I <: HeapIdentifier[I]] extends HeapIdSetDomain[I] with SetDomain.Bottom[I, HeapIdSetDomain[I]] {

    def ids = IdentifierSet.Bottom

    def typ = SystemParameters.tm.Bottom

    override def transform(f: (Expression => Expression)): Expression = this

    def pp = DummyProgramPoint // TODO: Not well-defined

    def contains(f: (Expression => Boolean)): Boolean = f(this)

    def value = Set.empty

    def merge(rep: Replacement): HeapIdSetDomain[I] = this

  }

  trait Inner[I <: HeapIdentifier[I]] extends HeapIdSetDomain[I] with SetDomain.Inner[I, HeapIdSetDomain[I], Inner[I]] {

    override def equals(x: Any): Boolean = x match {
      case x: I @unchecked  => if (value.size == 1) x.equals(value.head) else false
      case _ => super.equals(x);
    }

    def merge(rep: Replacement): HeapIdSetDomain[I] = {

      if (this.isBottom || this.isTop || this.value.isEmpty) return this

      var result = this.value
      for ((froms, tos) <- rep.value) {

        val fromsI = froms collect {
          case x: I @unchecked => x
        }
        val tosI = tos collect {
          case x: I @unchecked => x
        }

        if (value.intersect(fromsI).nonEmpty) {
          result = result -- fromsI
          result = result ++ tosI
        }

      }

      factory(result)
    }

    override def transform(f: (Expression => Expression)): Expression = factory(value.map(x => f(x).asInstanceOf[I]))

    def lubWithReplacement[S <: SemanticDomain[S]](other: HeapIdSetDomain[I], state: S): (HeapIdSetDomain[I], Replacement) =
      (super.lub(other), new Replacement)

    def contains(f: (Expression => Boolean)): Boolean = f(this) || value.map(_.contains(f)).foldLeft(false)(_ || _)

    def ids = IdentifierSet.Inner(this.value.toSet[Identifier])

    def typ = value.foldLeft(SystemParameters.tm.Bottom)(_ lub _.typ)
  }

  trait MayBe[I <: HeapIdentifier[I]]
    extends HeapIdSetDomain[I] {

    def bottom() = MayBe.Bottom()
    def top() = MayBe.Top()
    def factory(v:Set[I]) = MayBe.Inner(pp,v)

    def convert(add: I): HeapIdSetDomain[I] = MayBe.Inner(add.pp, Set(add))

    def combinator[S <: Lattice[S]](s1: S, s2: S): S = s1.lub(s2)

    def heapCombinator[H <: LatticeWithReplacement[H], S <: SemanticDomain[S]](h1: H, h2: H, s1: S, s2: S): (H, Replacement) = h1.lubWithReplacement(h2)

  }

  object MayBe {

    case class Top[I <: HeapIdentifier[I]]() extends MayBe[I] with HeapIdSetDomain.Top[I]
    case class Bottom[I <: HeapIdentifier[I]]() extends MayBe[I] with HeapIdSetDomain.Bottom[I]

    case class Inner[I <: HeapIdentifier[I]](pp: ProgramPoint, value: Set[I] = Set.empty[I])
      extends Definite[I] with HeapIdSetDomain.Inner[I] {

    }

  }

  trait Definite[I <: HeapIdentifier[I]] extends HeapIdSetDomain[I] {

    def bottom() = Definite.Bottom()
    def top() = Definite.Top()
    def factory(v:Set[I]) = Definite.Inner(pp,v)

    def convert(add: I): HeapIdSetDomain[I] = Definite.Inner(add.pp, Set(add))

    def combinator[S <: Lattice[S]](s1: S, s2: S): S = s1.glb(s2)

    def heapCombinator[H <: LatticeWithReplacement[H], S <: SemanticDomain[S]](h1: H, h2: H, s1: S, s2: S): (H, Replacement) = h1.lubWithReplacement(h2)
  }

  object Definite {

    case class Top[I <: HeapIdentifier[I]]() extends Definite[I] with HeapIdSetDomain.Top[I]
    case class Bottom[I <: HeapIdentifier[I]]() extends Definite[I] with HeapIdSetDomain.Bottom[I]

    case class Inner[I <: HeapIdentifier[I]](pp: ProgramPoint, value: Set[I] = Set.empty[I])
      extends Definite[I] with HeapIdSetDomain.Inner[I] {

    }

  }


}
