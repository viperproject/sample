package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.util.UndirectedGraph


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
 * @author Pietro Ferrara
 * @version 0.1
 */
class Replacement(val value : scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]]) {
  def this() {
    this(new scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]]());
  }

  /**
   * Compute lub of replacements. Note that this was developed with the interval domain in mind
   * (may be too specific for that case).
   * @author Raphael Fuchs
   */
  def lub(l : Replacement, r : Replacement) : Replacement = {
    type Entry = (Set[Identifier], Set[Identifier])
    def adjacent(x: Entry, y: Entry): Boolean = x != y && !(x._2 intersect y._2).isEmpty

    val entries = (l.value.toList ++ r.value.toList).distinct
    val g = UndirectedGraph.build(entries, adjacent _)
    val lubEntries =
      for (component <- g.getComponents) yield {
        component reduceLeft {(l:Entry, r:Entry) => (l._1 union r._1, l._2 union r._2) }
      }

    new Replacement(scala.collection.mutable.HashMap(lubEntries: _*))
  }

  def glb(l : Replacement, r : Replacement) : Replacement = new Replacement(
    l.value.retain( {
        case (a, b) => r.value.keySet.contains(a) && r.value.apply(a).equals(b);
      }
    )
  )

  def isEmpty() = value.isEmpty;

  def keySet() = value.keySet;

  def apply(k : Set[Identifier]) = value.apply(k);

  /**
   * Prett-print replacement in the notation described above
   */
  override def toString = {
    val lines = for ((k,v) <- value) yield k.mkString("{",",","}") + "->" + v.mkString("{",",","}")
    lines.mkString(", ")
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
trait HeapDomain[T <: HeapDomain[T, I], I <: HeapIdentifier[I]] extends Analysis with HeapLattice[T] {

  /**
   This method creates an object of a given type
  
   @param typ The type of the object to be created
   @param pp The point of the program that creates the reference
   @return the identifier of the created object and the state of the heap after that
   */
  def createObject(typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], T, Replacement);


  /**
   This method creates an array

   @param length the length of the array to be created
   @param typ its type
   @param pp the program point
   @return the heap id of the array, the state after this action, and the replacement caused by the creation of the array
   */
  def createArray(length : Expression, typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], T, Replacement);

  /**
   This method returns an id of the length of a given array

   @param arrayId the id of the array we want to know the length
   @return the heap id of the length of the array, the state after this action, and a replacement
   */
  def getArrayLength(arrayId : Assignable) : (HeapIdSetDomain[I], T, Replacement);

  /**
   This method returns the identifier of the field of an object
  
   @param objectIdentifier the identifier of the object to be accessed
   @param field the name of the field
   @param typ the type of the accessed field
   @param pp the program point that accesses the field
   @return the identifier of accessed field and the state of the heap after that
   */ 
  def getFieldIdentifier(objectIdentifier : Assignable, name : String, typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], T, Replacement);

  /**
   This method is used to signal that we have ended to assign something. For instance,
   in TVLA we could create some temporary nodes when assigning. This method signals to
   TVLA to drop all these temporary nodes.

   @return the state of the heap after the action and a replacement
   */
   def endOfAssignment() : (T, Replacement);

  /**
   This method returns the identifier of the cell of an array

   @param arrayIdentifier the identifier of the array to be accessed
   @param index the index used to access the array
   @param state the state of the semantic domain at that point. This could be useful to precisely
   analyze inside which bounds of the array the access is
   @param typ the type of the accessed cell
   @return the identifier of accessed cell, the state of the heap after that (since we could create new
   abstract ids when accessing the array in order to be more precise), and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier : Assignable, index : Expression, state : S, typ : Type) : (HeapIdSetDomain[I], T, Replacement);

  /**
   This method sets to top a given variable

   @param variable the variable to be set to top
   @return the state after this action
   */
  def setToTop(variable : Assignable) : (T, Replacement);

  /**
   This method assigns a given variable to the given expression

   @param variable the variable to be assigned
   @param expr the expression to be assigned
   @param state the state of the semantic domain at that point
   @return the state after this action and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def assign[S <: SemanticDomain[S]](variable : Assignable, expr : Expression, state : S) : (T, Replacement);

  /**
   This method assigns a given field of a given objectto the given expression

   @param obj the object whose field has to be assigned
   @param field the field to be assigned
   @param expr the expression to be assigned
   @return the state after this action and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def assignField(obj : Assignable, field : String, expr : Expression) : (T, Replacement);


  /**
   This method assigns a given field of a given objectto the given expression

   @param obj the array whose cell has to be assigned
   @param index the index to be assigned
   @param expr the expression to be assigned
   @param state the state of the semantic domain (useful to refine eventually the splitting of the array)
   @return the state after this action and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def assignArrayCell[S <: SemanticDomain[S]](obj : Assignable, index : Expression, expr : Expression, state : S) : (T, Replacement);

  /**
   This method set a paramenter (usually the parameter passed to a method) to the given expression

   @param variable the variable to set
   @param expr the expression to set
   @return the state after this action
   */
  def setArgument(variable : Assignable, expr : Expression) : (T, Replacement);

  /**
   This method assumes that a given expression holds

   @param expr the expression to be assumed
   @return the state after this action
   */
  def assume(expr : Expression) : (T, Replacement);

  /**
   This method creates a variable

   @param variable the variable to be created
   @param typ its type
   @return the state after this action
   */
  def createVariable(variable : Assignable, typ : Type) : (T, Replacement);

  /**
   This method creates a variable that is a parameter of the analyzed method

   @param variable the variable to be created
   @param typ its type
   @return the state after this action and a map relating identifiers to the path starting with the parameter
     to access them (this is useful for the heap domain that has to create abstract references to approximate
     the initial heap structure)
   */
  def createVariableForArgument(variable : Assignable, typ : Type, path : List[String]) : (T, Map[Identifier, List[String]], Replacement);

  /**
   This method removed a variable

   @param variable the variable to be removed
   @param typ its type
   @return the state after this action
   */
  def removeVariable(variable : Assignable) : (T, Replacement);


  /**
   This method provides the backward semantics of assignment

   @param variable
   @param expr
   @return the state before variable=expr
   */
  def backwardAssign(variable : Assignable, expr : Expression) : (T, Replacement);

  /**
   This method returns all the ids over whom the HeapDomain is defined

   @return all ids contained in the heap
   */
  def getIds() : scala.collection.Set[Identifier]

}

trait Assignable {
  def getProgramPoint() : ProgramPoint;
  def getType() : Type;
}

abstract class HeapIdSetDomain[I <: HeapIdentifier[I]] extends Expression(null) with SetDomain[I, HeapIdSetDomain[I]] with Assignable{

  override def equals(x : Any) : Boolean = x match {
	  case x : I => if(value.size==1) return x.equals(value.elements.next); else return false;
	  case _ => return super.equals(x);
  }
  def convert(add : I) : HeapIdSetDomain[I];
  override def factory() : HeapIdSetDomain[I];

  //Used to now if it's definite - glb - or maybe - lub.
  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S;
}


class MaybeHeapIdSetDomain[I <: HeapIdentifier[I]] extends HeapIdSetDomain[I] {

  def convert(add : I) : HeapIdSetDomain[I] = new MaybeHeapIdSetDomain().add(add);
  override def getType() : Type = {
    var res=SystemParameters.getType().bottom();
    for(a <- this.value)
      res=res.lub(res, a.getType());
    return res;
  }

  def factory() : HeapIdSetDomain[I]=new MaybeHeapIdSetDomain[I]();

  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S = s1.lub(s1, s2);
}

class DefiniteHeapIdSetDomain[I <: HeapIdentifier[I]] extends HeapIdSetDomain[I] {

  def convert(add : I) : HeapIdSetDomain[I] = new DefiniteHeapIdSetDomain().add(add);
  override def getType() : Type = {
    var res=SystemParameters.getType().top();
    for(a <- this.value)
      res=res.glb(res, a.getType());
    return res;
  }

  def factory() : HeapIdSetDomain[I]=new DefiniteHeapIdSetDomain[I]();

  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S = s1.glb(s1, s2);
}