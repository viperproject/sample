package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._

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
class Replacement(val value : scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]]) extends  {
  def this() {
    this(new scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]]());
  }
  //I'm not sure if this cast is fine
  def lub(l : Replacement, r : Replacement) : Replacement = new Replacement(l.value.++(r.value));

  def glb(l : Replacement, r : Replacement) : Replacement = new Replacement(
    l.value.retain( {
        case (a, b) => r.value.keySet.contains(a) && r.value.apply(a).equals(b);
      }
    )
  )

  def isEmpty() = value.isEmpty;

  def keySet() = value.keySet;

  def apply(k : Set[Identifier]) = value.apply(k);
}

/**
 * A <code>HeapDomain</code> is a domain aimed at tracking information
 * about the heap structure.
 *
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait HeapDomain[T <: HeapDomain[T, I], I <: HeapIdentifier[I]] extends Analysis with LatticeWithReplacement[T] {

  /**
   This method creates an object of a given type
  
   @param typ The type of the object to be created
   @param pp The point of the program that creates the reference
   @return the identifier of the created object and the state of the heap after that
   */ 
  def createObject(typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], T, Replacement);
  
  /**
   This method returns the identifier of the field of an object
  
   @param objectIdentifier the identifier of the object to be accessed
   @param field the name of the field
   @param typ the type of the accessed field
   @param pp the program point that accesses the field
   @return the identifier of accessed field and the state of the heap after that
   */ 
  def getFieldIdentifier(objectIdentifier : Expression, name : String, typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], T, Replacement);

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
  def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier : Expression, index : Expression, state : S, typ : Type) : (HeapIdSetDomain[I], T, Replacement);

  /**
   This method sets to top a given variable

   @param variable the variable to be set to top
   @return the state after this action
   */
  def setToTop(variable : Identifier) : (T, Replacement);

  /**
   This method assigns a given variable to the given expression

   @param variable the variable to be assigned
   @param expr the expression to be assigned
   @param state the state of the semantic domain at that point
   @return the state after this action and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def assign[S <: SemanticDomain[S]](variable : Identifier, expr : Expression, state : S) : (T, Replacement);

  /**
   This method assigns a given field of a given objectto the given expression

   @param obj the object whose field has to be assigned
   @param field the field to be assigned
   @param expr the expression to be assigned
   @return the state after this action and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def assignField(obj : Identifier, field : String, expr : Expression) : (T, Replacement);

  /**
   This method set a paramenter (usually the parameter passed to a method) to the given expression

   @param variable the variable to set
   @param expr the expression to set
   @return the state after this action
   */
  def setParameter(variable : Identifier, expr : Expression) : (T, Replacement);

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
  def createVariable(variable : Identifier, typ : Type) : (T, Replacement);

  /**
   This method creates a variable that is a parameter of the analyzed method

   @param variable the variable to be created
   @param typ its type
   @return the state after this action and a map relating identifiers to the path starting with the parameter
     to access them (this is useful for the heap domain that has to create abstract references to approximate
     the initial heap structure)
   */
  def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) : (T, Map[Identifier, List[String]], Replacement);

  /**
   This method removed a variable

   @param variable the variable to be removed
   @param typ its type
   @return the state after this action
   */
  def removeVariable(variable : Identifier) : (T, Replacement);


  /**
   This method provides the backward semantics of assignment

   @param variable
   @param expr
   @return the state before variable=expr
   */
  def backwardAssign(variable : Identifier, expr : Expression) : (T, Replacement);

  /**
   This method returns all the ids over whom the HeapDomain is defined

   @return all ids contained in the heap
   */
  def getIds() : scala.collection.Set[Identifier]

}

sealed abstract class HeapIdSetDomain[I <: HeapIdentifier[I]](id : I) extends Expression(null) with /*HeapIdentifier[HeapIdSetDomain[I]](id.getType, id.getProgramPoint) with*/ SetDomain[I, HeapIdSetDomain[I]]{

  //override def getField() : Option[String] = if(value.size==1) return value.elements.next.getField() else return None;

  override def equals(x : Any) : Boolean = x match {
	  case x : I => if(value.size==1) return x.equals(value.elements.next); else return false;
	  case _ => return super.equals(x);
  }

  /*override def getType() : Type = {
    var res=typ.bottom();
    for(a <- this.value)
      res=res.lub(res, a.getType());
    return res;
  }

  override def getName() : String = this.toString();
    */

  def convert(add : I) : HeapIdSetDomain[I];
  override def factory() : HeapIdSetDomain[I];
  //Used to now if it's definite - glb - or maybe - lub.
  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S;
  //override def representSingleVariable() : Boolean;
}


final class MaybeHeapIdSetDomain[I <: HeapIdentifier[I]](id : I) extends HeapIdSetDomain[I](id) {

  def convert(add : I) : HeapIdSetDomain[I] = new MaybeHeapIdSetDomain(add).add(add);
  override def getType() : Type = {
    var res=id.getType().bottom();
    for(a <- this.value)
      res=res.lub(res, a.getType());
    return res;
  }

  def factory() : HeapIdSetDomain[I]=new MaybeHeapIdSetDomain[I](id);

  /*def representSingleVariable() : Boolean = {
    if(this.value.size==1)
      return this.value.elements.next.representSingleVariable();
    else return false;
  } */
  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S = s1.lub(s1, s2);
}

final class DefiniteHeapIdSetDomain[I <: HeapIdentifier[I]](id : I) extends HeapIdSetDomain[I](id) {

  def convert(add : I) : HeapIdSetDomain[I] = new DefiniteHeapIdSetDomain(add).add(add);
  override def getType() : Type = {
    var res=id.getType().bottom();
    for(a <- this.value)
      res=res.glb(res, a.getType());
    return res;
  }

  def factory() : HeapIdSetDomain[I]=new DefiniteHeapIdSetDomain[I](id);

 /* def representSingleVariable() : Boolean = {
    for(el <- this.value)
      if(! el.representSingleVariable())
        return false;
    return true;
  }*/
  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S = s1.glb(s1, s2);
}