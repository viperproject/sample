trait HeapDomain[T <: HeapDomain[T, I], I <: HeapIdentifier[I]] extends Analysis with LatticeWithReplacement[T] {
  /**
   Creates an object on the heap
   */
  def createObject(typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], T, Replacement);
  /**
   Returns an identifier which represents the field of a given object
   */ 
  def getFieldIdentifier(objectIdentifier : Assignable, name : String, typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], T, Replacement);

  /**
   Called when an assignment was completed.
   */
   def endOfAssignment() : (T, Replacement);
  /**
   Assigns the given expression to the given variable
   */
  def assign[S <: SemanticDomain[S]](variable : Assignable, expr : Expression, state : S) : (T, Replacement);
  /**
   Assigns the given expression to the field of identifier
   */
  def assignField(obj : Assignable, field : String, expr : Expression) : (T, Replacement);
  /**
   Assumes that a given expression holds
   */
  def assume(expr : Expression) : (T, Replacement);
  /**
   Create a variable
   */
  def createVariable(variable : Assignable, typ : Type) : (T, Replacement);
  /**
   Create a variable which is a parameter of analyzed method
   */
  def createVariableForParameter(variable : Assignable, typ : Type, path : List[String]) : (T, Map[Identifier, List[String]], Replacement);
  /**
   Removes a variable
   */
  def removeVariable(variable : Assignable) : (T, Replacement);
  /**
   Computes the least upper bound of two heaps
  */
  def lubWithReplacement(left : T, right : T) : (T, Replacement)
  /**
   Computes the greatest lower bound of two heaps
  */
  def glbWithReplacement(left : T, right : T) : (T, Replacement)
}
