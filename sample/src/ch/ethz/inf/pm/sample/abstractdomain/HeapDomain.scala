package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._

/** 
 * A <code>HeapDomain</code> is a domain aimed at tracking information
 * about the heap structure
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait HeapDomain[T <: HeapDomain[T, I], I <: HeapIdentifier[I]] extends SemanticDomain[T] with Analysis[T] {
  
  /**
   This method creates an object of a given type
  
   @param typ The type of the object to be created
   @param pp The point of the program that creates the reference
   @return the identifier of the created object
   */ 
  def createObject(typ : Type, pp : ProgramPoint) : I;
  
  /**
   This method returns the identifier of the field of an object
  
   @param objectIdentifier the identifier of the object to be accessed
   @param field the name of the field
   @param typ the type of the accessed field
   @return the identifier of accessed field
   */ 
  def getFieldIdentifier(objectIdentifier : Expression, name : String, typ : Type) : I;
}

trait AddressedDomain[I <: HeapIdentifier[I]] {
  def getAddresses() : Set[I];
}