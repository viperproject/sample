package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample.oorepresentation._

trait Warning
trait Validated

/**
 * This class represents a generic output
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
abstract sealed class Output {
  override def hashCode() : Int = 1;
}

/**
 * This class represents a warning over a program point
 *
 * @param pp The program point
 * @param message The message
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class WarningProgramPoint(val pp : ProgramPoint, val message : String) extends Output with Warning {

  override def equals(o : Any) : Boolean = o match {
    case x: WarningProgramPoint => return x.pp.equals(pp) && x.message.equals(message)
    case _ => false
  }
  
  override def toString() : String = "Warning: "+message+" at line "+pp.getLine()+" column "+pp.getColumn()
}

/**
 * This class represents the validation of a property over a program point
 *
 * @param pp The program point
 * @param message The message
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class ValidatedProgramPoint(val pp : ProgramPoint, val message : String) extends Output with Validated {

  override def equals(o : Any) : Boolean = o match {
    case x: ValidatedProgramPoint => return x.pp.equals(pp) && x.message.equals(message)
    case _ => false
  }

  override def toString() : String = "Validated: "+message+" at line "+pp.getLine()+" column "+pp.getColumn()
}

/**
 * This class represents a warning over a method
 *
 * @param classe The class
 * @param method The name of the method
 * @param message The message
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class WarningMethod(val classe : Type, val method : String, val message : String) extends Output with Warning {

  override def equals(o : Any) : Boolean = o match {
    case x: WarningMethod => return x.classe.equals(classe) && x.method.equals(method) && x.message.equals(message)
    case _ => false
  }

  override def toString() : String = "Warning: "+message+" on method "+method+" of class "+classe.getName()
}

/**
 * This class represents the validation of a property over a method
 *
 * @param classe The class
 * @param method The name of the method
 * @param message The message
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class ValidatedMethod(val classe : Type, val method : String, val message : String) extends Output with Validated {

  override def equals(o : Any) : Boolean = o match {
    case x: ValidatedMethod => return x.classe.equals(classe) && x.method.equals(method) && x.message.equals(message)
    case _ => false
  }

  override def toString() : String = "Validated: "+message+" on method "+method+" of class "+classe.getName()
}

/**
 * This class represents a contract that has been inferred by the analysis
 *
 * @param c The contract
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class InferredContract(val c : Annotation) extends Output

/**
 * This class collects the outputs of the analysis
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
class OutputCollector {
  var outputs : Set[Output] = Set.empty[Output];

  	/**
	   * Add an output
	   *
	   * @param a The output
	   */
  def add(a : Output) : Unit = outputs=outputs+a;

  	/**
	   * Return a string representing all the outputs
	   */
  def output() : String = outputs.mkString("\n")

  	/**
	   * Return the number of validated properties
	   */
  def validated() : Int = {
    var c : Int = 0;
    for(w <- outputs) w match {
      case x : Validated => c = c+1;
      case _ =>
    }
    return c;
  }

  	/**
	   * Return the number of warnings
	   */
  def notvalidated() : Int = {
    var c : Int = 0;
    for(w <- outputs) w match {
      case x : Warning => c = c+1;
      case _ =>
    }
    return c;
  }

  	/**
	   * Return the number of inferred contracts
	   */
  def inferredcontracts() : Int = {
    var c : Int = 0;
    for(w <- outputs) w match {
      case x : InferredContract => c = c+1;
      case _ =>
    }
    return c;
  }
}