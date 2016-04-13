/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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
  override def hashCode() : Int = 1

  def getMessage() : String
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
case class WarningProgramPoint(pp: ProgramPoint, message: String) extends Output with Warning {

  override def equals(o : Any) : Boolean = o match {
    case x: WarningProgramPoint => x.pp.equals(pp) && x.message.equals(message)
    case _ => false
  }
  override def getMessage()=message

  override def toString = "Warning: "+message+" "+pp.description
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
case class ValidatedProgramPoint(pp: ProgramPoint, message: String) extends Output with Validated {

  override def equals(o : Any) : Boolean = o match {
    case x: ValidatedProgramPoint => x.pp.equals(pp) && x.message.equals(message)
    case _ => false
  }
  override def getMessage()=message

  override def toString = "Validated: "+message+" "+pp.description
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
case class WarningMethod(classe: Type, method: String, message: String) extends Output with Warning {

  override def equals(o : Any) : Boolean = o match {
    case x: WarningMethod => x.classe.equals(classe) && x.method.equals(method) && x.message.equals(message)
    case _ => false
  }
  override def getMessage()=message

  override def toString = "Warning: "+message+" on method "+method+" of class "+classe.name
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
case class ValidatedMethod(classe: Type, method: String, message: String) extends Output with Validated {

  override def equals(o : Any) : Boolean = o match {
    case x: ValidatedMethod => x.classe.equals(classe) && x.method.equals(method) && x.message.equals(message)
    case _ => false
  }
  override def getMessage()=message

  override def toString = "Validated: "+message+" on method "+method+" of class "+classe.name
}

/**
 * This class represents a contract that has been inferred by the analysis
 *
 * @param c The contract
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class InferredContract(c: Annotation) extends Output {
  override def getMessage()=c.getMessage()
}

/**
 * This class collects the outputs of the analysis
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
class OutputCollector {
  var outputs : Set[Output] = Set.empty[Output]


  var preconditions = Map.empty[String, Map[String, String]]
  var postconditions = Map.empty[String, Map[String, String]]
  var invariants = Map.empty[String, String]
  var predicates = Map.empty[String, Map[String, String]]

  private def add(map : Map[String, Map[String, String]], classe : String, method : String, contract : String) : Map[String, Map[String, String]] = {
    if(map.keySet.contains(classe)) {
      if(map.apply(classe).keySet.contains(method)) {
        map+((classe, map.apply(classe)+((method, map.apply(classe).apply(method)+" && "+contract))))
      }
      else {
        map+((classe, map.apply(classe)+((method, contract))))
      }
    }
    else {
        map+((classe, Map.empty[String, String]+((method, contract))))
      }
  }

  private def addPrecondition(classe : String, method : String, contract : String) = preconditions=this.add(preconditions, classe, method, contract)

  private def addPostcondition(classe : String, method : String, contract : String) = postconditions=this.add(postconditions, classe, method, contract)

  private def addPredicate(classe : String, method : String, contract : String) = predicates=this.add(predicates, classe, method, contract)

  private def addInvariant(classe : String, contract : String) = {
    if(invariants.keySet.contains(classe))
        invariants=invariants+((classe, invariants.apply(classe)+" && "+contract))
    else
        invariants=invariants+((classe, contract))
  }

  def output() : String = {
    var result : String = ""
    for(classe <- preconditions.keySet.++(postconditions.keySet)++ invariants.keySet ++ predicates.keySet) {
      result = result+"\n\nClass "+classe+"\n"
      invariants.get(classe) match {
        case Some(s) => result=result+"invariant "+s+"\n";
        case None =>
      }
      predicates.get(classe) match {
        case Some(s) =>
          for(predicate <- s.keySet)
            result=result+"predicate "+predicate+" "+s.apply(predicate)+"\n";
        case None =>
      }
      preconditions.get(classe) match {
        case Some(s) =>
          for(method <- s.keySet)
            result=result+"method "+method+" requires "+s.apply(method)+"\n";
        case None =>
      }
      postconditions.get(classe) match {
        case Some(s) =>
          for(method <- s.keySet)
            result=result+"method "+method+" ensures "+s.apply(method)+"\n";
        case None =>
      }
    }
    if (!result.isEmpty) result+"\n"+outputs.mkString("\n")
    else outputs.mkString("\n")
  }

  	/**
	   * Add an output
	   *
	   * @param a The output
	   */
  def add(a : Output) : Unit = a match {
    case InferredContract(c) =>
      c match {
        case Invariant(classe, e) => this.addInvariant(classe, e); outputs=outputs+a;
        case Predicate(classe, name, e) => this.addPredicate(classe, name, e); outputs=outputs+a;
        case PreCondition(classe, method, e) => this.addPrecondition(classe, method, e); outputs=outputs+a;
        case PostCondition(classe, method, e) => this.addPostcondition(classe, method, e); outputs=outputs+a;
        case _ => outputs=outputs+a;
      }
    case _ => outputs=outputs+a;
  }

  	/**
	   * Return the number of validated properties
	   */
  def validated() : Int = {
    var c : Int = 0
      for(w <- outputs) w match {
      case x : Validated => c = c+1;
      case _ =>
    }
    c
    }

  	/**
	   * Return the number of warnings
	   */
  def notvalidated() : Int = {
    var c : Int = 0
      for(w <- outputs) w match {
      case x : Warning => c = c+1;
      case _ =>
    }
    c
    }

  	/**
	   * Return the number of inferred contracts
	   */
  def inferredcontracts() : Int = {
    var c : Int = 0
      for(w <- outputs) w match {
      case x : InferredContract => c = c+1;
      case _ =>
    }
    c
    }
}