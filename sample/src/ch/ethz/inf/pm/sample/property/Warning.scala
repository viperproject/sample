package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample.oorepresentation._

trait Warning
trait Validated

abstract sealed class Output {
  override def hashCode() : Int = 1;
}

case class WarningProgramPoint(val pp : ProgramPoint, val message : String) extends Output with Warning {

  override def equals(o : Any) : Boolean = o match {
    case x: WarningProgramPoint => return x.pp.equals(pp) && x.message.equals(message)
    case _ => false
  }
  
  override def toString() : String = "Warning: "+message+" at line "+pp.getLine()+" column "+pp.getColumn()
}

case class ValidatedProgramPoint(val pp : ProgramPoint, val message : String) extends Output with Validated {

  override def equals(o : Any) : Boolean = o match {
    case x: ValidatedProgramPoint => return x.pp.equals(pp) && x.message.equals(message)
    case _ => false
  }

  override def toString() : String = "Validated: "+message+" at line "+pp.getLine()+" column "+pp.getColumn()
}

case class WarningMethod(val classe : Type, val method : String, val message : String) extends Output with Warning {

  override def equals(o : Any) : Boolean = o match {
    case x: WarningMethod => return x.classe.equals(classe) && x.method.equals(method) && x.message.equals(message)
    case _ => false
  }

  override def toString() : String = "Warning: "+message+" on method "+method+" of class "+classe.getName()
}

case class ValidatedMethod(val classe : Type, val method : String, val message : String) extends Output with Validated {

  override def equals(o : Any) : Boolean = o match {
    case x: ValidatedMethod => return x.classe.equals(classe) && x.method.equals(method) && x.message.equals(message)
    case _ => false
  }

  override def toString() : String = "Validated: "+message+" on method "+method+" of class "+classe.getName()
}

case class InferredContract(val c : Annotation) extends Output

class OutputCollector {
  var warnings : Set[Output] = Set.empty[Output];
  
  def add(a : Output) : Unit = warnings=warnings+a;

  def output() : String = {
    var result = "";
    for(warning <- warnings)
      result=result+warning.toString()+"\n"
    result
  }

  def validated() : Int = {
    var c : Int = 0;
    for(w <- warnings) w match {
      case x : Validated => c = c+1;
      case _ =>
    }
    return c;
  }

  def notvalidated() : Int = {
    var c : Int = 0;
    for(w <- warnings) w match {
      case x : Validated => c = c+1;
      case _ =>
    }
    return c;
  }

  def inferredcontracts() : Int = {
    var c : Int = 0;
    for(w <- warnings) w match {
      case x : InferredContract => c = c+1;
      case _ =>
    }
    return c;
  }
}