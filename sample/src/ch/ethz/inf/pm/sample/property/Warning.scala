package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample.oorepresentation._

abstract sealed class Warning

case class NotValidated(val pp : ProgramPoint, val file : String, val message : String) extends Warning {

  override def equals(o : Any) : Boolean = o match {
    case x: NotValidated => return x.pp.equals(pp) && x.file.equals(file) && x.message.equals(message)
    case _ => false
  }
  
  override def toString() : String = "Warning, file "+file+" "+pp+": "+message 
  
}

case class Validated(val pp : ProgramPoint, val file : String, val message : String) extends Warning {

  override def equals(o : Any) : Boolean = o match {
    case x: Validated => return x.pp.equals(pp) && x.file.equals(file) && x.message.equals(message)
    case _ => false
  }
  
  override def toString() : String = "Correct, file "+file+" "+pp+": "+message 
  
}

class OutputCollector {
  var warnings : Set[Warning] = Set.empty[Warning];
  
  def add(a : Warning) : Unit = warnings=warnings+a;
  
  def statistics() : String = {
    var notvalidated : Double = 0;
    var validated : Double = 0;
    for(warning <- warnings) {
      warning match {
        case x : Validated => validated=validated+1;
        case x : NotValidated => notvalidated=notvalidated+1;
      }
    }
    val total=validated+notvalidated;
    val precision : Double=if(total==0) 100 else (validated/total)*100;
    "Checked: "+total+"\nValidated :"+validated+"\nNot validated: "+notvalidated+"\nPrecision: "+precision+"%";
  }
  
  def output() : String = {
    var result = "";
    for(warning <- warnings)
      result=result+warning.toString()+"\n"
    result
  }
  
}