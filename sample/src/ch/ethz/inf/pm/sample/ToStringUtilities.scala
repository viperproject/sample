package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._

import javax.swing.JComponent

//TODO:Comment or remove this code

trait SingleLineRepresentation {
  def toSingleLineString() : String
}

object ToStringUtilities {

  def indent(x:String):String = x.split("\n").map("  "+_).mkString("\n")

  def listStatementToCommasRepresentationSingleLine[S <: State[S]](list : List[Statement]) : String = list match {
    case x :: (y :: z) => x.toSingleLineString() + "," + listStatementToCommasRepresentationSingleLine(y :: z)
    case x :: Nil => x.toSingleLineString()
    case _ => ""
  }
  
  def listToCommasRepresentation[T](list : List[T]) : String = list match {
    case x :: (y :: z) => x + "," + listToCommasRepresentation[T](y :: z)
    case x :: Nil => x+""
    case _ => ""
  }
  
  def listToNewLineRepresentation[T](list : List[T]) : String = list match {
    case x :: (y :: z) => x + "\n" + listToNewLineRepresentation[T](y :: z)
    case x :: Nil => x+""
    case _ => ""
  }
  
  def listToNewLineRepresentationSingleLine(list : List[SingleLineRepresentation]) : String = list match {
    case x :: (y :: z) => x.toSingleLineString + "\n" + listToNewLineRepresentationSingleLine(y :: z)
    case x :: Nil => x.toSingleLineString+""
    case _ => ""
  }
  
  def listToDotCommaRepresentationSingleLine(list : List[SingleLineRepresentation]) : String = list match {
    case x :: (y :: z) => x.toSingleLineString + ";\n" + listToNewLineRepresentationSingleLine(y :: z)
    case x :: Nil => x.toSingleLineString+";"
    case _ => ""
  }
  
  def listOfListToCommasRepresentation[T](list : List[List[T]]) : String = list match {
    case x :: y => "(" + listToCommasRepresentation[T](x) + ")" + listOfListToCommasRepresentation[T](y)
    case _ => ""
  }
  
  def parametricTypesToString[T](list : List[T]) : String = list match {
    case Nil => ""
    case x => "<"+listToCommasRepresentation[T](list)+">"
  }

  def toStringIfNotNull(obj : AnyRef) : String = obj match {
    case null => ""
    case x => x.toString()
  }
  
  def assignedIfNotNull(obj : AnyRef) : String = obj match {
    case null => ""
    case x: EmptyStatement => ""
    case x => " = "+x.toString()
  }
  
  def optionToString[T](x : Option[T]) : String = x match {
    case None => ""
    case Some(x) => x.toString
  }

  /**
   * Converts a map to a string of form
   * K1 -> V1
   * K2 -> V2
   * ...
   */
  def mapToString[T,K](map : Map[T,K]) : String = {
    var result : String = ""
    for(key <- map.keySet) {
      val k = if (key != null) key.toString else "null"
      val v = if (map.get(key).get != null) map.get(key).get.toString else "null"
      result=result+k+" -> "+v+"\n"
    }
    result
  }
  
  def setToString[T](set : Set[T]) : String = set.mkString(",")

  def setOfListToString[T](set : Set[List[T]]) : String = {
    var result : String = "{";
    for(el <- set)
      result=result+"["+listToCommasRepresentation(el)+"], ";
    result+"}"
  }
  
}
