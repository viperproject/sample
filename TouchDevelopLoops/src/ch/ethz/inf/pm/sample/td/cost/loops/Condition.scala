package ch.ethz.inf.pm.sample.td.cost.loops

abstract class Condition (val id : String) {

  var child1 : Condition = null
  var child2 : Condition = null
  var parent : Condition = null
  var nextAnd : String = null
  def getNextAnd() : String = if (nextAnd != null) nextAnd else (if (parent == null) null else parent.getNextAnd())

  def setChild1(c : Condition) = {
    child1 = c
    if (c != null) c.parent = this
  }

  def setChild2(c : Condition) = {
    child2 = c
    if (c != null) c.parent = this
  }

  def debugSymbol : String

  def debugToString(offset : Int) : String = {
    var spaces = ""
    for (i <- 0 to offset) spaces = spaces + " "
    var result = spaces + id + " " + debugSymbol + " " + nextAnd + "\n"
    if (child1 != null) result = result + child1.debugToString(offset+2)
    if (child2 != null) result = result + child2.debugToString(offset+2)
    result
  }

}


class BaseCondition (override val id : String, val relation : LinearRelation) extends Condition (id) {
  def debugSymbol : String = relation.toString
}


class OrCondition (override val id : String) extends Condition (id) {
  def debugSymbol : String = "OR"
}


class AndCondition (override val id : String) extends Condition (id) {
  def debugSymbol : String = "AND"
}














