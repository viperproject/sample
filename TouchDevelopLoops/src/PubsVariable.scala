package ch.ethz.inf.pm.sample.td.cost.loops

/*
    A variable that appears in a control structure. Stores information about the initial value and the update rule of this variable.
 */
class PubsVariable (val name: String, val isParameter: Boolean) {

  // null if this is an argument (or return value), or if we can't find a initial value
  var initialValue : LinearExpression = null

  // for debugging only
  def debugToString : String = {
    " PubsVariable name: " + name + ", isParameter: " + isParameter + ", isArgument: " + isArgument + ", initialValue: " + initialValue + " , kU: " + knownUpdate + " , update: " + update + " , updateIncreasing: " + updateIncreasing + " , updateDecreasing: " + updateDecreasing
  }

  def knownUpdate = update != null // do we know how the value of this variable is updated in the loop?
  var update : LinearExpression = null

  var updateIncreasing = false
  var updateDecreasing = false
  def updateConstant : Boolean = if (update != null) update.isConstUpdate(name) else false

  def isArgument = LoopCostHelper.isArgument(name)
  def isConstant = isArgument || updateConstant
  var isPrime = false

  // name of the variable as we can use it for PUBS
  override def toString : String = {
    var result = ""
    if (isParameter) result = "I_" + name + (if (isPrime) "_prime" else "")
    else if (name.toUpperCase.charAt(0) == name.charAt(0))  result = "U_" + name + (if (isPrime) "_prime" else "")
    else  result = "L_" + name + (if (isPrime) "_prime" else "")
    result.replace("(","_OB").replace(")","_CB").replace("}","_CCB").replace("{","_OCB").replace(",","_CO").replace(".","_D")
  }

  override def hashCode : Int = name.hashCode

  override def equals(other: Any) = {
    other match {
      case pv : PubsVariable => pv.name == name && pv.isParameter == isParameter
      case _ => false
    }
  }

}