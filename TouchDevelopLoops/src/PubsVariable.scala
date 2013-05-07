package ch.ethz.inf.pm.sample.td.cost.loops


object NameEncoder {
  private var sourceName2TranslatedNames = Map.empty[String, String];
  private var index = 0;


  def reset() = {
    index = 0;
    sourceName2TranslatedNames = Map.empty[String, String];
  }

  def getVariableName (c: String) : String = {
    if(sourceName2TranslatedNames.keySet.contains(c)) return sourceName2TranslatedNames.apply(c);
    else {
      val newName=getFreshVariableName();
      sourceName2TranslatedNames = sourceName2TranslatedNames + ((c, newName))
      //println("Variable "+c+" mapped to "+newName)
      return newName;
    }
  }

  def getSourceName (c: String) : String = {
    sourceName2TranslatedNames.map({_.swap}).apply(c)
  }

  private def getFreshVariableName() = {
    index=index+1;
    indexToString(index-1)
  };

  private def indexToString(index : Int) : String = {
    if (index>=10)
      intToChar(index%10)+indexToString(index/10);
    else intToChar(index)+"";
  }

  private def intToChar(i : Int) : Char = i match {
    case 0 => 'A'
    case 1 => 'B'
    case 2 => 'C'
    case 3 => 'D'
    case 4 => 'E'
    case 5 => 'F'
    case 6 => 'G'
    case 7 => 'H'
    case 8 => 'I'
    case 9 => 'L'
  }
}

/*
    A variable that appears in a control structure. Stores information about the initial value and the update rule of this variable.
 */
class PubsVariable (val sourceName: String, val isParameter: Boolean) {
  val name = NameEncoder.getVariableName(sourceName)

  def getSourceName() = sourceName;

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