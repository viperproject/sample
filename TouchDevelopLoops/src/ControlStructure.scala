package ch.ethz.inf.pm.sample.td.cost.loops

import ch.ethz.inf.pm.sample.abstractdomain._
import collection.mutable
import ch.ethz.inf.pm.sample.oorepresentation.Variable

/*
    Data structure used by LoopCostInternal to hold the retrieved data about a loop or conditional.
 */
abstract class ControlStructure[S <: State[S]] (val parent: LoopCostInternal[S], val nodeId: Int) {

  /*--------------------------------------------
    debug
  ---------------------------------------------*/

  def variablesDebug : String = {
    var result = "Loop " + nodeId + " variables overview: \n"
    for (p <- parameters) result = result + p.debugToString + "\n"
    for (v <- variables) result = result + v.debugToString + "\n"
    result + "\n"
  }

  /*--------------------------------------------
    data
  ---------------------------------------------*/

  def node = parent.cfg.nodes(nodeId)

  var canHandle = true
  def canHandleAll : Boolean = canHandle && innerCs.forall((cs: ControlStructure[S]) => cs.canHandleAll)

  var condition : Condition = null

  /*--------------------------------------------
    inner control structures
  ---------------------------------------------*/

  var innerCsIds : List[Int] = List.empty // loops: inner control structures directly contained in this loop; conditionals: inner control structures directly contained in the true branch of this conditional
  var innerCsIdsFalse : List[Int] = List.empty // loops: not used; conditionals: inner control structures directly contained in the false branch of this conditional

  def addInnerCs(index: Int, branch: Boolean) = {
    if (branch == true) innerCsIds = index :: innerCsIds
    else innerCsIdsFalse = index :: innerCsIdsFalse
  }

  def innerAsString = {
    var result = "inner cs: "
    for (i <- innerCsIds) result = result + " " + i.toString
    result
  }

  def innerFalseAsString = {
    var result = "inner cs false: "
    for (i <- innerCsIdsFalse) result = result + " " + i.toString
    result
  }

  def innerCs = {
    var result : List[ControlStructure[S]] = List.empty
    for (i <- innerCsIds) result ::= parent.controlStructures(i)
    for (i <- innerCsIdsFalse) result ::= parent.controlStructures(i)
    result
  }

  /*--------------------------------------------
    input for PUBS
  ---------------------------------------------*/

  def parameters : scala.collection.Set[PubsVariable] // parameters of the cost model (c1, c2, ...)

  def variables : collection.Set[PubsVariable] = {
    var result : collection.Set[PubsVariable] = variablesNoInner
    for (cs <- innerCs) result = (result.union(cs.variables))
    result.toSet[PubsVariable]
  }

  def variablesNoInner : scala.collection.Set[PubsVariable] // same as variables, but doesn't contain variables that only appear in inner control structures

  def getVariable (name: String): PubsVariable = {
    var result : PubsVariable = null
    for (v <- variables) {
      if (v.name == name) result = v
      else if (v.name+"_prime" == name) result = v
    }
    result
  }

  def pubsName : String = {
    var temp = ""
    for (p <- parameters) temp = temp + p + ","
    for (v <- variables if v.initialValue == null && !v.isParameter) temp = temp + v + ","
    if (temp.size > 0) "c" + nodeId + "(" + temp.substring(0, temp.size-1) + ")"
    else "c" + nodeId
  }

  def pubsVariables : String = {
    var result = ""
    for (p <- parameters) result = result + p + ","
    for (v <- variables if !v.isParameter) result = result + v + ","
    if (result.size > 0) result = result.substring(0, result.size-1)
    result
  }

  def pubsVariablesInitial : String = {
    var result = ""
    for (p <- parameters) result = result + p + ","
    for (v <- variables if !v.isParameter) {
      if (v.initialValue == null) result = result + v + ","
      else result = result + v.initialValue + ","
    }
    if (result.size > 0) result = result.substring(0, result.size-1)
    result
  }

  def pubsVariablesRecursive : String = {
    var result = ""
    for (p <- parameters) result = result + p + ","
    for (v <- variables if !v.isParameter) {
      if (v.isArgument) result = result + v + ","
      else result = result + v + "_prime,"
    }
    if (result.size > 0) result = result.substring(0, result.size-1)
    result
  }

  /*
      The cost relation system for this control structure, which we then pass to PUBS.
      See section 7.6 in the thesis.
   */
  def pubsCes : String

  /*--------------------------------------------
    output from PUBS
  ---------------------------------------------*/

  // the string that we get from PUBS
  var answer : String = null

  def answerToString : String = if (answer == null || answer.contains("failed")) "no upper bound found" else answerTranslated

  // the cost expression that we get from parsing 'answer'
  def costExpression : CostExpr = {
    if (answer == null || answer.contains("failed")) null
    else {
      try {
      val c = CostExpressionParser(answer)
      translateCostExpr(c)
      c
      }
      catch {
        case e => e.printStackTrace()
        null;
      }
    }
  }

  private def translateCostExpr (c: CostExpr) : Unit = {
    c match {
      case n : NatExpr => translateLinearExpr(n.expr)
      case p : PowExpr => translateCostExpr(p.expr)
      case l : LogExpr => translateCostExpr(l.expr)
      case cc : CompositeCostExpr => {
        translateCostExpr(cc.left)
        translateCostExpr(cc.right)
      }
      case u : UnparsedCostExpr => u.text = answerTranslated
      case _ =>    // nothing to do for RationalNumber
    }
  }

  private def translateLinearExpr (l: LinearExpr) : Unit = {
    l match {
      case v : VariableExpr => v.name = translatedVariableName(v.name.charAt(0))
      case cl : CompositeLinearExpr => {
        translateLinearExpr(cl.left)
        translateLinearExpr(cl.right)
      }
      case _ =>    // nothing to do for RationalNumber
    }
  }

  private def answerTranslated : String = {
    var result = answer.toString
    // supports all 26 capital letters
    for (c <- 'A' to 'Z' ) result = result.replace(c.toString, translatedVariableName(c))
    result
  }

  private def translatedVariableName (c: Char) : String = {
    var index = c.toInt - 'A'
      var result = ""
      var i = 0
      for (p <- parameters) {
        if (i == index) {
          result = NameEncoder.getSourceName(p.name)
          // replace node id by program point
          val id = result.substring(1, result.size).toInt
          val pp = parent.cfg.nodes(id).head.getPC()
          result = result.substring(0,1) + "(" + pp.toString + ")"
        }
        i += 1
      }
      for (v <- variables if v.initialValue == null && v.isParameter == false) {
        if (i == index) result = NameEncoder.getSourceName(v.name)
        i += 1
      }
    if (result.isEmpty) result = c.toString
    result
  }

}


class Conditional[S <: State[S]] (override val parent: LoopCostInternal[S], override val nodeId: Int) extends ControlStructure(parent, nodeId) {

  /*--------------------------------------------
    data
  ---------------------------------------------*/

  var elseCondition : Condition = null

  /*--------------------------------------------
    inner control structures
  ---------------------------------------------*/

  def innerCsTrue = {
    var result : List[ControlStructure[S]] = List.empty
    for (i <- innerCsIds) result ::= parent.controlStructures(i)
    result
  }

  def innerCsFalse = {
    var result : List[ControlStructure[S]] = List.empty
    for (i <- innerCsIdsFalse) result ::= parent.controlStructures(i)
    result
  }

  /*--------------------------------------------
    input for PUBS
  ---------------------------------------------*/

  def variablesNoInner : collection.Set[PubsVariable] = {
    var result : collection.Set[PubsVariable] = collection.Set.empty[PubsVariable]
    val queue : mutable.Queue[Condition] = new mutable.Queue[Condition]
    queue.enqueue(condition)
    while (!queue.isEmpty) {
      val cond = queue.dequeue()
      cond match {
        case b : BaseCondition => {
          result = result.union(b.relation.variables)
        }
        case _ =>
      }
      if (cond != null && cond.child1 != null) queue.enqueue(cond.child1)
      if (cond != null && cond.child2 != null) queue.enqueue(cond.child2)
    }

    for (cs <- innerCs) result = (result.union(cs.variables))
    result.toSet[PubsVariable]
  }

  def parameters : scala.collection.Set[PubsVariable] = {
      var result = Set.empty[PubsVariable] + new PubsVariable("t" + nodeId.toString, true)
      result = result + new PubsVariable("f" + nodeId.toString, true)
      for (cs <- innerCs) result = result.union(cs.parameters)
      result
  }

  /*
     The cost relation system for this conditional, which we then pass to PUBS.
     See section 7.6 in the thesis.
  */
  def pubsCes : String = {

      val prefix = "c" + nodeId.toString
      val pv = pubsVariables

      var result = "eq(" + pubsName + ",0,[c" + nodeId.toString() + "y1(" + pubsVariablesInitial+"),c"+ nodeId.toString() + "n1(" + pubsVariablesInitial+")],[]).\n"
      for (branch <- List(true, false)) {
        val queue : mutable.Queue[Condition] = new mutable.Queue[Condition]
        val ic = if (branch) pubsInnerCallsTrue else pubsInnerCallsFalse
        val cost = "nat(" + parent.costModel.conditionalCostExpression(parent.cfg, nodeId, branch) + ")"
        if (branch) queue.enqueue(condition) else queue.enqueue(elseCondition)

        while (!queue.isEmpty) {
          val cond = queue.dequeue()
          cond match {
            case b : BaseCondition => {
              if (b.getNextAnd == null) {
                result = result + "eq(" + prefix + b.id + "(" + pv + ")," + cost + ",[" + ic + "],[" + b.relation +"]).\n"
              } else {
                result = result + "eq(" + prefix + b.id + "(" + pv + "),0,[" + prefix + b.getNextAnd + "(" + pv + ")],[" + b.relation + "]).\n"
              }
            }
            case a : AndCondition => {
              result = result + "eq(" + prefix + a.id + "(" + pv + "),0,[" + prefix + a.id + "1(" + pv + ")],[]).\n"
            }
            case o : OrCondition => {
              result = result + "eq(" + prefix + o.id + "(" + pv + "),0,[" + prefix + o.id + "1(" + pv + ")],[]).\n"
              result = result + "eq(" + prefix + o.id + "(" + pv + "),0,[" + prefix + o.id + "2(" + pv + ")],[]).\n"
            }
            case _ =>
          }
          if (cond != null && cond.child1 != null) queue.enqueue(cond.child1)
          if (cond != null && cond.child2 != null) queue.enqueue(cond.child2)
        }
      }

      // add the cost equations for the inner control structures
      for (cs <- innerCs) result = result + cs.pubsCes
      result
  }

  def pubsInnerCallsTrue : String = {
    var result = ""
    var first = true
    for (cs <- if (true) innerCsTrue else innerCsFalse) {
      if (first) {
        result = cs.pubsName
        first = false
      } else {
        result = result + ", " + cs.pubsName
      }
    }
    result
  }

  def pubsInnerCallsFalse : String = {
    var result = ""
    var first = true
    for (cs <- innerCsFalse) {
      if (first) {
        result = cs.pubsName
        first = false
      } else {
        result = result + ", " + cs.pubsName
      }
    }
    result
  }

}


class Loop[S <: State[S]] (override val parent: LoopCostInternal[S], override val nodeId: Int, val lastNode: Int, val prevNode: Int) extends ControlStructure(parent, nodeId) {

  // list of additional constraints retrieved by findConstraints (currently not used)
  var constraints : List[LinearRelation] = List.empty[LinearRelation]

  def constraintsDebug : String = {
    var result = "Loop " + nodeId + " constraints overview: \n"
    for (c <- constraints) result = result + c + "\n"
    result + "\n"
  }

  /*--------------------------------------------
    input for PUBS
  ---------------------------------------------*/

  def variablesNoInner : collection.Set[PubsVariable] = {
    var result : collection.Set[PubsVariable] = collection.Set.empty[PubsVariable]
    val queue : mutable.Queue[Condition] = new mutable.Queue[Condition]
    if (condition != null) queue.enqueue(condition)
    while (!queue.isEmpty) {
      val cond = queue.dequeue()
      cond match {
        case b : BaseCondition => {
           result = result.union(b.relation.variables)
        }
        case _ =>
      }
      if (cond.child1 != null) queue.enqueue(cond.child1)
      if (cond.child2 != null) queue.enqueue(cond.child2)
    }
    var done = false
    while (!done) {
      val oldCnt = result.size
      for (v <- result if v.initialValue != null) result = result.union(v.initialValue.variables)
      if (result.size == oldCnt) done = true
    }
    done = false
    while (!done) {
      val oldCnt = result.size
      for (v <- result if v.update != null) result = result.union(v.update.variables)
      if (result.size == oldCnt) done = true
    }
    result.toSet[PubsVariable]
  }

  def parameters : scala.collection.Set[PubsVariable] = {
      var result = Set.empty[PubsVariable] + new PubsVariable("c" + nodeId.toString, true)
      for (cs <- innerCs) result = result.union(cs.parameters)
      result
  }

  def pubsInnerCalls : String = {
    var result = ""
    for (loop <- innerCs) result = result + ", " + loop.pubsName
    result
  }

  /*
     The cost relation system for this loop, which we then pass to PUBS.
     See section 7.6 in the thesis.
  */
  def pubsCes : String = {
    var boundsString = ""
    for (v <- variablesNoInner if !v.isArgument && v.knownUpdate) {
      val m = v.update.multiplier()
      boundsString =  boundsString + m + "*" + v + "_prime = " + v.update.multiple(m) + ","
    }

    for (v <- variablesNoInner if v.knownUpdate && v.initialValue != null) {
      if (v.updateIncreasing) {
        if (v.updateDecreasing) boundsString = boundsString + v + "=" +v.initialValue.toStringWithValues + ","
        else boundsString = boundsString + v + ">=" +v.initialValue.toStringWithValues + ","
      }
      else if (v.updateDecreasing) boundsString = boundsString + v + "=<" +v.initialValue.toStringWithValues + ","
    }
    // pick all constraints that might help as well
    // also add the other constraints we get from the numerical analysis
    // (but only if they only involve variables of our CES)
    for (c <- constraints) {
      var ok = true
      for (v <- c.lhs.variables) {
        if (getVariable(v.name) == null) ok = false
      }
      if (ok) boundsString = boundsString + c + ","
    }

    val prefix = "l" + nodeId.toString + "x"
    val pv = pubsVariables
    val cost = "nat(" + parent.costModel.loopCostExpression(parent.cfg, nodeId) + ")"
    val pvr = pubsVariablesRecursive

    var result = "eq(" + pubsName + ",0,[l" + nodeId.toString() + "x1(" + pubsVariablesInitial+")],[]).\n"

    val queue : mutable.Queue[Condition] = new mutable.Queue[Condition]
    if (condition != null) queue.enqueue(condition)
    while (!queue.isEmpty) {
      val cond = queue.dequeue()
      cond match {
        case b : BaseCondition => {
          if (b.getNextAnd == null) {
            result = result + "eq(" + prefix + b.id + "(" + pv + ")," + cost + ",[" + prefix + "1(" + pvr + ")" + pubsInnerCalls + "],[" + boundsString + b.relation +"]).\n"
          } else {
            result = result + "eq(" + prefix + b.id + "(" + pv + "),0,[" + prefix + b.getNextAnd + "(" + pv + ")],[" + b.relation + "]).\n"
          }
        }
        case a : AndCondition => {
          result = result + "eq(" + prefix + a.id + "(" + pv + "),0,[" + prefix + a.id + "1(" + pv + ")],[]).\n"
        }
        case o : OrCondition => {
          result = result + "eq(" + prefix + o.id + "(" + pv + "),0,[" + prefix + o.id + "1(" + pv + ")],[]).\n"
          result = result + "eq(" + prefix + o.id + "(" + pv + "),0,[" + prefix + o.id + "2(" + pv + ")],[]).\n"
        }
        case _ =>
      }
      if (cond.child1 != null) queue.enqueue(cond.child1)
      if (cond.child2 != null) queue.enqueue(cond.child2)
    }

    // add the cost equations for the inner control structures
    for (cs <- innerCs) result = result + cs.pubsCes
    result
  }

}