package ch.ethz.inf.pm.sample.td.cost.loops

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain._
import apron.Lincons1
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td.domain.{InvalidAnd, StringsAnd}
import collection.mutable
import ch.ethz.inf.pm.sample.property.{ValidatedProgramPoint, WarningProgramPoint, OutputCollector, Property}
import ch.ethz.inf.pm.td.analysis.TouchAnalysisWithApron
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain, NumericWithStringDomain}
import ch.ethz.inf.pm.sample.execution.CFGState


class CostAnalysis[D <: NumericalDomain[D]] extends TouchAnalysisWithApron[D,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]] {

  override def reset() : Unit = NameEncoder.reset();

  override def getLabel(): String = "TouchDevelop loop cost analysis"

  override def numericalDomainList = ("Domain", List("ApronLinearEqualities"))

  override def getProperties: List[Property] = super.getProperties ::: List(new LoopCostProperty())

}

class LoopCostProperty extends Property {
  def getLabel() = "Loop Cost"

  def check[S <: State[S]](className : Type, methodName : MethodDeclaration, result : CFGState[S], printer : OutputCollector) : Unit = {
    if (methodName != null) {
      val analysis = new LoopCostInternal[S](methodName, result, new ParameterizedCostModel())  // my existing code for the cost analysis
      analysis.run()
      for ((programPoint, answer) <- analysis.result) {
        if (answer == null) {
          // no upper bound found
          printer.add(new WarningProgramPoint(programPoint, "method: " + methodName.name + ", no upper bound found"))
        } else if (answer.isInstanceOf[UnparsedCostExpr])  {
          // found an upper bound, but the answer from PUBS could not successfully be parsed
          printer.add(new WarningProgramPoint(programPoint, "method: " + methodName.name + ", cost: "+answer.toString))
        } else {
          // found and parsed an upper bound
          printer.add(new ValidatedProgramPoint(programPoint, "method: " + methodName.name + ", cost: "+answer.toString))
        }
      }
    }
  }

  def finalizeChecking(printer : OutputCollector) {}

}

/*
    contains the static methods of class LoopCostInternal
 */
object LoopCostHelper {

  private var arguments : mutable.HashSet[String] = new mutable.HashSet[String]()

  def findArguments (method: MethodDeclaration) = {
    // find all arguments
    arguments = new mutable.HashSet[String]()
    for (list <- method.arguments) {
      for (argument <- list) {
        arguments.add(NameEncoder.getVariableName(argument.variable.getName))
      }
    }
  }

  def isArgument(v: String) = {
    val sourceName = NameEncoder.getSourceName(v);
    arguments.contains(sourceName)  || (sourceName.startsWith("Length"))// && sourceName.length > 7 && sourceName.charAt(7) != '{')
    //TODO: it is not acceptable to hard code this looking to Length or what else, find a better solution - e.g., var.isreadonly
  }

}


/*
   Performs the loop cost analysis when its 'run()' method is called. The result can then be retrieved by calling 'result'.
   See chapter 7 in the thesis.
*/
class LoopCostInternal[S <: State[S]](val method: MethodDeclaration, val cfge : CFGState[S], val costModel : CostModel) {

  /*--------------------------------------------
    debug
  ---------------------------------------------*/

  val DEBUG = true // if set to true, the input for PUBS is shown
  val DEBUG_LINCON = true // if set to true, information about the linear constraints that we get from the numerical analysis is shown

  /*--------------------------------------------
    public interface
  ---------------------------------------------*/

  var result : mutable.HashMap[ProgramPoint, CostExpr] = null

  /*
       Finds all loops, and performs the loop cost analysis on them. Writes the result of the analysis to 'result'.
   */
  def run() = {
    println("\n     -----  performing LoopCostAnalysis  -----\nmethod under attack: "+method.name)
    result = mutable.HashMap.empty[ProgramPoint, CostExpr]
    if (method != null) LoopCostHelper.findArguments(method)
    findControlStructures()
    findNesting()

    // do the analysis for all loops that we can handle
    for (cs <- controlStructures if cs != null && cs.isInstanceOf[Loop[S]])  {
      if (DEBUG) println("\nloop at "+cs.nodeId + " canHandle: " + cs.canHandleAll +" variables: "+cs.pubsVariables+ "\n")
      if (DEBUG) println(cs.variablesDebug)
      if (cs.canHandleAll) handleCs(cs)
      result.update(cfg.nodes(cs.nodeId).head.getPC(), cs.costExpression)
    }
  }

  /*--------------------------------------------
    data
  ---------------------------------------------*/

  def cfg : ControlFlowGraph = cfge.cfg
  var controlStructures : Array[ControlStructure[S]] = new Array[ControlStructure[S]](cfg.nodes.size)

  /*--------------------------------------------
    pre-analysis (find control structures & find out about control structure nesting)
  ---------------------------------------------*/

  private def isLoop (index: Int) = cfg.exitEdges(index).size == 2 && cfg.entryEdges(index).size == 2
  private def isConditional (index: Int) = cfg.exitEdges(index).size == 2 && cfg.entryEdges(index).size == 1
  private def isControlStructure (index: Int) = cfg.exitEdges(index).size == 2
  private def isConditionalExit (index: Int) = cfg.exitEdges(index).size == 1 && cfg.entryEdges(index).size == 2

  /*
      Finds all control structures. Collects the needed information (condition, update rules, inital values)
      for each of them and stores this information in a new object of type Loop or Conditional.
      See section 7.1 in the thesis.                                                                                                                                                                                                                                                                                                                                                          See section 6 in the thesis.
   */
  private def findControlStructures() = {
    var index = 0
    for (node <- cfg.nodes) {
      if (isControlStructure(index)) {
        var lastNode = -1
        var edgeWeight = cfg.entryEdges(index).head._3
        if (isLoop(index)) { // loop
        // find index of the last node in the loop
        val done = new Array[Boolean](cfg.nodes.size)
          var edges : Set[(Int, Int, Option[Boolean])] = trueEdge(cfg.exitEdges(index))
          while (lastNode == -1 && edges.size > 0) {
            val i1 = edges.head._1
            val i2 = edges.head._2
            val weight = edges.head._3
            edges = edges.drop(1)
            if (i2 == index) {
              lastNode = i1
              edgeWeight = weight
            }
            else if (!done.apply(i2)) { // we see this node the first time
              done.update(i2, true)
              edges = edges ++ cfg.exitEdges(i2)
            }
          }
          // find index of previous node before the loop
          var prevNode = cfg.entryEdges(index).head._1
          if (prevNode == lastNode) prevNode = cfg.entryEdges(index).last._1
          if (lastNode >= 0) {
            val loop = new Loop[S](this, index, lastNode, prevNode)
            checkLoop(loop)
            controlStructures(index) = loop
          }
        } else { // conditional
        val conditional = new Conditional[S](this, index)
          checkCondition(conditional)
          controlStructures(index) = conditional
        }
      }
      index += 1
    }
  }

  /*
      The set of all edges in 'exitEdges' that are labelled true.
      (Usually 'exitEdges' contains two edges, on labelled true and one labelled false, and so
      the resulting set contains the one edge labelled true.)
   */
  private def trueEdge(exitEdges: Set[(Int, Int, Option[Boolean])]) : Set[(Int, Int, Option[Boolean])] = {
    var result : Set[(Int, Int, Option[Boolean])] = Set.empty
    for((i1, i2, w) <- exitEdges)
      if (w.getOrElse(false) == true)
        result += ((i1,i2,w))
    result
  }

  /*
      for each control structure, find out whether it is contained in some other control structure
      See section 7.1 in the thesis.
    */
  private def findNesting() {
    for (cs <- controlStructures if cs != null)  {
      var node = cs.nodeId
      var edgeWeight = cfg.entryEdges(node).head._3
      if (isLoop(cs.nodeId)) {
        node = controlStructures(cs.nodeId).asInstanceOf[Loop[S]].prevNode
        val edgeOrigin = cfg.entryEdges(cs.nodeId).head._1 // try out both entry edges, use the one that is not inside the loop
        if (edgeOrigin != node) edgeWeight = cfg.entryEdges(cs.nodeId).last._3
      } else {
        node = cfg.entryEdges(node).head._1
      }
      var done = false
      var splitCount = 0
      while (!done) {
        if (isConditionalExit(node)) {
          // another inner conditional, take any of the two paths
          splitCount += 1
          edgeWeight = cfg.entryEdges(node).head._3
          node = cfg.entryEdges(node).head._1
        }
        else if (isConditional(node) && splitCount == 0) {
          // success: parent found
          done = true
          controlStructures(node).addInnerCs(cs.nodeId, edgeWeight.getOrElse(false))
        }
        else if (isConditional(node)) {
          splitCount -= 1
          edgeWeight = cfg.entryEdges(node).head._3
          node = cfg.entryEdges(node).head._1
        }
        else if (isLoop(node) && splitCount == 0 && edgeWeight.getOrElse(false) == true ) {
          // success: parent found
          done = true
          controlStructures(node).addInnerCs(cs.nodeId, true)
        }
        else if (isLoop(node)) {
          // another inner loop, or a loop inside a conditional: skip it
          val temp = controlStructures(node).asInstanceOf[Loop[S]].prevNode
          val edgeOrigin = cfg.entryEdges(node).head._1 // try out both entry edges, use the one that is not inside the loop
          edgeWeight = if (edgeOrigin == node) cfg.entryEdges(node).head._3 else cfg.entryEdges(node).last._3
          node = temp
        }
        else if (cfg.entryEdges(node).size == 0) {
          // initial node of the cfg
          done = true
        }
        else {
          edgeWeight = cfg.entryEdges(node).head._3
          node = cfg.entryEdges(node).head._1
        }
      }
    }
  }


  /*--------------------------------------------
    analysis of loops and conditionals
  ---------------------------------------------*/

  /**
   * Find an upper bound of the cost of the control structure using PUBS
   */
  private def handleCs(cs: ControlStructure[S]) = {
    if (cs.answer == null) {
      if (DEBUG) println("Input for PUBS:\n"+cs.pubsCes+"\n\n")
      cs.answer = PubsInterface.run(cs.pubsCes)
    }
  }

  /**
   * Extract all information that is needed for assembling the Cost Equation System
   */
  private def checkLoop(loop: Loop[S]) = {
    checkCondition(loop)
    checkVariableUpdates(loop)
    findInitialValues(loop)
  }

  /**
   * Find out the condition of the loop
   * See section 7.2 in the thesis.
   */
  private def checkCondition(loop: Loop[S]) = {
    val node = loop.node
    if (node.size == 1) {
      val state = cfge.statesOfBlock(loop.nodeId).head
      val expressions = node.head.forwardSemantics(state).expr.getSetOfExpressions
      if (expressions.size == 1) {
        loop.condition = getCondition(expressions.head, "1")
      } else {
        loop.canHandle = false
        if (expressions.size == 0) {  // we have bottom inside the loop, i.e. it is never executed
          loop.answer = "0"
        }
      }
    }
    if (loop.condition == null) loop.canHandle = false
  }

  /**
   * Find out the condition and the negated condition (elseCondition) of the conditional
   * See sections 7.2 and 7.5 in the thesis.
   */
  private def checkCondition(conditional: Conditional[S]) = {
    var noCondition = false // even if we can't find a valid condition, we know a overapproximation
    //  for the cost of the conditional: cost of the true-branch + cost of the false-branch
    val node = conditional.node
    if (node.size == 1) {
      val state = cfge.statesOfBlock(conditional.nodeId).head
      val expressions = node.head.forwardSemantics(state).expr.getSetOfExpressions
      if (expressions.size == 1) {
        conditional.condition = getCondition(expressions.head, "y1")
        conditional.elseCondition = getConditionNeg(expressions.head, "n1")
      } else noCondition = true
    } else noCondition = true
    if (noCondition || conditional.condition == null || conditional.elseCondition == null) {
      // create a dummy condition saying "we don't know anything"
      val lhs = LinearExpressionFactory.fromVariable(conditional.parameters.head) // some variable about which we don't know anything for sure
      val rhs = LinearExpressionFactory.fromConstant(1)
      val r = new LinearRelation(lhs, ">", rhs)
      conditional.condition = new BaseCondition("y1", r)
      conditional.elseCondition = new BaseCondition("n1", r)
    }
  }

  /**
   * The Condition that is represented by the Expression expr
   * id: identifier of the Cost Equation that will represent the Condition in the Cost Equation System
   */
  private def getCondition(expr: Expression, id: String) : Condition = {
    var result = true
    expr match {
      case c: Constant => {
        val zero = LinearExpressionFactory.fromConstant(0)
        val one = LinearExpressionFactory.fromConstant(1)
        if (c.constant.equalsIgnoreCase("true")) new BaseCondition(id, new LinearRelation(zero, "=", zero))
        else new BaseCondition(id, new LinearRelation(zero, "=", one))
      }
      case nbe : NegatedBooleanExpression => getConditionNeg(nbe.exp, id)
      case bae : BinaryArithmeticExpression => {
        var operator : String = ""
        bae.op match {
          case ArithmeticOperator.< => operator = "<"
          case ArithmeticOperator.<= => operator = "<="
          case ArithmeticOperator.> => operator = ">"
          case ArithmeticOperator.>= => operator = ">="
          case ArithmeticOperator.!= => operator = "!="
          case ArithmeticOperator.== => operator = "="
          case _ =>  result = false
        }
        val lhs = LinearExpressionFactory.fromExpression(bae.left)
        val rhs = LinearExpressionFactory.fromExpression(bae.right)
        if (lhs == null || rhs == null) result = false

        if (result == true && operator == "!=") {
          val r1 = new LinearRelation(lhs, "<", rhs)
          val r2 = new LinearRelation(lhs, ">", rhs)
          val cond = new OrCondition(id)
          cond.setChild1(new BaseCondition(id+"1", r1))
          cond.setChild2(new BaseCondition(id+"2", r2))
          cond
        } else if (result == true) {
          val r = new LinearRelation(lhs, operator, rhs)
          new BaseCondition(id, r)
        }  else null
      }
      case bbe : BinaryBooleanExpression => {
        bbe.op match {
          case BooleanOperator.&& => {
            val cond = new AndCondition(id)
            cond.setChild1(getCondition(bbe.left, id+"1"))
            if (cond.child1 != null) cond.child1.nextAnd = id+"2"
            cond.setChild2(getCondition(bbe.right, id+"2"))
            if (cond.child1 == null || cond.child2 == 0) null else cond
          }
          case BooleanOperator.|| => {
            val cond = new OrCondition(id)
            cond.setChild1(getCondition(bbe.left, id+"1"))
            cond.setChild2(getCondition(bbe.right, id+"2"))
            if (cond.child1 == null || cond.child2 == 0) null else cond
          }
          case _ => null
        }
      }
      case _ => null
    }
  }

  /**
   * The Condition that is represented by the negation of Expression expr
   * id: identifier of the Cost Equation that will represent the Condition in the Cost Equation System
   */
  private def getConditionNeg(expr: Expression, id: String) : Condition = {
    var result = true
    expr match {
      case c: Constant => {
        val zero = LinearExpressionFactory.fromConstant(0)
        val one = LinearExpressionFactory.fromConstant(1)
        if (c.constant.equalsIgnoreCase("false")) new BaseCondition(id, new LinearRelation(zero, "=", zero))
        else new BaseCondition(id, new LinearRelation(zero, "=", one))
      }
      case nbe : NegatedBooleanExpression => getCondition(nbe.exp, id)
      case bae : BinaryArithmeticExpression => {
        var operator : String = ""
        bae.op match {
          case ArithmeticOperator.< => operator = ">="
          case ArithmeticOperator.<= => operator = ">"
          case ArithmeticOperator.> => operator = "<="
          case ArithmeticOperator.>= => operator = "<"
          case ArithmeticOperator.!= => operator = "="
          case ArithmeticOperator.== => operator = "!="
          case _ =>  result = false
        }
        val lhs = LinearExpressionFactory.fromExpression(bae.left)
        val rhs = LinearExpressionFactory.fromExpression(bae.right)
        if (lhs == null || rhs == null) result = false
        if (result == true && operator == "!=") {
          val r1 = new LinearRelation(lhs, "<", rhs)
          val r2 = new LinearRelation(lhs, ">", rhs)
          val cond = new OrCondition(id)
          cond.setChild1(new BaseCondition(id+"1", r1))
          cond.setChild2(new BaseCondition(id+"2", r2))
          cond
        } else if (result == true) {
          val r = new LinearRelation(lhs, operator, rhs)
          new BaseCondition(id, r)
        }  else null
      }
      case bbe : BinaryBooleanExpression => {
        bbe.op match {
          case BooleanOperator.&& => {
            // negated, so we have !child1 || !child2 using the DeMorgan laws
            val cond = new OrCondition(id)
            cond.setChild1(getConditionNeg(bbe.left, id+"1"))
            cond.setChild2(getConditionNeg(bbe.right, id+"2"))
            if (cond.child1 == null || cond.child2 == 0) null else cond

          }
          case BooleanOperator.|| => {
            // negated, so we have !child1 && !child2 using the DeMorgan laws
            val cond = new AndCondition(id)
            cond.setChild1(getConditionNeg(bbe.left, id+"1"))
            if (cond.child1 != null) cond.child1.nextAnd = id+"2"
            cond.setChild2(getConditionNeg(bbe.right, id+"2"))
            if (cond.child1 == null || cond.child2 == 0) null else cond
          }
          case _ => null
        }
      }
      case _ => null
    }
  }

  /*
       Retrieves all linear constraints that we get from the numerical analysis and stores them in loop.constraints.
       (We can later on add them to the CES that we pass to PUBS, which may help to get a better result.)
       Currently not used. To use it, add a call to it in method checkLoop.
   */
  private def findConstraints(loop: Loop[S]) = {
    val lastNode = loop.lastNode
    val state = cfge.statesOfBlock(lastNode)(cfge.statesOfBlock(lastNode).size-1)
    state match {
      case as : AbstractState[StringsAnd[InvalidAnd[ApronInterface.Default],_,_],_,_] => {
        val apronInterface = as.getSemanticDomain._1.numericalDomain
        val vars = apronInterface.instantiateState().getEnvironment.getVars
        val lincons = apronInterface.instantiateState().toLincons(apronInterface.instantiateState().getCreationManager)
        for (l <- lincons if l.isLinear) {
          if (DEBUG_LINCON) println("cvu lincon in FC:" + l)

          val e = LinearExpressionFactory.fromLincon(l, vars)
          val zero = LinearExpressionFactory.fromConstant(0)
          l.getKind match {
            case Lincons1.EQ => loop.constraints = loop.constraints.::(new LinearRelation(e, "=", zero))
            case Lincons1.SUPEQ => loop.constraints = loop.constraints.::(new LinearRelation(e, ">=", zero))
            case Lincons1.SUP => loop.constraints = loop.constraints.::(new LinearRelation(e, ">", zero))
            case _ => // other cases: not used
          }
        }
      }
      case _ =>
    }
  }

  /**
   * For each variable of the loop which is not a constant, find out how it is updated in one iteration of the loop,
   * and if its value increases or decreases
   * See section 7.3 in the thesis.
   */
  private def checkVariableUpdates(loop: Loop[S]) = {
    val lastNode = loop.lastNode
    val state = cfge.statesOfBlock(lastNode)(cfge.statesOfBlock(lastNode).size-1)

    state match {
      case as : AbstractState[StringsAnd[InvalidAnd[ApronInterface.Default],_,_],_,_] => {
        val apronInterface = as.getSemanticDomain._1.numericalDomain
        val vars = apronInterface.instantiateState().getEnvironment.getVars
        var oldCount = 0
        var count = loop.variables.size

        while (count > oldCount) {
          oldCount = count
          for (i <- loop.variables if i.update == null) {
            val iVar = i.sourceName // the variable we are currently looking at
            var list : List[String] = List.empty[String]
            for (v <- vars) {
              if (v != iVar && v != "old_" + iVar) list = v +: list
            }

            var variableOk = false
            for (v <- vars) if (v == iVar) variableOk = true
            if (i.isArgument) variableOk = false

            if (variableOk) {
              if (DEBUG_LINCON) println("cvu variable:" + iVar)
              var result = false
              // do 2 runs
              // first run: try to find a lincon of the form i = a*old_i + b
              // second run: if nothing found in the first run, try to find a lincon of the form i = expr (where expr is a linear expression)
              for (useForgetCopy <- List(true, false)) {
                val lincons = if (useForgetCopy) apronInterface.instantiateState().forgetCopy(apronInterface.instantiateState().getCreationManager, list.toArray[String], false).toLincons(apronInterface.instantiateState().getCreationManager)
                else apronInterface.instantiateState().toLincons(apronInterface.instantiateState().getCreationManager)
                for (l <- lincons if !result) {
                  if (DEBUG_LINCON) println("cvu lincon:" + l)
                  val iCoeff = getCoeff(l, iVar)
                  if (iCoeff != 0.0 && l.getKind == Lincons1.EQ)  {
                    result = true
                    for (v <- vars if (v.startsWith("old_") && v != "old_" + iVar)) {
                      if (!l.getCoeff(v).isZero) result = false
                    }
                    if (result) {
                      i.update = LinearExpressionFactory.fromLincon(l, vars, iVar)
                    }
                  }
                }
              }

            }
          }
          count = loop.variables.size
        }

      }
      case _ =>
    }

    // check which variables are increasing
    var increasingVariables : Set[String] = Set.empty[String]
    var oldCount = -1
    var count = 0
    while (count > oldCount) {
      oldCount = count
      for (i <- loop.variables if i.update != null && i.updateIncreasing == false) {
        if (i.update.isIncUpdate(i.sourceName, increasingVariables)) {
          i.updateIncreasing = true
          increasingVariables += i.sourceName
        }
      }
      count = increasingVariables.size
    }

    // check which variables are decreasing
    var decreasingVariables : Set[String] = Set.empty[String]
    oldCount = -1
    count = 0
    while (count > oldCount) {
      oldCount = count
      for (i <- loop.variables if i.update != null && i.updateDecreasing == false) {
        if (i.update.isDecUpdate(i.sourceName, decreasingVariables)) {
          i.updateDecreasing = true
          decreasingVariables += i.sourceName
        }
      }
      count = decreasingVariables.size
    }
  }

  /**
   * For each variable of the loop which is not a constant, find its initial value
   * (which is a LinearExpression) before the first iteration of the loop
   * See section 7.4 in the thesis.
   */
  private def findInitialValues(loop: Loop[S]) = {
    val prevNode = loop.prevNode
    val state = cfge.statesOfBlock(prevNode)(cfge.statesOfBlock(prevNode).size-1)
    state match {
      case as : AbstractState[StringsAnd[InvalidAnd[ApronInterface.Default],_,_],_,_] => {
        val apronInterface = as.getSemanticDomain._1.numericalDomain
        val vars = apronInterface.instantiateState().getEnvironment.getVars
        val lincons = apronInterface.instantiateState().toLincons(apronInterface.instantiateState().getCreationManager)
        for (l <- lincons) {
          if (DEBUG_LINCON) println("lincon: "+l)
          if (l.getKind == Lincons1.EQ && l.isLinear) {
            // 1. check if we can use this lincons for finding the initial value of some variable
            var definedVariable : String = null // the variable whose value is defined by this lincons
            var linconOk = true // true iff definedVariable is unambiguous
            for (v <- vars
                 if (!l.getCoeff(v).isZero &&
                   !LoopCostHelper.isArgument(NameEncoder.getVariableName(v)) &&
                   loop.getVariable(NameEncoder.getVariableName(v)) != null)) {
              if (definedVariable != null) linconOk = false
              else definedVariable = v
            }
            // 2. if yes, create the linear expression describing the initial value
            if (linconOk && definedVariable != null) {
              val coefficients : mutable.HashMap[PubsVariable, Rational] = mutable.HashMap.empty
              val iCoeff = getCoeff(l, definedVariable).toInt
              for (v <- vars if (v != definedVariable)) {
                val value = new Rational(-getCoeff(l, v).toInt, iCoeff)
                if (!value.isZero) {
                  coefficients.put(new PubsVariable(v, false), value)
                }
              }
              val constant = new Rational(- getCst(l).toInt, iCoeff)
              if (linconOk && loop.getVariable(NameEncoder.getVariableName(definedVariable)) != null) {
                val iv = new LinearExpression(constant, coefficients)
                loop.getVariable(NameEncoder.getVariableName(definedVariable)).initialValue = iv
              }
            }
          }
        }
      }
      case _ =>
    }
  }


  /*--------------------------------------------
    interfacing with APRON
  ---------------------------------------------*/

  private def getCoeff(l: Lincons1, v: String) = {
    val temp = new Array[Double](1)
    if (l.getCoeff(v).isScalar) {
      l.getCoeff(v).sup().toDouble(temp, 0)
      temp.apply(0)
    } else 0.0
  }

  private def getCst(l: Lincons1) = {
    val temp = new Array[Double](1)
    if (l.getCst.isScalar) {
      l.getCst.sup().toDouble(temp, 0)
      temp.apply(0)
    } else 0.0
  }

}

