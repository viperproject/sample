package ch.ethz.inf.pm.sample.td.cost.loops

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td.compiler._

/*
    A compiler that does the same as TouchCompiler, and additionally augments the CFG.
    See section 6 in the thesis.
 */
class LoopCostCompiler extends TouchCompiler {

  //true iff there is a loop in the compiled code
  var loops : Boolean = false;

  override def compileFile(path: String): List[ClassDefinition] = augment(super.compileFile(path))

  override def getLabel(): String = "TouchDevelop (augmented for Cost Analysis)"

  /*
      Augment the CFG. See section 6.1 in the thesis.
   */
  private def augment(l: List[ClassDefinition]) = {
    for (c <- l) {
      for (m <- c.methods) {
        val cfg = m.body
        var index = 0
        for (node <- cfg.nodes) {
          if(cfg.exitEdges(index).size == 2 && cfg.entryEdges(index).size == 2) {
            var done = new Array[Boolean](cfg.nodes.size)
            var edges : Set[(Int, Int, Option[Boolean])] = trueEdge(cfg.exitEdges(index))
            var loopFound = false
            var firstNode = -1
            var lastNode = -1
            while (edges.size > 0) {
              val i1 = edges.head._1
              val i2 = edges.head._2
              if (firstNode == -1) {
                firstNode = i2
              }
              edges = edges.drop(1)
              if (i2 == index) {
                loopFound = true
                lastNode = i1
              }
              else if (!done.apply(i2)) {
                // we see this node the first time
                done.update(i2, true)
                edges = edges ++ cfg.exitEdges(i2)
              }
            }
            if (loopFound) {
              loops = true
              // go backwards to find all variables declared before
              var allVariables: Set[Variable] = assignmentVariables(cfg.nodes(index)).toSet[Variable]
              done = new Array[Boolean](cfg.nodes.size)
              edges = inEdge(cfg.entryEdges(index), lastNode)
              while (edges.size > 0) {
                val i1 = edges.head._1
                edges = edges.drop(1)
                // !(i1==index) added later - maybe we should exclude EVERYTHING that comes after
                if (!done.apply(i1) && !(i1==index)) {
                  // we see this node the first time
                  val variablesFound: List[Variable] = assignmentVariables(cfg.nodes(i1))
                  for (v <- variablesFound if !v.isInstanceOf[OldVariable]) allVariables = allVariables + v
                  done.update(i1, true)
                  edges = edges ++ cfg.entryEdges(i1)
                }
              }

              //New version
              //var allVariables: Set[Variable] = assignmentVariables(cfg.nodes(index)).toSet[Variable]
              //for(block<-cfg.getEdgesEntryingTo(index))
              //    allVariables=allVariables++assignmentVariables(cfg.nodes(block))


              var newNode: List[Statement] = cfg.nodes.apply(firstNode)
              var namesUsed: Set[String] = Set.empty
              val programPoint = node.head.getPC()
              for (v <- allVariables) {
                if (!namesUsed.apply(v.id.toString)) {
                  newNode = new Assignment(programPoint, new OldVariable(v), v) :: newNode
                  namesUsed = namesUsed + v.id.toString
                }
              }
              cfg.setNode(firstNode, newNode)
            }
          }
          index += 1
        }
      }
    }
    l
  }

  private def inEdge(entryEdges: Set[(Int, Int, Option[Boolean])], lastNode : Int) : Set[(Int, Int, Option[Boolean])] = {
    var result : Set[(Int, Int, Option[Boolean])] = Set.empty
    for((i1, i2, w) <- entryEdges if (i1 != lastNode)) result += ((i1,i2,w))
    result
  }

  private def trueEdge(exitEdges: Set[(Int, Int, Option[Boolean])]) : Set[(Int, Int, Option[Boolean])] = {
    var result : Set[(Int, Int, Option[Boolean])] = Set.empty
    for((i1, i2, w) <- exitEdges)
      if (w.getOrElse(false) == true)
        result += ((i1,i2,w))
    result
  }

  // all variables appearing in statement s
  private def variablesFromStatementAsString(s: Statement) : Set[String] = {
    s match {
      case a: Assignment => {
        variablesFromStatementAsString(a.left).union(variablesFromStatementAsString(a.right))
      }
      case mc: MethodCall => {
        var result : Set[String] = variablesFromStatementAsString(mc.method)
        for (p <- mc.parameters) result = result.union(variablesFromStatementAsString(p))
        result
      }
      case fa: FieldAccess => variablesFromStatementAsString(fa.obj)
      case v: Variable => {
        val result : Set[String] = Set.empty[String]
        result + v.getName
      }
      case vd: VariableDeclaration =>  variablesFromStatementAsString(vd.variable)
      case _ => Set.empty[String]
    }
  }

  // all variables appearing in statement s
  private def variablesFromStatement(s: Statement) : Set[Variable] = {
    s match {
      case a: Assignment => {
        variablesFromStatement(a.left).union(variablesFromStatement(a.right))
      }
      case mc: MethodCall => {
        var result : Set[Variable] = variablesFromStatement(mc.method)
        for (p <- mc.parameters) result = result.union(variablesFromStatement(p))
        result
      }
      case fa: FieldAccess => variablesFromStatement(fa.obj)
      case v: Variable => {
        val result : Set[Variable] = Set.empty[Variable]
        result + v
      }
      case vd: VariableDeclaration =>  variablesFromStatement(vd.variable)
      case _ => Set.empty[Variable]
    }
  }

  // all variables appearing on the left hand side of some assignment, or in a variable declaration
  private def assignmentVariables(statements: List[Statement]) = {
    var result : List[Variable] = List.empty
    for (statement <- statements) {
      statement match {
        case a@MethodCall(_,FieldAccess(_,subject,":=",_),_,_,_) => {
          result = variablesFromStatement(subject).toList ::: result
        }
        case vd: VariableDeclaration => {
          result = vd.variable :: result
        }
        case _ =>
      }
    }
    result
  }

}
