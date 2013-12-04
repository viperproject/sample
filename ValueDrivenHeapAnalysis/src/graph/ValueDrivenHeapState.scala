package graph

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.property._
import apron.{Octagon, Box, Polka}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.{BooleanNativeMethodSemantics, IntegerNativeMethodSemantics, ObjectNativeMethodSemantics}


/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 5/10/13
 * Time: 4:27 PM
 * To change this template use File | Settings | File Templates.
 */
class ValueDrivenHeapState[S <: SemanticDomain[S]](val abstractHeap: HeapGraph[S],
                                                   val generalValState: S,
                                                   val expr: ExpressionSet,
                                                   val isTop: Boolean,
                                                   val isBottom: Boolean) extends State[ValueDrivenHeapState[S]] {


  if (isTop && isBottom) throw new Exception("Being Top and Bottom in the same time is not allowed.")

  def this(newExpr: ExpressionSet) = this(new HeapGraph[S](), new ApronInterface(None, new Polka(false), false, Set.empty[Identifier]).top().asInstanceOf[S], newExpr, false, false)
//  def this(newExpr: ExpressionSet) = this(new HeapGraph[S](), new ApronInterface(None, new Octagon()).top().asInstanceOf[S], newExpr, false, false)
//  def this(newExpr: ExpressionSet) = this(new HeapGraph[S](), new ApronInterface(None, new Box()).top().asInstanceOf[S], newExpr, false, false)


  def this(newAbstractHeap: HeapGraph[S], newGeneralValState: S, newExpr: ExpressionSet)  = this(newAbstractHeap,newGeneralValState,newExpr, false, false)


  /**
   Signals that we are going to analyze the statement at program point pp
   This is particularly important to eventually partition a state following
   the specified directives

   @param pp The point of the program that is going to be analyzed
   @return The abstract state eventually modified
  */
  def before(pp: ProgramPoint): ValueDrivenHeapState[S] = {
    return this
  }

  /**
  Creates a variable

   @param x The name of the variable
  @param typ The static type of the variable                                    \
  @param pp The program point that creates the variable
  @return The abstract state after the creation of the variable
    */
  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = {
    //**println("creatVariable(" + x.toString + ", " + typ.toString + ". " + pp.toString + ") is called")
    if(this.isBottom) return this
    if(x.getSetOfExpressions.size != 1 || x.getSetOfExpressions.head.isInstanceOf[VariableIdentifier]==false)
      throw new SymbolicSemanticException("Cannot declare multiple variables together")
    var result = this
    for(el <- x.getSetOfExpressions) {
      el match {
        case variable : VariableIdentifier => {
          if (variable.getType().isObject()) {
            result = new ValueDrivenHeapState[S](result.abstractHeap.addNewVertex(variable.getName(), typ)._1,
                                                 result.generalValState,
                                                 new ExpressionSet(typ).add(new VariableIdentifier(variable.getName(), variable.getType(), variable.getProgramPoint(), variable.scope)),
                                                 result.isTop, result.isBottom)
          } else {
            result = new ValueDrivenHeapState[S](result.abstractHeap.createVariableInAllStates(variable),
                                                 //result.generalValState.createVariable(variable.asInstanceOf[Identifier], variable.getType()),
                                                 result.generalValState.createVariable(variable, variable.getType()),
                                                 new ExpressionSet(typ).add(new VariableIdentifier(variable.getName(), variable.getType(), variable.getProgramPoint(), variable.scope)),
                                                 result.isTop, result.isBottom)
          }
        }
        case _ => throw new Exception("Only variables can be created.")
      }
    }
    return result
//    throw new Exception("Method createVariable is not implemented")
  }

  /**
  Creates a variable for an argument

   @param x The name of the argument
  @param typ The static type of the argument
  @return The abstract state after the creation of the argument
    */
  def createVariableForArgument(x: ExpressionSet, typ: Type): ValueDrivenHeapState[S] = {
    if(this.isBottom) return this
    //**println("createVariableForArgument(" + x.toString + ", " + typ.toString + ") is called")
//    if (x.getSetOfExpressions.size < 1)
//      return this
    if(x.getSetOfExpressions.size != 1)
      throw new Exception("Cannot declare multiple variables together")
    if(!x.getSetOfExpressions.head.isInstanceOf[VariableIdentifier])
      throw new Exception("Cannot declare variables for type that is not variable")
//    var result=this.bottom()
    if (x.getSetOfExpressions.size > 1)
      throw new Exception("More than one expression for variable to be created.")
    for(el <- x.getSetOfExpressions) {
      el match {
        case variable : VariableIdentifier => {
          var result = createVariable(x, typ, variable.getProgramPoint())
          if (variable.getType().isObject()) {
            val varExpr = result.getExpression()
            result = result.createObject(typ, variable.getProgramPoint(), Some(typ.getPossibleFields()))
            result = result.assignVariable(varExpr, result.getExpression())
          }
          return result
        }
        case _ => throw new Exception("Argument to be created is not a variable")
      }
    }
    return this
  }

  // Also initialize the created object
//  private def createObjectWithReplacement(typ: Type): (ValueDrivenHeapState[S], Replacement, Vertex) = {
//    var resReplacement = new Replacement()
//    val (newAbstractHeap, newVertex) = abstractHeap.addNewVertex(VertexConstants.DEFINITE)
//    assert(newVertex.isInstanceOf[DefiniteHeapVertex], "The newly created object should be definite")
//    var (newAH, addedVertex) = (newAbstractHeap, newVertex)
//    for (objField <- typ.getPossibleFields()) {
//      if (objField.getType().isObject()) {
//        // (newAH, addedVertex) = newAH.addNewVertex(VertexConstants.NULL)
//        newAH = newAH.addEdges(Set(new EdgeWithState[S](newVertex, generalValState, Some(objField.getName()), addedVertex)))
//      } else {
//        // This means that we have a value field and this should be included in all abstract states on edges
//        // This is done via Replacement
//        resReplacement.value += (Set.empty[Identifier] -> Set(new ValueHeapIdentifier(newVertex.asInstanceOf[DefiniteHeapVertex], objField.getName(), objField.getType(), objField.getProgramPoint())))
//      }
//    }
//    return (new ValueDrivenHeapState[S](newAH, generalValState, this.expr, this.isTop, this.isBottom), resReplacement, newVertex)
//  }

  /**
  Assigns an expression to a variable

   @param x The assigned variable
  @param right The assigned expression
  @return The abstract state after the assignment
    */
  def assignVariable(x: ExpressionSet, right: ExpressionSet): ValueDrivenHeapState[S] = {
    //**println("assignVariable(" + x.toString + "," + right.toString +") is called on " + this.toString())
    if(this.isBottom) return this
    if(right.isTop)
      return this.setVariableToTop(x).removeExpression()
    if(right.isTop) return top()
    var result = this
    if (x.getSetOfExpressions.size != 1 || right.getSetOfExpressions.size != 1)
      throw new Exception("More than one identifier or right expression are to be assigned.")
    if (right.getSetOfExpressions.size > 1)
      throw new Exception("Trying to assign more expressions on the right-hand side.")
    // TODO: Implement this properly - HIGH PRIORITY - need to implement the evaluation of access paths
    for (el <- x.getSetOfExpressions) {
      el match {
        case variable: VariableIdentifier => {
          if (el.getType().isNumericalType()) {
            val rightExp = right.getSetOfExpressions.head
            rightExp match {
              case v: VariableIdentifier => {
                result = new ValueDrivenHeapState[S](abstractHeap.assignValueVariable(variable, v), generalValState.assign(variable, v), new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(el.getType().top(), el.getProgramPoint)), false, result.isBottom)
              }
              case c: Constant => {
                result = new ValueDrivenHeapState[S](abstractHeap.assignValueVariable(variable, c), generalValState.assign(variable, c), new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(el.getType().top(), el.getProgramPoint)), false, result.isBottom)
              }
              // TODO: HIGH PRIORITY - Implement properly
              case _ => {
                result = new ValueDrivenHeapState[S](abstractHeap.assignValueVariable(variable, rightExp), generalValState.assign(variable, rightExp), new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(el.getType().top(), el.getProgramPoint)), false, result.isBottom)
//                throw new Exception("Not implemented yet.")
              }
            }
          } else {
            // remove edges from the assigned LocalVariableVertex
            val varVertex = abstractHeap.vertices.filter(v => v.name == variable.getName()).head
            val edgesToRemove = abstractHeap.edges.filter(e => e.source.equals(varVertex))
            val rightExp = right.getSetOfExpressions.head
            rightExp match {
              case verExpr: VertexExpression => {
                assert(abstractHeap.vertices.contains(verExpr.vertex), "Assigning a non-existing node")
                // val replacement = new Replacement()
                var newEdgeState = generalValState
                if (verExpr.vertex.isInstanceOf[HeapVertex]) {
                  // adding edge local information
                  var addedIdentifiers = Set.empty[Identifier]
                  assert(varVertex.typ.equals(verExpr.getType()), "We support only exact type, that is the fields should be the same")
                  for (valField <- varVertex.typ.getPossibleFields().filter(f => !f.getType().isObject())) {
                    val edgeLocalId = new EdgeLocalIdentifier(List.empty[String], valField.getName(), valField.getType(), valField.getProgramPoint())
                    addedIdentifiers = addedIdentifiers + edgeLocalId
                    newEdgeState = newEdgeState.createVariable(edgeLocalId, edgeLocalId.getType())
                    val resId = new ValueHeapIdentifier(verExpr.vertex.asInstanceOf[HeapVertex], valField.getName(), valField.getType(), verExpr.getProgramPoint())
                    newEdgeState = newEdgeState.assume(new BinaryArithmeticExpression(resId, edgeLocalId, ArithmeticOperator.==, null))
//                    newGeneralState = newGeneralState.assume(new BinaryBooleanExpression())
                  }
                }
                val addedEdges = Set(new EdgeWithState[S](varVertex, newEdgeState, None, verExpr.vertex))
                var resultingAH = abstractHeap.removeEdges(edgesToRemove)
                resultingAH = resultingAH.addEdges(addedEdges)
//                resultingAH = resultingAH.removeEdges(edgesToRemove -- addedEdges)
                result = new ValueDrivenHeapState[S](resultingAH, generalValState, new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(el.getType().top(), el.getProgramPoint)), false, false)
              }
              case v: VariableIdentifier => {
                val edgesOfRight = abstractHeap.edges.filter(e => e.source.name.equals(v.getName()))
                var edgesToAdd = Set.empty[EdgeWithState[S]]
                val sourceVertices = abstractHeap.vertices.filter(v => v.name.equals(variable.getName()))
                assert(sourceVertices.size == 1, "The local variable vertices should be one of each.")
                for (edge <- edgesOfRight) {
                  edgesToAdd = edgesToAdd + new EdgeWithState[S](sourceVertices.head, edge.state, None, edge.target)
                }
                var resultingAH = abstractHeap.addEdges(edgesToAdd)
                resultingAH = resultingAH.removeEdges(edgesToRemove -- edgesToAdd)
                result = new ValueDrivenHeapState[S](resultingAH, generalValState, new ExpressionSet(v.getType()).add(variable), false, isBottom)
              }
              case ap: AccessPathExpression => {
                val evaluatedRight = evaluateExpression(ap)
                val varVertex = abstractHeap.vertices.filter(_.name.equals(variable.getName())).head
                var edgesToAdd = Set.empty[EdgeWithState[S]]
                if (evaluatedRight._2.isEmpty)
                  return bottom()
                for (er <- evaluatedRight._2)
                  er._3.get(ap) match {
                    case None => throw new Exception("This means that there is no vertex at the end of access path.")
                    case Some(edgeSeq) => {
                      assert(evaluatedRight._1.isInstanceOf[Identifier], "This should be an identifier. Null, variable or access path.")
                      val edgeState = createEdgeLocalState(variable, evaluatedRight._1.asInstanceOf[Identifier], None, er._1, er._2, varVertex, edgeSeq.last.target)
                      edgesToAdd = edgesToAdd + new EdgeWithState[S](varVertex, edgeState, None, edgeSeq.last.target)
                    }
                  }
                var resultingAH = abstractHeap.removeEdges(edgesToRemove)
                resultingAH = resultingAH.addEdges(edgesToAdd)
                result = new ValueDrivenHeapState[S](resultingAH, generalValState, new ExpressionSet(variable.getType()).add(variable), false, isBottom)


//                val evaluatedRight = evaluateExpression(ap)
//                val varVertex = abstractHeap.vertices.filter(_.name.equals(variable.getName())).head
//                var edgesToAdd = Set.empty[EdgeWithState[S]]
//                for (er <- evaluatedRight._2)
//                  er._3 match {
//                    case None => throw new Exception("This means that there is no vertex at the end of access path.")
//                    case Some(edgeSeq) => {
//                      assert(evaluatedRight._1.isInstanceOf[Identifier], "This should be an identifier. Null, variable or access path.")
//                      val edgeState = createEdgeLocalState(variable, evaluatedRight._1.asInstanceOf[Identifier], None, er._1, er._2, varVertex, edgeSeq.last.target)
//                      edgesToAdd = edgesToAdd + new EdgeWithState[S](varVertex, edgeState, None, edgeSeq.last.target)
//                    }
//                  }
//                var resultingAH = abstractHeap.addEdges(edgesToAdd)
//                resultingAH = resultingAH.removeEdges(edgesToRemove -- edgesToAdd)
//                result = new ValueDrivenHeapState[S](resultingAH, generalValState, new ExpressionSet(variable.getType()).add(variable), false, isBottom)
              }
              case c : Constant => {
                assert(c.toString() == "null", "The only object constant is null.")
                var (resultingAH, nullVertex) = abstractHeap.addNewVertex(VertexConstants.NULL, c.getType())
                val varVertex = abstractHeap.vertices.filter(_.name.equals(variable.getName())).head
                resultingAH = resultingAH.removeEdges(resultingAH.edges.filter(_.source.equals(varVertex)))
                val (tempAH, idsToRemove) = resultingAH.prune()
                val newGenValState = Utilities.removeVariablesFromState(generalValState, idsToRemove)
                val addedEdge = new EdgeWithState[S](varVertex, newGenValState, None, nullVertex)
                resultingAH = tempAH.addEdges(Set(addedEdge))
                result = new ValueDrivenHeapState[S](resultingAH, newGenValState, new ExpressionSet(variable.getType()).add(variable), false, false)
              }
              case _ => throw new Exception("Not implemented yet.")
              // TODO: Implement
            }
          }
        }
        case _ => throw new Exception("Left-hand side of variable assignment is not a variable.")
      }
    }
    assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
    return result
//    throw new Exception("Method assignVariable is not implemented")
  }

  private def createEdgeLocalState(srcId: Identifier, trgId: Identifier, field: Option[String], state: S, addedIds: Set[Identifier], sourceVertex: Vertex, targetVertex: Vertex): S = {
    if (sourceVertex.isInstanceOf[NullVertex] || state.lessEqual(state.bottom())) return state.bottom()
    assert(srcId.getType().isObject(), "This is used only when objects are assigned.")
    assert(trgId.getType().isObject(), "This is used only when objects are assigned.")
    val edgeLocTrgPath = field match {
      case None => List.empty[String]
      case Some(f) => List(f)
    }
    //val resultingRep = new Replacement()
    var resultingState = state
    // TODO: Refactor this code, loads of duplication
    // adding source edge-local information
    var renamendIds = Set.empty[Identifier]
    if (sourceVertex.isInstanceOf[HeapVertex]) {
      for (valField <- sourceVertex.typ.getPossibleFields().filter(f => !f.getType().isObject())) {
        val srcEdgeLocId = new EdgeLocalIdentifier(List.empty[String], valField.getName(), valField.getType(), valField.getProgramPoint())
        val correspAddedId = addedIds.filter(id => id.getName().equals(srcId.getName() + "." + valField)).head
        resultingState = resultingState.rename(List(correspAddedId), List(srcEdgeLocId))
        renamendIds = renamendIds + correspAddedId
//        resultingRep.value += (Set(correspAddedId) -> Set(correspAddedId, srcEdgeLocId))
      }
    }
    // adding target edge-local information
    if (targetVertex.isInstanceOf[HeapVertex]) {
      for (valField <- targetVertex.typ.getPossibleFields().filter(f => !f.getType().isObject())) {
        val trgEdgeLocId = new EdgeLocalIdentifier(edgeLocTrgPath, valField.getName(), valField.getType(), valField.getProgramPoint())
        val correspAddedId = addedIds.filter(id => id.getName().equals(trgId.getName() + "." + valField)).head
//        resultingRep.value += (Set(correspAddedId) -> Set(correspAddedId, trgEdgeLocId))
        resultingState = resultingState.rename(List(correspAddedId), List(trgEdgeLocId))
        renamendIds = renamendIds + correspAddedId
      }
    }
    for (id <- addedIds -- renamendIds) {
      resultingState = resultingState.removeVariable(id)
    }
    return resultingState
  }

    /**
     * This methods returns an expression that represents the given expression, the set of states in which this
     * expression may exist and the set of added identifiers to the states with the last vertex of the expression.
     */
  private def evaluateExpression(expr: Expression): (Expression, Set[(S, Set[Identifier], Map[AccessPathExpression, List[EdgeWithState[S]]])])= {
    expr match {
      case v: VariableIdentifier => {
        if (v.getType().isObject())
          return evaluateExpression(new AccessPathExpression(v.getProgramPoint(),v.getType(), List(v.getName())))
        else
          return (v, Set((generalValState, Set.empty[Identifier], Map.empty[AccessPathExpression, List[EdgeWithState[S]]])))
      }
      case c: Constant => {
//        if (c.getType().isObject())
//          throw new Exception("Null constants should be handled separately.")
//        else
        return (c, Set((generalValState, Set.empty[Identifier], Map.empty[AccessPathExpression, List[EdgeWithState[S]]])))
      }
      case ap: AccessPathExpression => {
        // First we need to evaluate the access path, depending on whether it represents an object or a value
        // 1. we get all possible graph paths that the access path can follow
        val objAccPath = if (!ap.typ.isObject()) ap.path.dropRight(1) else ap.path
        var graphPaths = abstractHeap.getPaths(objAccPath)
        // 2. We evaluate each graph path and collect only the valid ones
        var validGraphPaths = Set.empty[(S, Set[Identifier], Map[AccessPathExpression, List[EdgeWithState[S]]])]
        for (path <- graphPaths) {
          val (st, apExp, apIds, apexpEdgeSeqMap) = evaluateGraphPath(path, expr.getProgramPoint())
          val isValid: Boolean = !st.lessEqual(st.bottom()) && (if (!ap.getType().isObject()) apIds.map(id => id.getName()).contains(ap.toString()) else true)
          if (isValid) {
            validGraphPaths = validGraphPaths + ((st, apIds.asInstanceOf[Set[Identifier]], apexpEdgeSeqMap))
          }
        }
        return (new AccessPathIdentifier(ap.path, ap.getType(), ap.getProgramPoint(), true), validGraphPaths)
      }
      case BinaryArithmeticExpression(l,r,o,t) => {
        val leftEval = evaluateExpression(l)
        val rightEval = evaluateExpression(r)
        val finalExp = new BinaryArithmeticExpression(leftEval._1, rightEval._1, o, t)
        var validStates = Set.empty[(S, Set[Identifier], Map[AccessPathExpression, List[EdgeWithState[S]]])]
        for (lStIdsMap <- leftEval._2) {
          var currentState = lStIdsMap._1
          for (rStIdsMap <- rightEval._2) {
            var prefixesAgree = true
            var newAccPathExpEdgeMap = Map.empty[AccessPathExpression, List[EdgeWithState[S]]]
            val intersectedKeys = lStIdsMap._3.keySet.intersect(rStIdsMap._3.keySet)
            for (key <- intersectedKeys) {
              if (prefixesAgree && lStIdsMap._3.apply(key).equals(rStIdsMap._3.apply(key))) {
                newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
              } else {
                prefixesAgree = false
              }
            }
            if (prefixesAgree) {
              currentState = Utilities.createVariablesForState(currentState, rStIdsMap._2)
              currentState = currentState.glb(currentState, Utilities.createVariablesForState(rStIdsMap._1, lStIdsMap._2))
              if (!currentState.lessEqual(currentState.bottom())) {
                for (key <- lStIdsMap._3.keySet -- intersectedKeys) {
                  newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
                }
                for (key <- rStIdsMap._3.keySet -- intersectedKeys) {
                  newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> rStIdsMap._3.apply(key))
                }
                validStates = validStates + ((currentState, rStIdsMap._2 ++ lStIdsMap._2, newAccPathExpEdgeMap))
              }
            }
          }
        }
        return (finalExp, validStates)
      }
      case NegatedBooleanExpression(e) => {
        val evaluatedExp = evaluateExpression(e)
        return (new NegatedBooleanExpression(evaluatedExp._1), evaluatedExp._2)
      }
      case BinaryBooleanExpression(l,r,o, t) => {
        val leftEval = evaluateExpression(l)
        val rightEval = evaluateExpression(r)
        val finalExp = new BinaryBooleanExpression(leftEval._1, rightEval._1, o, t)
        var validStates = Set.empty[(S, Set[Identifier], Map[AccessPathExpression, List[EdgeWithState[S]]])]
        for (lStIdsMap <- leftEval._2) {
          var currentState = lStIdsMap._1
          for (rStIdsMap <- rightEval._2) {
            var prefixesAgree = true
            var newAccPathExpEdgeMap = Map.empty[AccessPathExpression, List[EdgeWithState[S]]]
            val intersectedKeys = lStIdsMap._3.keySet.intersect(rStIdsMap._3.keySet)
            for (key <- intersectedKeys) {
              if (prefixesAgree && lStIdsMap._3.apply(key).equals(rStIdsMap._3.apply(key))) {
                newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
              } else {
                prefixesAgree = false
              }
            }
            if (prefixesAgree) {
              currentState = Utilities.createVariablesForState(currentState, rStIdsMap._2)
              currentState = currentState.glb(currentState, Utilities.createVariablesForState(rStIdsMap._1, lStIdsMap._2))
              if (!currentState.lessEqual(currentState.bottom())) {
                for (key <- lStIdsMap._3.keySet -- intersectedKeys) {
                  newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
                }
                for (key <- rStIdsMap._3.keySet -- intersectedKeys) {
                  newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> rStIdsMap._3.apply(key))
                }
                validStates = validStates + ((currentState, rStIdsMap._2 ++ lStIdsMap._2, newAccPathExpEdgeMap))
              }
            }
          }
        }
        return (finalExp, validStates)
      }
      case ReferenceComparisonExpression(l,r,o,t) => {
        val leftEval = evaluateExpression(l)
        val rightEval = evaluateExpression(r)
        val finalExp = new ReferenceComparisonExpression(leftEval._1, rightEval._1, o, t)
        var validStates = Set.empty[(S, Set[Identifier], Map[AccessPathExpression, List[EdgeWithState[S]]])]
        for (lStIdsMap <- leftEval._2) {
          var currentState = lStIdsMap._1
          for (rStIdsMap <- rightEval._2) {
            var prefixesAgree = true
            var newAccPathExpEdgeMap = Map.empty[AccessPathExpression, List[EdgeWithState[S]]]
            val intersectedKeys = lStIdsMap._3.keySet.intersect(rStIdsMap._3.keySet)
            for (key <- intersectedKeys) {
              if (prefixesAgree && lStIdsMap._3.apply(key).equals(rStIdsMap._3.apply(key))) {
                newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
              } else {
                prefixesAgree = false
              }
            }
            if (prefixesAgree) {
              currentState = Utilities.createVariablesForState(currentState, rStIdsMap._2)
              currentState = currentState.glb(currentState, Utilities.createVariablesForState(rStIdsMap._1, lStIdsMap._2))
              if (!currentState.lessEqual(currentState.bottom())) {
                for (key <- lStIdsMap._3.keySet -- intersectedKeys) {
                  newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
                }
                for (key <- rStIdsMap._3.keySet -- intersectedKeys) {
                  newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> rStIdsMap._3.apply(key))
                }
                validStates = validStates + ((currentState, rStIdsMap._2 ++ lStIdsMap._2, newAccPathExpEdgeMap))
              }
            }
          }
        }
        return (finalExp, validStates)
      }
      case _ => throw new Exception("Not implemented yet.")
    }
  }

//  /**
//  * This methods returns an expression that represents the given expression, the set of states in which this
//  * expression may exist and the set of added identifiers to the states with the last vertex of the expression.
//   */
//  private def evaluateExpression(expr: Expression): (Expression, Set[(S, Set[Identifier], Option[List[EdgeWithState[S]]])])= {
//    expr match {
//      case v: VariableIdentifier => {
//        if (v.getType().isObject())
//          return evaluateExpression(new AccessPathExpression(v.getProgramPoint(),v.getType(), List(v.getName())))
//        else
//          return (v, Set((generalValState, Set.empty[Identifier], None)))
//      }
//      case c: Constant => {
////        if (c.getType().isObject())
////          throw new Exception("Null constants should be handled separately.")
////        else
//        return (c, Set((generalValState, Set.empty[Identifier], None)))
//      }
//      case ap: AccessPathExpression => {
//        // First we need to evaluate the access path, depending on whether it represents an object or a value
//        // 1. we get all possible graph paths that the access path can follow
//        val objAccPath = if (!ap.typ.isObject()) ap.path.dropRight(1) else ap.path
//        var graphPaths = abstractHeap.getPaths(objAccPath)
//        // 2. We evaluate each graph path and collect only the valid ones
//        var validGraphPaths = Set.empty[(S, Set[Identifier], Option[List[EdgeWithState[S]]])]
//        for (path <- graphPaths) {
//          val (st, apExp, apIds) = evaluateGraphPath(path, expr.getProgramPoint())
//          val isValid: Boolean = !st.lessEqual(st.bottom()) && (if (!ap.getType().isObject()) apIds.map(id => id.getName()).contains(ap.toString()) else true)
//          if (isValid)
//            validGraphPaths = validGraphPaths + ((st, apIds.asInstanceOf[Set[Identifier]], Some(path)))
//        }
//        return (new AccessPathIdentifier(ap.path, ap.getType(), ap.getProgramPoint(), true), validGraphPaths)
//      }
//      case BinaryArithmeticExpression(l,r,o,t) => {
//        val leftEval = evaluateExpression(l)
//        val rightEval = evaluateExpression(r)
//        val finalExp = new BinaryArithmeticExpression(leftEval._1, rightEval._1, o, t)
//        var validStates = Set.empty[(S, Set[Identifier], Option[List[EdgeWithState[S]]])]
//        for (lStIds <- leftEval._2) {
//          var currentState = lStIds._1
//          for (rStIds <- rightEval._2) {
//            currentState = Utilities.createVariablesForState(currentState, rStIds._2)
//            currentState = currentState.glb(currentState, Utilities.createVariablesForState(rStIds._1, lStIds._2))
//            if (!currentState.lessEqual(currentState.bottom()))
//              validStates = validStates + ((currentState, rStIds._2 ++ lStIds._2, None))
//          }
//        }
//        return (finalExp, validStates)
//      }
//      case NegatedBooleanExpression(e) => {
//        val evaluatedExp = evaluateExpression(e)
//        return (new NegatedBooleanExpression(evaluatedExp._1), evaluatedExp._2)
//      }
//      case BinaryBooleanExpression(l,r,o, t) => {
//        val leftEval = evaluateExpression(l)
//        val rightEval = evaluateExpression(r)
//        val finalExp = new BinaryBooleanExpression(leftEval._1, rightEval._1, o, t)
//        var validStates = Set.empty[(S, Set[Identifier], Option[List[EdgeWithState[S]]])]
//        for (lStIds <- leftEval._2) {
//          var currentState = lStIds._1
//          for (rStIds <- rightEval._2) {
//            currentState = Utilities.createVariablesForState(currentState, rStIds._2)
//            currentState = currentState.glb(currentState, Utilities.createVariablesForState(rStIds._1, lStIds._2))
//            if (!currentState.lessEqual(currentState.bottom()))
//              validStates = validStates + ((currentState, rStIds._2 ++ lStIds._2, None))
//          }
//        }
//        return (finalExp, validStates)
//      }
//      case ReferenceComparisonExpression(l,r,o,t) => {
//        val leftEval = evaluateExpression(l)
//        val rightEval = evaluateExpression(r)
//        val finalExp = new ReferenceComparisonExpression(leftEval._1, rightEval._1, o, t)
//        var validStates = Set.empty[(S, Set[Identifier], Option[List[EdgeWithState[S]]])]
//        for (lStIds <- leftEval._2) {
//          var currentState = lStIds._1
//          for (rStIds <- rightEval._2) {
//            currentState = Utilities.createVariablesForState(currentState, rStIds._2)
//            currentState = currentState.glb(currentState, Utilities.createVariablesForState(rStIds._1, lStIds._2))
//            if (!currentState.lessEqual(currentState.bottom()))
//              validStates = validStates + ((currentState, rStIds._2 ++ lStIds._2, None))
//          }
//        }
//        return (finalExp, validStates)
//      }
//      case _ => throw new Exception("Not implemented yet.")
//    }
//  }

  /**
  Create an array of length

   @param length The length of the array
  @param typ The type of the array
  @param length The program point that created the array
  @return The abstract state after the creation of the array
    */
  def createArray(length: ExpressionSet, typ: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = {
    throw new Exception("Method createArray is not implemented")
  }

  /**
  Assigns an expression to a field of an object

   @param obj The object whose field is assigned
  @param field The assigned field
  @param right The assigned expression
  @return The abstract state after the assignment
    */
  def assignField(obj: List[ExpressionSet], field: String, right: ExpressionSet): ValueDrivenHeapState[S] = {
    if (isBottom) return this
    //**println("assignFiled(" + obj + ", " + field + ", " + right + ") is called.")
    assert(obj.size == 1 && obj.head.getSetOfExpressions.size == 1, "We are allowed to assign only single access path.")
    assert(right.getSetOfExpressions.size == 1, "We allow to assign only single expression")
    val leftExp = obj.head.getSetOfExpressions.head
    val rightExp = right.getSetOfExpressions.head
    assert(leftExp.isInstanceOf[AccessPathExpression], "The left hand side od the assignment is not an AccessPathExpression")
    val leftAccPath = leftExp.asInstanceOf[AccessPathExpression]
    val leftPaths: Set[List[EdgeWithState[S]]] = abstractHeap.getPathsToBeAssigned(leftAccPath).filter(_.last.target.isInstanceOf[HeapVertex])
    if (leftPaths.size == 0)
      return this.bottom()
    // Identifying valid left paths
    // TODO: validLeftPaths should be computed differently, using graphPathCondition
    var validLeftPaths =  Set.empty[(List[EdgeWithState[S]], S, AccessPathExpression, Set[AccessPathIdentifier])]
    for (lPath <- leftPaths) {
      val (pathState, pathIdentifier, pathAddedIds, accPathExpEdgeMap) = evaluateGraphPath(lPath, leftAccPath.pp)
      if (!pathState.lessEqual(pathState.bottom()))
        validLeftPaths = validLeftPaths + new Tuple4(lPath, pathState, pathIdentifier, pathAddedIds)
    }
    if (rightExp.getType().isObject()) {
      var edgesToAdd = Set.empty[EdgeWithState[S]]
      rightExp match {
        case x: VariableIdentifier => {
          val rightPaths = abstractHeap.getPaths(List(x.getName()))
          edgesToAdd = referencePathAssignmentEdges(leftAccPath.path.last, leftPaths, rightPaths)
        }
        case rAP: AccessPathExpression => {
          val rightPaths = abstractHeap.getPaths(rAP.path)
          edgesToAdd = referencePathAssignmentEdges(leftAccPath.path.last, leftPaths, rightPaths)
        }
        case v: VertexExpression => {
          // Add new edges
          for ((lPath, pathState, pathIdentifier, pathAddedIds) <- validLeftPaths) {
            var renamendIdsToRemove = Set.empty[Identifier]
            val lastPathVertex: Vertex = validLeftPaths.head._1.last.target
            // val pathStateReplacement = new Replacement()
            var resultState = pathState
            // Adding source edge local information to the state
            for (valField <- lPath.last.target.typ.getPossibleFields().filter(f => !f.getType().isObject())) {
              val newEdgeLocId: Identifier = new EdgeLocalIdentifier(List.empty[String], valField.getName(), valField.getType(), valField.getProgramPoint())
              val idToReplace: Identifier = new AccessPathIdentifier(pathIdentifier.path :+ valField.getName(), valField.getType(), valField.getProgramPoint())
              resultState = resultState.rename(List(idToReplace), List(newEdgeLocId))
              renamendIdsToRemove = renamendIdsToRemove + idToReplace
            }
            if (!v.vertex.isInstanceOf[NullVertex]) {
              // Only if the assigned vertex is not null, add target edge local information
              for (valField <- v.vertex.typ.getPossibleFields().filter(f => !f.getType().isObject())) {
                val newEdgeLocId: Identifier = new EdgeLocalIdentifier(List(leftAccPath.path.last), valField.getName(), valField.getType(), valField.getProgramPoint())
                assert(v.vertex.isInstanceOf[HeapVertex], "LocalVariableVertex should never be assigned.")
                val idToAssign: Identifier = new ValueHeapIdentifier(v.vertex.asInstanceOf[HeapVertex], valField.getName(), valField.getType(), valField.getProgramPoint())
                resultState = resultState.createVariable(newEdgeLocId, newEdgeLocId.getType())
                resultState = resultState.assign(idToAssign, newEdgeLocId)
              }
            }
            val renamedIdsAsStrings = renamendIdsToRemove.map(_.getName())
            for (id <- pathAddedIds.filter(pId => !renamedIdsAsStrings.contains(pId.getName()))) {
              resultState = resultState.removeVariable(id)
            }
            edgesToAdd = edgesToAdd + new EdgeWithState[S](lastPathVertex, resultState, Some(leftAccPath.path.last), v.vertex)
          }
        }
        case c : Constant => {
          assert(c.toString.equals("null"), "We expect only null constants.")
          val (newAH, nullVertex) = abstractHeap.addNewVertex(VertexConstants.NULL, c.getType())
          return new ValueDrivenHeapState[S](newAH, generalValState, expr, false, false).assignField(obj, field, new ExpressionSet(c.getType()).add(new VertexExpression(c.getProgramPoint(), c.getType(), nullVertex)))
        }
        case _ => throw new Exception("Assigning " + rightExp + " is not allowed (or supported:)). ")
      }
      // If there is no possible assignment, return bottom
      if (edgesToAdd.isEmpty)
        return bottom()
      // If the assignment is strong, remove edges that are killed by the assignment. Then add edges representing the
      // assignment. Last, prune the heap.
      var resultingAH: HeapGraph[S] = abstractHeap
      val sources : Set[Vertex] = edgesToAdd.map(_.source)
      if (sources.size == 1 && (sources.head.isInstanceOf[DefiniteHeapVertex] || sources.head.isInstanceOf[LocalVariableVertex])) {
        // Strong update - removing the edges from the target of the path labeled with the assigned filed
        val lastPathVertex: Vertex = sources.head
        val edgesToRemove = resultingAH.edges.filter(e => e.source.equals(lastPathVertex) && e.field.equals(Some(leftAccPath.path.last)))
        resultingAH = resultingAH.removeEdges(edgesToRemove)
      }
      resultingAH = resultingAH.addEdges(edgesToAdd)
      resultingAH = resultingAH.joinCommonEdges()
      val (prunedResAH, prunedIds) = resultingAH.prune()
      if (prunedResAH.isBottom())
        return this.bottom()
      // If some nodes were pruned, remove the identifiers corresponding to them from the general state.
      if (!prunedIds.isEmpty)
        return new ValueDrivenHeapState[S](prunedResAH, Utilities.removeVariablesFromState[S](generalValState,prunedIds), new ExpressionSet(right.getType()).add(leftAccPath), false, false)
      else
        return new ValueDrivenHeapState[S](resultingAH, generalValState, new ExpressionSet(right.getType()).add(leftAccPath), false, isBottom)
    } else {
      assert(rightExp.getType().isNumericalType(), "For now we allow only numerical values")
      if (validLeftPaths.size == 1) {
        val lastEdge = validLeftPaths.head._1.last
        val assignVertex = lastEdge.target.asInstanceOf[HeapVertex]
        // We are assigning to a single definite node (strong update)
        rightExp match {
          case v: VariableIdentifier => {
            // In this case we assign all the states.
            // NOTE: typing the left expression is a hack, assigning the right expression's type to it
            val leftID = new ValueHeapIdentifier(assignVertex, leftAccPath.path.last, rightExp.getType(), leftAccPath.getProgramPoint())
            var resultAH = abstractHeap.assignAllValStates(leftID, v)
            // Need to update also edge-local information
            var edgesToUpdate = Set.empty[EdgeWithState[S]]
            var resEdges = Set.empty[EdgeWithState[S]]
            for (edge <- resultAH.edges) {
              if (edge.target.equals(assignVertex))
                edgesToUpdate = edgesToUpdate + edge
              else
                resEdges = resEdges + edge
            }
            // Updating Targets
            for (edge <- edgesToUpdate) {
              var idToUpdate: EdgeLocalIdentifier = null
              edge.field match {
                case None => {
                  // Updating target of a local variable
                  idToUpdate = new EdgeLocalIdentifier(List.empty[String], leftAccPath.path.last, v.getType(), v.getProgramPoint())
                }
                case Some(f) => {
                  // Updating target of a heap vertex
                  idToUpdate = new EdgeLocalIdentifier(List(f), leftAccPath.path.last, v.getType(), v.getProgramPoint())
                }
              }
              if (edge.weakEquals(lastEdge))
                resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, v), edge.field, edge.target)
              else {
                if (leftID.representSingleVariable())
                  resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, v), edge.field, edge.target)
                else
                  resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, leftID), edge.field, edge.target)
              }
            }
            resultAH = new HeapGraph[S](resultAH.vertices, resEdges)
            val outEdgesToUpdate = resultAH.edges.filter(e => e.source.equals(assignVertex))
            resEdges = resultAH.edges.filter(e => !e.source.equals(assignVertex))
            for (edge <- outEdgesToUpdate) {
              assert(!edge.source.isInstanceOf[LocalVariableVertex], "This should never happen, as LocalVariableVertices do not carry value information.")
              val idToUpdate = new EdgeLocalIdentifier(List.empty[String], leftAccPath.path.last, v.getType(), v.getProgramPoint())
              if (leftID.representSingleVariable())
                resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, v), edge.field, edge.target)
              else
                resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, leftID), edge.field, edge.target)
            }
            resultAH = new HeapGraph[S](resultAH.vertices, resEdges)
            val (prunedResAH, prunedIds) = resultAH.prune()
            if (prunedResAH.isBottom())
              return this.bottom()
            if (!prunedIds.isEmpty) {
              val result = new ValueDrivenHeapState[S](prunedResAH, Utilities.removeVariablesFromState[S](generalValState, prunedIds), new ExpressionSet(right.getType()).add(leftAccPath), false, false)
              assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
              return result

            } else {
              val result = new ValueDrivenHeapState[S](resultAH, generalValState.assign(leftID, v), obj.head, false, false)
              assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
              return result
            }
          }
          case c: Constant => {
            // NOTE: typing the left expression is a hack, assigning the right expression's type to it
            val leftID = new ValueHeapIdentifier(assignVertex, leftAccPath.path.last, rightExp.getType(), leftAccPath.getProgramPoint())
            var resultAH = abstractHeap.assignAllValStates(leftID, c)
            // Need to update also edge-local information
            var edgesToUpdate = Set.empty[EdgeWithState[S]]
            var resEdges = Set.empty[EdgeWithState[S]]
            for (edge <- resultAH.edges) {
              if (edge.target.equals(assignVertex))
                edgesToUpdate = edgesToUpdate + edge
              else
                resEdges = resEdges + edge
            }
            for (edge <- edgesToUpdate) {
              var idToUpdate: EdgeLocalIdentifier = null
              edge.field match {
                case None => {
                  // Updating target of a local variable
                  idToUpdate = new EdgeLocalIdentifier(List.empty[String], leftAccPath.path.last, c.getType(), c.getProgramPoint())
                }
                case Some(f) => {
                  // Updating target of a heap vertex
                  idToUpdate = new EdgeLocalIdentifier(List(f), leftAccPath.path.last, c.getType(), c.getProgramPoint())
                }
              }
              if (edge.weakEquals(lastEdge))
                resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, c), edge.field, edge.target)
              else {
                if (leftID.representSingleVariable())
                  resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, c), edge.field, edge.target)
                else
                  resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, leftID), edge.field, edge.target)
              }
            }
            resultAH = new HeapGraph[S](resultAH.vertices, resEdges)
            val outEdgesToUpdate = resultAH.edges.filter(e => e.source.equals(assignVertex))
            resEdges = resultAH.edges.filter(e => !e.source.equals(assignVertex))
            for (edge <- outEdgesToUpdate) {
              assert(!edge.source.isInstanceOf[LocalVariableVertex], "This should never happen, as LocalVariableVertices do not carry value information.")
              val idToUpdate = new EdgeLocalIdentifier(List.empty[String], leftAccPath.path.last, c.getType(), c.getProgramPoint())
              if (leftID.representSingleVariable())
                resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, c), edge.field, edge.target)
              else
                resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(idToUpdate, leftID), edge.field, edge.target)
            }
            resultAH = new HeapGraph[S](resultAH.vertices, resEdges)
            val (prunedResAH, prunedIds) = resultAH.prune()
            if (prunedResAH.isBottom())
              return this.bottom()
            if (!prunedIds.isEmpty) {
              val result = new ValueDrivenHeapState[S](prunedResAH, Utilities.removeVariablesFromState(generalValState, prunedIds), new ExpressionSet(right.getType()).add(leftAccPath), false, false)
              assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
              return result
            }
            else {
              val result = new ValueDrivenHeapState[S](resultAH, generalValState.assign(leftID, c), obj.head, false, false)
              assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
              return result
            }
          }
          case _ => throw new Exception("Not supported yet.")
        }
      } else {
        throw new Exception("Weak updates not supported yet.")
      }
    }
//    throw new Exception("Method assignField is not implemented")
  }

  /**
   * This method computes the edges that correspond to the reference assignment.
   *
   * @param field to be assigned to the target nodes of the leftPaths
   * @param leftPaths sequence of edges that correspond to paths of LHS of the assignment (without the last field)
   * @param rightPaths sequence of edges that correspond to paths of RHS of the assignment
   * @return the set of edges that represent the reference assignment
   *
   * @author Milos Novacek
   */
  private def referencePathAssignmentEdges(field: String, leftPaths : Set[List[EdgeWithState[S]]], rightPaths : Set[List[EdgeWithState[S]]]) : Set[EdgeWithState[S]] = {val rightStateWithVertex : Set[(S, Vertex)] = rightPaths.map(p => (graphPathCondition(p), p.last.target))
    var edgesToAdd = Set.empty[EdgeWithState[S]]
    for (lPath <- leftPaths) {
      var leftCond = graphPathCondition(lPath)
      if (!leftCond.lessEqual(leftCond.bottom())) {
        // The condition of the left path is not bottom. (i.e. can be possibly assigned)
        for(rPath <- rightPaths) {
          val rightCond = graphPathCondition(rPath)
          var renameFrom = List.empty[EdgeLocalIdentifier]
          var renameTo = List.empty[EdgeLocalIdentifier]
          val idsToRename = rightCond.getIds().filter(_.isInstanceOf[EdgeLocalIdentifier]).asInstanceOf[Set[EdgeLocalIdentifier]]
          for (elId <- idsToRename) {
            renameFrom = elId :: renameFrom
            renameTo = new EdgeLocalIdentifier(List(field), elId.field, elId.getType(), elId.getProgramPoint()) :: renameTo
          }
          var newEdgeState = rightCond.rename(renameFrom, renameTo)
          leftCond = Utilities.createVariablesForState(leftCond, renameTo.toSet[Identifier])
          newEdgeState = Utilities.createVariablesForState(newEdgeState, renameFrom.toSet[Identifier])
          newEdgeState = newEdgeState.glb(leftCond, newEdgeState)
          if (!newEdgeState.lessEqual(rightCond.bottom())){
            // add edge that represents the assignment
            edgesToAdd = edgesToAdd + new EdgeWithState[S](lPath.last.target, newEdgeState, Some(field), rPath.last.target)
          }
        }
      }
    }
    edgesToAdd
  }

  /**
   * Given the set of edges that represent a single reference assignment, the method returns true if the edges represent
   * a strong assignment, false otherwise.
   *
   * @param edges representing a single reference assignemtn
   * @return true if the edges represent a strong assignment, false otherwise
   */
  private def isStrongReferenceAssignment(edges : Set[EdgeWithState[S]]) : Boolean = {
    val sources : Set[Vertex]  = edges.map(_.source)
    sources.size == 1 && (sources.head.isInstanceOf[DefiniteHeapVertex] || sources.head.isInstanceOf[LocalVariableVertex])
  }


  /**
   * The method for computing the condition that is satisfied by the given path.
   *
   * @param path for which the condition should be computed
   * @return abstract value condition that is satisfied by the given path
   */
  private def graphPathCondition(path : List[EdgeWithState[S]]) : S = {
    // The path can not be empty. This should be caught previously and evaluated to bottom.
    assert(!path.isEmpty)
    // Every path must start with a local variable.
    assert(path.head.source.isInstanceOf[LocalVariableVertex])

    /**
     * Inner helper method for computing the condition recursively.
     *
     * @param path to be proccesed
     * @param state - starting state where are only the edge-loca identifiers with empty sequence of field access that represent targets
     * @return
     */
    def graphPathConditionRecursive(path : List[EdgeWithState[S]], state : S) : S = {
      // Base case is when the path is empty. (Termination)
      if (path.isEmpty) {
        assert((state.getIds().filter(_.isInstanceOf[EdgeLocalIdentifier])
          -- state.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] && id.asInstanceOf[EdgeLocalIdentifier].accPath.isEmpty)).isEmpty)
        return state
      }

      // If the path is non-empty, the head of it must refer to a field (i.e. the firs node must be a HeapVertex).
      assert(path.head.source.isInstanceOf[HeapVertex])
      val edge = path.head

      // Only the edge-local identifiers that refer to target are present in the given state. (i.e. the once with empty sequence of field accesses).
      assert(state.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] && !id.asInstanceOf[EdgeLocalIdentifier].accPath.isEmpty).isEmpty)

      // Originally, the edge local identifiers of the given state with the empty sequence of fields refer to the target
      // and no other edge-local identifiers are present in the given state. We need to add them so that the edge-local
      // identifiers of the currently procces edge do not get lost.
      val edgeLocalIdsToAdd = edge.state.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] && !id.asInstanceOf[EdgeLocalIdentifier].accPath.isEmpty)
      var newState: S = Utilities.createVariablesForState(state, edgeLocalIdsToAdd.toSet[Identifier])
      newState = newState.glb(newState, edge.state)

      // Now, we need to rename source-edge local identifiers to the ones that are target of this edge and remove any others.
      val originalSourceIds = newState.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] && id.asInstanceOf[EdgeLocalIdentifier].accPath.isEmpty).toSet[Identifier]
      newState = Utilities.removeVariablesFromState(newState, originalSourceIds)
      // Renaming
      val idsToRenameToSource = newState.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] && id.asInstanceOf[EdgeLocalIdentifier].accPath == 1
        && id.asInstanceOf[EdgeLocalIdentifier].accPath.head.equals(
        edge.field match {
          case None => ""
          case Some(f) => f
        })).asInstanceOf[Set[EdgeLocalIdentifier]]
      // Building lists for renaming
      var renameFrom = List.empty[EdgeLocalIdentifier]
      var renameTo = List.empty[EdgeLocalIdentifier]
      for (elId <- idsToRenameToSource) {
        renameFrom = elId :: renameFrom
        renameTo = new EdgeLocalIdentifier(List.empty[String], elId.field, elId.getType(), elId.getProgramPoint()) :: renameTo
      }
      newState = newState.rename(renameFrom, renameTo)
      // Now we remove all edge-local identifiers that can not be the targets.
      val elIdsToRemove = newState.getIds().filter(x => x.isInstanceOf[EdgeLocalIdentifier]) -- renameTo
      newState = Utilities.removeVariablesFromState(newState, elIdsToRemove.toSet[Identifier])


      // return
      graphPathConditionRecursive(path.tail, newState)
    }

    /* The head of the path (edge sequence) is starting from a variable. Therefore, the edge local variables
       that represent the target edge locat variables have an empty sequence of fields. However, we need to remove all
       other edge-local identifier that might be possibly present.
    */
    val elIdsToRemove = path.head.state.getIds().filter(id => id.isInstanceOf[EdgeLocalIdentifier] && !id.asInstanceOf[EdgeLocalIdentifier].accPath.isEmpty).asInstanceOf[Set[Identifier]]
    // return
    graphPathConditionRecursive(path.tail, Utilities.removeVariablesFromState(path.head.state, elIdsToRemove))
  }



  /**
   * This methods computes the state that must be satisfied when following the given access path,
   * the string representation of the access path and the set of identifiers that were newly created.
   * Furthermore, the resulting state does not contain the edge local identifiers.
   *
   * @param path
   * @return
   */
  private def evaluateGraphPath(path : List[EdgeWithState[S]], pp: ProgramPoint): (S, AccessPathExpression, Set[AccessPathIdentifier], Map[AccessPathExpression, List[EdgeWithState[S]]]) = {
    assert(path.head.source.isInstanceOf[LocalVariableVertex], "The source of the path must represent a local variable.")
    for (edge <- path.tail) {
      edge.field match {
        case None => throw new Exception("The tail of the path should always be a field edge.")
        case Some(f) =>
      }
    }
//    var sequenceOfStates: List[S] = for (edge <- path) yield edge.state
    var sequenceOfStates = List.empty[S]
    var currentPathList = path.head.source.name :: Nil
    var addedIdentifiers = Set.empty[AccessPathIdentifier]
    var expEdgeMap = Map.empty[AccessPathExpression, List[EdgeWithState[S]]]
    var currentAccPathExp: AccessPathExpression = null
    var currentEdgeSeq = List.empty[EdgeWithState[S]]
    for (edge <- path) {
      // Taking care of source edge local information
      var currentState = edge.state
      for (valField <- edge.source.typ.getPossibleFields().filter(f => !f.getType().isObject())) {
        edge.field match {
          case  None =>
          case Some(f) => {
            val edgeLocId: Identifier = new EdgeLocalIdentifier(List.empty[String], valField.getName(), valField.getType(), valField.getProgramPoint())
            val newId = new AccessPathIdentifier(currentPathList :+ valField.getName(), valField.getType(), valField.getProgramPoint())
            addedIdentifiers = addedIdentifiers + newId
            currentState = currentState.rename(List(edgeLocId), List(newId))
          }
        }
      }
      // Set currentId for target
      edge.field match {
        case None => {
          assert(currentPathList.size == 1, "At this point, currentId should be already set")
        }
        case Some(f) => {
          currentPathList = currentPathList :+ f
        }
      }
      currentEdgeSeq = currentEdgeSeq :+ edge
      currentAccPathExp = new AccessPathExpression(pp, edge.source.typ, currentPathList)
      // Taking care of target edge local information
      if (!edge.target.isInstanceOf[NullVertex]) {
        for (valField <- edge.target.typ.getPossibleFields().filter(f => !f.getType().isObject())) {
          val newId = new AccessPathIdentifier(currentPathList :+ valField.getName(), valField.getType(), valField.getProgramPoint())
          addedIdentifiers = addedIdentifiers + newId
          edge.field match {
            case None => {
              val edgeLocId: Identifier = new EdgeLocalIdentifier(List.empty[String], valField.getName(), valField.getType(), valField.getProgramPoint())
              currentState = currentState.rename(List(edgeLocId), List(newId))
            }
            case Some(f) => {
              val edgeLocId: Identifier = new EdgeLocalIdentifier(List(f), valField.getName(), valField.getType(), valField.getProgramPoint())
              currentState = currentState.rename(List(edgeLocId), List(newId))
            }
          }
        }
      }
      sequenceOfStates = sequenceOfStates :+ currentState
      expEdgeMap = expEdgeMap + (currentAccPathExp -> currentEdgeSeq)
    }

    // Now we need to apply the computed changes.
    var index = 0
    var isInitialized = false
    var resultState = generalValState.top()
    for (state <- sequenceOfStates) {
      var newState = Utilities.createVariablesForState(state, addedIdentifiers.asInstanceOf[Set[Identifier]])
      if (!isInitialized) {
        resultState = newState
        isInitialized = true
      }
      resultState = resultState.glb(resultState, newState)
    }
    val finalType = path.last.target.typ
    var finalId: AccessPathExpression = new AccessPathExpression(pp, finalType, currentPathList)
    return (resultState, finalId, addedIdentifiers, expEdgeMap)
  }

  /**
  Assign a cell of an array

   @param obj The object on which the array assignment
  @param index The assigned index
  @param typ The type of the cell
  @param right The assigned expression
  @return The abstract state obtained after the array cell assignment
    */
  def assignArrayCell(obj: ExpressionSet, index: ExpressionSet, right: ExpressionSet, typ: Type): ValueDrivenHeapState[S] = {
    throw new Exception("Method assignArrayCell is not implemented")
  }

  /**
  Assigns an expression to an argument

   @param x The assigned argument
  @param right The expression to be assigned
  @return The abstract state after the assignment
    */
  def setArgument(x: ExpressionSet, right: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method setArgument is not implemented")
  }

  /**
  Forgets the value of a variable

   @param x The variable to be forgotten
  @return The abstract state obtained after forgetting the variable
    */
  def setVariableToTop(x: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method setVariableToTop is not implemented")
  }

  /**
  Removes a variable

   @param x The variable to be removed
  @return The abstract state obtained after removing the variable
    */
  def removeVariable(x: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method removeVariable is not implemented")
  }

  /**
  Throws an exception

   @param t The thrown exception
  @return The abstract state after the thrown
    */
  def throws(t: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method throws is not implemented")
  }

  /**
  Gets the value of a variable

   @param id The variable to access
  @return The abstract state obtained after accessing the variable, that is, the state that contains as expression the symbolic representation of the value of the given variable
    */
  def getVariableValue(id: Assignable): ValueDrivenHeapState[S] = {
    //**println("getVariableValue(" + id.toString + ") is called")
    if(this.isBottom) return this
    return new ValueDrivenHeapState[S](abstractHeap, generalValState, new ExpressionSet(id.getType()).add(id.asInstanceOf[Expression]), isTop, isBottom)
    //throw new Exception("Method getVariableValue is not implemented")
  }

  /**
  Accesses a field of an object

   @param obj The object on which the field access is performed
  @param field The name of the field
  @param typ The type of the field
  @return The abstract state obtained after the field access, that is, the state that contains as expression the symbolic representation of the value of the given field access
    */
  def getFieldValue(obj: List[ExpressionSet], field: String, typ: Type): ValueDrivenHeapState[S] = {
    //**println("getFiledValue(" + obj.head.getSetOfExpressions.head + "," + field + "," + typ + ") is called")
    assert(obj.size == 1, "There should be only one ExpressionSet in obj.")
    assert(obj.head.getSetOfExpressions.size == 1, "We only support single field access.")
    assert(obj.head.getSetOfExpressions.head.isInstanceOf[AccessPathExpression], "The field access should be accessed via access path.")
    // TODO: May be I should check whether this exist and is feasible already here.
    return new ValueDrivenHeapState[S](abstractHeap, generalValState, new ExpressionSet(typ).add(obj.head), isTop, isBottom)
    // throw new Exception("Method getFieldValue is not implemented")
  }

  /**
  Accesses a field of an object

   @param obj The object on which the field access is performed
  @param index The name of the field
  @param typ The type of the field
  @return The abstract state obtained after the field access, that is, the state that contains as expression the symbolic representation of the value of the given field access
    */
  def getArrayCell(obj: ExpressionSet, index: ExpressionSet, typ: Type): ValueDrivenHeapState[S] = {
    throw new Exception("Method getArrayCell is not implemented")
  }

  /**
  Returns the identifier representing the length of the given array

   @param array The array from which we want to access the length
  @return A state that contains as expression the symbolic representation of the length of the given array
    */
  def getArrayLength(array: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method getArrayLength is not implemented")
  }

  /**
  Performs the backward semantics of a variable access

   @param id The accessed variable
  @return The abstract state obtained BEFORE accessing the variable
    */
  def backwardGetVariableValue(id: Assignable): ValueDrivenHeapState[S] = {
    throw new Exception("Method backwardGetVariableValue is not implemented")
  }

  /**
  Performs the backward semantics of a field access

   @param objs The object on which the field access is performed
  @param field The name of the field
  @param typ The type of the field
  @return The abstract state obtained before the field access
    */
  def backwardGetFieldValue(objs: List[ExpressionSet], field: String, typ: Type): ValueDrivenHeapState[S] = {
    throw new Exception("Method backwardGetFieldValue is not implemented")
  }

  /**
  Performs the backward semantics of an assignment

   @param x The assigned variable
  @param right The assigned expression
  @return The abstract state before the assignment
    */
  def backwardAssignVariable(x: ExpressionSet, right: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method backwardAssignVariable is not implemented")
  }

  /**
  Evaluates a numerical constant

   @param value The string representing the numerical constant
  @param typ The type of the numerical constant
  @param pp The program point that contains the constant
  @return The abstract state after the evaluation of the constant, that is, the state that contains an expression representing this constant
    */
  def evalConstant(value: String, typ: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = {
    //**println("evalConsta(" + value + "," + typ + "," + pp + ") is called.")
    if(this.isBottom) return this
    this.setExpression(new ExpressionSet(typ).add(new Constant(value, typ, pp)))
    // throw new Exception("Method evalConstant is not implemented")
  }

  /**
  Assumes that a boolean expression holds

   @param cond The assumed expression
  @return The abstract state after assuming that the expression holds
    */
  def assume(cond: ExpressionSet): ValueDrivenHeapState[S] = {
    if (isBottom) return this
    assert(cond.getSetOfExpressions.size == 1, "Condition of several expressions are not supported.")
    val condition = cond.getSetOfExpressions.head
    condition match {
      case NegatedBooleanExpression(e) => {
        return assume(new ExpressionSet(e.getType()).add(Utilities.negateExpression(e)))
      }
      case baExp : BinaryArithmeticExpression => {
        // TODO: Implement this properly, using the edge sequences from evaluation of expressions
        val evaluatedCondition = evaluateExpression(baExp)
        var result = bottom()
        for (el <- evaluatedCondition._2) {
          val currentState = el._1.assume(evaluatedCondition._1)
          val newGeneralState = currentState.glb(currentState, generalValState)
          if (newGeneralState.lessEqual(newGeneralState.bottom()))
            return bottom()
          val tempAH = abstractHeap.meetStateOnAllEdges(currentState)
          var (resultAH, removedIds) = tempAH.prune()
          if (!resultAH.isBottom()) {
            val currentState = new ValueDrivenHeapState[S](resultAH, Utilities.removeVariablesFromState(newGeneralState, removedIds), getExpression(), false, false)
            result = lub(result, currentState)
          }
        }
        assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
        return result
      }
      case BinaryBooleanExpression(l,r,o,t) => o match {
        case BooleanOperator.&& => {
          val result = assume(new ExpressionSet(l.getType()).add(l)).assume(new ExpressionSet(r.getType()).add(r))
          assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
          return result
        }
        case BooleanOperator.|| => {
          val result = lub(assume(new ExpressionSet(l.getType()).add(l)), assume(new ExpressionSet(r.getType()).add(r)))
          assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
          return result
        }
      }
      case ReferenceComparisonExpression(l,r,o,t) => {
        assert(l.getType().isObject(), "Reference comparison can be performed only on objects, not values.")
        assert(r.getType().isObject(), "Reference comparison can be performed only on objects, not values.")
        l match {
          case cL: Constant => {
            assert(cL.toString().equals("null"))
            r match {
              case cR : Constant => {
                assert(cR.toString().equals("null"))
                o match {
                  case ArithmeticOperator.!= => {
                    return bottom()
                  }
                  case ArithmeticOperator.== => {
                    return this
                  }
                  case _ => throw new Exception("ReferenceComparisonExpression should not have the operator " + o)
                }
              }
              case apR : AccessPathExpression => {
                val evalApR = evaluateExpression(apR)
                if (evalApR._2.isEmpty)
                  return bottom()
                var result = bottom()
                for (singleEvalApR <- evalApR._2) {
                  assert(singleEvalApR._3.get(apR) != None, "This should not happen. Would mean that expression evaluation does not work.")
                  val apPrefixVertex = singleEvalApR._3.apply(apR).last.source
                  var edgesToRemove = Set.empty[EdgeWithState[S]]
                  if (apPrefixVertex.isInstanceOf[DefiniteHeapVertex]) {
                    o match {
                      case ArithmeticOperator.!= => {
                        edgesToRemove = abstractHeap.edges.filter(e => e.source.equals(apPrefixVertex) && e.target.isInstanceOf[NullVertex])
                      }
                      case ArithmeticOperator.== => {
                        edgesToRemove = abstractHeap.edges.filter(e => e.source.equals(apPrefixVertex) && !e.target.isInstanceOf[NullVertex])
                      }
                      case _ => throw new Exception("ReferenceComparisonExpression should not have the operator " + o)
                    }
                  }
                  val tempAH = abstractHeap.removeEdges(edgesToRemove)
                  val (resultingAH, idsToRemove) = tempAH.prune()
                  var newGeneralValState = Utilities.removeVariablesFromState(generalValState, idsToRemove)
                  if (!newGeneralValState.lessEqual(newGeneralValState.bottom()) && !resultingAH.isBottom()) {
                    val newState = new ValueDrivenHeapState[S](resultingAH, newGeneralValState, new ExpressionSet(SystemParameters.getType().top), false, false)
                    result = lub(result, newState)
                  }
                }
                assert(result.abstractHeap.isNormalized(), "The abstract heap is not normalized.")
                return result
              }
              case vR: VariableIdentifier => {
                val varVertex = abstractHeap.vertices.filter(_.name.equals(vR.getName())).head
                var edgesToRemove = Set.empty[EdgeWithState[S]]
                o match {
                  case ArithmeticOperator.!= => {
                    edgesToRemove = abstractHeap.edges.filter(e => e.source.equals(varVertex) && e.target.isInstanceOf[NullVertex])
                  }
                  case ArithmeticOperator.== => {
                    edgesToRemove = abstractHeap.edges.filter(e => e.source.equals(varVertex) && !e.target.isInstanceOf[NullVertex])
                  }
                  case _ => throw new Exception("ReferenceComparisonExpression should not have the operator " + o)
                }
                val tempAH = abstractHeap.removeEdges(edgesToRemove)
                val (resultingAH, idsToRemove) = tempAH.prune()
                var newGeneralValState = Utilities.removeVariablesFromState(generalValState, idsToRemove)
                if (!newGeneralValState.lessEqual(newGeneralValState.bottom()) && !resultingAH.isBottom())
                  return new ValueDrivenHeapState[S](resultingAH, newGeneralValState, new ExpressionSet(SystemParameters.getType().top), false, false)
                else
                  return bottom()
              }
              case _ => throw new Exception("Not supported (e.g. result of method calls).")
            }
          }
          case apL : AccessPathExpression => {
            r match {
              case cR : Constant => {
                return assume(new ExpressionSet(cond.getType()).add(new ReferenceComparisonExpression(r,l,o,t)))
              }
              // TODO: Implement the rest
              case _ => {
                println("Not implemented properly.")
                return this
              }
            }
          }
          case v: VariableIdentifier => {
            r match {
              case cR : Constant => {
                return assume(new ExpressionSet(cond.getType()).add(new ReferenceComparisonExpression(r,l,o,t)))
              }
              // TODO: Implement the rest
              case _ => {
                println("Not implemented properly.")
                return this
              }
            }
          }
          case _ => throw new Exception("Not supported.")
        }
      }
      case _ => throw new Exception("Not supported.")
    }





//    for (el <- evaluatedCondition._2) {
//      val currentState = el._1.assume(evaluatedCondition._1)
//      val newGeneralState = currentState.glb(currentState, generalValState)
//      if (newGeneralState.lessEqual(newGeneralState.bottom()))
//        return bottom()
//      val (resultAH, removedIds, resultIsBottom) = abstractHeap.meetStateOnAllEdges(currentState)
//      if (resultIsBottom)
//        return bottom()
//      else
//        return new ValueDrivenHeapState[S](resultAH, Utilities.removeVariablesFromState(newGeneralState, removedIds), getExpression(), false, false)
//    }
  }

  /**
  Assumes that the current expression holds

   @return The abstract state after assuming that the expression holds
    */
  def testTrue(): ValueDrivenHeapState[S] = {
    //**println("testTrue() is called")
    return assume(getExpression())
//    throw new Exception("Method testTrue is not implemented")
  }

  /**
  Assumes that the current expression does not hold

   @return The abstract state after assuming that the expression does not hold
    */
  def testFalse(): ValueDrivenHeapState[S] = {
    val negatedExpressions = getExpression().getSetOfExpressions.map(exp => new NegatedBooleanExpression(exp))
    var negatedExpSet = new ExpressionSet(getExpression().getType())
    for (ne <- negatedExpressions)
      negatedExpSet = negatedExpSet.add(ne)
    return assume(negatedExpSet)
//    throw new Exception("Method testFalse is not implemented")
  }

  /**
  Returns the current expression

   @return The current expression
    */
  def getExpression(): ExpressionSet = {
    //**println("getExpression is called")
    return expr
    //throw new Exception("Method getExpression is not implemented")
  }

  /**
  Sets the current expression

   @param newExpr The current expression
  @return The abstract state after changing the current expression with the given one
    */
  def setExpression(newExpr: ExpressionSet): ValueDrivenHeapState[S] = {
    //**println("setExpression(" + newExpr + ") is called")
    return new ValueDrivenHeapState[S](abstractHeap, generalValState, newExpr, isTop, isBottom)
    //throw new Exception("Method setExpression is not implemented")
  }

  /**
  Removes the current expression

   @return The abstract state after removing the current expression
    */
  def removeExpression(): ValueDrivenHeapState[S] = {
    //**println("removeExpression() is called")
    return new ValueDrivenHeapState(abstractHeap, generalValState, new ExpressionSet(SystemParameters.getType().top), isTop, isBottom)
    //throw new Exception("Method removeExpression is not implemented")
  }

  /**
  Create a collection (set, map, list...)

  @param collTyp The type of the collection
  @param keyTyp The type of the key
  @param valueTyp The type of the value
  @param lengthTyp The type of the length (integer/number)
  @param tpp The program point of creation
  @return The abstract state after the creation of the collection
    */
  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, tpp: ProgramPoint): ValueDrivenHeapState[S] = {
    throw new Exception("Method createCollection is not implemented")
  }

  /**
  Assign a cell of an collection

  @param collectionSet The set of collection expressions
  @param keySet The set of key expressions
  @param rightSet The set of values
  @return The abstract state obtained after the collection cell assignment
    */
  def assignCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method assignCollectionCell is not implemented")
  }

  /**
  Insert a cell of an collection at the given index

  @param collectionSet The set of collection expressions
  @param keySet The set of key expressions
  @param rightSet The set of values
  @return The abstract state obtained after the collection cell assignment
    */
  def insertCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method insertCollectionCell is not implemented")
  }

  /**
  Remove a cell of an collection

  @param collectionSet The set of collection expressions
  @param keySet The set of key expressions
  @return The abstract state obtained after the collection cell assignment
    */
  def removeCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method removeCollectionCell is not implemented")
  }

  /**
  Accesses a cell of a collection

  @param collectionSet The set of collection expressions
  @param keySet The set of key expressions
  @return The abstract state obtained after the field access, that is, the state that contains as expression the symbolic representation of the value of the given field access
    */
  def getCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method getCollectionCell is not implemented")
  }

  /**
   * Clears a collection
   */
  def clearCollection(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method clearCollection is not implemented")
  }

  /**
  Returns the identifier representing the length of the given collection
   @param collectionSet The collection from which we want to access the length
  @return A state that contains as expression the symbolic representation of the length of the given collection
    */
  def getCollectionLength(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = {
    throw new Exception("Method getCollectionLength is not implemented")
  }


  /**
   * Performs abstract garbage collection
   */
  def pruneUnreachableHeap(): ValueDrivenHeapState[S] = {
    throw new Exception("Method pruneUnreachableHeap is not implemented")
  }

  /**
  Returns a new instance of the lattice
   @return A new instance of the current object
    */
  def factory(): ValueDrivenHeapState[S] = {
    throw new Exception("Method factory is not implemented")
  }

  /**
  Returns the top value of the lattice
   @return The top value, that is, a value x that is greater or equal than any other value
    */
  def top(): ValueDrivenHeapState[S] = {
    //**println("top() is called")
    return new ValueDrivenHeapState[S](new HeapGraph[S](), new ApronInterface(None, new Polka(false), false, Set.empty[Identifier]).top().asInstanceOf[S], new ExpressionSet(SystemParameters.getType().top()), true, false)
    throw new Exception("Method top is not implemented")
  }

  /**
  Returns the bottom value of the lattice
   @return The bottom value, that is, a value x that is less or equal than any other value
    */
  def bottom(): ValueDrivenHeapState[S] = {
    //**println("bottom() is called")
    return new ValueDrivenHeapState[S](new HeapGraph[S](), generalValState.bottom(), expr, false, true)
//    throw new Exception("Method bottom is not implemented")
  }

  /**
  Computes the upper bound of two elements

   @param left One of the two values
  @param right The other value
  @return The least upper bound, that is, an element that is greater or equal than the two arguments
    */
  def lub(left: ValueDrivenHeapState[S], right: ValueDrivenHeapState[S]): ValueDrivenHeapState[S] = {
    //**println("lub(" + left.toString() + ", " + right.toString() + ") is called")
    if (left.isBottom || right.isTop)
      return right
    if (left.isTop || right.isBottom)
      return left
    // TODO: Implement this properly
    val (resAH, renameRightFrom, renameRightTo) = left.abstractHeap.lub(left.abstractHeap, right.abstractHeap)
    val resGeneralState = left.generalValState.lub(left.generalValState, right.generalValState.rename(renameRightFrom, renameRightTo))

    //**println("REAL LUB IS CALLED.")

    return new ValueDrivenHeapState[S](resAH, resGeneralState, new ExpressionSet(SystemParameters.getType().top()), false, false)

//    return new ValueDrivenHeapState[S](right.abstractHeap,
//                                       right.generalValState.lub(left.generalValState, right.generalValState),
//                                       right.expr, false, false)
//    throw new Exception("Method lub is not implemented")
  }

  /**
  Computes the greatest lower bound of two elements

   @param left One of the two values
  @param right The other value
  @return The greatest upper bound, that is, an element that is less or equal than the two arguments, and greater or equal than any other lower bound of the two arguments
    */
  def glb(left: ValueDrivenHeapState[S], right: ValueDrivenHeapState[S]): ValueDrivenHeapState[S] = {
    if (left.isBottom || right.isBottom)
      return bottom()
    if (left.isTop)
      return right
    if (right.isTop)
      return left
    val (resultingAH, removeIds, renameFrom, renameTo) = left.abstractHeap.glb(left.abstractHeap, right.abstractHeap)
    var newRightGeneralValState = Utilities.removeVariablesFromState(right.generalValState, removeIds)
    newRightGeneralValState = newRightGeneralValState.rename(renameFrom, renameTo)
    val newGeneralValState = left.generalValState.glb(left.generalValState, newRightGeneralValState)
    if (resultingAH.isBottom() || newGeneralValState.lessEqual(newGeneralValState.bottom()))
      return bottom()
    return new ValueDrivenHeapState[S](resultingAH, newGeneralValState, new ExpressionSet(SystemParameters.getType().top), false, false)
    throw new Exception("Method glb is not implemented")
  }

  /**
  Computes widening of two elements

   @param left The previous value
  @param right The new value
  @return The widening of <code>left</code> and <code>right</code>
    */
  def widening(left: ValueDrivenHeapState[S], right: ValueDrivenHeapState[S]): ValueDrivenHeapState[S] = {
    //**println("WIDENING IS CALLED")
    val (mergedLeft, replacementLeft) = left.abstractHeap.mergePointedNodes()
    val (mergedRight, replacementRight) = right.abstractHeap.mergePointedNodes()
    if (!mergedLeft.vertices.equals(mergedRight.vertices)) {
//      val newLeft = new ValueDrivenHeapState[S](mergedLeft, left.generalValState.merge(replacementLeft), new ExpressionSet(SystemParameters.getType().top), false, false)
//      val newRight = new ValueDrivenHeapState[S](mergedRight, right.generalValState.merge(replacementRight), new ExpressionSet(SystemParameters.getType().top), false, false)
      val result = lub(left, right)
      return result
    }
    val newGeneralValState = generalValState.widening(left.generalValState.merge(replacementLeft), right.generalValState.merge(replacementRight))
    val result = new ValueDrivenHeapState[S](mergedLeft.wideningAfterMerge(mergedLeft, mergedRight), newGeneralValState, new ExpressionSet(SystemParameters.getType().top), false, false)
    return result
//    return left.lub(left, right)
//    throw new Exception("Method widening is not implemented")
  }

  /**
  Returns true iff <code>this</code> is less or equal than <code>r</code>

   @param r The value to compare
  @return true iff <code>this</code> is less or equal than <code>r</code>
    */
  def lessEqual(r: ValueDrivenHeapState[S]): Boolean = {
    // TODO: Implement properly
    //**println("lessEqua(" + r.toString + ") is called on " + this.toString)
    if (isBottom)
      return true
    if (r.isBottom)
      return false
    if (abstractHeap.vertices.size <= r.abstractHeap.vertices.size && generalValState.lessEqual(r.generalValState)) {
      for (edge <- abstractHeap.edges) {
        val rEdges = r.abstractHeap.edges.filter(e => e.weakEquals(edge))
        if (rEdges.isEmpty)
          return false
        val rEdgeState = rEdges.head.state
        if (!edge.state.lessEqual(rEdgeState))
          return false
      }
      return true
    } else {
//    } else {
//      if (abstractHeap.vertices.size <= r.abstractHeap.vertices.size) {
//        if (widening(this, r).lessEqual(this))
//          return true
//        else
//          return false
//      }
//    }
      return false
    }
//    throw new Exception("Method lessEqual is not implemented")
  }

//  override def toString() : String = {
//    val eol = System.getProperty("line.separator");
//    var result = "ValState : " + generalValState.toString
//    result = result + eol + "; AbstractHeap: " + abstractHeap.toString
//    return result
//  }

  /**
  Gets the Identifier of all the keys of the collection that match the given key expresssion.
     A key expression (key) matches a Identifier if the Identifier represents a key of the collection
     and has value k assigned such that
        lub(k, key) != bottom

    @param collectionSet  The collection expressions
  @param keySet The key expressions
  @return The state that has the mapped Identifier as expression
    */
  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = ???

  /**
  Gets the Identifier of all the values of the collection for which the key Identifier matches the given
    key expression.
    A key expression (key) matches a Identifier if the Identifier represents a key of the collection
    and has value k assigned such that
        lub(k, key) != bottom

    @param collectionSet  The collection expressions
  @param keySet The key expressions
  @param valueTyp The type of the collection's values
  @return The state that has the mapped Identifier as expression
    */
  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet, valueTyp: Type): ValueDrivenHeapState[S] = ???

  /**
  Gets the HeapIdentifier of all the values of the collection that match the given value expresssion.
    A value expression (value) matches a HeapIdentifier if the Heapidentifier represents a value of the collection
    and has value v assigned such that
        lub(v, value) != bottom

    @param collectionSet  The collection expressions
  @param valueSet The value expressions
  @return The state that has the mapped HeapIdentifiers as expression
    */
  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): ValueDrivenHeapState[S] = ???

  /**
  Creates a new collection that contains all keys of the provided collection (fromCollection)
    as values.

    @param fromCollection The collection from which the keys shall be extracted
  @param collTyp  The collection type of the newly created collection
  @param keyTyp  The key type of the newly created collection
  @param valueTyp The value type of the newly created collection
  @param lengthTyp  The length type of the newly created collection@param pp
  @return The state that contains the newly created collection and has it's CollectionHeapIdentifier as expression
    */
  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = ???

  /**
  Copies all the key-value tuples from one collection to the other.

    @param fromCollectionSet The collection from which the tuples are copied.
  @param toCollectionSet The collection to which the tuples are copied to.
  @param keyTyp  The key type of the collections
  @param valueTyp  The value type of the collection
  @return The state that has a copy of all the tuples of the fromCollection in the toCollection
    */
  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet, keyTyp: Type, valueTyp: Type): ValueDrivenHeapState[S] = ???

  /**
  Creates a new key-value tuple and adds it to the collection.

    @param collectionSet The collection to which the key-value pair shall be added
  @param keySet  The expression that is assigned to the key node
  @param rightSet  The expression that is assigned to the value node
  @param pp  The program point that the new tuple shall have.
               Key-value tuples of a collection are distinguished by their program point.
               If a tuple with this program point already exists in the collection the new tuple
               will be summarized with this tuple.
  @return The state that contains the collection with the added collection-tuple
    */
  def insertCollectionValue(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint): ValueDrivenHeapState[S] = ???

  /**
  Removes the values from the collection who's key identifiers match the given key expression.
    If the key expression (k) matches the key identifier's assigned value exactly, the tuple is completely removed.
    Otherwise the tuple is not removed but the semantic state contains the assumption
      key identifier != k

    @param collectionSet The collection from which the value is removed
  @param keySet The key expressions
  @param valueTyp The value type of the collection
  @return The state in which the collection is represented without the collection value
    */
  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet, valueTyp: Type): ValueDrivenHeapState[S] = ???

  /**
  Removes the values from the collection who's value identifiers match the given value expression.
    If the value expression (v) matches the value identifier's assigned value exactly, the tuple is completely removed.
    Otherwise the tuple is not removed but the semantic state contains the assumption
      value identifier != v

    @param collectionSet The collection from which the value is removed
  @param valueSet The value expressions
  @param keyTyp The key type of the collection
  @return The state in which the collection is represented without the collection value
    */
  def removeCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, keyTyp: Type): ValueDrivenHeapState[S] = ???

  /**
   * Assigns the value expression to all key identifiers of the collection.
   *
   * @param collectionSet The collection
   * @param valueSet  The value expression
   * @param keyTyp  The collection's key type
   * @return  The state in which all the key identifiers of the collection have the value expression assigned
   */
  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet, keyTyp: Type): ValueDrivenHeapState[S] = ???

  /**
   * Removes all the key-value tuples from a collection and sets it's length to 0.
   * @param collectionSet The collection to be cleared
   * @param keyTyp The key type of the collection
   * @param valueTyp The value type of the collection
   * @return The state with the cleared collection
   */
  def clearCollection(collectionSet: ExpressionSet, keyTyp: Type, valueTyp: Type): ValueDrivenHeapState[S] = ???

  /**
   * Indicates whether any collection in the ExpressionSet represents multiple collections.
   *
   * @param collectionId The collection set
   * @return True if any collection is a summary node, false otherwise.
   */
  def isSummaryCollection(collectionSet: ExpressionSet): Boolean = ???

  /**
  Creates an object

  @param typ The dynamic type of the created object
  @param pp The point of the program that creates the object
  @param fields If this is defined, the given fields will be created instead of the types fields (e.g. for reducing
                  the set of initialized fields)
  @return The abstract state after the creation of the object
    */
  def createObject(typ: Type, pp: ProgramPoint, fields: Option[Set[Identifier]]): ValueDrivenHeapState[S] = {
    //**println("creatObject(" + typ + "," + pp + "," + fields + ") is called.")
    if (this.isBottom) return this

    var resIds = Set.empty[Identifier]
    var (newAbstractHeap, newVertex) = abstractHeap.addNewVertex(VertexConstants.DEFINITE, typ)
    assert(newVertex.isInstanceOf[DefiniteHeapVertex], "The newly created object should be definite")
    val createdObjVertex = newVertex.asInstanceOf[DefiniteHeapVertex]

    //    var edgeLocalIds = Set.empty[Identifier]
    var resIdsAndEdgeLocalIds = Set.empty[(Identifier, Identifier)]

    for (valField <- typ.getPossibleFields().filter(of => !of.getType().isObject())) {
      // This means that we have a value field and this should be included in all abstract states on edges
      // This is done via Replacement
      val resId = new ValueHeapIdentifier(newVertex.asInstanceOf[DefiniteHeapVertex], valField.getName(), valField.getType(), valField.getProgramPoint())
      val edgeLocalId = new EdgeLocalIdentifier(List.empty[String], valField.getName(), valField.getType(), valField.getProgramPoint())
      resIdsAndEdgeLocalIds = resIdsAndEdgeLocalIds + ((resId, edgeLocalId))
      resIds= resIds + resId
      //      edgeLocalIds = edgeLocalIds + new EdgeLocalIdentifier(List.empty[String], valField.getName(), valField.getType(), valField.getProgramPoint())
    }
    //    val resAH = newAbstractHeap.createVariablesInAllStates(resIds)
    newAbstractHeap = newAbstractHeap.createVariablesInAllStates(resIds)
    var newGeneralState = generalValState
    for ((id,_) <- resIdsAndEdgeLocalIds)
      newGeneralState = newGeneralState.createVariable(id, id.getType())
    for (objField <- typ.getPossibleFields().filter(of => of.getType().isObject())) {
      val res = newAbstractHeap.addNewVertex(VertexConstants.NULL, typ)
      newAbstractHeap = res._1
      newVertex = res._2
      var edgeState = newGeneralState
      for ((resId, edgeLocalId) <- resIdsAndEdgeLocalIds) {
        //        edgeState = edgeState.createVariable(resId, resId.getType())
        edgeState = edgeState.createVariable(edgeLocalId, edgeLocalId.getType())
        edgeState = edgeState.assume(new BinaryArithmeticExpression(resId,edgeLocalId, ArithmeticOperator.==, null))
      }
      newAbstractHeap = newAbstractHeap.addEdges(Set(new EdgeWithState[S](createdObjVertex, edgeState, Some(objField.getName()), newVertex)))
    }

    //    for (objField <- typ.getPossibleFields().filter(of => of.getType().isObject())) {
    //      val res = newAbstractHeap.addNewVertex(VertexConstants.NULL, typ)
    //      newAbstractHeap = res._1
    //      newVertex = res._2
    //      var edgeState = generalValState
    //      for (edgeId <- edgeLocalIds)
    //        edgeState = edgeState.createVariable(edgeId, edgeId.getType())
    //      newAbstractHeap = newAbstractHeap.addEdges(Set(new EdgeWithState[S](createdObjVertex, edgeState, Some(objField.getName()), newVertex)))
    //    }

    // Now we need to apply the replacement to all the states, including the general value state.

    return new ValueDrivenHeapState[S](newAbstractHeap,
      newGeneralState,
      new ExpressionSet(typ).add(new VertexExpression(pp, typ, createdObjVertex)),
      isTop, isBottom)

    //    throw new Exception("Method createObject is not implemented")
  }

  /**
  Creates an empty collection.

    @param collTyp  The type of the collection
  @param keyTyp The type of the collection's keys
  @param valueTyp The type of the collection's values
  @param lengthTyp The type of the collection length
  @param tpp  The program point at which the collection is created
    */
  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, keyCollectionTyp: Option[Type], tpp: ProgramPoint, fields: Option[Set[Identifier]]): ValueDrivenHeapState[S] = ???

  /**
   * Returns for each collection in the collectionSet either the collection identifier or if a summary collection for
   * this collection identifier exists the identifier of the summary collection
   *
   * @param collectionSet
   * @return The state with either the summary collection identifier or the collection identifier in the expression
   */
  def getSummaryCollectionIfExists(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???

  /**
   * Gets the values that are stored at the Collection Tuple Value Identifiers.
   *
   * @param valueIds  The Collection Tuple Value Identifiers
   * @return  The state with the values as expression
   */
  def getCollectionValue(valueIds: ExpressionSet): ValueDrivenHeapState[S] = ???

  def insertCollectionTopElement(collectionSet: ExpressionSet, keyTop: ExpressionSet, valueTop: ExpressionSet, pp: ProgramPoint): ValueDrivenHeapState[S] = ???

  /**
  Gets the Identifier of all the values of the collection for which the key Identifier matches the given
    key expression.
    A key expression (key) matches a Identifier if the Identifier represents a key of the collection
    and has value k assigned such that
        lub(k, key) != bottom

    @param collectionSet  The collection expressions
  @param keySet The key expressions
  @param valueTyp The type of the collection's values
  @return The state that has the mapped Identifier as expression
    */
  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = ???

  /**
  Creates a new collection that contains all keys of the provided collection (fromCollection)
    as values.

    @param fromCollection The collection from which the keys shall be extracted
  @param collTyp  The collection type of the newly created collection
  @param keyTyp  The key type of the newly created collection
  @param valueTyp The value type of the newly created collection
  @param lengthTyp  The length type of the newly created collection@param pp
  @return The state that contains the newly created collection and has it's CollectionHeapIdentifier as expression
    */
  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, fromCollectionTyp: Type, collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = ???

  //TODO: comment
  def getOriginalCollection(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???

  def getKeysCollection(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???

  def removeCollectionKeyConnection(origCollectionSet: ExpressionSet, keyCollectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???

  /**
  Copies all the key-value tuples from one collection to the other.

    @param fromCollectionSet The collection from which the tuples are copied.
  @param toCollectionSet The collection to which the tuples are copied to.
  @param keyTyp  The key type of the collections
  @param valueTyp  The value type of the collection
  @return The state that has a copy of all the tuples of the fromCollection in the toCollection
    */
  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???

  /**
  Creates a new key-value tuple and adds it to the collection.

    @param collectionSet The collection to which the key-value pair shall be added
  @param keySet  The expression that is assigned to the key node
  @param rightSet  The expression that is assigned to the value node
  @param pp  The program point that the new tuple shall have.
               Key-value tuples of a collection are distinguished by their program point.
               If a tuple with this program point already exists in the collection the new tuple
               will be summarized with this tuple.
  @return The state that contains the collection with the added collection-tuple
    */
  def insertCollectionElement(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint): ValueDrivenHeapState[S] = ???

  /**
  Removes the values from the collection who's key identifiers match the given key expression.
    If the key expression (k) matches the key identifier's assigned value exactly, the tuple is completely removed.
    Otherwise the tuple is not removed but the semantic state contains the assumption
      key identifier != k

    @param collectionSet The collection from which the value is removed
  @param keySet The key expressions
  @return The state in which the collection is represented without the collection value
    */
  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = ???

  /**
   * Removes the first occurence of the value in a collection.
   *
   * @param collectionSet The set of collections from which the value is removed
   * @param valueSet The value to be removed
   * @return The state in which the collection is represented without the first occurence of the collection value
   */
  def removeFirstCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): ValueDrivenHeapState[S] = ???

  /**
   * Assigns the value expression to all key identifiers of the collection.
   *
   * @param collectionSet The collection
   * @param valueSet  The value expression
   * @return  The state in which all the key identifiers of the collection have the value expression assigned
   */
  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet): ValueDrivenHeapState[S] = ???

  def collectionContainsKey(collectionSet: ExpressionSet, keySet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = ???

  def collectionContainsValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = ???

  /**
   * Removes all variables satisfying filter
   */
  def pruneVariables(filter: (Identifier) => Boolean): ValueDrivenHeapState[S] = ???

  /**
   * Detects summary nodes that are only reachable via a single access path and converts
   * them to non-summary nodes
   */
  def optimizeSummaryNodes(): ValueDrivenHeapState[S] = ???
}

class ValueDrivenHeapAnalysis extends Analysis {

  def getLabel(): String = "Value Driven Heap analysis"

  def parameters(): List[(String, Any)] = Nil

  def setParameter(label: String, value: Any) {
//    label match {
//      case "Domain" => value match {
//        case "Interval" => domain = new Box()
//        case "PPL" => domain = new Polka(false) //new PplPoly(false); FIXIT: Change back to PPL
//        case "Octagons" => domain = new Octagon()
//        case "Polka" => domain = new Polka(false)
//        case "Linear equalities" => domain = new PolkaEq()
//      }
//    }
  }

  def reset() {}

  def getInitialState(): ValueDrivenHeapState[ApronInterface] = new ValueDrivenHeapState[ApronInterface](new ExpressionSet(SystemParameters.getType.top)).top()

  def getProperties(): List[Property] = List(
//    new ShowGraphProperty().asInstanceOf[Property]
//    new SingleStatementProperty(DivisionByZero),
//    new SingleStatementProperty(new LowerBoundedValue("y", 0)),
//    new SingleStatementProperty(new BoundedValue("y", -4, 4))
  )

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = List(ObjectNativeMethodSemantics, IntegerNativeMethodSemantics, BooleanNativeMethodSemantics)

}

case class ValueHeapIdentifier(obj: HeapVertex, field: String, typ1 : Type, pp : ProgramPoint) extends Identifier(typ1, pp) {
  /**
  Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
   if they are the same identifier
   @return The name of the identifier
    */
  def getName(): String = obj.name + "." + field

  /**
  Returns the name of the field that is represented by this identifier if it is a heap identifier.

   @return The name of the field pointed by this identifier
    */
  def getField(): Option[String] = Some(field)

  /**
  Since an abstract identifier can be an abstract node of the heap, it can represent more than one concrete
   identifier. This function tells if a node is a summary node.

   @return true iff this identifier represents exactly one variable
    */
  def representSingleVariable(): Boolean = obj.isInstanceOf[DefiniteHeapVertex]

  def identifiers(): Set[Identifier] = Set(this)

  override def toString(): String = obj.name + "." + field

  override def hashCode(): Int = {
    return toString.hashCode()
  }

  override def equals(obj : Any): Boolean = obj match {
    case x : ValueHeapIdentifier =>
      this.toString().equals(x.toString())
    case _ => false
  }
}

case class EdgeLocalIdentifier(accPath: List[String],val field: String, typ1: Type, pp: ProgramPoint) extends Identifier(typ1, pp) {

  assert(!typ1.isObject(), "EdgeLocalIdentifier should represent value information.")
  assert(accPath.size <= 1, "For now, we allow at most single step look-ahead.")

  /**
  Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
   if they are the same identifier
   @return The name of the identifier
    */
  def getName(): String = {
    var result = ValeDrivenHeapStateConstants.edgeLocalIdentifier
    for (ap <- accPath)
      result = result + "." + ap
    result = result + "." + field
    return result
  }

  /**
  Returns the name of the field that is represented by this identifier if it is a heap identifier.

   @return The name of the field pointed by this identifier
    */
  def getField(): Option[String] = Some(field)

  /**
  Edge-local identifier always represents a field of a single object. Hence, this method always returns true.

   @return true
    */
  def representSingleVariable(): Boolean = true

  def identifiers(): Set[Identifier] = Set(this)

  override def toString(): String = getName

  override def hashCode(): Int = {
    return toString.hashCode()
  }

  override def equals(obj : Any): Boolean = obj match {
    case x: EdgeLocalIdentifier =>
      this.toString().equals(x.toString())
    case _ => false
  }
}

case class AccessPathIdentifier(accPath: List[String], typ1: Type, pp: ProgramPoint, repSingle: Boolean = true) extends Identifier(typ1, pp) {
  assert(accPath.size > 0, "The access path should not be empty.")

  /**
  Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
   if they are the same identifier
   @return The name of the identifier
    */
  def getName(): String = {
    var result = ""
    for (s <- accPath.dropRight(1)) {
      result = result + s + "."
    }
    return result + accPath.last
  }

  /**
  Returns the name of the field that is represented by this identifier if it is a heap identifier.

   @return The name of the field pointed by this identifier
    */
  def getField(): Option[String] = ???

  /**
  Since an abstract identifier can be an abstract node of the heap, it can represent more than one concrete
   identifier. This function tells if a node is a summary node.

   @return true iff this identifier represents exactly one variable
    */
  def representSingleVariable(): Boolean = repSingle

  def identifiers(): Set[Identifier] = ???

  override def equals(obj: Any): Boolean = obj match {
    case other: AccessPathIdentifier => other.getName().equals(getName())
    case _ => false
  }

  override def hashCode(): Int = getName().hashCode()

  override def toString(): String = getName()
}

case class VertexExpression(pp : ProgramPoint, typ: Type, vertex: Vertex) extends Expression(pp) {

  def getType(): Type = typ

  def identifiers(): Set[Identifier] = throw new Exception("identifiers() should never be called!")

  override def toString(): String = vertex.name

  override def hashCode(): Int = {
    return toString.hashCode()
  }

  override def equals(obj : Any): Boolean = obj match {
    case VertexExpression(_, objTyp, objVertex) =>
      this.typ.equals(objTyp) && this.vertex.equals(objVertex)
    case _ => false
  }

  /**
   * Runs f on the expression and all sub-expressions
   *
   * This also replaces identifiers inside heap ID sets.
   *
   * @param f the transformer
   * @return the transformed expression
   */
  def transform(f: (Expression) => Expression): Expression = ???
}

object ValeDrivenHeapStateConstants {
  val edgeLocalIdentifier = "eLocId"
}



