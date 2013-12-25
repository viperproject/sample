package graph

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{StaticModifier, NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.property._
import apron.{Box, Octagon, Polka}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.{BooleanNativeMethodSemantics, IntegerNativeMethodSemantics, ObjectNativeMethodSemantics}
import scala.collection.mutable

case class ValueDrivenHeapState[S <: SemanticDomain[S]](
    abstractHeap: HeapGraph[S],
    generalValState: S,
    expr: ExpressionSet,
    isTop: Boolean,
    isBottom: Boolean) extends State[ValueDrivenHeapState[S]] {

  require(!isTop || !isBottom, "cannot be top and bottom at the same time")

  def this(newExpr: ExpressionSet) = this(new HeapGraph[S](), new ApronInterface(None, new Polka(false), env =  Set.empty[Identifier]).top().asInstanceOf[S], newExpr, false, false)
//  def this(newExpr: ExpressionSet) = this(new HeapGraph[S](), new ApronInterface(None, new Octagon(), env = Set.empty[Identifier]).top().asInstanceOf[S], newExpr, false, false)
//  def this(newExpr: ExpressionSet) = this(new HeapGraph[S](), new ApronInterface(None, new Box(), env = Set.empty[Identifier]).top().asInstanceOf[S], newExpr, false, false)

  def this(newAbstractHeap: HeapGraph[S], newGeneralValState: S, newExpr: ExpressionSet)  = this(newAbstractHeap,newGeneralValState,newExpr, false, false)

  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = {
    if(this.isBottom) return this
    if(x.getSetOfExpressions.size != 1 || !x.getSetOfExpressions.head.isInstanceOf[VariableIdentifier])
      throw new SymbolicSemanticException("Cannot declare multiple variables together")
    var result = this
    for(el <- x.getSetOfExpressions) {
      el match {
        case variable : VariableIdentifier => {
          if (variable.getType.isObject()) {
            result = result.copy(
              abstractHeap = result.abstractHeap.addNewVertex(variable.getName, typ)._1,
              expr = new ExpressionSet(typ).add(variable)
            )
          } else {
            result = result.copy(
              abstractHeap = result.abstractHeap.createVariablesInAllStates(Set(variable)),
              generalValState = result.generalValState.createVariable(variable, variable.getType),
              expr = new ExpressionSet(typ).add(variable)
            )
          }
        }
        case _ => throw new Exception("Only variables can be created.")
      }
    }
    result
  }

  def createVariableForArgument(x: ExpressionSet, typ: Type): ValueDrivenHeapState[S] = {
    if(this.isBottom) return this
    if(x.getSetOfExpressions.size != 1)
      throw new Exception("Cannot declare multiple variables together.")
    var result = bottom()
    for(el <- x.getSetOfExpressions) {
      el match {
        case variable : VariableIdentifier => {
          if (variable.getType.isObject()) {
            // If the variable is an object, we need to create an object for a method argument. This is different than
            // the normal object creation as we need to create such an object as Top and it can possibly alias(and be
            // aliased by any) other argument (or this).
            /**
             * STRATEGY:
             * We inspect the type and figure out whether such object already exists in the abstract heap. If so, we
             * check, whether it is a summary or a definite node. If it is definite, we change it to summary. Than we
             * point the variable to the summary node. If the object of the given type does not exist, we create it.
             * We need to create objects for all the types that are reachable via fields of the given object. We need
             * to take under consideration also the aliasing information.
             *
             * Furthermore, an argument may also point to null.
             */
            // We first check, whether an object of the given type already exists.
            assert(abstractHeap.vertices.count(n => n.isInstanceOf[HeapVertex] && n.typ.equals(typ)) <= 1, "There should not be more than one heap node of a given type when creating arguments. If so, it should be a single summary node.")
            // Let us first create all the vertices and only then the edges.
            var definiteTypes = abstractHeap.vertices.filter(_.isInstanceOf[DefiniteHeapVertex]).map(_.typ)
            var summaryTypes = abstractHeap.vertices.filter(_.isInstanceOf[SummaryHeapVertex]).map(_.typ)
            val typeStack = mutable.Stack(typ)
            while (!typeStack.isEmpty) {
              val typeAtTop = typeStack.pop()
              if (!definiteTypes.contains(typeAtTop) && !summaryTypes.contains(typeAtTop)) {
                definiteTypes = definiteTypes + typeAtTop
              } else if (definiteTypes.contains(typeAtTop)) {
                definiteTypes = definiteTypes - typeAtTop
                summaryTypes = summaryTypes + typeAtTop
              }
              for (objectField <- typeAtTop.objectFields)
                if (!summaryTypes.contains(objectField.getType))
                  typeStack.push(objectField.getType)
            }
            var newVertices = abstractHeap.vertices.filter(_.isInstanceOf[LocalVariableVertex])
            var idsToCreate = generalValState.getIds().filter(_.isInstanceOf[VariableIdentifier])
            // Add null vertex and LocalVariableVertex that represents the argument under creation
            val nullVertex = new NullVertex()
            newVertices = newVertices ++ Set(nullVertex, new LocalVariableVertex(variable.getName, variable.getType))
            // The vertex version (bit of a hack but more efficient than creating new HeapGraph, needs refactoring)
            var vertexId = 0
            // Adding definite vertices and corresponding identifiers
            for (defType <- definiteTypes) {
              val defVertexToAdd = new DefiniteHeapVertex(vertexId, defType)
              vertexId = vertexId + 1
              newVertices = newVertices + defVertexToAdd
              for (valField <- defType.nonObjectFields)
                idsToCreate = idsToCreate + ValueHeapIdentifier(defVertexToAdd, valField)
            }
            // Adding summary vertices and corresponding identifiers
            for (sumType <- summaryTypes) {
              val sumVertexToAdd = new SummaryHeapVertex(vertexId, sumType)
              vertexId = vertexId + 1
              newVertices = newVertices + sumVertexToAdd
              for (valField <- sumType.nonObjectFields)
                idsToCreate = idsToCreate + ValueHeapIdentifier(sumVertexToAdd, valField)
            }
            var newGenValState = generalValState.factory()
            newGenValState = newGenValState.createVariables(idsToCreate)
            // Create edges between HeapVertices taking into account sub-typing.
            val resultingEdges = scala.collection.mutable.Set.empty[EdgeWithState[S]]
            for (heapVertex <- newVertices.collect({ case v: HeapVertex => v })) {
              // Setting up source EdgeLocalIdentifiers
              var sourceValState = newGenValState
              for (valField <- heapVertex.typ.nonObjectFields) {
                val srcEdgeLocId = EdgeLocalIdentifier(List.empty[String], valField)
                val valHeapId = ValueHeapIdentifier(heapVertex, valField)
                sourceValState = sourceValState.createVariable(srcEdgeLocId, srcEdgeLocId.getType)
                sourceValState = sourceValState.assume(new BinaryArithmeticExpression(valHeapId, srcEdgeLocId, ArithmeticOperator.==, null))
              }
              for (objField <- heapVertex.typ.objectFields) {
                // objField can always point to null (which has no target EdgeLocalIdentifiers)
                resultingEdges += EdgeWithState(heapVertex, sourceValState, Some(objField.getName), nullVertex)
                // Finding all possible HeapVertices to which this object field can point to, taking into account sub-typing
                for (canPointToVertex <- newVertices.collect({ case v: HeapVertex if v.typ.lessEqual(objField.getType) => v })) {
                  var trgValState = sourceValState
                  for (objValField <- objField.getType.nonObjectFields) {
                    val trgEdgeLocId = EdgeLocalIdentifier(List(objField.getName), objValField)
                    val valHeapId = ValueHeapIdentifier(heapVertex, objField)
                    trgValState = trgValState.createVariable(trgEdgeLocId, trgEdgeLocId.getType)
                    trgValState = trgValState.assume(new BinaryArithmeticExpression(valHeapId, trgEdgeLocId, ArithmeticOperator.==, null))
                  }
                  resultingEdges += EdgeWithState(heapVertex, trgValState, Some(objField.getName), canPointToVertex)
                }
              }
            }
            // Creating edges from LocalVariableVertices to HeapVertices (with sub-typing) and to NullVertex (except for "this" variable)
            for (locVarVertex <- newVertices.collect({ case v: LocalVariableVertex => v })) {
              // Only treat a local variable vertex named "this" differently
              // if the current method is non-static.
              // This is a simple work-around for the following issue:
              // https://bitbucket.org/semperproject/sample/issue/16
              // TODO: The constant "this" should probably not be hard-coded
              // into analyses, if we want the analyses to be independent
              // from source languages.

              // We cannot use the compiler method `getMethod` as the current
              // parameter list is not available.
              val methods = SystemParameters.compiler.getMethods(SystemParameters.currentMethod)
              // Works as long as there is no method overloading
              assert(methods.size == 1)

              val isInstanceVar = locVarVertex.name.equals("this") &&
                !methods.head._2.modifiers.contains(StaticModifier)

              // Arguments can point to null
              if (!isInstanceVar) {
                // Only arguments other than "this" can point to null
                resultingEdges += EdgeWithState(locVarVertex, newGenValState, None, nullVertex)
              }
              for (heapVertex <- newVertices.collect({ case v: HeapVertex if v.typ.lessEqual(locVarVertex.typ) => v })) {
                // "this" must have an exact type
                if (!isInstanceVar || heapVertex.typ.equals(locVarVertex.typ)) {
                  // Create target EdgeLocalIdentifiers
                  var trgValState = newGenValState
                  for (valField <- heapVertex.typ.nonObjectFields) {
                    val trgEdgeLocId = EdgeLocalIdentifier(List.empty[String], valField)
                    val valHeapId = ValueHeapIdentifier(heapVertex, valField)
                    trgValState = trgValState.createVariable(trgEdgeLocId, trgEdgeLocId.getType)
                    trgValState = trgValState.assume(new BinaryArithmeticExpression(valHeapId, trgEdgeLocId, ArithmeticOperator.==, null))
                  }
                  resultingEdges += EdgeWithState(locVarVertex, trgValState, None, heapVertex)
                }
              }
            }
            result = result.lub(ValueDrivenHeapState(HeapGraph(newVertices, resultingEdges.toSet), newGenValState, new ExpressionSet(variable.getType).add(variable), isTop, isBottom))
          } else {
            // Arguments that are not objects are values and can not be aliased. Therefore, we just create them in the
            // ordinary fashion.
            result = result.lub(createVariable(x, typ, variable.getProgramPoint))
          }
        }
        case _ => throw new Exception("Argument to be created is not a variable")
      }
    }
    // return
    result
  }

  def assignVariable(x: ExpressionSet, right: ExpressionSet): ValueDrivenHeapState[S] = {
    if(this.isBottom) return this
    if(right.isTop)
      return this.setVariableToTop(x).removeExpression()
    // if(right.isTop) return top()
    var result = this
    assert(x.getSetOfExpressions.size == 1 && right.getSetOfExpressions.size == 1, "More than one identifier or right expression are to be assigned.")
    val rightExp = right.getSetOfExpressions.head
    for (el <- x.getSetOfExpressions) {
      el match {
        case variable: VariableIdentifier => {
          if (el.getType.isNumericalType()) {
            /**
             * Computing resulting general value state
             */
            var resultGenValState = generalValState.bottom()
            val rightExpConditions = newEvaluateExpression(rightExp)
            val genValAndExpressionConds = Utilities.applyConditions(Set(generalValState), rightExpConditions)
            for (c <- genValAndExpressionConds)
              resultGenValState = resultGenValState.lub(c.assign(variable, rightExp))

            if (resultGenValState.lessEqual(resultGenValState.bottom()))
              return bottom()

            /**
             * Updating the abstract heap
             */
            val tempAH = abstractHeap.valueAssignOnEachEdge(Some(variable), Map.empty[Path[S],S], None, rightExp, rightExpConditions)
            result = ValueDrivenHeapState(tempAH, resultGenValState, new ExpressionSet(variable.getType).add(variable), false, false)
          } else {
            val varVertex = abstractHeap.vertices.filter(_.name == variable.getName).head
            val edgesToRemove = abstractHeap.outEdges(varVertex)
            var edgesToAdd = Set.empty[EdgeWithState[S]]
            rightExp match {
              case verExpr: VertexExpression => {
                assert(abstractHeap.vertices.contains(verExpr.vertex), "Assigning a non-existing node")
                var newEdgeState = generalValState
                if (verExpr.vertex.isInstanceOf[HeapVertex]) {
                  // adding edge local information
                  var addedIdentifiers = Set.empty[Identifier]
                  assert(varVertex.typ.equals(verExpr.getType), "We support only exact type, that is the fields should be the same")
                  for (valField <- varVertex.typ.nonObjectFields) {
                    val edgeLocalId = EdgeLocalIdentifier(List.empty[String], valField)
                    addedIdentifiers = addedIdentifiers + edgeLocalId
                    newEdgeState = newEdgeState.createVariable(edgeLocalId, edgeLocalId.getType)
                    val resId = ValueHeapIdentifier(verExpr.vertex.asInstanceOf[HeapVertex], valField)
                    newEdgeState = newEdgeState.assume(new BinaryArithmeticExpression(resId, edgeLocalId, ArithmeticOperator.==, null))
                  }
                }
                edgesToAdd = edgesToAdd + EdgeWithState(varVertex, newEdgeState, None, verExpr.vertex)
              }
              case v: VariableIdentifier => {
                val edgesOfRight = abstractHeap.edges.filter(_.source.name.equals(v.getName))
                val sourceVertices = abstractHeap.vertices.filter(_.name.equals(variable.getName))
                assert(sourceVertices.size == 1, "The local variable vertices should be one of each.")
                for (edge <- edgesOfRight) {
                  edgesToAdd = edgesToAdd + EdgeWithState(sourceVertices.head, edge.state, None, edge.target)
                }
              }
              case rAP: AccessPathExpression => {
                val rightPaths = abstractHeap.getPaths(rAP.path)
                for (rPath <- rightPaths) {
                  val rCond = graphPathCondition(rPath)
                  if (!rCond.lessEqual(rCond.bottom())) {
                    edgesToAdd = edgesToAdd + EdgeWithState(varVertex, rCond, None, rPath.last.target)
                  }
                }
              }
              case c : Constant => {
                assert(c.toString() == "null", "The only object constant is null.")
                val (tempAH, nullVertex) = abstractHeap.addNewVertex(VertexConstants.NULL, c.getType)
                val newState = ValueDrivenHeapState(tempAH, generalValState, x, false, false)
                return newState.assignVariable(x, new ExpressionSet(variable.typ).add(new VertexExpression(variable.typ, nullVertex)(c.pp)))
              }
              case _ => throw new Exception("Not supported (should not happen, let me know if does (Milos)).")
            }
            if (edgesToAdd.isEmpty)
              result = bottom()
            else {
              // Remove the old edges before adding the new ones.
              // It's possible that the two sets of edges overlap.
              val tempAH = abstractHeap
                .removeEdges(edgesToRemove)
                .addEdges(edgesToAdd)
                .joinCommonEdges()
              val newExpr = new ExpressionSet(rightExp.getType).add(variable)
              result = ValueDrivenHeapState(tempAH, generalValState, newExpr, false, isBottom)
            }
          }
        }
        case _ => throw new Exception("Left-hand side of variable assignment is not a variable.")
      }
    }
    assert(result.abstractHeap.isNormalized, "The abstract heap is not normalized.")
    result.prune()
  }

  private def createEdgeLocalState(srcId: Identifier, trgId: Identifier, field: Option[String], state: S, addedIds: Set[Identifier], sourceVertex: Vertex, targetVertex: Vertex): S = {
    if (sourceVertex.isInstanceOf[NullVertex] || state.lessEqual(state.bottom())) return state.bottom()
    assert(srcId.getType.isObject(), "This is used only when objects are assigned.")
    assert(trgId.getType.isObject(), "This is used only when objects are assigned.")
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
      for (valField <- sourceVertex.typ.nonObjectFields) {
        val srcEdgeLocId = EdgeLocalIdentifier(List.empty[String], valField)
        val correspAddedId = addedIds.filter(id => id.getName.equals(srcId.getName + "." + valField)).head
        resultingState = resultingState.rename(List(correspAddedId), List(srcEdgeLocId))
        renamendIds = renamendIds + correspAddedId
//        resultingRep.value += (Set(correspAddedId) -> Set(correspAddedId, srcEdgeLocId))
      }
    }
    // adding target edge-local information
    if (targetVertex.isInstanceOf[HeapVertex]) {
      for (valField <- targetVertex.typ.nonObjectFields) {
        val trgEdgeLocId = EdgeLocalIdentifier(edgeLocTrgPath, valField)
        val correspAddedId = addedIds.filter(id => id.getName.equals(trgId.getName + "." + valField)).head
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

  private def newEvaluateExpression(expr: Expression) : Set[S] = {
    //assert(!expr.getType().isObject(), "This should be evaluation of only value expressions!")
    expr match {
      case v : VariableIdentifier => {
        Set(generalValState)
      }
      case c : Constant => {
        Set(generalValState)
      }
      case ap: AccessPathExpression => {
        val field = ap.path.last
        val resultingSet = scala.collection.mutable.Set.empty[S]
        // Those that lead to null are not interesting
        for (path <- abstractHeap.getPaths(ap.path.dropRight(1)).filter(_.last.target.isInstanceOf[HeapVertex])) {
          // We find the condition for the path
          var cond = graphPathCondition(path)
          // We rename edge local identifier that corresponds to the access path to the access path
          val renameFrom = cond.getIds().collect({
            case id: EdgeLocalIdentifier if id.accPath.isEmpty && id.field == field => id
          }).toList
          assert(renameFrom.size == 1, "This should not happen, there should be exactly one identifier to rename.")
          val renameTo : List[Identifier] = List(new AccessPathIdentifier(ap.path, ap.getType, ap.getProgramPoint))
          cond = cond.rename(renameFrom, renameTo)
          // The AccessPathIdentifier must agree also with the ValueHeapIdentifier
          val resId = ValueHeapIdentifier(path.last.target.asInstanceOf[HeapVertex], field, renameTo.head.getType, renameTo.head.getProgramPoint)
          cond = cond.assume(new BinaryArithmeticExpression(resId, renameTo.head, ArithmeticOperator.==, null))
          // We remove all edge local identifiers
          cond = cond.removeVariables(cond.getIds().filter(_.isInstanceOf[EdgeLocalIdentifier]))
          resultingSet += cond
        }
        resultingSet.toSet[S]
      }
      case BinaryArithmeticExpression(l,r,o,t) => {
        // First, we calculate conditions from each side.
        val leftConds = newEvaluateExpression(l)
        val rightConds = newEvaluateExpression(r)
        // Then we combine them together
        Utilities.applyConditions(leftConds, rightConds)
      }
      case BinaryBooleanExpression(l,r,o, t) => {
        // First, we calculate conditions from each side.
        val leftConds = newEvaluateExpression(l)
        val rightConds = newEvaluateExpression(r)
        // Then we combine them together
        Utilities.applyConditions(leftConds, rightConds)
      }
      case NegatedBooleanExpression(e) => {
        newEvaluateExpression(e)
      }
      case ReferenceComparisonExpression(l,r,o,t) => {
        throw new Exception("This method should not be called with ReferenceComparisonExpression. " +
          "(At least for now I do not see why it should be)")
      }
      case _ =>
        throw new Exception("Not supported. Let me know. (Milos)")
    }
  }

  /**
   * This methods returns an expression that represents the given expression, the set of states in which this
   * expression may exist and the set of added identifiers to the states with the last vertex of the expression.
   */
  private def evaluateExpression(expr: Expression): (Expression, Set[(S, Set[Identifier], Map[AccessPathExpression, Path[S]])])= {
    expr match {
      case v: VariableIdentifier => {
        if (v.getType.isObject())
          return evaluateExpression(new AccessPathExpression(v.getProgramPoint,v.getType, List(v.getName)))
        else
          return (v, Set((generalValState, Set.empty[Identifier], Map.empty[AccessPathExpression, Path[S]])))
      }
      case c: Constant => {
//        if (c.getType().isObject())
//          throw new Exception("Null constants should be handled separately.")
//        else
        return (c, Set((generalValState, Set.empty[Identifier], Map.empty[AccessPathExpression, Path[S]])))
      }
      case ap: AccessPathExpression => {
        // First we need to evaluate the access path, depending on whether it represents an object or a value
        // 1. we get all possible graph paths that the access path can follow
        val objAccPath = if (!ap.typ.isObject()) ap.path.dropRight(1) else ap.path
        var graphPaths = abstractHeap.getPaths(objAccPath)
        // 2. We evaluate each graph path and collect only the valid ones
        var validGraphPaths = Set.empty[(S, Set[Identifier], Map[AccessPathExpression, Path[S]])]
        for (path <- graphPaths) {
          val (st, apExp, apIds, apexpEdgeSeqMap) = evaluateGraphPath(path, expr.getProgramPoint)
          val isValid: Boolean = !st.lessEqual(st.bottom()) && (if (!ap.getType.isObject()) apIds.map(id => id.getName).contains(ap.toString()) else true)
          if (isValid) {
            validGraphPaths = validGraphPaths + ((st, apIds.asInstanceOf[Set[Identifier]], apexpEdgeSeqMap))
          }
        }
        return (new AccessPathIdentifier(ap.path, ap.getType, ap.getProgramPoint), validGraphPaths)
      }
      case BinaryArithmeticExpression(l,r,o,t) => {
        val leftEval = evaluateExpression(l)
        val rightEval = evaluateExpression(r)
        val finalExp = new BinaryArithmeticExpression(leftEval._1, rightEval._1, o, t)
        var validStates = Set.empty[(S, Set[Identifier], Map[AccessPathExpression, Path[S]])]
        for (lStIdsMap <- leftEval._2) {
          var currentState = lStIdsMap._1
          for (rStIdsMap <- rightEval._2) {
            var prefixesAgree = true
            var newAccPathExpEdgeMap = Map.empty[AccessPathExpression, Path[S]]
            val intersectedKeys = lStIdsMap._3.keySet.intersect(rStIdsMap._3.keySet)
            for (key <- intersectedKeys) {
              if (prefixesAgree && lStIdsMap._3.apply(key).equals(rStIdsMap._3.apply(key))) {
                newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
              } else {
                prefixesAgree = false
              }
            }
            if (prefixesAgree) {
              currentState = currentState.createVariables(rStIdsMap._2)
              currentState = currentState.glb(rStIdsMap._1.createVariables(lStIdsMap._2))
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
        var validStates = Set.empty[(S, Set[Identifier], Map[AccessPathExpression, Path[S]])]
        for (lStIdsMap <- leftEval._2) {
          var currentState = lStIdsMap._1
          for (rStIdsMap <- rightEval._2) {
            var prefixesAgree = true
            var newAccPathExpEdgeMap = Map.empty[AccessPathExpression, Path[S]]
            val intersectedKeys = lStIdsMap._3.keySet.intersect(rStIdsMap._3.keySet)
            for (key <- intersectedKeys) {
              if (prefixesAgree && lStIdsMap._3.apply(key).equals(rStIdsMap._3.apply(key))) {
                newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
              } else {
                prefixesAgree = false
              }
            }
            if (prefixesAgree) {
              currentState = currentState.createVariables(rStIdsMap._2)
              currentState = currentState.glb(rStIdsMap._1.createVariables(lStIdsMap._2))
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
        var validStates = Set.empty[(S, Set[Identifier], Map[AccessPathExpression, Path[S]])]
        for (lStIdsMap <- leftEval._2) {
          var currentState = lStIdsMap._1
          for (rStIdsMap <- rightEval._2) {
            var prefixesAgree = true
            var newAccPathExpEdgeMap = Map.empty[AccessPathExpression, Path[S]]
            val intersectedKeys = lStIdsMap._3.keySet.intersect(rStIdsMap._3.keySet)
            for (key <- intersectedKeys) {
              if (prefixesAgree && lStIdsMap._3.apply(key).equals(rStIdsMap._3.apply(key))) {
                newAccPathExpEdgeMap = newAccPathExpEdgeMap + (key -> lStIdsMap._3.apply(key))
              } else {
                prefixesAgree = false
              }
            }
            if (prefixesAgree) {
              currentState = currentState.createVariables(rStIdsMap._2)
              currentState = currentState.glb(rStIdsMap._1.createVariables(lStIdsMap._2))
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

  def assignField(obj: ExpressionSet, field: String, right: ExpressionSet): ValueDrivenHeapState[S] = {
    if (isBottom) return this
    assert(right.getSetOfExpressions.size == 1, "We allow to assign only single expression")
    val leftExp = obj.getSetOfExpressions.head
    val rightExp = right.getSetOfExpressions.head
    assert(leftExp.isInstanceOf[AccessPathExpression], "The left hand side od the assignment is not an AccessPathExpression")
    val leftAccPath = leftExp.asInstanceOf[AccessPathExpression]
    val leftPaths: Set[Path[S]] = abstractHeap.getPathsToBeAssigned(leftAccPath).filter(_.last.target.isInstanceOf[HeapVertex])
    if (leftPaths.size == 0)
      return this.bottom()
    if (rightExp.getType.isObject()) {
      var edgesToAdd = Set.empty[EdgeWithState[S]]
      rightExp match {
        case x: VariableIdentifier => {
          val rightPaths = abstractHeap.getPaths(List(x.getName))
          edgesToAdd = referencePathAssignmentEdges(leftAccPath.path.last, leftPaths, rightPaths)
        }
        case rAP: AccessPathExpression => {
          val rightPaths = abstractHeap.getPaths(rAP.path)
          edgesToAdd = referencePathAssignmentEdges(leftAccPath.path.last, leftPaths, rightPaths)
        }
        case v: VertexExpression => {
          // We assume that all edges have ValueHeapIdentifiers for the given vertex expression
          // Add new edges
          for (lPath <- leftPaths) {
            // leftCond should contain source EdgeLocalIdentifiers
            val leftCond = graphPathCondition(lPath)
            // We build the replacement that for each ValueHeapIdentifier corresponding to the vertex, expands it to
            // target EdgeLocalIdentifier.
            val repl = new Replacement()
            for (id <- leftCond.getIds().collect({ case id: ValueHeapIdentifier if id.obj.equals(v.vertex) => id })) {
              repl.value.update(Set(id), Set(id, EdgeLocalIdentifier(List(leftAccPath.path.last), id)))
            }
            edgesToAdd = edgesToAdd + EdgeWithState(lPath.last.target, leftCond.merge(repl), Some(leftAccPath.path.last), v.vertex)
          }
        }
        case c : Constant => {
          assert(c.toString.equals("null"), "We expect only null constants.")
          val (newAH, nullVertex) = abstractHeap.addNewVertex(VertexConstants.NULL, c.getType)
          return ValueDrivenHeapState(newAH, generalValState, expr, false, false).assignField(obj, field, new ExpressionSet(c.getType).add(new VertexExpression(c.getType, nullVertex)(c.getProgramPoint)))
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
        val edgesToRemove = resultingAH.outEdges(lastPathVertex, Some(leftAccPath.path.last))
        resultingAH = resultingAH.removeEdges(edgesToRemove)
      }
      resultingAH = resultingAH.addEdges(edgesToAdd)
      resultingAH = resultingAH.joinCommonEdges()
      val newExpr = new ExpressionSet(right.getType()).add(leftAccPath)
      ValueDrivenHeapState(resultingAH, generalValState, newExpr, false, isBottom).prune()
    } else {
      assert(rightExp.getType.isNumericalType(), "For now we allow only numerical values")
      val field = leftAccPath.path.last
      // We construct a map that says which id is assigned and under which condition
      val pathsToAssignUnderConditions = scala.collection.mutable.Map.empty[Path[S], S]
      for (lPath <- leftPaths) {
        val lPathCond = graphPathCondition(lPath)
        val lPathCondEdgeLocalIds = lPathCond.getIds().collect({case id: EdgeLocalIdentifier => id })
        val newPathCond = lPathCond.removeVariables(lPathCondEdgeLocalIds)
        if (!newPathCond.lessEqual(newPathCond.bottom()))
          pathsToAssignUnderConditions.update(lPath, newPathCond)
      }

      if (pathsToAssignUnderConditions.isEmpty)
        return bottom()

      /**
       * Computing resulting general value state
       */
      var resultGenValState = generalValState.bottom()
      val rightExpConditions = newEvaluateExpression(rightExp)
      val genValAndExpressionConds = Utilities.applyConditions(Set(generalValState), rightExpConditions)

      for ((path,cond) <- pathsToAssignUnderConditions) {
        for (c <- Utilities.applyConditions(Set(cond), genValAndExpressionConds)) {
          val lhsIdToAssign = ValueHeapIdentifier(path.last.target.asInstanceOf[HeapVertex], field, leftExp.getType, leftExp.getProgramPoint)
          resultGenValState = resultGenValState.lub(c.assign(lhsIdToAssign, rightExp))
        }
      }

      if (resultGenValState.lessEqual(resultGenValState.bottom()))
        return bottom()

      /**
       * Updating the abstract heap
       */
      val tempAH = abstractHeap.valueAssignOnEachEdge(None, pathsToAssignUnderConditions.toMap, Some(field), rightExp, rightExpConditions)
      val expr = new ExpressionSet(leftExp.getType).add(leftExp)
      ValueDrivenHeapState(tempAH, resultGenValState, expr, false, false).prune()
    }
  }

  /**
   * This method computes the edges that correspond to a reference assignment.
   *
   * @param field to be assigned to the target nodes of the leftPaths
   * @param leftPaths sequence of edges that correspond to paths of LHS
   *                  of the assignment (without the last field)
   * @param rightPaths sequence of edges that correspond to paths of RHS
   *                   of the assignment
   * @return the set of edges that represent the reference assignment
   *
   * @author Milos Novacek
   */
  private def referencePathAssignmentEdges(field: String, leftPaths : Set[Path[S]], rightPaths : Set[Path[S]]) : Set[EdgeWithState[S]] = {val rightStateWithVertex : Set[(S, Vertex)] = rightPaths.map(p => (graphPathCondition(p), p.last.target))
    var edgesToAdd = Set.empty[EdgeWithState[S]]
    for (lPath <- leftPaths) {
      var leftCond = graphPathCondition(lPath)
      if (!leftCond.lessEqual(leftCond.bottom())) {
        // The condition of the left path is not bottom. (i.e. can be possibly assigned)
        for(rPath <- rightPaths) {
          val rightCond = graphPathCondition(rPath)
          var renameFrom = List.empty[EdgeLocalIdentifier]
          var renameTo = List.empty[EdgeLocalIdentifier]
          val idsToRename = rightCond.getIds().collect({ case id: EdgeLocalIdentifier => id })
          for (elId <- idsToRename) {
            renameFrom = elId :: renameFrom
            renameTo = elId.copy(accPath=List(field))(elId.getProgramPoint) :: renameTo
          }
          var newEdgeState = rightCond.rename(renameFrom, renameTo)
          if (renameTo.isEmpty) {
            // This is the case when RHS is null. Hence, we need to create source edge-local identifiers in the right
            // state in order to preserve the once from LHS.
            val sourceIdsOfLHS = leftCond.getIds().collect({ case id: EdgeLocalIdentifier if id.accPath.isEmpty => id })
            newEdgeState = newEdgeState.createVariables(sourceIdsOfLHS.toSet[Identifier])
          }
          leftCond = leftCond.createVariables(renameTo.toSet[Identifier])
          newEdgeState = newEdgeState.createVariables(renameFrom.toSet[Identifier])
          newEdgeState = leftCond.glb(newEdgeState)
          if (!newEdgeState.lessEqual(rightCond.bottom())){
            // add edge that represents the assignment
            edgesToAdd = edgesToAdd + EdgeWithState(lPath.last.target, newEdgeState, Some(field), rPath.last.target)
          }
        }
      }
    }
    edgesToAdd
  }

  /**
   * The method for computing the condition that is satisfied by the given path.
   *
   * @param path for which the condition should be computed
   * @return abstract value condition that is satisfied by the given path
   */
  private def graphPathCondition(path : Path[S]) : S = {
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
    def graphPathConditionRecursive(path : Path[S], state : S) : S = {
      val stateEdgeLocalIds = state.getIds().collect({ case id: EdgeLocalIdentifier => id })
      // Base case is when the path is empty. (Termination)
      if (path.isEmpty) {
        assert((stateEdgeLocalIds -- stateEdgeLocalIds.filter(_.accPath.isEmpty)).isEmpty)
        return state
      }

      // If the path is non-empty, the head of it must refer to a field (i.e. the firs node must be a HeapVertex).
      assert(path.head.source.isInstanceOf[HeapVertex])
      val edge = path.head

      val field = edge.field match {
        case None => throw new Exception("This should not happen.")
        case Some(f) => f
      }

      // Only the edge-local identifiers that refer to target are present in the given state. (i.e. the once with empty sequence of field accesses).
      assert(stateEdgeLocalIds.forall(_.accPath.isEmpty))

      // Originally, the edge local identifiers of the given state with the empty sequence of fields refer to the target
      // and no other edge-local identifiers are present in the given state. We need to add them so that the edge-local
      // identifiers of the currently procces edge do not get lost.
      val edgeLocalIdsToAdd = edge.state.getIds().collect({ case id: EdgeLocalIdentifier if !id.accPath.isEmpty => id })
      var newState: S = state.createVariables(edgeLocalIdsToAdd.toSet[Identifier])
      newState = newState.glb(edge.state)

      // Now, we need to rename source-edge local identifiers to the ones that are target of this edge and remove any others.
      val originalSourceIds = newState.getIds().collect({ case id: EdgeLocalIdentifier if id.accPath.isEmpty => id }).toSet[Identifier]
      newState = newState.removeVariables(originalSourceIds)
      // Renaming
      val idsToRenameToSource = newState.getIds().collect({ case id: EdgeLocalIdentifier if id.accPath.equals(List(field)) => id })
      // Building lists for renaming
      var renameFrom = List.empty[EdgeLocalIdentifier]
      var renameTo = List.empty[EdgeLocalIdentifier]
      for (elId <- idsToRenameToSource) {
        renameFrom = elId :: renameFrom
        renameTo = elId.copy(accPath=List.empty)(elId.getProgramPoint) :: renameTo
      }
      newState = newState.rename(renameFrom, renameTo)
      // Now we remove all edge-local identifiers that can not be the targets.
      val elIdsToRemove = newState.getIds().filter(_.isInstanceOf[EdgeLocalIdentifier]) -- renameTo
      newState = newState.removeVariables(elIdsToRemove.toSet[Identifier])


      // return
      graphPathConditionRecursive(path.tail, newState)
    }

    /* The head of the path (edge sequence) is starting from a variable. Therefore, the edge local variables
       that represent the target edge local variables have an empty sequence of fields. However, we need to remove all
       other edge-local identifier that might be possibly present.
    */
    val elIdsToRemove = path.head.state.getIds().collect({ case id: EdgeLocalIdentifier if !id.accPath.isEmpty => id })
    graphPathConditionRecursive(path.tail, path.head.state.removeVariables(elIdsToRemove))
  }

  /**
   * This methods computes the state that must be satisfied when following
   * the given access path, the string representation of the access path and
   * the set of identifiers that were newly created. Furthermore,
   * the resulting state does not contain the edge local identifiers.
   */
  private def evaluateGraphPath(path : Path[S], pp: ProgramPoint): (S, AccessPathExpression, Set[AccessPathIdentifier], Map[AccessPathExpression, Path[S]]) = {
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
    var expEdgeMap = Map.empty[AccessPathExpression, Path[S]]
    var currentAccPathExp: AccessPathExpression = null
    var currentEdgeSeq = List.empty[EdgeWithState[S]]
    for (edge <- path) {
      // Taking care of source edge local information
      var currentState = edge.state
      for (valField <- edge.source.typ.nonObjectFields) {
        edge.field match {
          case  None =>
          case Some(f) => {
            val edgeLocId = EdgeLocalIdentifier(List.empty[String], valField)
            val newId = new AccessPathIdentifier(currentPathList :+ valField.getName, valField.getType, valField.getProgramPoint)
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
        for (valField <- edge.target.typ.nonObjectFields) {
          val newId = new AccessPathIdentifier(currentPathList :+ valField.getName, valField.getType, valField.getProgramPoint)
          addedIdentifiers = addedIdentifiers + newId
          edge.field match {
            case None => {
              val edgeLocId: Identifier = EdgeLocalIdentifier(List.empty[String], valField)
              currentState = currentState.rename(List(edgeLocId), List(newId))
            }
            case Some(f) => {
              val edgeLocId: Identifier = EdgeLocalIdentifier(List(f), valField)
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
      var newState = state.createVariables(addedIdentifiers.asInstanceOf[Set[Identifier]])
      if (!isInitialized) {
        resultState = newState
        isInitialized = true
      }
      resultState = resultState.glb(newState)
    }
    val finalType = path.last.target.typ
    var finalId: AccessPathExpression = new AccessPathExpression(pp, finalType, currentPathList)
    return (resultState, finalId, addedIdentifiers, expEdgeMap)
  }

  def setArgument(x: ExpressionSet, right: ExpressionSet): ValueDrivenHeapState[S] = ???
  def setVariableToTop(x: ExpressionSet): ValueDrivenHeapState[S] = ???
  def removeVariable(x: ExpressionSet): ValueDrivenHeapState[S] = ???
  def throws(t: ExpressionSet): ValueDrivenHeapState[S] = ???

  def getVariableValue(id: Assignable): ValueDrivenHeapState[S] = {
    if(this.isBottom) return this
    assert(id.isInstanceOf[VariableIdentifier], "This should be VariableIdentifier.")
    copy(expr = new ExpressionSet(id.getType).add(id.asInstanceOf[Expression]))
  }

  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): ValueDrivenHeapState[S] = {
    assert(obj.getSetOfExpressions.size == 1, "We only support single field access.")
    assert(obj.getSetOfExpressions.head.isInstanceOf[AccessPathExpression], "The field access should be accessed via access path.")
    // TODO: May be I should check whether this exist and is feasible already here.
    if (ValueDrivenHeapProperty.materialize) {
      val apObj = obj.getSetOfExpressions.head.asInstanceOf[AccessPathExpression]
      val tempResult = materializePath({if (apObj.typ.isObject()) apObj.path else apObj.path.dropRight(1)})
      tempResult.copy(expr = new ExpressionSet(typ).add(obj))
    } else
      copy(expr = new ExpressionSet(typ).add(obj))
  }

  def evalConstant(value: String, typ: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = {
    //**println("evalConsta(" + value + "," + typ + "," + pp + ") is called.")
    if(this.isBottom) return this
    this.setExpression(new ExpressionSet(typ).add(new Constant(value, typ, pp)))
  }

  def assume(cond: ExpressionSet): ValueDrivenHeapState[S] = {
    if (isBottom) return this

    assert(cond.getSetOfExpressions.size == 1, "Condition of several expressions are not supported.")
    val condition = cond.getSetOfExpressions.head
    val result = condition match {
      case NegatedBooleanExpression(e) => {
        assume(new ExpressionSet(e.getType).add(Utilities.negateExpression(e)))
      }
      case BinaryBooleanExpression(l,r,o,t) => {
        val result = o match {
          case BooleanOperator.&& =>
            assume(new ExpressionSet(l.getType).add(l)).assume(new ExpressionSet(r.getType).add(r))
          case BooleanOperator.|| =>
            assume(new ExpressionSet(l.getType).add(l)).lub(assume(new ExpressionSet(r.getType).add(r)))
        }
        assert(result.abstractHeap.isNormalized, "The abstract heap is not normalized.")
        result
      }
      case baExp : BinaryArithmeticExpression => {
        val baExpConds = newEvaluateExpression(baExp)

        // Compute new general value state
        val expGenCond = Utilities.applyConditions(Set(generalValState), baExpConds)
        var resultingGenCond = generalValState.bottom()
        for (cond <- expGenCond) {
          resultingGenCond = resultingGenCond.lub(cond.assume(baExp))
        }
        resultingGenCond = Utilities.removeAccessPathIdentifiers(resultingGenCond)

        // Update abstract heap
        val tempAH = abstractHeap.valueAssumeOnEachEdge(baExp, expGenCond)
        val expr = new ExpressionSet(SystemParameters.getType().top)
        ValueDrivenHeapState(tempAH, resultingGenCond, expr, false, false).prune()
      }
      case x =>
        println("ValueDrivenHeapState.assume: " + x + " is not supported.")
        this
    }
    result
    // See version control history for the original code
  }

  def testTrue(): ValueDrivenHeapState[S] = {
    //**println("testTrue() is called")
    return assume(getExpression)
  }

  def testFalse(): ValueDrivenHeapState[S] = {
    val negatedExpressions = getExpression.getSetOfExpressions.map(exp => new NegatedBooleanExpression(exp))
    var negatedExpSet = new ExpressionSet(getExpression.getType())
    for (ne <- negatedExpressions)
      negatedExpSet = negatedExpSet.add(ne)
    return assume(negatedExpSet)
  }

  def getExpression: ExpressionSet = expr

  def setExpression(newExpr: ExpressionSet): ValueDrivenHeapState[S] =
    copy(expr = newExpr)

  def removeExpression(): ValueDrivenHeapState[S] =
    copy(expr = new ExpressionSet(SystemParameters.getType().top))

  def pruneUnreachableHeap(): ValueDrivenHeapState[S] = ???
  def factory(): ValueDrivenHeapState[S] = ???

  def top(): ValueDrivenHeapState[S] = {
    return ValueDrivenHeapState(new HeapGraph[S](), new ApronInterface(None, new Polka(false), false, Set.empty[Identifier]).top().asInstanceOf[S], new ExpressionSet(SystemParameters.getType().top()), true, false)
  }

  def bottom(): ValueDrivenHeapState[S] = {
    return ValueDrivenHeapState(new HeapGraph[S](), generalValState.bottom(), expr, false, true)
  }

  def lub(other: ValueDrivenHeapState[S]): ValueDrivenHeapState[S] = {
    //**println("lub(" + toString() + ", " + right.toString() + ") is called")
    if (isBottom || other.isTop)
      return other
    if (isTop || other.isBottom)
      return this
    // TODO: Implement this properly
    val (resAH, renameRightFrom, renameRightTo) = abstractHeap.lub(other.abstractHeap)
    val resGeneralState = generalValState.lub(other.generalValState.rename(renameRightFrom, renameRightTo))

    //**println("REAL LUB IS CALLED.")

    return ValueDrivenHeapState(resAH, resGeneralState, new ExpressionSet(SystemParameters.getType().top()), false, false)

//    return ValueDrivenHeapState(right.abstractHeap,
//                                       right.generalValState.lub(generalValState, right.generalValState),
//                                       right.expr, false, false)
  }

  def glb(other: ValueDrivenHeapState[S]): ValueDrivenHeapState[S] = {
    if (isBottom || other.isBottom)
      return bottom()
    if (isTop)
      return other
    if (other.isTop)
      return this
    val (resultingAH, removeIds, renameFrom, renameTo) = abstractHeap.glb(other.abstractHeap)
    var newRightGeneralValState = other.generalValState.removeVariables(removeIds)
    newRightGeneralValState = newRightGeneralValState.rename(renameFrom, renameTo)
    val newGeneralValState = generalValState.glb(newRightGeneralValState)
    if (resultingAH.isBottom() || newGeneralValState.lessEqual(newGeneralValState.bottom()))
      return bottom()
    return ValueDrivenHeapState(resultingAH, newGeneralValState, new ExpressionSet(SystemParameters.getType().top), false, false)
  }

  def widening(other: ValueDrivenHeapState[S]): ValueDrivenHeapState[S] = {
    def areGraphsIdentical(l: HeapGraph[S], r: HeapGraph[S]) : Boolean = {
      var areGraphsIdentical = true
      for (rEdge <- r.edges) {
        areGraphsIdentical = areGraphsIdentical &&
          {
            val edgeSet = l.edges.filter(lEdge => lEdge.source.equals(rEdge.source) && lEdge.target.equals(rEdge.target))
            edgeSet.size == 1 && edgeSet.head.state.getIds().equals(rEdge.state.getIds())
          }
      }
      areGraphsIdentical
    }


//    //**println("WIDENING IS CALLED")
//    val tempRight = left.lub(other)
//    val (mergedLeft, replacementLeft) = abstractHeap.mergePointedNodes()
//    val (mergedRight, replacementRight) = tempRight.abstractHeap.mergePointedNodes()
//    if (!mergedLeft.vertices.equals(mergedRight.vertices) || !areGraphsIdentical(mergedLeft, mergedRight)) {
//      return tempRight
//    }
//    val newGeneralValState = generalValState.widening(generalValState.merge(replacementLeft), tempRight.generalValState.merge(replacementRight))
//    return ValueDrivenHeapState(mergedLeft.wideningAfterMerge(mergedLeft, mergedRight), newGeneralValState, new ExpressionSet(SystemParameters.getType().top), false, false)


    /**
     * ORIGINAL CODE
     */
    val (mergedLeft, replacementLeft) = abstractHeap.mergePointedNodes()
    val (mergedRight, replacementRight) = other.abstractHeap.mergePointedNodes()
    val rightGenValState = other.generalValState.merge(replacementRight)
    var newRight = ValueDrivenHeapState(mergedRight, rightGenValState, new ExpressionSet(SystemParameters.getType().top), false, false)
    val newLeft = ValueDrivenHeapState(mergedLeft, generalValState.merge(replacementLeft), new ExpressionSet(SystemParameters.getType().top), false, false)
    newRight = newLeft.lub(newRight)
    if (!mergedLeft.vertices.equals(newRight.abstractHeap.vertices) || !areGraphsIdentical(mergedLeft, mergedRight)) {
      return newRight
    }
    val newGeneralValState = newLeft.generalValState.widening(newRight.generalValState.merge(replacementRight))
    val result = ValueDrivenHeapState(mergedLeft.wideningAfterMerge(newRight.abstractHeap), newGeneralValState, new ExpressionSet(SystemParameters.getType().top), false, false)
    return result
  }

  private def materializePath(pathToMaterialize : List[String]) : ValueDrivenHeapState[S] = {
    val verticesToAdd = mutable.Set.empty[Vertex]
    val edgesToAdd = mutable.Set.empty[EdgeWithState[S]]
    val edgesToRemove = mutable.Set.empty[EdgeWithState[S]]
    val repl = new Replacement(isPureExpanding = true)
    var resultingAH = abstractHeap
    val queue = mutable.Queue.empty[(EdgeWithState[S], List[String])]
    for (e <- abstractHeap.edges.filter(edg => edg.source.name.equals(pathToMaterialize.head) && edg.target.isInstanceOf[HeapVertex]))
      queue.enqueue((e, pathToMaterialize.tail))
    while (!queue.isEmpty) {
      val (edge, path) = queue.dequeue()
      assert(edge.source.isInstanceOf[DefiniteHeapVertex] || edge.source.isInstanceOf[LocalVariableVertex])
      if (edge.target.isInstanceOf[SummaryHeapVertex]) {
        edgesToRemove += edge
        // Creating a vertex that is a materialization of the summary vertex
        val (tempAH, definiteVertex) = resultingAH.addNewVertex(VertexConstants.DEFINITE, edge.target.typ)
        resultingAH = tempAH
        // Add the information about the corresponding identifiers to replacement
        for (valField <- definiteVertex.typ.nonObjectFields) {
          val sumValHeapId = ValueHeapIdentifier(edge.target.asInstanceOf[SummaryHeapVertex], valField)
          val defValHeapId = ValueHeapIdentifier(definiteVertex.asInstanceOf[DefiniteHeapVertex], valField)
          repl.value.update(Set(sumValHeapId), Set(sumValHeapId, defValHeapId))
        }

        /**
         * Adding edges
         */
        // Edge that represents the processed edge
        edgesToAdd += edge.copy(target = definiteVertex)
        for (e <- resultingAH.edges -- edgesToRemove ++ edgesToAdd) {
          // Incoming edges
          if (e.target.equals(edge.target)) {
            edgesToAdd += e.copy(target = definiteVertex)
          }
          // Outgoing edges
          if (e.source.equals(edge.target)) {
            val edgeToAdd = e.copy[S](source = definiteVertex)
            if (!path.isEmpty && edgeToAdd.field.equals(Some(path.head)))
              queue.enqueue((edgeToAdd, path.tail))
            edgesToAdd += edgeToAdd
          }
          // Self-loop edges
          if (e.source.equals(edge.target) && e.target.equals(edge.target)) {
            val edgeToAdd = e.copy[S](source = definiteVertex, target = definiteVertex)
            if (!path.isEmpty && edgeToAdd.field.equals(Some(path.head)))
              queue.enqueue((edgeToAdd, path.tail))
            edgesToAdd += edgeToAdd
          }
        }
      } else {
        // Nothing to materialize for this edge
        if (!path.isEmpty)
          for (e <- (resultingAH.edges -- edgesToRemove ++ edgesToAdd).filter(edg => edg.source.equals(edge.target) && edg.field.equals(Some(path.head)) && edg.target.isInstanceOf[HeapVertex]))
            queue.enqueue((e, path.tail))
      }
    }
    resultingAH = resultingAH.removeEdges(edgesToRemove.toSet)
    resultingAH = resultingAH.applyReplacement(repl)
    // Updating source and target EdgeLocalIdentifiers in the edges to add.
    val updatedEdgesToAdd = mutable.Set.empty[EdgeWithState[S]]
    for (e <- edgesToAdd) {
      // Updating EdgeLocalIdentifiers with empty path
      var updatedEdgeState = e.state.merge(repl)
      if (e.source.isInstanceOf[HeapVertex] || e.source.isInstanceOf[LocalVariableVertex]) {
        val vtx = if (e.source.isInstanceOf[HeapVertex]) e.source else e.target
        for (valHeapId <- updatedEdgeState.getIds().collect({ case id: ValueHeapIdentifier if id.obj == vtx => id })) {
          val edgLocId = EdgeLocalIdentifier(List.empty[String], valHeapId.field, valHeapId.getType)(valHeapId.getProgramPoint)
          updatedEdgeState = updatedEdgeState.assume(new BinaryArithmeticExpression(valHeapId, edgLocId, ArithmeticOperator.==, null))
        }
      }
      // Updating EdgeLocalIdentifiers with non-empty path
      if (e.target.isInstanceOf[HeapVertex] && !e.source.isInstanceOf[LocalVariableVertex]) {
        for (valHeapId <- updatedEdgeState.getIds().collect({ case id: ValueHeapIdentifier if id.obj == e.target => id })) {
          val edgLocId = EdgeLocalIdentifier(List(e.field match {case None => throw new Exception("Should not happen") case Some(f) => f}), valHeapId.field, valHeapId.getType)(valHeapId.getProgramPoint)
          updatedEdgeState = updatedEdgeState.assume(new BinaryArithmeticExpression(valHeapId, edgLocId, ArithmeticOperator.==, null))
        }
      }
      updatedEdgesToAdd += e.copy(state = updatedEdgeState)
    }
    resultingAH = resultingAH.addEdges(updatedEdgesToAdd.toSet)
    copy(abstractHeap = resultingAH, generalValState = generalValState.merge(repl))
  }

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
  }

  def createObject(typ: Type, pp: ProgramPoint, fields: Option[Set[Identifier]]): ValueDrivenHeapState[S] = {
    //**println("creatObject(" + typ + "," + pp + "," + fields + ") is called.")
    if (this.isBottom) return this

    var resIds = Set.empty[Identifier]
    var (newAbstractHeap, newVertex) = abstractHeap.addNewVertex(VertexConstants.DEFINITE, typ)
    assert(newVertex.isInstanceOf[DefiniteHeapVertex], "The newly created object should be definite")
    val createdObjVertex = newVertex.asInstanceOf[DefiniteHeapVertex]

    //    var edgeLocalIds = Set.empty[Identifier]
    var resIdsAndEdgeLocalIds = Set.empty[(Identifier, Identifier)]

    for (valField <- typ.nonObjectFields) {
      // This means that we have a value field and this should be included in all abstract states on edges
      // This is done via Replacement
      val resId = ValueHeapIdentifier(newVertex.asInstanceOf[DefiniteHeapVertex], valField)
      val edgeLocalId = EdgeLocalIdentifier(List.empty[String], valField)
      resIdsAndEdgeLocalIds = resIdsAndEdgeLocalIds + ((resId, edgeLocalId))
      resIds= resIds + resId
      //      edgeLocalIds = edgeLocalIds + EdgeLocalIdentifier(List.empty[String], valField.getName)
    }
    //    val resAH = newAbstractHeap.createVariablesInAllStates(resIds)
    newAbstractHeap = newAbstractHeap.createVariablesInAllStates(resIds)
    var newGeneralState = generalValState
    for ((id,_) <- resIdsAndEdgeLocalIds)
      newGeneralState = newGeneralState.createVariable(id, id.getType)
    for (objField <- typ.objectFields) {
      val res = newAbstractHeap.addNewVertex(VertexConstants.NULL, typ)
      newAbstractHeap = res._1
      newVertex = res._2
      var edgeState = newGeneralState
      for ((resId, edgeLocalId) <- resIdsAndEdgeLocalIds) {
        //        edgeState = edgeState.createVariable(resId, resId.getType())
        edgeState = edgeState.createVariable(edgeLocalId, edgeLocalId.getType)
        edgeState = edgeState.assume(new BinaryArithmeticExpression(resId,edgeLocalId, ArithmeticOperator.==, null))
      }
      newAbstractHeap = newAbstractHeap.addEdges(Set(EdgeWithState(createdObjVertex, edgeState, Some(objField.getName), newVertex)))
    }

    // Now we need to apply the replacement to all the states, including the general value state.

    return ValueDrivenHeapState(newAbstractHeap,
      newGeneralState,
      new ExpressionSet(typ).add(new VertexExpression(typ, createdObjVertex)(pp)),
      isTop, isBottom)
  }

  /**
   * Prunes the abstract heap and removes all pruned identifiers
   * from the general value state.
   */
  private def prune(): ValueDrivenHeapState[S] = {
    val (newAbstractHeap, idsToRemove) = abstractHeap.prune()
    val newGeneralValState = generalValState.removeVariables(idsToRemove)
    if (newAbstractHeap.isBottom())
      bottom()
    else
      copy(abstractHeap = newAbstractHeap, generalValState = newGeneralValState)
  }

  override def toString: String =
    s"ValueDrivenHeapState(${abstractHeap.vertices.size} vertices, " +
    s"${abstractHeap.edges.size} edges)"

  def before(pp: ProgramPoint): ValueDrivenHeapState[S] = this

  /**
   * Removes all variables satisfying filter
   */
  def pruneVariables(filter: (Identifier) => Boolean): ValueDrivenHeapState[S] = ???

  /**
   * Detects summary nodes that are only reachable via a single access path
   * and converts them to non-summary nodes.
   */
  def optimizeSummaryNodes(): ValueDrivenHeapState[S] = ???

  // Backwards analyses are currently not supported
  def backwardGetVariableValue(id: Assignable): ValueDrivenHeapState[S] = ???
  def backwardGetFieldValue(obj: ExpressionSet, field: String, typ: Type): ValueDrivenHeapState[S] = ???
  def backwardAssignVariable(x: ExpressionSet, right: ExpressionSet): ValueDrivenHeapState[S] = ???

  // Collections are currently not supported
  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, tpp: ProgramPoint): ValueDrivenHeapState[S] = ???
  def assignCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def insertCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def removeCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def getCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def clearCollection(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def getCollectionLength(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet, valueTyp: Type): ValueDrivenHeapState[S] = ???
  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = ???
  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet, keyTyp: Type, valueTyp: Type): ValueDrivenHeapState[S] = ???
  def insertCollectionValue(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint): ValueDrivenHeapState[S] = ???
  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet, valueTyp: Type): ValueDrivenHeapState[S] = ???
  def removeCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, keyTyp: Type): ValueDrivenHeapState[S] = ???
  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet, keyTyp: Type): ValueDrivenHeapState[S] = ???
  def clearCollection(collectionSet: ExpressionSet, keyTyp: Type, valueTyp: Type): ValueDrivenHeapState[S] = ???
  def isSummaryCollection(collectionSet: ExpressionSet): Boolean = ???
  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, keyCollectionTyp: Option[Type], tpp: ProgramPoint, fields: Option[Set[Identifier]]): ValueDrivenHeapState[S] = ???
  def getSummaryCollectionIfExists(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def getCollectionValue(valueIds: ExpressionSet): ValueDrivenHeapState[S] = ???
  def insertCollectionTopElement(collectionSet: ExpressionSet, keyTop: ExpressionSet, valueTop: ExpressionSet, pp: ProgramPoint): ValueDrivenHeapState[S] = ???
  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, fromCollectionTyp: Type, collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = ???
  def getOriginalCollection(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def getKeysCollection(collectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def removeCollectionKeyConnection(origCollectionSet: ExpressionSet, keyCollectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def insertCollectionElement(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint): ValueDrivenHeapState[S] = ???
  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def removeFirstCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet): ValueDrivenHeapState[S] = ???
  def collectionContainsKey(collectionSet: ExpressionSet, keySet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = ???
  def collectionContainsValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = ???
}

object ValueDrivenHeapStateConstants {
  val edgeLocalIdentifier = "eLocId"
}