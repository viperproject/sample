package graph

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{StaticModifier, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.SystemParameters
import scala.collection.mutable

case class ValueDrivenHeapState[S <: SemanticDomain[S]](
    abstractHeap: HeapGraph[S],
    generalValState: S,
    expr: ExpressionSet,
    isTop: Boolean = false,
    isBottom: Boolean = false) extends State[ValueDrivenHeapState[S]] {

  require(!isTop || !isBottom, "cannot be top and bottom at the same time")

  def createVariable(vars: ExpressionSet, typ: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = {
    require(vars.getSetOfExpressions.size == 1,
      "Cannot declare multiple variables together")
    require(vars.getSetOfExpressions.head.isInstanceOf[VariableIdentifier],
      "Expression must be a VariableIdentifier")
    val id = vars.getSetOfExpressions.head.asInstanceOf[VariableIdentifier]
    createVariable(id, typ)
  }

  private def createVariable(variable: VariableIdentifier, typ: Type): ValueDrivenHeapState[S] = {
    if (this.isBottom) return this

    if (variable.getType.isObject()) {
      // Initialize references variables to null such that the heap is not
      // treated as bottom, making it possible to analyze programs with more
      // than one local reference variable. This is unsound, e.g., since two
      // uninitialized reference variables 'a' and 'b' should not be
      // considered equal.
      // A sound implementation would use a summary node (representing top)
      // similar to `createVariableForArgument`.
      val nullVertex = new NullVertex()
      val varVertex = new LocalVariableVertex(variable.getName, variable.getType)
      val newVertices = Set(nullVertex, varVertex)
      val edgeToNull = EdgeWithState(varVertex, generalValState, None, nullVertex)
      val newAbstractHeap = abstractHeap.addVertices(newVertices).addEdges(Set(edgeToNull))
      copy(
        abstractHeap = newAbstractHeap,
        expr = new ExpressionSet(typ).add(variable))
    } else {
      copy(
        abstractHeap = abstractHeap.createVariablesInAllStates(Set(variable)),
        generalValState = generalValState.createVariable(variable, variable.getType),
        expr = new ExpressionSet(typ).add(variable))
    }
  }

  def createVariableForArgument(vars: ExpressionSet, typ: Type): ValueDrivenHeapState[S] = {
    require(vars.getSetOfExpressions.size == 1,
      "Cannot declare multiple variables together")
    require(vars.getSetOfExpressions.head.isInstanceOf[VariableIdentifier],
      "Expression must be a VariableIdentifier")
    val id = vars.getSetOfExpressions.head.asInstanceOf[VariableIdentifier]
    createVariableForArgument(id, typ)
  }

  private def createVariableForArgument(variable: VariableIdentifier, typ: Type): ValueDrivenHeapState[S] = {
    if (this.isBottom) return this
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
      val resultingEdges = mutable.Set.empty[EdgeWithState[S]]
      for (heapVertex <- newVertices.collect({ case v: HeapVertex => v })) {
        // Setting up source EdgeLocalIdentifiers
        var sourceValState = newGenValState
        for (valField <- heapVertex.typ.nonObjectFields) {
          val srcEdgeLocId = EdgeLocalIdentifier(valField)
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

        val unitContext = SystemParameters.analysisUnitContext
        val method = unitContext.method

        val isInstanceVar = locVarVertex.name.equals("this") &&
          !method.modifiers.contains(StaticModifier)

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
              val trgEdgeLocId = EdgeLocalIdentifier(valField)
              val valHeapId = ValueHeapIdentifier(heapVertex, valField)
              trgValState = trgValState.createVariable(trgEdgeLocId, trgEdgeLocId.getType)
              trgValState = trgValState.assume(new BinaryArithmeticExpression(valHeapId, trgEdgeLocId, ArithmeticOperator.==, null))
            }
            resultingEdges += EdgeWithState(locVarVertex, trgValState, None, heapVertex)
          }
        }
      }
      val newAbstractHeap = HeapGraph[S](newVertices, resultingEdges.toSet)
      ValueDrivenHeapState(newAbstractHeap, newGenValState, ExpressionSet(variable), isTop, isBottom)
    } else {
      // Arguments that are not objects are values and can not be aliased. Therefore, we just create them in the
      // ordinary fashion.
      createVariable(variable, typ)
    }
  }

  def assignVariable(left: ExpressionSet, right: ExpressionSet): ValueDrivenHeapState[S] = {
    if (isBottom) return this
    if (right.isTop)
      return setVariableToTop(left).removeExpression()
    var result = this
    assert(left.getSetOfExpressions.size == 1 && right.getSetOfExpressions.size == 1,
      "More than one identifier or right expression are to be assigned.")
    val leftExp = left.getSetOfExpressions.head
    val rightExp = right.getSetOfExpressions.head
    leftExp match {
      case variable: VariableIdentifier => {
        if (leftExp.getType.isNumericalType()) {
          result = evalExp(rightExp).apply().map(_.assign(variable, rightExp)).join
        } else {
          val varVertex = abstractHeap.localVarVertex(variable.getName)
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
                  val edgeLocalId = EdgeLocalIdentifier(valField)
                  addedIdentifiers = addedIdentifiers + edgeLocalId
                  newEdgeState = newEdgeState.createVariable(edgeLocalId, edgeLocalId.getType)
                  val resId = ValueHeapIdentifier(verExpr.vertex.asInstanceOf[HeapVertex], valField)
                  newEdgeState = newEdgeState.assume(new BinaryArithmeticExpression(resId, edgeLocalId, ArithmeticOperator.==, null))
                }
              }
              edgesToAdd = edgesToAdd + EdgeWithState(varVertex, newEdgeState, None, verExpr.vertex)
            }
            case v: VariableIdentifier => {
              val edgesOfRight = abstractHeap.edges.filter(_.source.name == v.getName)
              val sourceVertex = abstractHeap.localVarVertex(variable.getName)
              for (edge <- edgesOfRight) {
                edgesToAdd = edgesToAdd + EdgeWithState(sourceVertex, edge.state, None, edge.target)
              }
            }
            case rAP: AccessPathIdentifier => {
              val rightPaths = abstractHeap.paths(rAP.path)
              for (rPath <- rightPaths) {
                val rCond = rPath.condition
                if (!rCond.lessEqual(rCond.bottom())) {
                  edgesToAdd = edgesToAdd + EdgeWithState(varVertex, rCond, None, rPath.target)
                }
              }
            }
            case c: Constant => {
              assert(c.toString() == "null", "The only object constant is null.")
              val (tempAH, nullVertex) = abstractHeap.addNewVertex(VertexConstants.NULL, c.getType)
              val newState = ValueDrivenHeapState(tempAH, generalValState, left)
              return newState.assignVariable(left, ExpressionSet(new VertexExpression(variable.typ, nullVertex)(c.pp)))
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
            result = ValueDrivenHeapState(tempAH, generalValState, ExpressionSet(), false, isBottom).prune()
          }
        }
      }
      case _ => throw new Exception("Left-hand side of variable assignment is not a variable.")
    }
    assert(result.abstractHeap.isNormalized, "The abstract heap is not normalized.")
    result
  }

  /**
   * Replaces all `VariableIdentifier`s in the given expression
   * with a corresponding `AccessPathIdentifier`.
   */
  private def normalizeExpression(exp: Expression): Expression =
    exp.transform({
      case v: VariableIdentifier => AccessPathIdentifier(v)
      case e => e
    })

  /**
   * Convenience method that converts the state to a `CondHeapGraph` and
   * evaluates the given expression.
   */
  private def evalExp(expr: Expression): CondHeapGraphSeq[S] =
    CondHeapGraph(this).evalExp(expr)

  def assignField(obj: ExpressionSet, field: String, right: ExpressionSet): ValueDrivenHeapState[S] = {
    if (isBottom) return this
    assert(right.getSetOfExpressions.size == 1, "We allow to assign only single expression")
    val leftExp = obj.getSetOfExpressions.head
    val rightExp = right.getSetOfExpressions.head
    assert(leftExp.isInstanceOf[AccessPathIdentifier], "The left hand side od the assignment is not an AccessPathIdentifier")
    val leftAccPath = leftExp.asInstanceOf[AccessPathIdentifier]
    val leftPaths = abstractHeap.getPathsToBeAssigned(leftAccPath).filter(_.target.isInstanceOf[HeapVertex])
    if (leftPaths.size == 0)
      return this.bottom()

    // TODO: The 'field' parameter is actually an empty string
    val actualField = leftAccPath.path.last

    import CondHeapGraph._

    if (rightExp.getType.isObject()) {
      var edgesToAdd = Set.empty[EdgeWithState[S]]
      rightExp match {
        case x: VariableIdentifier => {
          val rightPaths = abstractHeap.paths(List(x.getName))
          edgesToAdd = referencePathAssignmentEdges(leftAccPath.path.last, leftPaths, rightPaths)
        }
        case rAP: AccessPathIdentifier => {
          val rightPaths = abstractHeap.paths(rAP.path)
          edgesToAdd = referencePathAssignmentEdges(leftAccPath.path.last, leftPaths, rightPaths)
        }
        case v: VertexExpression => {
          // We assume that all edges have ValueHeapIdentifiers for the given vertex expression
          // Add new edges
          for (lPath <- leftPaths) {
            // leftCond should contain source EdgeLocalIdentifiers
            val leftCond = lPath.condition
            // We build the replacement that for each ValueHeapIdentifier corresponding to the vertex, expands it to
            // target EdgeLocalIdentifier.
            val repl = new Replacement()
            for (id <- leftCond.getIds().collect({ case id: ValueHeapIdentifier if id.obj.equals(v.vertex) => id })) {
              repl.value.update(Set(id), Set(id, EdgeLocalIdentifier(List(leftAccPath.path.last), id)))
            }
            edgesToAdd = edgesToAdd + EdgeWithState(lPath.target, leftCond.merge(repl), Some(leftAccPath.path.last), v.vertex)
          }
        }
        case c : Constant => {
          assert(c.toString.equals("null"), "We expect only null constants.")
          val (newAH, nullVertex) = abstractHeap.addNewVertex(VertexConstants.NULL, c.getType)
          return ValueDrivenHeapState(newAH, generalValState, expr).assignField(obj, field, ExpressionSet(new VertexExpression(c.getType, nullVertex)(c.getProgramPoint)))
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
      assert(rightExp.getType.isNumericalType(), "only numerical values allowed")

      evalExp(leftExp).intersect(evalExp(rightExp)).apply().mapCondHeaps(condHeap => {
        val leftTakenPath = condHeap.takenPath(leftAccPath.objPath)
        val vertexToAssign = leftTakenPath.target.asInstanceOf[HeapVertex]
        val idToAssign = ValueHeapIdentifier(vertexToAssign, actualField, leftExp.getType, leftExp.getProgramPoint)

        val condHeapAssigned = condHeap
          .map(_.assign(idToAssign, rightExp))
          .mapEdges(edge => {
            var newState = edge.state
            if (edge.source == vertexToAssign) {
              val edgeLocId = EdgeLocalIdentifier(List.empty, actualField, rightExp.getType)(rightExp.getProgramPoint)
              newState = newState.assign(edgeLocId, rightExp)
            }
            if (edge.target == vertexToAssign && !edge.source.isInstanceOf[SummaryHeapVertex]) {
              val path = List(edge.field).flatten
              val edgeLocId = EdgeLocalIdentifier(path, actualField, rightExp.getType)(rightExp.getProgramPoint)
              newState = newState.assign(edgeLocId, rightExp)
            }
            newState
          })

        // Perform a weak update if assigning to a summary heap vertex
        if (vertexToAssign.isInstanceOf[SummaryHeapVertex]) Seq(condHeap, condHeapAssigned)
        else Seq(condHeapAssigned)
      }).join
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
  private def referencePathAssignmentEdges(
      field: String,
      leftPaths: Set[RootedHeapGraphPath[S]],
      rightPaths: Set[RootedHeapGraphPath[S]]): Set[EdgeWithState[S]] = {
    var edgesToAdd = Set.empty[EdgeWithState[S]]
    for (lPath <- leftPaths) {
      var leftCond = lPath.condition
      if (!leftCond.lessEqual(leftCond.bottom())) {
        // The condition of the left path is not bottom. (i.e. can be possibly assigned)
        for(rPath <- rightPaths) {
          val rightCond = rPath.condition
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
            edgesToAdd = edgesToAdd + EdgeWithState(lPath.target, newEdgeState, Some(field), rPath.target)
          }
        }
      }
    }
    edgesToAdd
  }

  // For the obsolete methods `evaluateExpression` and `evaluateGraphPath`,
  // see https://bitbucket.org/semperproject/sample/commits/e1f5be3

  def setArgument(x: ExpressionSet, right: ExpressionSet): ValueDrivenHeapState[S] = ???
  def setVariableToTop(x: ExpressionSet): ValueDrivenHeapState[S] = ???
  def removeVariable(x: ExpressionSet): ValueDrivenHeapState[S] = ???
  def throws(t: ExpressionSet): ValueDrivenHeapState[S] = ???

  def getVariableValue(id: Assignable): ValueDrivenHeapState[S] = {
    if(this.isBottom) return this
    assert(id.isInstanceOf[VariableIdentifier], "This should be VariableIdentifier.")
    copy(expr = ExpressionSet(id.asInstanceOf[VariableIdentifier]))
  }

  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): ValueDrivenHeapState[S] = {
    assert(obj.getSetOfExpressions.size == 1, "We only support single field access.")
    assert(obj.getSetOfExpressions.head.isInstanceOf[AccessPathIdentifier], "The field access should be accessed via access path.")
    // TODO: May be I should check whether this exist and is feasible already here.
    if (ValueDrivenHeapProperty.materialize) {
      val apObj = obj.getSetOfExpressions.head.asInstanceOf[AccessPathIdentifier]
      val tempResult = materializePath(apObj.objPath)
      tempResult.copy(expr = new ExpressionSet(typ).add(obj))
    } else
      copy(expr = new ExpressionSet(typ).add(obj))
  }

  def evalConstant(value: String, typ: Type, pp: ProgramPoint): ValueDrivenHeapState[S] = {
    if(this.isBottom) return this
    this.setExpression(ExpressionSet(new Constant(value, typ, pp)))
  }

  def assume(cond: ExpressionSet): ValueDrivenHeapState[S] = {
    if (isBottom) return this

    import CondHeapGraph._

    assert(cond.getSetOfExpressions.size == 1, "Condition of several expressions are not supported.")
    val condition = cond.getSetOfExpressions.head

    def notSupported() = {
      println(s"ValueDrivenHeapState.assume: $condition is not supported.")
      this
    }

    val result: ValueDrivenHeapState[S] = condition match {
      case Constant("false", _, _) => bottom()
      case Constant("true", _, _) => this
      case NegatedBooleanExpression(e) => {
        assume(ExpressionSet(Utilities.negateExpression(e)))
      }
      case BinaryBooleanExpression(l,r,o,t) => {
        val result = o match {
          case BooleanOperator.&& =>
            assume(ExpressionSet(l)).assume(ExpressionSet(r))
          case BooleanOperator.|| =>
            assume(ExpressionSet(l)).lub(assume(ExpressionSet(r)))
        }
        assert(result.abstractHeap.isNormalized, "The abstract heap is not normalized.")
        result
      }
      case exp: BinaryArithmeticExpression =>
        evalExp(exp).apply().map(_.assume(exp)).join
      case ReferenceComparisonExpression(_left, _right, op, returnTyp) => {
        assert(_left.getType.isObject(),
          "Reference comparison can be performed only on objects, not values.")
        assert(_right.getType.isObject(),
          "Reference comparison can be performed only on objects, not values.")

        val left = normalizeExpression(_left)
        val right = normalizeExpression(_right)

        import ArithmeticOperator._

        left match {
          case Constant("null", _, _) => right match {
            case Constant("null", _, _) =>
              op match { case `==` => this case `!=` => bottom() }
            case AccessPathIdentifier(path) =>
              // It's unsound to remove null/non-null edges if there are
              // summary heap vertices on the path. Thus, materialize first.
              val state = if (path.size == 1) this else materializePath(path.dropRight(1))
              var result = bottom()
              val paths = state.abstractHeap.paths(path)

              // The very last vertex on a path may be a summary node
              val sourceVertices = paths.map(_.edges.map(_.source)).flatten
              assert(sourceVertices.forall(!_.isInstanceOf[SummaryHeapVertex]),
                "paths contain summary source vertices (materialization bug?)")

              // TODO: Ignore impossible paths (bottom path condition)
              paths.map(_.edges.last).groupBy(_.source).values.foreach(edges => {
                val (nullEdges, nonNullEdges) = edges.partition(_.target.isInstanceOf[NullVertex])
                val edgesToRemove = op match { case `==` => nonNullEdges case `!=` => nullEdges }
                result = result.lub(state.copy(abstractHeap = state.abstractHeap.removeEdges(edgesToRemove)))
              })
              result.prune()
            case _ => notSupported()
          }
          case _ => right match {
            case Constant("null", _, _) =>
              assume(new ExpressionSet(cond.getType()).add(
                new ReferenceComparisonExpression(right, left, op, returnTyp)))
            case _ => notSupported()
          }
        }
      }
      case _ => notSupported()
    }
    result
    // See version control history for the original code
  }

  def testTrue(): ValueDrivenHeapState[S] = {
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
    copy(expr = ExpressionSet())

  def pruneUnreachableHeap(): ValueDrivenHeapState[S] = ???
  def factory(): ValueDrivenHeapState[S] = ???

  def top(): ValueDrivenHeapState[S] =
    ValueDrivenHeapState(HeapGraph(), generalValState.top(), ExpressionSet(), isTop = true, isBottom = false)

  def bottom(): ValueDrivenHeapState[S] =
    ValueDrivenHeapState(HeapGraph(), generalValState.bottom(), expr, isTop = false, isBottom = true)

  def lub(other: ValueDrivenHeapState[S]): ValueDrivenHeapState[S] = {
    if (isBottom || other.isTop)
      return other
    if (isTop || other.isBottom)
      return this
    // TODO: Implement this properly
    val (resAH, renameMap) = abstractHeap.lub(other.abstractHeap)
    val resGeneralState = generalValState.lub(other.generalValState.rename(renameMap))

    ValueDrivenHeapState(resAH, resGeneralState, ExpressionSet())
  }

  def glb(other: ValueDrivenHeapState[S]): ValueDrivenHeapState[S] = {
    if (isBottom || other.isBottom)
      return bottom()
    if (isTop)
      return other
    if (other.isTop)
      return this
    val (resultingAH, removeIds, renameMap) = abstractHeap.glb(other.abstractHeap)
    var newRightGeneralValState = other.generalValState.removeVariables(removeIds)
    newRightGeneralValState = newRightGeneralValState.rename(renameMap)
    val newGeneralValState = generalValState.glb(newRightGeneralValState)
    if (resultingAH.isBottom() || newGeneralValState.lessEqual(newGeneralValState.bottom()))
      return bottom()
    return ValueDrivenHeapState(resultingAH, newGeneralValState, ExpressionSet())
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

    val (mergedLeft, replacementLeft) = abstractHeap.mergePointedNodes()
    val (mergedRight, replacementRight) = other.abstractHeap.mergePointedNodes()
    val rightGenValState = other.generalValState.merge(replacementRight)
    var newRight = ValueDrivenHeapState(mergedRight, rightGenValState, ExpressionSet())
    val newLeft = ValueDrivenHeapState(mergedLeft, generalValState.merge(replacementLeft), ExpressionSet())
    newRight = newLeft.lub(newRight)
    if (!mergedLeft.vertices.equals(newRight.abstractHeap.vertices) || !areGraphsIdentical(mergedLeft, mergedRight)) {
      return newRight
    }
    val newGeneralValState = newLeft.generalValState.widening(newRight.generalValState.merge(replacementRight))
    ValueDrivenHeapState(mergedLeft.wideningAfterMerge(newRight.abstractHeap), newGeneralValState, ExpressionSet())
  }

  private def materializePath(pathToMaterialize : List[String]) : ValueDrivenHeapState[S] = {
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
          // TODO: Should use a multimap to make the code more readable
          repl.value.update(Set(sumValHeapId), repl.value.getOrElse(Set(sumValHeapId), Set(sumValHeapId.asInstanceOf[Identifier])) union Set(defValHeapId))
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
    copy(abstractHeap = resultingAH, generalValState = generalValState.merge(repl)).prune()
  }

  def lessEqual(r: ValueDrivenHeapState[S]): Boolean = {
    // TODO: Implement properly
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
      val edgeLocalId = EdgeLocalIdentifier(valField)
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

    ValueDrivenHeapState(newAbstractHeap,
      newGeneralState,
      ExpressionSet(VertexExpression(typ, createdObjVertex)(pp)),
      isTop, isBottom)
  }

  /**
   * Prunes the abstract heap and removes all pruned identifiers
   * from the general value state.
   */
  def prune(): ValueDrivenHeapState[S] = {
    val (newAbstractHeap, idsToRemove) = abstractHeap.prune()
    val newGeneralValState = generalValState.removeVariables(idsToRemove)
    if (newAbstractHeap.isBottom() || newGeneralValState.lessEqual(newGeneralValState.bottom()))
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