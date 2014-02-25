package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import scala.collection.mutable
import VertexConstants._

trait ValueDrivenHeapState[
    S <: SemanticDomain[S],
    T <: ValueDrivenHeapState[S, T]]
  extends SimpleState[T] with StateWithCollectionStubs[T] { self: T =>

  val abstractHeap: HeapGraph[S]
  val generalValState: S
  val expr: ExpressionSet
  val isTop: Boolean

  require(!isTop || !isBottom, "cannot be top and bottom at the same time")

  /** Builds a copy of this value-driven heap state overriding fields
    * at the discretion of the caller.
    */
  def copy(
      abstractHeap: HeapGraph[S] = abstractHeap,
      generalValState: S = generalValState,
      expr: ExpressionSet = expr,
      isTop: Boolean = isTop): T =
    factory(abstractHeap, generalValState, expr, isTop)

  /** Builds a new value-driven heap state. */
  def factory(
      abstractHeap: HeapGraph[S],
      generalValState: S,
      expr: ExpressionSet,
      isTop: Boolean = false): T

  override def isBottom = {
    abstractHeap.isBottom || generalValState.lessEqual(generalValState.bottom())
  }

  def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    if (variable.typ.isObject) {
      // Initialize references variables to null such that the heap is not
      // treated as bottom, making it possible to analyze programs with more
      // than one local reference variable. This is unsound, e.g., since two
      // uninitialized reference variables 'a' and 'b' should not be
      // considered equal.
      // A sound implementation would use a summary node (representing top)
      // similar to `createVariableForArgument`.
      val varVertex = LocalVariableVertex(variable)
      val newVertices = Set(NullVertex, varVertex)
      val edgeToNull = Edge(varVertex, generalValState, None, NullVertex)
      val newAbstractHeap = abstractHeap.addNonHeapVertices(newVertices).addEdge(edgeToNull)
      copy(
        abstractHeap = newAbstractHeap,
        expr = new ExpressionSet(typ).add(variable))
    } else {
      createNonObjectVariables(Set(variable)).copy(expr = ExpressionSet(variable))
    }
  }

  /** Creates non-object variables in both the general value state and also
    * the state of every edge in the heap graph.
    *
    * @param ids Can also be `ValueHeapIdentifier`s. Usually, multiple
    *            identifiers are added at once, so this method takes a set.
    * @return the new state, with unmodified expression
    */
  protected def createNonObjectVariables(ids: Set[Identifier]): T = {
    require(ids.forall(!_.typ.isObject), "cannot create object variables")
    copy(
      abstractHeap = abstractHeap.createVariables(ids),
      generalValState = generalValState.createVariables(ids))
  } ensuring (result =>
    ids.subsetOf(result.generalValState.ids) &&
    result.abstractHeap.edges.forall(e => ids.subsetOf(e.state.ids)),
    "the variable is created in all value states")

  def createVariableForArgument(variable: VariableIdentifier, typ: Type): T = {
    if (variable.typ.isObject) {
      // The current implementation always creates a new state from scratch
      // and thus does not retain any assumptions made earlier.

      // It creates a summary heap vertex for every distinct object type
      // reachable via fields from the current set of parameter variables.
      // Taking into account subtyping, it then creates all possible edges
      // between vertices of the heap, including edges to the null vertex.

      // The old implementation used to create a definite vertex to represent
      // an object that could only be referenced by a single parameter variable
      // and no object field in the whole heap. This is not really worth
      // the effort. In a case as described above, materialization will
      // replace the summary heap vertex with a definite heap vertex anyway.

      val localVarVertices = abstractHeap.localVarVertices + LocalVariableVertex(variable)
      val objectTypes = localVarVertices.flatMap(_.typ.reachableObjectTypes)

      // Create new abstract heap (no edges yet)
      val newAbstractHeap = HeapGraph[S]()
        .addNonHeapVertices(Set(NullVertex) ++ localVarVertices)
        .addHeapVertices(SUMMARY, objectTypes)

      // Create general value state, retaining old variable identifiers
      val newGeneralValState = generalValState.top()
        .createVariables(generalValState.variableIds)
        .createVariables(newAbstractHeap.heapVertices.flatMap(_.valueHeapIds))

      // Create new edges
      val newEdges = newAbstractHeap.vertices.flatMap(sourceVertex => {
        sourceVertex.neededEdgeFieldsAndTypes.flatMap({ case (field, fieldTyp) =>
          newAbstractHeap.possibleTargetVertices(fieldTyp).map(targetVertex => {
            Edge(sourceVertex, newGeneralValState, field, targetVertex)
          })
        })
      }).map(_.createEdgeLocalIds())

      factory(
        newAbstractHeap.addEdges(newEdges),
        newGeneralValState,
        ExpressionSet(variable),
        isTop = true)
    } else {
      // Arguments that are not objects are values and can not be aliased.
      // Therefore, we just create them in the ordinary fashion.
      createVariable(variable, typ, variable.pp)
    }
  }

  def assignVariable(left: Expression, right: Expression): T = {
    var result: T = this
    left match {
      case variable: VariableIdentifier => {
        val normalRight = normalizeExpression(right)
        if (left.typ.isBooleanType) {
          result = evalExp(normalRight).apply().mapCondHeaps(condHeap => {
            val isCertainlyFalse = condHeap.assume(normalRight).isBottom
            val isCertainlyTrue = condHeap.assume(NegatedBooleanExpression(normalRight)).isBottom
            Seq(condHeap.map(state => {
              if (isCertainlyFalse) state.assign(variable, Constant("false", left.typ, normalRight.pp))
              else if (isCertainlyTrue) state.assign(variable, Constant("true", left.typ, normalRight.pp))
              // When neither is certain, fall back to an assignment.
              // The variable will be set to top unless the semantic domain
              // can actually handle the right-hand side.
              else state.assign(variable, right)
            }))
          }).join
        } else if (left.typ.isNumericalType) {
          result = evalExp(right).apply().map(_.assign(variable, right)).join
        } else {
          val varVertex = abstractHeap.localVarVertex(variable.getName)
          val edgesToRemove = abstractHeap.outEdges(varVertex)
          var edgesToAdd = Set.empty[Edge[S]]
          normalizeExpression(right) match {
            case verExpr: VertexExpression => {
              assert(abstractHeap.vertices.contains(verExpr.vertex),
                "Assigning a non-existing node")
              assert(varVertex.typ.equals(verExpr.typ),
                "We support only exact type, that is the fields should be the same")

              var edge = Edge(varVertex, generalValState, None, verExpr.vertex)
              edgesToAdd = edgesToAdd + edge.createEdgeLocalIds()
            }
            case rAP: AccessPathIdentifier => {
              val rightPaths = abstractHeap.paths(rAP.path)
              for (rPath <- rightPaths) {
                val rCond = rPath.condition
                if (!rCond.lessEqual(rCond.bottom())) {
                  // rCond contains *source* edge-local identifiers that refer
                  // to fields of the *target* of the new edge. Thus, in order
                  // to use rCond as the state of the new edge, one needs to
                  // rename the source edge-local identifiers to target edge-
                  // local identifiers first.

                  // TODO: This code is very similar to the one in
                  // referencePathAssignmentEdges.
                  val renameMap = rCond.edgeLocalIds.map(id => {
                    id -> id.copy(accPath = List(None))(id.pp)
                  }).toMap

                  val newEdgeState = rCond.rename(renameMap)
                  edgesToAdd = edgesToAdd + Edge(varVertex, newEdgeState, None, rPath.target)
                }
              }
            }
            case c: Constant => {
              assert(c.toString == "null", "The only object constant is null.")
              val tempAH = abstractHeap.addNonHeapVertex(NullVertex)
              val newState = copy(abstractHeap = tempAH)
              return newState.assignVariable(left, new VertexExpression(variable.typ, NullVertex)(c.pp))
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
            result = copy(abstractHeap = tempAH, isTop = false).prune()
          }
        }
      }
      case _ => throw new Exception("Left-hand side of variable assignment is not a variable.")
    }
    assert(result.abstractHeap.isNormalized, "The abstract heap is not normalized.")
    result
  }

  /**
   * Convenience method that converts the state to a `CondHeapGraph` and
   * evaluates the given expression.
   */
  private def evalExp(expr: Expression): CondHeapGraphSeq[S] =
    CondHeapGraph[S, T](this).evalExp(expr)

  /** Assigns an expression to a field and returns the resulting state.
    *
    * The value-driven heap analysis does not use the `field` parameter.
    * Instead, both the receiver and field must be represented by `left` as an
    * `AccessPathIdentifier`.
    *
    * This is a wrapper method so the actual implementation does not need to
    * perform checks and casts and there is no danger of accidentally using the
    * (possibly) arbitrary value of `field`.
    */
  final def assignField(left: Expression, field: String, right: Expression): T = left match {
    case left: AccessPathIdentifier => assignField(left, right)
    case _ => throw new IllegalArgumentException(
      "LHS must be an AccessPathIdentifier")
  }

  def assignField(left: AccessPathIdentifier, right: Expression): T = {
    val leftPaths = abstractHeap.getPathsToBeAssigned(left).filter(_.target.isInstanceOf[HeapVertex])
    if (leftPaths.size == 0)
      return this.bottom()

    if (right.typ.isObject) {
      var edgesToAdd = Set.empty[Edge[S]]
      normalizeExpression(right) match {
        case rAP: AccessPathIdentifier => {
          val rightPaths = abstractHeap.paths(rAP.path)
          edgesToAdd = referencePathAssignmentEdges(left.path.last, leftPaths, rightPaths)
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
            for (id <- leftCond.ids.collect({
              case id: ValueHeapIdentifier if id.obj.equals(v.vertex) => id
            })) {
              repl.value.update(Set(id), Set(id, EdgeLocalIdentifier(List(Some(left.path.last)), id)))
            }
            edgesToAdd = edgesToAdd + Edge(lPath.target, leftCond.merge(repl), Some(left.path.last), v.vertex)
          }
        }
        case c: Constant => {
          assert(c.toString.equals("null"), "We expect only null constants.")
          val newAH = abstractHeap.addNonHeapVertex(NullVertex)
          return copy(abstractHeap = newAH).assignField(left, new VertexExpression(c.typ, NullVertex)(c.pp))
        }
        case _ => throw new Exception("Assigning " + right + " is not allowed (or supported:)). ")
      }
      // If there is no possible assignment, return bottom
      if (edgesToAdd.isEmpty)
        return bottom()
      // If the assignment is strong, remove edges that are killed by the assignment. Then add edges representing the
      // assignment. Last, prune the heap.
      var resultingAH: HeapGraph[S] = abstractHeap
      val sources: Set[Vertex] = edgesToAdd.map(_.source)
      if (sources.size == 1 && (sources.head.isInstanceOf[DefiniteHeapVertex] || sources.head.isInstanceOf[LocalVariableVertex])) {
        // Strong update - removing the edges from the target of the path labeled with the assigned filed
        val lastPathVertex: Vertex = sources.head
        val edgesToRemove = resultingAH.outEdges(lastPathVertex, Some(left.path.last))
        resultingAH = resultingAH.removeEdges(edgesToRemove)
      }
      resultingAH = resultingAH.addEdges(edgesToAdd)
      resultingAH = resultingAH.joinCommonEdges()
      copy(abstractHeap = resultingAH, isTop = false).prune()
    } else {
      evalExp(left).intersect(evalExp(right)).apply().mapCondHeaps(_.assignField(left, right)).join
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
      leftPaths: Set[RootedPath[S]],
      rightPaths: Set[RootedPath[S]]): Set[Edge[S]] = {
    var edgesToAdd = Set.empty[Edge[S]]
    for (lPath <- leftPaths) {
      var leftCond = lPath.condition
      if (!leftCond.lessEqual(leftCond.bottom())) {
        // The condition of the left path is not bottom. (i.e. can be possibly assigned)
        for (rPath <- rightPaths) {
          val rightCond = rPath.condition
          val renameMap = rightCond.edgeLocalIds.map(id => {
            id -> id.copy(accPath = List(Some(field)))(id.pp)
          }).toMap

          var newEdgeState = rightCond.rename(renameMap)
          if (renameMap.isEmpty) {
            // This is the case when RHS is null. Hence, we need to create
            // source edge-local identifiers in the right state in order to
            // preserve the ones from LHS.
            val sourceIdsOfLHS = leftCond.sourceEdgeLocalIds
            newEdgeState = newEdgeState.createVariables(sourceIdsOfLHS)
          }
          leftCond = leftCond.createVariables(renameMap.values.toSet)
          newEdgeState = newEdgeState.createVariables(renameMap.keySet)
          newEdgeState = leftCond.glb(newEdgeState)
          if (!newEdgeState.lessEqual(rightCond.bottom())) {
            // add edge that represents the assignment
            edgesToAdd = edgesToAdd + Edge(lPath.target, newEdgeState, Some(field), rPath.target)
          }
        }
      }
    }
    edgesToAdd
  }

  // For the obsolete methods `evaluateExpression` and `evaluateGraphPath`,
  // see https://bitbucket.org/semperproject/sample/commits/e1f5be3

  /** Delegates to `getVariableValue(id: VariableIdentifier)`. */
  final def getVariableValue(id: Assignable): T = id match {
    case id: VariableIdentifier => getVariableValue(id)
    case _ => throw new IllegalArgumentException(
      "variable access must occur via a variable identifier")
  }

  def getVariableValue(id: VariableIdentifier): T = {
    val result = if (id.typ.isObject) materializePath(List(id.name)) else this
    result.copy(expr = ExpressionSet(id))
  }

  /** Delegates to `getFieldValue(id: AccessPathIdentifier)`. */
  final def getFieldValue(obj: Expression, field: String, typ: Type): T = obj match {
    case obj: AccessPathIdentifier => getFieldValue(obj)
    case _ => throw new IllegalArgumentException(
      "field access must occur via an access path identifier")
  }

  def getFieldValue(id: AccessPathIdentifier): T = {
    materializePath(id.objPath).copy(expr = ExpressionSet(id))
  }

  def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    if (this.isBottom) return this
    this.setExpression(ExpressionSet(new Constant(value, typ, pp)))
  }

  def assume(cond: Expression): T =
    CondHeapGraph[S, T](this).assume(normalizeExpression(cond)).join

  def setExpression(newExpr: ExpressionSet): T =
    copy(expr = newExpr)

  def removeExpression(): T =
    copy(expr = ExpressionSet())

  def top(): T =
    factory(HeapGraph(), generalValState.top(), ExpressionSet(), isTop = true)

  def bottom(): T =
    factory(HeapGraph(), generalValState.bottom(), expr, isTop = false)

  def lub(other: T): T = {
    if (isBottom || other.isTop)
      return other
    if (isTop || other.isBottom)
      return this
    // TODO: Implement this properly
    val (resAH, renameMap) = abstractHeap.lub(other.abstractHeap)
    val valueRenameMap = Vertex.vertexMapToValueHeapIdMap(renameMap)
    val resGeneralState = generalValState.lub(other.generalValState.rename(valueRenameMap.toMap))

    factory(resAH, resGeneralState, ExpressionSet())
  }

  def glb(other: T): T = {
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
    factory(resultingAH, newGeneralValState, ExpressionSet())
  }

  def widening(other: T): T = {
    def areGraphsIdentical(l: HeapGraph[S], r: HeapGraph[S]): Boolean = {
      var areGraphsIdentical = true
      for (rEdge <- r.edges) {
        areGraphsIdentical = areGraphsIdentical && {
          val edgeSet = l.edges.filter(lEdge => lEdge.source.equals(rEdge.source) && lEdge.target.equals(rEdge.target))
          edgeSet.size == 1 && edgeSet.head.state.ids == rEdge.state.ids
        }
      }
      areGraphsIdentical
    }

    val (mergedLeft, replacementLeft, mergeMapLeft) = abstractHeap.mergePointedNodes()
    val (mergedRight, replacementRight, mergeMapRight) = other.abstractHeap.mergePointedNodes()
    val rightGenValState = other.generalValState.merge(replacementRight)
    var newRight = factory(mergedRight, rightGenValState, ExpressionSet())
    val newLeft = factory(mergedLeft, generalValState.merge(replacementLeft), ExpressionSet())
    newRight = newLeft.lub(newRight)
    if (!mergedLeft.vertices.equals(newRight.abstractHeap.vertices) || !areGraphsIdentical(mergedLeft, mergedRight)) {
      return newRight
    }
    val newGeneralValState = newLeft.generalValState.widening(newRight.generalValState.merge(replacementRight))
    factory(mergedLeft.wideningAfterMerge(newRight.abstractHeap), newGeneralValState, ExpressionSet())
  }

  protected def materializePath(pathToMaterialize: List[String]): T = {
    if (!ValueDrivenHeapProperty.materialize) return this

    val edgesToAdd = mutable.Set.empty[Edge[S]]
    val edgesToRemove = mutable.Set.empty[Edge[S]]
    val repl = new Replacement(isPureExpanding = true)
    var resultingAH = abstractHeap
    val queue = mutable.Queue.empty[(Edge[S], List[String])]
    for (e <- abstractHeap.edges.filter(edg => edg.source.name.equals(pathToMaterialize.head) && edg.target.isInstanceOf[HeapVertex]))
      queue.enqueue((e, pathToMaterialize.tail))
    while (!queue.isEmpty) {
      val (edge, path) = queue.dequeue()
      assert(edge.source.isInstanceOf[DefiniteHeapVertex] || edge.source.isInstanceOf[LocalVariableVertex])
      edge.target match {
        case summaryVertex: SummaryHeapVertex =>
          edgesToRemove += edge
          // Creating a vertex that is a materialization of the summary vertex
          val (tempAH, definiteVertex) = resultingAH.addHeapVertex(VertexConstants.DEFINITE, summaryVertex.typ)
          resultingAH = tempAH
          // Add the information about the corresponding identifiers to replacement
          for (valField <- definiteVertex.typ.nonObjectFields) {
            val sumValHeapId = ValueHeapIdentifier(summaryVertex, valField)
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
            if (e.target == summaryVertex &&
              (!ValueDrivenHeapProperty.materializeOnlyAcyclic || e.source != e.target)) {
              edgesToAdd += e.copy(target = definiteVertex)
            }
            // Outgoing edges
            if (e.source == summaryVertex) {
              val edgeToAdd = e.copy[S](source = definiteVertex)
              if (!path.isEmpty && edgeToAdd.field.equals(Some(path.head)))
                queue.enqueue((edgeToAdd, path.tail))
              edgesToAdd += edgeToAdd
            }
            // Self-loop edges
            if (e.source == summaryVertex && e.target == summaryVertex && !ValueDrivenHeapProperty.materializeOnlyAcyclic) {
              val edgeToAdd = e.copy[S](source = definiteVertex, target = definiteVertex)
              if (!path.isEmpty && edgeToAdd.field.equals(Some(path.head)))
                queue.enqueue((edgeToAdd, path.tail))
              edgesToAdd += edgeToAdd
            }
          }
        case _ =>
          // Nothing to materialize for this edge
          if (!path.isEmpty)
            for (e <- (resultingAH.edges -- edgesToRemove ++ edgesToAdd).filter(edg => edg.source.equals(edge.target) && edg.field.equals(Some(path.head)) && edg.target.isInstanceOf[HeapVertex]))
              queue.enqueue((e, path.tail))
      }
    }
    resultingAH = resultingAH.removeEdges(edgesToRemove.toSet)
    resultingAH = resultingAH.applyReplacement(repl)
    // Updating source and target EdgeLocalIdentifiers in the edges to add.
    val updatedEdgesToAdd = mutable.Set.empty[Edge[S]]
    for (e <- edgesToAdd) {
      // Updating EdgeLocalIdentifiers with empty path
      var updatedEdgeState = e.state.merge(repl)
      val vtx = if (e.source.isInstanceOf[LocalVariableVertex]) e.target else e.source
      for (valHeapId <- updatedEdgeState.ids.collect({
        case id: ValueHeapIdentifier if id.obj == vtx => id
      })) {
        val edgLocId = EdgeLocalIdentifier(List.empty, valHeapId.field, valHeapId.typ)(valHeapId.pp)
        updatedEdgeState = updatedEdgeState.assume(BinaryArithmeticExpression(valHeapId, edgLocId, ArithmeticOperator.==))
      }
      // Updating EdgeLocalIdentifiers with non-empty path
      if (e.target.isInstanceOf[HeapVertex] && !e.source.isInstanceOf[LocalVariableVertex]) {
        for (valHeapId <- updatedEdgeState.ids.collect({
          case id: ValueHeapIdentifier if id.obj == e.target => id
        })) {
          val edgLocId = EdgeLocalIdentifier(List(e.field), valHeapId.field, valHeapId.typ)(valHeapId.pp)
          updatedEdgeState = updatedEdgeState.assume(BinaryArithmeticExpression(valHeapId, edgLocId, ArithmeticOperator.==))
        }
      }
      updatedEdgesToAdd += e.copy(state = updatedEdgeState)
    }
    resultingAH = resultingAH.addEdges(updatedEdgesToAdd.toSet)
    copy(abstractHeap = resultingAH, generalValState = generalValState.merge(repl)).prune()
  }

  def lessEqual(r: T): Boolean = {
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

  def createObject(typ: Type, pp: ProgramPoint, fields: Option[Set[Identifier]]): T = {
    if (isBottom) return this

    // Create the new heap vertex and ensure that there is a null vertex
    var (newAbstractHeap, newVertex) = abstractHeap.addDefiniteHeapVertex(typ)
    newAbstractHeap = newAbstractHeap.addNonHeapVertex(NullVertex)

    // Create the value heap identifiers in all states
    var newState = copy(abstractHeap = newAbstractHeap)
    newState = newState.createNonObjectVariables(newVertex.valueHeapIds)

    // Create the new edges
    val newEdges = typ.objectFields.map(field => {
      Edge(newVertex, newState.generalValState, Some(field.getName), NullVertex)
    }).map(_.createEdgeLocalIds())

    newState.copy(
      abstractHeap = newAbstractHeap.addEdges(newEdges),
      expr = ExpressionSet(VertexExpression(typ, newVertex)(pp)))
  }

  /**
   * Implicitly converts a conditional heap graph to a state
   * with an empty expression. It also automatically prunes the state.
   *
   * @todo Pruning should happen before the conversion
   */
  implicit def CondHeapGraphToValueDrivenHeapState(condHeap: CondHeapGraph[S]): T =
    factory(condHeap.heap, condHeap.cond, ExpressionSet()).prune()

  /**
   * Prunes the abstract heap and removes all pruned identifiers
   * from the general value state.
   */
  def prune(): T = {
    val (newAbstractHeap, idsToRemove) = abstractHeap.prune()
    val newGeneralValState = generalValState.removeVariables(idsToRemove)
    copy(abstractHeap = newAbstractHeap, generalValState = newGeneralValState)
  }

  override def toString: String =
    s"ValueDrivenHeapState(${abstractHeap.vertices.size} vertices, " +
      s"${abstractHeap.edges.size} edges)"

  def before(pp: ProgramPoint): T = this

  def setArgument(x: ExpressionSet, right: ExpressionSet): T = ???
  def setVariableToTop(x: ExpressionSet): T = ???
  def removeVariable(x: ExpressionSet): T = ???
  def throws(t: ExpressionSet): T = ???
  def pruneUnreachableHeap(): T = ???
  def factory(): T = ???
  def removeObject(oldPreState: T, obj: ExpressionSet, fields: Option[Set[Identifier]]): T = ???
  def undoPruneVariables(unprunedPreState: T, filter: (Identifier) => Boolean): T = ???
  def undoPruneUnreachableHeap(preState: T): T = ???
  def nonDeterminismSourceAt(pp: ProgramPoint, typ: Type): T = ???
  def pruneVariables(filter: (Identifier) => Boolean): T = ???
  def optimizeSummaryNodes(): T = ???
  def backwardGetVariableValue(id: Assignable): T = ???
  def backwardGetFieldValue(obj: ExpressionSet, field: String, typ: Type): T = ???
  def backwardAssignVariable(x: ExpressionSet, right: ExpressionSet): T = ???
  def backwardAssignVariable(oldPreState: T, x: Expression, right: Expression): T = ???
  def testBackwardTrue(): T = ???
  def testBackwardFalse(): T = ???
  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, summary: Boolean): T = ???
  def getNonDeterminismSource(pp: ProgramPoint, typ: Type): T = ???
  def backwardAssignField(oldPreState: T, obj: Expression, field: String, right: Expression): T = ???
}

object ValueDrivenHeapState {
  /** Default implementation of `ValueDrivenHeapState`. */
  case class Default[S <: SemanticDomain[S]](
      abstractHeap: HeapGraph[S],
      generalValState: S,
      expr: ExpressionSet,
      isTop: Boolean = false)
    extends ValueDrivenHeapState[S, Default[S]] {

    def factory(
        abstractHeap: HeapGraph[S] = abstractHeap,
        generalValState: S = generalValState,
        expr: ExpressionSet = expr,
        isTop: Boolean = isTop): Default[S] = {
      Default(abstractHeap, generalValState, expr, isTop)
    }
  }
}


object ValueDrivenHeapStateConstants {
  val edgeLocalIdentifier = "eLocId"
}