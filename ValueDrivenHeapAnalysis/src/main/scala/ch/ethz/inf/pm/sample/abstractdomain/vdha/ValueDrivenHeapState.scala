/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import scala.collection.mutable
import VertexConstants._
import scala.language.implicitConversions

trait ValueDrivenHeapState[
    S <: SemanticDomain[S],
    T <: ValueDrivenHeapState[S, T]]
  extends SimpleState[T] { self: T =>

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

  /** Applies a function to both the general value state and all edge states. */
  def map(f: S => S): T =
    toCondHeapGraph.map(f)

  /** Uses a function to compute the new state of each edge in the heap graph. */
  def mapEdges(f: Edge[S] => S): T =
    toCondHeapGraph.mapEdges(f)

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
      createNonObjectVariables(IdentifierSet.Inner(Set(variable))).copy(expr = ExpressionSet(variable))
    }
  }

  def removeVariable(id: VariableIdentifier): T = {
    if (id.typ.isObject) {
      val vertex = LocalVariableVertex(id)
      // Prune the heap as parts of it may have become unreachable
      copy(abstractHeap = abstractHeap.removeVertices(Set(vertex))).prune()
    } else {
      copy(
        abstractHeap = abstractHeap.removeVariable(id),
        generalValState = generalValState.removeVariable(id))
    }
  }

  /** Creates non-object variables in both the general value state and also
    * the state of every edge in the heap graph.
    *
    * @param ids Can also be `ValueHeapIdentifier`s. Usually, multiple
    *            identifiers are added at once, so this method takes a set.
    * @return the new state, with unmodified expression
    */
  protected def createNonObjectVariables(ids: IdentifierSet): T = {
    require(ids.getNonTopUnsafe.forall(!_.typ.isObject), "cannot create object variables")
    copy(
      abstractHeap = abstractHeap.createVariables(ids.getNonTopUnsafe),
      generalValState = generalValState.createVariables(ids.getNonTopUnsafe))
  } ensuring (result =>
    ids.lessEqual(result.generalValState.ids) &&
    result.abstractHeap.edges.forall(e => ids.lessEqual(e.state.ids)),
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
      val newGeneralValState = generalValState.factory()
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
    val result: T = left match {
      case variable: VariableIdentifier =>
        val normalRight = normalizeExpression(right)
        if (left.typ.isBooleanType) {
          evalExp(normalRight).apply().mapCondHeaps(condHeap => {
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
          evalExp(right).apply().map(_.assign(variable, right)).join
        } else {
          val varVertex = abstractHeap.localVarVertex(variable.getName)
          val edgesToRemove = abstractHeap.outEdges(varVertex)
          var edgesToAdd = Set.empty[Edge[S]]
          normalizeExpression(right) match {
            case verExpr: VertexExpression =>
              assert(abstractHeap.vertices.contains(verExpr.vertex),
                "Assigning a non-existing node")
              assert(varVertex.typ.equals(verExpr.typ),
                "We support only exact type, that is the fields should be the same")

              val edge = Edge(varVertex, generalValState, None, verExpr.vertex)
              edgesToAdd = edgesToAdd + edge.createEdgeLocalIds()
            case rAP: AccessPathIdentifier =>
              val rightPaths = abstractHeap.paths(rAP.stringPath)
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
                    id -> id.copy(accPath = List(None))
                  }).toMap

                  val newEdgeState = rCond.rename(renameMap)
                  edgesToAdd = edgesToAdd + Edge(varVertex, newEdgeState, None, rPath.target)
                }
              }
            case c: Constant =>
              assert(c.toString == "null", "The only object constant is null.")
              val tempAH = abstractHeap.addNonHeapVertex(NullVertex)
              val newState = copy(abstractHeap = tempAH)
              return newState.assignVariable(left, new VertexExpression(variable.typ, NullVertex)(c.pp))
            case _ => throw new Exception("Not supported (should not happen, let me know if does (Milos)).")
          }
          if (edgesToAdd.isEmpty)
            bottom()
          else {
            // Remove the old edges before adding the new ones.
            // It's possible that the two sets of edges overlap.
            val tempAH = abstractHeap
              .removeEdges(edgesToRemove)
              .addEdges(edgesToAdd)
              .joinCommonEdges()
            copy(abstractHeap = tempAH, isTop = false)
          }
        }
      case _ => throw new Exception("Left-hand side of variable assignment is not a variable.")
    }
    assert(result.abstractHeap.isNormalized, "The abstract heap is not normalized.")
    result.prune()
  }

  /**
   * Convenience method that converts the state to a `CondHeapGraph` and
   * evaluates the given expression.
   */
  protected def evalExp(expr: Expression, allowNullReceivers: Boolean = false): CondHeapGraphSeq[S] =
    toCondHeapGraph.evalExp(expr, allowNullReceivers)

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
    if (leftPaths.isEmpty)
      return this.bottom()

    val result: T = if (right.typ.isObject) {
      var edgesToAdd = Set.empty[Edge[S]]
      normalizeExpression(right) match {
        case rAP: AccessPathIdentifier =>
          val rightPaths = abstractHeap.paths(rAP.stringPath)
          edgesToAdd = referencePathAssignmentEdges(left.path.last.getName, leftPaths, rightPaths)
        case v: VertexExpression =>
          // We assume that all edges have ValueHeapIdentifiers for the given vertex expression
          // Add new edges
          for (lPath <- leftPaths) {
            // leftCond should contain source EdgeLocalIdentifiers
            val leftCond = lPath.condition
            // We build the replacement that for each ValueHeapIdentifier corresponding to the vertex, expands it to
            // target EdgeLocalIdentifier.
            val repl = new Replacement()
            for (id <- leftCond.ids.getNonTopUnsafe.collect({
              case id: ValueHeapIdentifier if id.obj.equals(v.vertex) => id
            })) {
              repl.value.update(Set(id), Set(id, EdgeLocalIdentifier(List(Some(left.path.last.getName)), id)))
            }
            edgesToAdd = edgesToAdd + Edge(lPath.target, leftCond.merge(repl), Some(left.path.last.getName), v.vertex)
          }
        case c: Constant =>
          assert(c.toString.equals("null"), "We expect only null constants.")
          val newAH = abstractHeap.addNonHeapVertex(NullVertex)
          return copy(abstractHeap = newAH).assignField(left, VertexExpression(c.typ, NullVertex)(c.pp))
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
        val edgesToRemove = resultingAH.outEdges(lastPathVertex, Some(left.path.last.getName))
        resultingAH = resultingAH.removeEdges(edgesToRemove)
      }
      resultingAH = resultingAH.addEdges(edgesToAdd)
      resultingAH = resultingAH.joinCommonEdges()
      copy(abstractHeap = resultingAH, isTop = false)
    } else {
      evalExp(left).intersect(evalExp(right)).apply().mapCondHeaps(_.assignField(left, right)).join
    }
    result.prune()
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
      val leftCond = lPath.condition
      if (!leftCond.lessEqual(leftCond.bottom())) {
        // The condition of the left path is not bottom. (i.e. can be possibly assigned)
        for (rPath <- rightPaths) {
          val rightCond = rPath.condition
          val renameMap = rightCond.edgeLocalIds.map(id => {
            id -> id.copy(accPath = List(Some(field)))
          }).toMap

          var newEdgeState = rightCond.rename(renameMap)
          if (renameMap.isEmpty) {
            // This is the case when RHS is null. Hence, we need to create
            // source edge-local identifiers in the right state in order to
            // preserve the ones from LHS.
            val sourceIdsOfLHS = leftCond.sourceEdgeLocalIds
            newEdgeState = newEdgeState.createVariables(sourceIdsOfLHS)
          }
          newEdgeState = leftCond.glbPreserveIds(newEdgeState)
          // The following code was used instead of glbPreserveIds in the past:
          // TODO: I am not a entirely sure the semantics is still the same
          // leftCond = leftCond.createVariables(renameMap.values.toSet)
          // newEdgeState = newEdgeState.createVariables(renameMap.keySet)
          // newEdgeState = leftCond.glb(newEdgeState)
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
  final def getVariableValue(id: Identifier): T = id match {
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
    this.setExpression(ExpressionSet(Constant(value, typ, pp)))
  }

  def assume(cond: Expression): T =
    toCondHeapGraph.assume(normalizeExpression(cond)).join.prune()

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
    val resGeneralState = generalValState.lub(other.generalValState.rename(valueRenameMap))

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

  def widening(right: T): T = {
    // Consistent naming
    val left = this

    val mergedLeft = left.mergePointedNodes()
    val mergedRight = right.mergePointedNodes()

    val joined = mergedLeft.lub(mergedRight)

    if (mergedLeft.hasGraphIdenticalTo(joined)) {
      val newAbstractHeap = mergedLeft.abstractHeap.wideningAfterMerge(joined.abstractHeap)
      val newGeneralValState = mergedLeft.generalValState.widening(joined.generalValState)
      factory(newAbstractHeap, newGeneralValState, ExpressionSet())
    } else {
      // Widening is not possible yet
      joined
    }
  }

  /** Merges nodes in the heap graph and applies the resulting replacement
    * to the general value state.
    *
    * Helper method for `widening`.
    */
  protected def mergePointedNodes(): T = {
    val (merged, replacement) = abstractHeap.mergePointedNodes()
    factory(merged, generalValState.merge(replacement), ExpressionSet())
  }

  /** Checks whether this state is ready for widening with another given state.
    *
    * Th heap graph in this state and that of the other state must have the same
    * vertices vertices and the set of identifiers on all of their edges must
    * agree.
    *
    * Disclaimer: The above is not a very precise description.
    */
  protected def hasGraphIdenticalTo(other: T): Boolean = {
    if (abstractHeap.vertices != other.abstractHeap.vertices) return false

    for (otherEdge <- other.abstractHeap.edges) {
      val edgeSet = abstractHeap.edges.filter(_.weakEquals(otherEdge))

      if (!(edgeSet.size == 1 && edgeSet.head.state.ids == otherEdge.state.ids))
        return false
    }
    true
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
    while (queue.nonEmpty) {
      val (edge, path) = queue.dequeue()
      assert(edge.source.isInstanceOf[DefiniteHeapVertex] || edge.source.isInstanceOf[LocalVariableVertex])
      edge.target match {
        case summaryVertex: SummaryHeapVertex =>
          edgesToRemove += edge
          // Creating a vertex that is a materialization of the summary vertex
          val (tempAH, definiteVertex) = resultingAH.addDefiniteHeapVertex(summaryVertex.typ)
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
          // Collect edges to add for this particular definite vertex first,
          // then add them to the rest
          val edgesToAddHere = mutable.Set.empty[Edge[S]]

          // Edge that represents the processed edge
          edgesToAddHere += edge.copy(target = definiteVertex)

          for (e <- resultingAH.edges -- edgesToRemove) {
            // Incoming edges
            if (e.target == summaryVertex) {
              edgesToAddHere += e.copy(target = definiteVertex)
            }
            // Outgoing edges
            if (e.source == summaryVertex) {
              val edgeToAdd = e.copy[S](source = definiteVertex)
              if (path.nonEmpty && edgeToAdd.field.contains(path.head))
                queue.enqueue((edgeToAdd, path.tail))
              edgesToAddHere += edgeToAdd
            }
            // Self-loop edges
            if (e.source == summaryVertex && e.target == summaryVertex) {
              val edgeToAdd = e.copy[S](source = definiteVertex, target = definiteVertex)
              if (path.nonEmpty && edgeToAdd.field.contains(path.head))
                queue.enqueue((edgeToAdd, path.tail))
              edgesToAddHere += edgeToAdd
            }
          }

          edgesToAdd ++= edgesToAddHere.toSet
        case _ =>
          // Nothing to materialize for this edge
          if (path.nonEmpty)
            for (e <- (resultingAH.edges -- edgesToRemove ++ edgesToAdd).filter(edg => edg.source.equals(edge.target) && edg.field.contains(path.head) && edg.target.isInstanceOf[HeapVertex]))
              queue.enqueue((e, path.tail))
      }
    }

    resultingAH = resultingAH.removeEdges(edgesToRemove.toSet)
    resultingAH = resultingAH.applyReplacement(repl)

    // Updating source and target EdgeLocalIdentifiers in the edges to add.
    val updatedEdgesToAdd = edgesToAdd.map(e => {
      e.copy(state = e.state.merge(repl)).assumeEdgeLocalIdEqualities()
    })

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
      true
    } else {
      //    } else {
      //      if (abstractHeap.vertices.size <= r.abstractHeap.vertices.size) {
      //        if (widening(this, r).lessEqual(this))
      //          return true
      //        else
      //          return false
      //      }
      //    }
      false
    }
  }

  /** Creates a new definite vertex in the heap graph and initializes
    * its reference fields to null and value fields to top.
    *
    * @return the new state (without expression) and the newly added vertex
    */
  protected def createObject(typ: Type): (T, DefiniteHeapVertex) = {
    // Create the new heap vertex and ensure that there is a null vertex
    var (newAbstractHeap, newVertex) = abstractHeap.addDefiniteHeapVertex(typ)
    newAbstractHeap = newAbstractHeap.addNonHeapVertex(NullVertex)

    // Create the value heap identifiers in all states
    var newState = copy(abstractHeap = newAbstractHeap)
    newState = newState.createNonObjectVariables(IdentifierSet.Bottom.factory(newVertex.valueHeapIds))

    // Create the new edges
    val newEdges = typ.objectFields.map(field => {
      Edge(newVertex, newState.generalValState, Some(field.getName), NullVertex)
    }).map(_.createEdgeLocalIds())

    newState = newState.copy(abstractHeap = newAbstractHeap.addEdges(newEdges))
    (newState, newVertex)
  }

  def createObject(typ: Type, pp: ProgramPoint): T = {
    if (isBottom) return this
    val (newState, newVertex) = createObject(typ)
    newState.copy(expr = ExpressionSet(VertexExpression(typ, newVertex)(pp)))
  }

  /**
   * Implicitly converts a conditional heap graph to a state
   * with an empty expression.
   */
  protected implicit def CondHeapGraphToValueDrivenHeapState(condHeap: CondHeapGraph[S]): T =
    factory(condHeap.heap, condHeap.cond, ExpressionSet())

  /** Convertes the state to a conditional heap graph. */
  def toCondHeapGraph: CondHeapGraph[S] =
    CondHeapGraph[S, T](this)

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
  def setVariableToTop(x: Expression): T = ???
  def throws(t: ExpressionSet): T = ???
  def pruneUnreachableHeap(): T = ???
  def factory(): T = ???
  def removeObject(oldPreState: T, obj: ExpressionSet, fields: Option[Set[Identifier]]): T = ???
  def undoPruneVariables(unprunedPreState: T, filter: (VariableIdentifier) => Boolean): T = ???
  def undoPruneUnreachableHeap(preState: T): T = ???
  def nonDeterminismSourceAt(pp: ProgramPoint, typ: Type): T = ???
  def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ???
  def optimizeSummaryNodes(): T = ???
  def refiningGetVariableValue(id: Identifier): T = ???
  def refiningGetFieldValue(obj: ExpressionSet, field: String, typ: Type): T = ???
  def backwardAssignVariable(x: ExpressionSet, right: ExpressionSet): T = ???
  def refiningAssignVariable(oldPreState: T, x: Expression, right: Expression): T = ???
  def testBackwardTrue(): T = ???
  def testBackwardFalse(): T = ???
  def refiningAssignField(oldPreState: T, obj: Expression, field: String, right: Expression): T = ???

  override def ids = generalValState.ids lub expr.ids
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