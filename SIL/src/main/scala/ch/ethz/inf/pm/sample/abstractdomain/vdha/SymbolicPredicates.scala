package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.vdha.ValueDrivenHeapStateWithSymbolicPredicates._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, DummyProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import apron.{Box, Polka, Manager, Abstract1}
import ch.ethz.inf.pm.sample.oorepresentation.sil._
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.vdha.SymbolicPredicateDef.ValFieldPermDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.VertexConstants._
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import semper.sil.{ast => sil}

case class ValueDrivenHeapStateWithSymbolicPredicates[S <: SemanticDomain[S]](
    abstractHeap: HeapGraph[EdgeStateDomain[S]],
    generalValState: EdgeStateDomain[S],
    expr: ExpressionSet,
    isTop: Boolean = false)
  extends PreciseValueDrivenHeapState[
    SemanticAndSymbolicPredicateDomain[S],
    ValueDrivenHeapStateWithSymbolicPredicates[S]] {

  // Shorthand for the self-type
  type T = ValueDrivenHeapStateWithSymbolicPredicates[S]

  import ValueDrivenHeapStateWithSymbolicPredicates._
  import SymbolicPredicateInstsDomain._
  import SymbolicPredicateDef._

  def factory(
      abstractHeap: HeapGraph[EdgeStateDomain[S]],
      generalValState: EdgeStateDomain[S],
      expr: ExpressionSet,
      isTop: Boolean) =
    ValueDrivenHeapStateWithSymbolicPredicates(abstractHeap, generalValState, expr, isTop)

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type) = {
    if (variable.typ.isObject) {
      // Reset ghost fields
      val refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType
      refType.fields = refType.fields.filter(_.typ != SymbolicPredicateDefType)
      SymbolicPredicateDef.resetId()

      val localVarVertices = abstractHeap.localVarVertices + LocalVariableVertex(variable)
      val objectTypes = localVarVertices.flatMap(_.typ.reachableObjectTypes)

      // Create new abstract heap (no edges yet)
      val newAbstractHeap = HeapGraph[EdgeStateDomain[S]]()
        .addNonHeapVertices(Set(NullVertex) ++ localVarVertices)
        .addHeapVertices(SUMMARY, objectTypes)

      val edgeVerticesToDefId = newAbstractHeap.localVarVertices.flatMap(localVarVertex => {
        localVarVertex.neededEdgeFieldsAndTypes.flatMap({ case (field, fieldTyp) =>
          newAbstractHeap.possibleTargetVertices(fieldTyp).map(targetVertex => {
            val predicateDefId = SymbolicPredicateDef.makeId()
            refType.fields += predicateDefId.toPredicateInstId
            Set(localVarVertex, targetVertex) -> predicateDefId
          })
        })
      }).toMap

      // Create general value state, retaining old variable identifiers
      val newGeneralValState = generalValState.top()
        .createVariables(generalValState.variableIds.filter(_.typ != SymbolicPredicateDefType))
        .createVariables(newAbstractHeap.heapVertices.flatMap(_.valueHeapIds))
        .createVariables(edgeVerticesToDefId.values.toSet)

      // Create new edges
      val newEdges = newAbstractHeap.vertices.flatMap(sourceVertex => {
        sourceVertex.neededEdgeFieldsAndTypes.flatMap({ case (field, fieldTyp) =>
          newAbstractHeap.possibleTargetVertices(fieldTyp).map(targetVertex => {
            var edge = Edge(sourceVertex, newGeneralValState, field, targetVertex).createEdgeLocalIds()
            edgeVerticesToDefId.get(edge.vertices) match {
              case Some(defId) =>
                val predicateInstId = defId.toPredicateInstId
                val edgeLocalInstId = EdgeLocalIdentifier(List(field), predicateInstId)
                edge = edge.copy(state = edge.state.assign(edgeLocalInstId, Folded))
              case None =>
            }
            edge
          })
        })
      })

      factory(
        newAbstractHeap.addEdges(newEdges),
        newGeneralValState,
        ExpressionSet(variable),
        isTop = true)
    } else {
      super.createVariableForArgument(variable, typ)
    }
  }

  override def getFieldValue(id: AccessPathIdentifier) = {
    val originalResult = super.getFieldValue(id)

    if (id.path.size == 1)
    // No need to make any changes to predicate definitions and instances
    // when we only access a variable
      originalResult
    else {
      val refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType

      var result: ValueDrivenHeapStateWithSymbolicPredicates[S] =
        CondHeapGraph[EdgeStateDomain[S], T](originalResult)
          .evalExp(AccessPathIdentifier(id.path.dropRight(1))(refType)).mapCondHeaps(condHeap => {
          val path = condHeap.takenPath(id.path.dropRight(1))
          val field = id.path.last

          assert(path.edges.size == 1, "currently only support paths of length 1")

          val predState = path.edges.head.state.valueState.symbolicPredicateState
          val predDefs = predState.definitions
          val predInsts = predState.instances

          val availPredInstEdgeLocalIds = predInsts.targetEdgeLocalIds
            .filter(predInstEdgeLocalId => {
              predInsts.isCertainlyFolded(predInstEdgeLocalId) ||
              predInsts.isCertainlyUnfolded(predInstEdgeLocalId)
          })

          if (availPredInstEdgeLocalIds.isEmpty) {
            println("there needs to be either a folded or unfolded predicate")
            Seq(condHeap)
          } else {
            val (foldedPredInstEdgeLocalIds, unfoldedPredInstEdgeLocalIds) = availPredInstEdgeLocalIds
              .partition(predInsts.isCertainlyFolded(_))

            val availablePredInstIds = availPredInstEdgeLocalIds.map(predInstEdgeLocalId => {
              VariableIdentifier(predInstEdgeLocalId.field)(SymbolicPredicateInstType)
            })

            val foldedPredInstIds = foldedPredInstEdgeLocalIds.map(predInstEdgeLocalId => {
              VariableIdentifier(predInstEdgeLocalId.field)(SymbolicPredicateInstType)
            })

            val unfoldedPredInstIds = unfoldedPredInstEdgeLocalIds.map(predInstEdgeLocalId => {
              VariableIdentifier(predInstEdgeLocalId.field)(SymbolicPredicateInstType)
            })

            def alreadyHasPermission(predInstId: VariableIdentifier): Boolean = {
              val predDef = predDefs.get(predInstId.toPredicateDefId)
              if (id.typ.isObject)
                predDef.refFieldPerms.map.contains(field)
              else
                predDef.valFieldPerms.value.contains(field)
            }

            assert(availablePredInstIds.count(alreadyHasPermission) <= 1,
              "there may at least be one predicate instance with the permission")

            unfoldedPredInstIds.find(alreadyHasPermission) match {
              case Some(unfoldedPredicateInstId) =>
                Seq(condHeap) // Nothing to do
              case None =>
                foldedPredInstIds.find(alreadyHasPermission) match {
                  case Some(foldedPredicateInstId) =>
                    assert(assertion = false, "should have unfolded")
                    Seq(condHeap)
                  case None =>
                    val predInstId = availablePredInstIds.head
                    val predDefId = predInstId.toPredicateDefId
                    val predDef = predDefs.get(predDefId)

                    val newPredicateDef = if (path.target == NullVertex)
                      predDef.bottom()
                    else if (id.typ.isObject)
                      // TODO: Currently assumes that predicates are always recursive
                      // Instead, should just create fresh predicate def id
                      predDef.addRefFieldPerm(field, predDefId)
                    else
                      predDef.addValFieldPerm(field)

                    val predInstAccPathId = AccessPathIdentifier(id.path.dropRight(1), predInstId)
                    val condHeapSeq = condHeap
                      .map(_.assign(predDefId, newPredicateDef))
                      .assignField(predInstAccPathId, Unfolded)
                    condHeapSeq
                }
            }
          }
        }).join

      // We've lost the expression due to the assignField calls
      result = result.setExpression(originalResult.getExpression)
      result
    }
  }

  override def createObject(typ: Type, pp: ProgramPoint, fields: Option[Set[Identifier]]) = {
    val predicateDefId = SymbolicPredicateDef.makeId()
    val refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType
    refType.fields += predicateDefId.toPredicateInstId

    var result = super.createObject(typ, pp, fields)
    val expressionSet = result.getExpression
    val expression = result.getExpression.getSetOfExpressions.head
    val heapVertex = expression.asInstanceOf[VertexExpression].vertex.asInstanceOf[HeapVertex]

    result = result.createVariable(predicateDefId, SymbolicPredicateDefType, DummyProgramPoint)

    val predicateInstId = predicateDefId.toPredicateInstId
    val predicateInstValueHeapId = ValueHeapIdentifier(heapVertex, predicateInstId)

    val condHeapGraph = CondHeapGraph[EdgeStateDomain[S], T](result).map(
      _.assume(BinaryArithmeticExpression(predicateInstValueHeapId, Unfolded, ArithmeticOperator.==, BoolType)))

    // Must not prune here
    result = result.copy(abstractHeap = condHeapGraph.heap, generalValState = condHeapGraph.cond)
    result = result.setExpression(expressionSet)

    result
  }
}

object ValueDrivenHeapStateWithSymbolicPredicates {
  type EdgeStateDomain[S <: SemanticDomain[S]] =
  PreciseValueDrivenHeapState.EdgeStateDomain[SemanticAndSymbolicPredicateDomain[S]]

  def makeTopEdgeState[S <: SemanticDomain[S]](s: S): EdgeStateDomain[S] = {
    PreciseValueDrivenHeapState.makeTopEdgeState(
      SemanticAndSymbolicPredicateDomain(s, SymbolicPredicateDomain()).top())
  }
}

case class SymbolicPredicateDomain(
    instances: SymbolicPredicateInstsDomain = SymbolicPredicateInstsDomain(),
    definitions: SymbolicPredicateDefsDomain = SymbolicPredicateDefsDomain())
  extends RoutingSemanticCartesianProductDomain[
    SymbolicPredicateInstsDomain,
    SymbolicPredicateDefsDomain,
    SymbolicPredicateDomain] {

  def factory(
      instances: SymbolicPredicateInstsDomain,
      definitions: SymbolicPredicateDefsDomain) =
    SymbolicPredicateDomain(instances, definitions)

  def _1 = instances

  def _1canHandle(id: Identifier) =
    id.typ == SymbolicPredicateInstType

  def _2 = definitions

  def _2canHandle(id: Identifier) =
    id.typ == SymbolicPredicateDefType
}

case class SymbolicPredicateDef(
    valFieldPerms: ValFieldPermDomain = ValFieldPermDomain.top(),
    refFieldPerms: RefFieldPermDomain = RefFieldPermDomain().top())
  extends CartesianProductDomain[
    ValFieldPermDomain,
    RefFieldPermDomain,
    SymbolicPredicateDef]
  with Expression {

  require(refFieldPerms.map.values.forall(_.value.size == 1),
    "currently, there should be exactly one pred def id")

  def factory(a: ValFieldPermDomain, b: RefFieldPermDomain) =
    SymbolicPredicateDef(a, b)

  def _1: ValFieldPermDomain = valFieldPerms

  def _2: RefFieldPermDomain = refFieldPerms

  def addValFieldPerm(field: String): SymbolicPredicateDef =
    copy(valFieldPerms = valFieldPerms.add(field))

  def addRefFieldPerm(field: String, symbolicPredicateId: Identifier) = {
    // TODO: What should happen if there is already an ID?
    val newSymbolicPredicateIds = refFieldPerms.get(field).add(symbolicPredicateId)
    copy(refFieldPerms = refFieldPerms.add(field, newSymbolicPredicateIds))
  }

  def transform(f: (Expression) => Expression): Expression = ???

  def ids = refFieldPerms.map.values.flatMap(_.value).toSet

  def pp = DummyProgramPoint

  def typ = SymbolicPredicateDefType

  def toSilPredicateBody(receiverName: String = "this", predMap: Map[VariableIdentifier, sil.Predicate]): sil.Exp = {
    if (valFieldPerms.isTop && refFieldPerms.isTop)
      sil.TrueLit()()
    else if (valFieldPerms.isBottom || refFieldPerms.isBottom)
      sil.FalseLit()()
    else {
      val refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType

      def toFieldAccessPred(fieldName: String): sil.FieldAccessPredicate = {
        val fieldId = refType.fields.find(_.getName == fieldName).get
        val accPathId = AccessPathIdentifier(List(receiverName), fieldId)
        val fieldAccess = DefaultSampleConverter.convert(accPathId).asInstanceOf[sil.FieldAccess]
        sil.FieldAccessPredicate(fieldAccess, sil.FullPerm()())()
      }

      val valAccessPreds = valFieldPerms.value.map(toFieldAccessPred)

      val refAccessPreds = refFieldPerms.map.map({
        case (fieldName, predDefIds) =>
          val predDefId = predDefIds.value.head.asInstanceOf[VariableIdentifier]
          val refFieldAccessPred = toFieldAccessPred(fieldName)

          val nonNullnessCond = sil.NeCmp(refFieldAccessPred.loc, sil.NullLit()())()

          val fieldId = refType.fields.find(_.getName == fieldName).get
          val accPathId = AccessPathIdentifier(List(receiverName), fieldId)
          val fieldAccess = DefaultSampleConverter.convert(accPathId).asInstanceOf[sil.FieldAccess]
          val pred = predMap(predDefId)
          val predAccessPred = sil.PredicateAccessPredicate(sil.PredicateAccess(Seq(fieldAccess), pred)(), sil.FullPerm()())()
          val condPredAccessPred = sil.Implies(nonNullnessCond, predAccessPred)()

          sil.And(refFieldAccessPred, condPredAccessPred)()
      })

      val preds = valAccessPreds.toList ++ refAccessPreds.toList

      preds.reduceLeft[sil.Exp](sil.And(_, _)())
    }
  }
}

object SymbolicPredicateDef {
  type ValFieldPermDomain = InverseSetDomain.Must[String]
  val ValFieldPermDomain = new ValFieldPermDomain().top()

  private val nextId = new ThreadLocal[Int]

  def resetId() = {
    nextId.set(0)
  }

  def makeId(): VariableIdentifier = {
    val id = nextId.get
    nextId.set(id + 1)
    id.toString
    VariableIdentifier(Constants.GhostSymbolPrefix + "p" + id)(SymbolicPredicateDefType)
  }

  implicit class ExtendedVariableIdentifier(variableId: VariableIdentifier) {
    def toPredicateDefId: VariableIdentifier = {
      require(variableId.typ == SymbolicPredicateInstType)
      variableId.copy()(typ = SymbolicPredicateDefType, pp = DummyProgramPoint)
    }

    def toPredicateInstId: VariableIdentifier = {
      require(variableId.typ == SymbolicPredicateDefType)
      variableId.copy()(typ = SymbolicPredicateInstType, pp = DummyProgramPoint)
    }
  }

  def extractPredInstId(id: Identifier): VariableIdentifier = id match {
    case EdgeLocalIdentifier(accPath, field, typ) =>
      VariableIdentifier(field)(typ)
    case id @ AccessPathIdentifier(path) =>
      VariableIdentifier(path.last)(id.typ)
  }
}

final case class RefFieldPermDomain(
    map: Map[String, InverseSetDomain.Must[Identifier]] = Map.empty[String, InverseSetDomain.Must[Identifier]],
    isTop: Boolean = false,
    isBottom: Boolean = false,
    defaultValue: InverseSetDomain.Must[Identifier] = InverseSetDomain.Must[Identifier]().top())
  extends FunctionalDomain[String, InverseSetDomain.Must[Identifier], RefFieldPermDomain]
  with Lattice.Must[RefFieldPermDomain] {

  def get(key: String) = map.getOrElse(key, defaultValue)

  def functionalFactory(value: Map[String, InverseSetDomain.Must[Identifier]], isBottom: Boolean, isTop: Boolean) =
    RefFieldPermDomain(value, isTop, isBottom, defaultValue)
}

case object SymbolicPredicateDefType extends AbstractType("Pred") {
  def isNumericalType = true
}

case object SymbolicPredicateInstType extends AbstractType("PredInstance") {
  override def isBooleanType = true
  def isNumericalType = true
}

case class SemanticAndSymbolicPredicateDomain[S <: SemanticDomain[S]](
    valueState: S, symbolicPredicateState: SymbolicPredicateDomain)
  extends RoutingSemanticCartesianProductDomain[
    S, SymbolicPredicateDomain, SemanticAndSymbolicPredicateDomain[S]] {

  def factory(valueState: S, symbolicPredicateState: SymbolicPredicateDomain) =
    SemanticAndSymbolicPredicateDomain(valueState, symbolicPredicateState)

  def _1 = valueState

  def _1canHandle(id: Identifier) =
    !_2canHandle(id)

  def _2 = symbolicPredicateState

  def _2canHandle(id: Identifier) =
    id.typ == SymbolicPredicateDefType || id.typ == SymbolicPredicateInstType
}

case class SymbolicPredicateDefsDomain(
    map: Map[Identifier, SymbolicPredicateDef] = Map.empty[Identifier, SymbolicPredicateDef],
    isTop: Boolean = false,
    isBottom: Boolean = false,
    defaultValue: SymbolicPredicateDef = SymbolicPredicateDef().top())
  extends BoxedDomain[SymbolicPredicateDef, SymbolicPredicateDefsDomain]
  with SemanticDomain[SymbolicPredicateDefsDomain] {

  def get(key: Identifier): SymbolicPredicateDef = map.getOrElse(key, defaultValue)

  def functionalFactory(
      value: Map[Identifier, SymbolicPredicateDef],
      isBottom: Boolean,
      isTop: Boolean) =
    SymbolicPredicateDefsDomain(value, isTop, isBottom, defaultValue)

  def removeVariable(variable: Identifier) = remove(variable)

  def createVariable(variable: Identifier, typ: Type) =
    add(variable, defaultValue.top())

  def setToTop(variable: Identifier) =
    add(variable, defaultValue.top())

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???

  def access(field: Identifier) = ???

  def assume(expr: Expression) = this

  def setArgument(variable: Identifier, expr: Expression) = ???

  def assign(variable: Identifier, expr: Expression) = expr match {
    case (expr: SymbolicPredicateDef) => add(variable, expr)
  }

  def backwardAssign(oldPreState: SymbolicPredicateDefsDomain, variable: Identifier, expr: Expression) = ???

  def backwardAccess(field: Identifier) = ???

  def toSilPredicates(receiverName: String = "this"): Seq[sil.Predicate] = {
    val predMap = map.keys.map(predDefId => {
      val formalArg = sil.LocalVarDecl(receiverName, sil.Ref)()
      predDefId.asInstanceOf[VariableIdentifier] -> sil.Predicate(predDefId.getName, Seq(formalArg), null)()
    }).toMap

    predMap.foreach({
      case (predDefId, pred) =>
        pred.body = map.get(predDefId).get.toSilPredicateBody(receiverName, predMap)
    })

    predMap.values.toSeq
  }

  /** @todo Does not detect mutually recursive predicate definitions. */
  def nonRecursiveIds: Set[Identifier] = {
    map.filterNot({
      case (id, predDef) => {
        val nestedIds = predDef.refFieldPerms.map.values.map(_.value).flatten.toSet
        nestedIds.contains(id)
      }
    }).keySet
  }
}

case class SymbolicPredicateInstsDomain(
    state: Option[Abstract1] = None,
    domain: Manager = new Box(),
    isPureBottom: Boolean = false,
    env: Set[Identifier] = Set.empty)
  extends ApronInterface[SymbolicPredicateInstsDomain] {

  import SymbolicPredicateInstsDomain._

  // TODO: What about this???
  // override def glb(other: SymbolicPredicateInstsDomain) = lub(other)

  def factory(
      state: Option[Abstract1],
      domain: Manager,
      isPureBottom: Boolean = false,
      env: Set[Identifier]) =
    SymbolicPredicateInstsDomain(state, domain, isPureBottom, env)

  def isCertainlyFolded(id: Identifier): Boolean =
    areEqual(id, Folded) == BooleanDomain.domTrue

  def isCertainlyUnfolded(id: Identifier): Boolean =
    areEqual(id, Unfolded) == BooleanDomain.domTrue

  def certainlyFoldedIds: Set[Identifier] =
    env.filter(isCertainlyFolded)

  def certainlyUnfoldedIds: Set[Identifier] =
    env.filter(isCertainlyUnfolded)
}

object SymbolicPredicateInstsDomain {
  val Folded = Constant("true", SymbolicPredicateInstType, DummyProgramPoint)
  val Unfolded = Constant("false", SymbolicPredicateInstType, DummyProgramPoint)
}