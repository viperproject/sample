package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample._
import abstractdomain._
import abstractdomain.Constant
import abstractdomain.VariableIdentifier
import oorepresentation._
import property.Property
import util.HeapIdSetFunctionalLifting
import scala.Some
import scala.collection.mutable

object NonRelationalHeapDomainSettings {
  var unsoundEntryState: Boolean = true
  var maxInitialNodes: Int = 5
}

case class TupleIdSetDomain[I <: HeapIdentifier[I]](
                                                     pp: ProgramPoint,
                                                     value: Set[I] = Set.empty[I],
                                                     isTop: Boolean = false,
                                                     isBottom: Boolean = false)
  extends HeapIdSetDomain[I] {

  def setFactory(
                  value: Set[I] = Set.empty[I],
                  isTop: Boolean = false,
                  isBottom: Boolean = false) =
    TupleIdSetDomain[I](pp, value, isTop, isBottom)

  def this() = this(null)

  override def typ(): Type = {
    var res = SystemParameters.getType().bottom();
    for (a <- this.value)
      res = res.glb(a.typ);
    return res;
  }

  def ids = this.value.asInstanceOf[Set[Identifier]]

  override def add(el: I): HeapIdSetDomain[I] =
    setFactory(value + el)

  override def lub(other: HeapIdSetDomain[I]): (HeapIdSetDomain[I]) = {
    if (isBottom) return other
    if (other.isBottom) return this


    var newValue = Set.empty[I]
    for (l <- value;
         r <- other.value) {
      if (l.isInstanceOf[CollectionTupleIdentifier] && r.isInstanceOf[CollectionTupleIdentifier]) {
        val leftTuple = l.asInstanceOf[CollectionTupleIdentifier]
        val rightTuple = r.asInstanceOf[CollectionTupleIdentifier]

        if (leftTuple.equals(rightTuple)) {
          newValue += l
        } else if (leftTuple.contains(rightTuple)) {
          newValue += l
        } else if (rightTuple.contains(leftTuple)) {
          newValue += r
        } else {
          newValue += l
          newValue += r
        }
      } else {
        newValue += l
        newValue += r
      }
    }

    if (value.isEmpty) {
      newValue = newValue ++ other.value
    } else if (other.value.isEmpty) {
      newValue = newValue ++ value
    }

    setFactory(newValue)
  }

  override def glb(other: HeapIdSetDomain[I]): HeapIdSetDomain[I] = {
    if (isBottom || other.isBottom) return bottom();
    if (isTop) return other;
    if (other.isTop) return this;
    setFactory(value ++ other.value)
  }

  override def widening(other: HeapIdSetDomain[I]): HeapIdSetDomain[I] = lub(other)

  override def lessEqual(right: HeapIdSetDomain[I]): Boolean = {
    if (this.isBottom) return true;
    if (right.isTop) return true;
    right.value.subsetOf(this.value)
  }

  def convert(add: I): HeapIdSetDomain[I] = new TupleIdSetDomain[I](add.pp).add(add)

  //Used to now if it's definite - glb - or maybe - lub.
  def combinator[S <: Lattice[S]](s1: S, s2: S): S = s1.glb(s2)

  def heapCombinator[H <: LatticeWithReplacement[H], S <: SemanticDomain[S]](h1: H, h2: H, s1: S, s2: S): (H, Replacement) = h1.lubWithReplacement(h2)
}

class HeapEnv[I <: NonRelationalHeapIdentifier[I]](val dom: HeapIdSetDomain[I],
                                                   val map: Map[I, HeapIdSetDomain[I]] = Map.empty[I, HeapIdSetDomain[I]],
                                                   override val isBottom: Boolean = false,
                                                   val isTop: Boolean = false)
  extends FunctionalDomain[I, HeapIdSetDomain[I], HeapEnv[I]]
  with LatticeWithReplacement[HeapEnv[I]] {

  def functionalFactory(_value: Map[I, HeapIdSetDomain[I]] = Map.empty[I, HeapIdSetDomain[I]],
                        _isBottom: Boolean = false,
                        _isTop: Boolean = false): HeapEnv[I] =
    new HeapEnv[I](dom, _value, _isBottom, _isTop)

  def hasId(id: Identifier): Boolean = {
    id match {
      case idx: I =>
        if (map.contains(idx)) return true
        for (x <- map.values) {
          if (x.value.contains(idx)) return true
        }
        return false
      case _ => return false
    }
    return false
  }

  def ids = this.getAddresses

  private var getAddressesCache: Option[Set[I]] = None

  def getAddresses: Set[I] = {
    getAddressesCache match {
      case None =>
        var result = mutable.HashSet.empty[I] ++ map.keySet
        val it: Iterator[HeapIdSetDomain[I]] = map.values.iterator
        for (v <- it) {
          result ++= v.value
        }
        val set = result.toSet
        getAddressesCache = Some(set)
        return set
      case Some(x) => x
    }
  }

  private var getReachableMapCache: Option[mutable.HashMap[I, mutable.Set[I]]] = None

  def getReachableMap: mutable.HashMap[I, mutable.Set[I]] = {
    getReachableMapCache match {
      case None =>
        val reachableMap = new mutable.HashMap[I, mutable.Set[I]] with mutable.MultiMap[I, I]
        for (x <- getAddresses) {
          x.getReachableFromId match {
            case Some(y) => reachableMap.addBinding(y, x)
            case None => ()
          }
        }
        getReachableMapCache = Some(reachableMap)
        return reachableMap
      case Some(x) => x
    }
  }

  def collectReferences(nodes: Set[I]): Set[I] = {
    if (nodes.isEmpty) return nodes
    val map = getReachableMap
    val refs = (for (node <- nodes) yield {
      map.get(node)
    }).flatten.flatten
    refs ++ collectReferences(refs -- nodes)
  }

  override def lub(other: HeapEnv[I]): HeapEnv[I] = throw new UnsupportedOperationException("Use lubWithReplacement")

  override def glb(other: HeapEnv[I]): HeapEnv[I] = throw new UnsupportedOperationException("Use glbWithReplacement")

  override def widening(other: HeapEnv[I]): HeapEnv[I] = throw new UnsupportedOperationException("Use wideningWithReplacement")

  // Provides access to lattice operators in super class
  private def _lub(other: HeapEnv[I]): HeapEnv[I] = super.lub(other)

  private def _glb(other: HeapEnv[I]): HeapEnv[I] = super.glb(other)

  def lubWithReplacementMust[S <: SemanticDomain[S]](other: HeapEnv[I]): (HeapEnv[I], Replacement) = {
    if (equals(other)) return (this, new Replacement())
    if (isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())

    val repSummaries = lubReplacementsForSummaries(other)
    val left = merge(repSummaries)
    val right = other.merge(repSummaries)

    val repTuples = left.lubReplacementsForTuplesSimple(right)

    val res = left._lub(right)
    val newHeap = res.merge(repTuples)
    val rep = repSummaries >> repTuples
    (newHeap, rep)
  }

  private def lubReplacementsForTuplesSimple(other: HeapEnv[I]): Replacement = {
    val leftTuples = ids collect {
      case x: CollectionTupleIdentifier => x
    }
    val rightTuples = other.ids collect {
      case x: CollectionTupleIdentifier => x
    }

    var removeIdentifiers = Set.empty[Identifier]

    for (t <- leftTuples -- rightTuples) {
      val key = t.getCollectionKey(t)
      val value = t.getCollectionValue(t)
      removeIdentifiers += t
      removeIdentifiers += key
      removeIdentifiers += value
    }

    for (t <- rightTuples -- leftTuples) {
      val key = t.getCollectionKey(t)
      val value = t.getCollectionValue(t)
      removeIdentifiers += t
      removeIdentifiers += key
      removeIdentifiers += value
    }

    val replacements = new Replacement
    replacements.value += (removeIdentifiers -> Set.empty[Identifier])
    replacements
  }

  private def lubReplacementsForSummaries(other: HeapEnv[I]): Replacement = {
    val leftSummaryNodes = ids collect {
      case x: I if !x.representsSingleVariable => x
    }
    val rightSummaryNodes = other.ids collect {
      case x: I if !x.representsSingleVariable => x
    }

    if (leftSummaryNodes.isEmpty && rightSummaryNodes.isEmpty) return new Replacement()

    val makeSummaryLeft = (rightSummaryNodes -- leftSummaryNodes).map(_.toNonSummaryNode)
    val makeSummaryRight = (leftSummaryNodes -- rightSummaryNodes).map(_.toNonSummaryNode)

    if (makeSummaryLeft.isEmpty && makeSummaryRight.isEmpty) return new Replacement()

    // Also convert nodes that refer to summary nodes (fields of summary nodes, length of summarized collections, collection tuples)
    val makeSummaryLeftRef = this.collectReferences(makeSummaryLeft)
    val makeSummaryRightRef = other.collectReferences(makeSummaryRight)

    val replaceLeft = new Replacement
    for (a <- makeSummaryLeft ++ makeSummaryLeftRef) {
      if (hasId(a)) {
        replaceLeft.value += (Set[Identifier](a) -> Set[Identifier](a.toSummaryNode))
      }
    }

    val replaceRight = new Replacement
    for (a <- makeSummaryRight ++ makeSummaryRightRef) {
      if (other.hasId(a)) {
        replaceRight.value += (Set[Identifier](a) -> Set[Identifier](a.toSummaryNode))
      }
    }

    replaceLeft ++ replaceRight
  }

  override def lubWithReplacement(other: HeapEnv[I]): (HeapEnv[I], Replacement) = {
    if (this.isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())

    val rep = lubReplacementsForSummaries(other)
    val result = merge(rep)._lub(other.merge(rep))
    (result, rep)
  }

  override def glbWithReplacement(other: HeapEnv[I]): (HeapEnv[I], Replacement) = {

    if (isBottom) return (this, new Replacement())
    if (other.isBottom) return (other, new Replacement())

    // The following code tries to make an id non-summary (by creating a replacement)
    // if it's a non-summary in one of the environments. We only need to do that for
    // ids occuring in _both_ environments, namely the ones in this set. All ids only occuring in one environment
    // should disappear anyway.
    val commonIdNames = ids.map(_.getName) intersect other.ids.map(_.getName)

    val leftNonSummaryNodes = ids.filter(id => commonIdNames.contains(id.getName)) collect {
      case x: I if x.representsSingleVariable => x
    }
    val rightNonSummaryNodes = other.ids.filter(id => commonIdNames.contains(id.getName)) collect {
      case x: I if x.representsSingleVariable => x
    }

    val makeNonSummaryLeft = rightNonSummaryNodes -- leftNonSummaryNodes
    val makeNonSummaryRight = leftNonSummaryNodes -- rightNonSummaryNodes

    // Also convert nodes that refer to summary nodes (fields of summary nodes, length of summarized collections, collection tuples)
    val makeNonSummaryLeftRef = this.collectReferences(makeNonSummaryLeft)
    val makeNonSummaryRightRef = other.collectReferences(makeNonSummaryRight)

    val replaceLeft = new Replacement
    for (a <- makeNonSummaryLeft ++ makeNonSummaryLeftRef)
      replaceLeft.value += (Set[Identifier](a.toSummaryNode) -> Set[Identifier](a))
    val replaceRight = new Replacement
    for (a <- makeNonSummaryRight ++ makeNonSummaryRightRef)
      replaceRight.value += (Set[Identifier](a.toSummaryNode) -> Set[Identifier](a))

    val result = merge(replaceLeft)._glb(other.merge(replaceRight))

    (result, replaceLeft ++ replaceRight)
  }

  override def wideningWithReplacement(other: HeapEnv[I]): (HeapEnv[I], Replacement) = lubWithReplacement(other)

  def get(key: I): HeapIdSetDomain[I] = this.map.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }

  def merge(rep: Replacement): HeapEnv[I] = {

    var curVal = map

    // handle keys
    for ((froms, tos) <- rep.value) {
      var right = dom.bottom()
      for (from <- froms) {
        from match {
          case x: I =>
            curVal.get(x) match {
              case Some(y) =>
                right = right.lub(y)
                curVal = curVal - x
              case None => ()
            }
          case _ => ()
        }
      }
      for (to <- tos) {
        to match {
          case x: I =>
            curVal = curVal + (x -> right)
          case _ => ()
        }
      }
    }

    // handle values
    curVal = curVal.mapValues(_.merge(rep))

    functionalFactory(curVal)
  }
}

class VariableEnv[I <: NonRelationalHeapIdentifier[I]](val dom: HeapIdSetDomain[I],
                                                       val map: Map[VariableIdentifier, HeapIdSetDomain[I]] = Map.empty[VariableIdentifier, HeapIdSetDomain[I]],
                                                       override val isBottom: Boolean = false,
                                                       val isTop: Boolean = false)
  extends FunctionalDomain[VariableIdentifier, HeapIdSetDomain[I], VariableEnv[I]]
  with LatticeWithReplacement[VariableEnv[I]] {

  def functionalFactory(_value: Map[VariableIdentifier, HeapIdSetDomain[I]] = Map.empty[VariableIdentifier, HeapIdSetDomain[I]],
                        _isBottom: Boolean = false,
                        _isTop: Boolean = false): VariableEnv[I] =
    new VariableEnv[I](dom, _value, _isBottom, _isTop)

  def ids: Set[Identifier] = (this.getVariables ++ this.getAddresses)

  def hasId(id: Identifier): Boolean = {
    id match {
      case idx: VariableIdentifier => return map.contains(idx)
      case idx: I =>
        for (x <- map.values) {
          if (x.value.contains(idx)) return true
        }
    }
    return false
  }

  def getVariables = map.keySet;

  private var getAddressesCache: Option[Set[I]] = None

  def getAddresses: Set[I] = {
    getAddressesCache match {
      case None =>
        var result = mutable.HashSet.empty[I]
        val it: Iterator[HeapIdSetDomain[I]] = map.values.iterator
        for (v <- it) {
          result ++= v.value
        }
        val set = result.toSet
        getAddressesCache = Some(set)
        return set
      case Some(x) => x
    }
  }

  private var getReachableMapCache: Option[mutable.HashMap[I, mutable.Set[I]]] = None

  def getReachableMap: mutable.HashMap[I, mutable.Set[I]] = {
    getReachableMapCache match {
      case None =>
        val reachableMap = new mutable.HashMap[I, mutable.Set[I]] with mutable.MultiMap[I, I]
        for (x <- getAddresses) {
          x.getReachableFromId match {
            case Some(y) => reachableMap.addBinding(y, x)
            case None => ()
          }
        }
        getReachableMapCache = Some(reachableMap)
        return reachableMap
      case Some(x) => x
    }
  }

  def collectReferences(nodes: Set[I]): Set[I] = {
    if (nodes.isEmpty) return nodes
    val map = getReachableMap
    val refs = (for (node <- nodes) yield {
      map.get(node)
    }).flatten.flatten
    refs ++ collectReferences(refs -- nodes)
  }

  override def lub(other: VariableEnv[I]): VariableEnv[I] = throw new UnsupportedOperationException("Use lubWithReplacement")

  override def glb(other: VariableEnv[I]): VariableEnv[I] = throw new UnsupportedOperationException("Use glbWithReplacement")

  override def widening(other: VariableEnv[I]): VariableEnv[I] = throw new UnsupportedOperationException("Use wideningWithReplacement")

  // Provides access to lattice operators in super class
  private def _lub(other: VariableEnv[I]): VariableEnv[I] = super.lub(other)

  private def _glb(other: VariableEnv[I]): VariableEnv[I] = super.glb(other)

  override def lubWithReplacement(other: VariableEnv[I]): (VariableEnv[I], Replacement) = {

    if (isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())

    val leftSummaryNodes = ids collect {
      case x: I if !x.representsSingleVariable => x
    }
    val rightSummaryNodes = other.ids collect {
      case x: I if !x.representsSingleVariable => x
    }

    val makeSummaryLeft = rightSummaryNodes -- leftSummaryNodes
    val makeSummaryRight = leftSummaryNodes -- rightSummaryNodes

    // Also convert nodes that refer to summary nodes (fields of summary nodes, length of summarized collections, collection tuples)
    val makeSummaryLeftRef = this.collectReferences(makeSummaryLeft)
    val makeSummaryRightRef = other.collectReferences(makeSummaryRight)

    if (makeSummaryLeft.isEmpty && makeSummaryRight.isEmpty) return (super.lub(other), new Replacement())

    val replaceLeft = new Replacement
    for (a <- makeSummaryLeft ++ makeSummaryLeftRef)
      replaceLeft.value += (Set[Identifier](a.toNonSummaryNode) -> Set[Identifier](a))
    val replaceRight = new Replacement
    for (a <- makeSummaryRight ++ makeSummaryRightRef)
      replaceRight.value += (Set[Identifier](a.toNonSummaryNode) -> Set[Identifier](a))

    val result = merge(replaceLeft)._lub(other.merge(replaceRight))

    (result, replaceLeft ++ replaceRight)
  }

  override def glbWithReplacement(other: VariableEnv[I]): (VariableEnv[I], Replacement) = {

    if (isBottom) return (this, new Replacement())
    if (other.isBottom) return (other, new Replacement())

    // The following code tries to make an id non-summary (by creating a replacement)
    // if it's a non-summary in one of the environments. We only need to do that for
    // ids occuring in _both_ environments, namely the ones in this set. All ids only occuring in one environment
    // should disappear anyway.
    val commonIdNames = ids.map(_.getName) intersect other.ids.map(_.getName)

    val leftNonSummaryNodes = ids.filter(id => commonIdNames.contains(id.getName)) collect {
      case x: I if x.representsSingleVariable => x
    }
    val rightNonSummaryNodes = other.ids.filter(id => commonIdNames.contains(id.getName)) collect {
      case x: I if x.representsSingleVariable => x
    }

    val makeNonSummaryLeft = rightNonSummaryNodes -- leftNonSummaryNodes
    val makeNonSummaryRight = leftNonSummaryNodes -- rightNonSummaryNodes

    // Also convert nodes that refer to summary nodes (fields of summary nodes, length of summarized collections, collection tuples)
    val makeNonSummaryLeftRef = this.collectReferences(makeNonSummaryLeft)
    val makeNonSummaryRightRef = other.collectReferences(makeNonSummaryRight)

    val replaceLeft = new Replacement
    for (a <- makeNonSummaryLeft ++ makeNonSummaryLeftRef)
      replaceLeft.value += (Set[Identifier](a.toSummaryNode) -> Set[Identifier](a))
    val replaceRight = new Replacement
    for (a <- makeNonSummaryRight ++ makeNonSummaryRightRef)
      replaceRight.value += (Set[Identifier](a.toSummaryNode) -> Set[Identifier](a))

    val result = merge(replaceLeft)._glb(other.merge(replaceRight))

    (result, replaceLeft ++ replaceRight)
  }

  override def wideningWithReplacement(other: VariableEnv[I]): (VariableEnv[I], Replacement) = lubWithReplacement(other)

  def get(key: VariableIdentifier): HeapIdSetDomain[I] = this.map.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }

  def merge(rep: Replacement): VariableEnv[I] = {

    var curVal = map

    // handle keys
    for ((froms, tos) <- rep.value) {
      var right = dom.bottom()
      for (from <- froms) {
        from match {
          case x: VariableIdentifier =>
            curVal.get(x) match {
              case Some(y) =>
                right = right.lub(y)
                curVal = curVal - x
              case None => ()
            }
          case _ => ()
        }
      }
      for (to <- tos) {
        to match {
          case x: VariableIdentifier =>
            curVal = curVal + (x -> right)
          case _ =>
            ()
        }
      }
    }

    curVal = curVal.mapValues(_.merge(rep))
    functionalFactory(curVal)
  }

}

trait NonRelationalHeapIdentifier[I <: NonRelationalHeapIdentifier[I]] extends HeapIdentifier[I] {
  def getLabel(): String;

  def createAddress(typ: Type, pp: ProgramPoint): I;

  def createAddressForArgument(typ: Type, p: ProgramPoint): I;

  def extractField(obj: I, field: String, typ: Type): I;

  def getArrayCell(array: Assignable, index: Expression): I;

  def getArrayLength(array: Assignable): I;

  def createArray(length: Expression, typ: Type, p: ProgramPoint): I = this.createAddress(typ, p);

  def accessStaticObject(typ: Type, p: ProgramPoint): I;

  def getNullNode(p: ProgramPoint): I;

  def isNormalized(): Boolean;

  def factory(): I;

  def toSummaryNode: I

  def toNonSummaryNode: I

  def getReachableFromId: Option[I]

  def getCounter: Int

  def setCounter(c: Int): I

  /**
   * Nodes of collections may have multiple access paths (through differnt keys/indexes).
   * Everything else should return false here
   */
  def hasMultipleAccessPaths: Boolean

  def createCollection(collTyp: Type, keyType: Type, valueType: Type, lengthTyp: Type, origCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp: ProgramPoint): I

  def getCollectionOverApproximation(collection: Assignable): I

  def getCollectionUnderApproximation(collection: Assignable): I

  def getCollectionSummaryApproximation(collection: Assignable): I

  def createCollectionSummaryTuple(collectionApprox: Assignable, keyType: Type, valueType: Type): I

  def createCollectionTuple(collectionApprox: Assignable, keyType: Type, valueType: Type, pp: ProgramPoint): I

  def createCollectionTuple(collectionApprox: Assignable, keyType: Type, valueType: Type, pps: Set[ProgramPoint]): I

  def createCollectionTuple(collectionTuple1: Assignable, collectionTuple2: Assignable): I

  def getCollectionTupleByKey(collectionKey: Assignable): I

  def getCollectionTupleByValue(collectionValue: Assignable): I

  def getCollectionLength(collection: Assignable): I

  def getCollectionKey(collectionTuple: Assignable): I

  def getCollectionValue(collectionTuple: Assignable): I

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, multiple: Boolean): I
}

trait AbstractNonRelationalHeapDomain[
I <: NonRelationalHeapIdentifier[I],
H <: AbstractNonRelationalHeapDomain[I, H]]
  extends CartesianProductDomain[VariableEnv[I], HeapEnv[I], H]
  with HeapDomain[H, I]
  with HeapAnalysis[H, I] {
  this: H =>

  // TODO: Three symbols for the same thing is a bit much
  def env: VariableEnv[I]

  def variableEnv = env

  def _1 = env

  def heap: HeapEnv[I]

  def heapEnv = heap

  def _2 = heap

  def cod: HeapIdSetDomain[I]

  def dom: I

  protected var alreadyInitialized: Set[I] = Set.empty[I];
  protected var fieldsInitialized: Set[I] = Set.empty[I];

  override def endOfAssignment() = (this.asInstanceOf[H], new Replacement())

  override def ids: Set[Identifier] = this._1.ids ++ this._2.ids

  def hasId(id: Identifier): Boolean = {
    this._1.hasId(id) || this._2.hasId(id)
  }

  override def lub(other: H): H = throw new UnsupportedOperationException("Use lubWithReplacement")

  override def glb(other: H): H = throw new UnsupportedOperationException("Use glbWithReplacement")

  override def widening(other: H): H = throw new UnsupportedOperationException("Use wideningWithReplacement")

  override def lubWithReplacement(other: H) = {
    val (res1, rep1) = _1.lubWithReplacement(other._1)
    val (res2, rep2) = _2.lubWithReplacement(other._2)
    (factory(res1, res2), rep1 ++ rep2)
  }

  override def glbWithReplacement(other: H) = {
    val (res1, rep1) = _1.glbWithReplacement(other._1)
    val (res2, rep2) = _2.glbWithReplacement(other._2)
    (factory(res1, res2), rep1 ++ rep2)
  }

  override def wideningWithReplacement(other: H) = {
    val (res1, rep1) = _1.wideningWithReplacement(other._1)
    val (res2, rep2) = _2.wideningWithReplacement(other._2)
    (factory(res1, res2), rep1 ++ rep2)
  }

  override def reset() {
    if (NonRelationalHeapDomainSettings.unsoundEntryState)
      ParameterIds.reset()
  }

  override def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier: Assignable, index: Expression, state: S, typ: Type) =
    (new MaybeHeapIdSetDomain().convert(dom.getArrayCell(arrayIdentifier, index)), this.asInstanceOf[H], new Replacement);

  override def createArray[S <: SemanticDomain[S]](length: Expression, typ: Type, pp: ProgramPoint, state: S) =
    (new MaybeHeapIdSetDomain().convert(dom.createArray(length, typ, pp)), this.asInstanceOf[H], new Replacement);

  override def getArrayLength(id: Assignable) = (new MaybeHeapIdSetDomain().convert(dom.getArrayLength(id)), this.asInstanceOf[H], new Replacement)

  override def assignArrayCell[S <: SemanticDomain[S]](obj: Assignable, index: Expression, expr: Expression, state: S) = {
    var result = this.bottom()
    val ids = this.getArrayCell(obj, index, state, expr.typ)._1
    for (id <- ids.value)
      result = result.lub(assign(id, expr, null)._1)
    (result, new Replacement)
  }

  override def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil;

  def getDomainLabel(): String = dom.getLabel()

  override def parameters(): List[(String, Any)] = List((("UnsoundEntryState"), true), (("MaxEntryNodes"), 10))

  override def setParameter(label: String, value: Any): Unit = label match {
    case "UnsoundEntryState" => value match {
      case b: Boolean => NonRelationalHeapDomainSettings.unsoundEntryState = b
      case s: String => NonRelationalHeapDomainSettings.unsoundEntryState = s.toBoolean
    }
    case "MaxEntryNodes" => value match {
      case b: Int => NonRelationalHeapDomainSettings.maxInitialNodes = b
      case s: String => NonRelationalHeapDomainSettings.maxInitialNodes = s.toInt
    }
  }

  override def getProperties(): List[Property] = Nil

  def getStringOfId(id: Identifier): String = id match {
    case x: VariableIdentifier => this.get(x).toString
    case x: HeapIdSetDomain[I] => this.get(x).toString
  }

  def get(key: VariableIdentifier): HeapIdSetDomain[I] = this._1.map.get(key) match {
    case None => cod.top()
    case Some(x) => x
  }

  def get(key: HeapIdSetDomain[I]): HeapIdSetDomain[I] = {
    var result = cod.bottom()
    for (addr <- key.value)
      this._2.map.get(addr) match {
        case None => return cod.top()
        case Some(x) => result = result.lub(x)
      }
    result
  }

  def get(key: I): HeapIdSetDomain[I] = this._2.get(key);

  override def createVariable(variable: Assignable, typ: Type) = {
    if (!hasId(variable.asInstanceOf[Identifier])) {
      variable match {
        case x: VariableIdentifier =>
          (factory(this._1.add(x, cod.bottom()), this._2), new Replacement)
        case x: I =>
          (factory(this._1, this._2.add(x, cod.bottom())), new Replacement)
      }
    } else (this.asInstanceOf[H], new Replacement)
  }

  override def createVariableForArgument(variable: Assignable, typ: Type, path: List[String]) = variable match {
    case x: VariableIdentifier =>
      if (typ.isObject) {
        var (result, r) = this.createVariable(variable, typ);
        //r will be always empty, so I ignore it
        var ids: Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
        alreadyInitialized = Set.empty[I];
        this.initializeObject(x, dom.createAddressForArgument(typ, x.pp), typ, result, path ::: variable.toString :: Nil);
      }
      else {
        var result = Map.empty[Identifier, List[String]];
        result = result + ((x, variable.toString :: Nil))
        (factory(this._1.add(x, cod.bottom()), this._2), result, new Replacement);
      }
    case x: HeapIdentifier[I] => {
      throw new Exception("This should not happen!");
    }
  }

  protected def initializeObject(x: Identifier, obj: I, typ: Type, heap: H, path: List[String]): (H, Map[Identifier, List[String]], Replacement) = {
    if ( /*typ.isObject && */ !alreadyInitialized.contains(obj)) {
      var result = heap
      var ids: Map[Identifier, List[String]] = Map.empty[Identifier, List[String]]
      ids = ids + ((obj, path))
      val newAdd = cod.convert(obj)
      if (!NonRelationalHeapDomainSettings.unsoundEntryState)
        for (add <- result.ids)
          if (add.isInstanceOf[I] && add.typ.lessEqual(typ))
            newAdd.add(add.asInstanceOf[I])
      if (x.isInstanceOf[VariableIdentifier]) {
        result = factory(result._1.add(x.asInstanceOf[VariableIdentifier], newAdd), result._2)
      }

      alreadyInitialized = alreadyInitialized + obj
      val c = typ.possibleFields;
      for (field <- c) {
        val adds = cod.convert(dom.createAddressForArgument(field.typ, x.pp))
        //I can ignore newHeap since it's equal to initial as it is not changed by getFieldIdentifier
        //in the same way I ignore rep
        val (fieldAdd, newHeap, rep) = result.getFieldIdentifier(obj, field.getName, field.typ, field.pp)
        for (id: I <- fieldAdd.value) {
          result = factory(result._1, result._2.add(id, adds))
          ids = ids + ((id, path ::: (field.getName) :: Nil))
          val r = initializeObject(id, id, id.typ, result, path ::: (field.getName) :: Nil)
          alreadyInitialized = alreadyInitialized + id
          result = r._1
          ids = r._2 ++ ids; //This order is quite important: in this way we keep the shortest path to arrive to an abstract node!
        }
      }
      (result, ids, new Replacement)
    }
    else (heap, Map.empty[Identifier, List[String]], new Replacement)
  }

  override def setArgument(variable: Assignable, expr: Expression) = this.assign(variable, expr, null)

  override def backwardAssign(oldPreState: H, variable: Assignable, expr: Expression): (H, Replacement) = {
    //    if(!variable.getType.isObject)
    //      return (this.asInstanceOf[H], new Replacement)

    variable match {
      case x: VariableIdentifier =>
        val h = factory(_1.add(x, this._1.get(x).top()), _2)
        (h, new Replacement)
      case x: I =>
        val value = this.eval(expr)
        // TODO: consider summary nodes
        val h = factory(_1, this._2.add(x, this.normalize(value)))
        (h, new Replacement)
    }
  }

  override def assignField(variable: Assignable, s: String, expr: Expression): (H, Replacement) = {
    var result = this.bottom()
    var replacement = new Replacement()
    val ids = this.getFieldIdentifier(variable, s, expr.typ, variable.pp)._1
    for (id <- ids.value) {
      val (assigned, repAssignment) = this.assign(id, expr, null)
      val (res, repLub) = result.lubWithReplacement(assigned)
      result = res
      replacement = replacement ++ repAssignment ++ repLub
    }

    (result, replacement)
    //We ignore the other parts since getting a field does not modify a non relational heap domain
  }

  override def backwardAssignField(oldPreState: H, variable: Assignable, s: String, expr: Expression): (H, Replacement) = {
    var result = this.bottom()
    var replacement = new Replacement()
    val ids = this.getFieldIdentifier(variable, s, expr.typ, variable.pp)._1
    for (id <- ids.value) {
      val (assigned, repAssignment) = this.backwardAssign(oldPreState, id, expr)
      val (res, repLub) = result.lubWithReplacement(assigned)
      result = res
      replacement = replacement ++ repAssignment ++ repLub
    }

    (result, replacement)
  }

  /*

    variable match {





      case x : HeapIdSetDomain[I] =>
        if(x.isTop)
          return (this.top(), new Replacement)
        var result=this._2
        val value=this.eval(expr)
        //TODO:Distinguish between definite and maybe
        for(addr <- x.value/*this.normalize(x).value*/)
          result=result.add(addr, value.lub(this.normalize(value), this._2.get(addr)))
        return (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);

    }
  }

   */


  override def assign[S <: SemanticDomain[S]](variable: Assignable, expr: Expression, state: S): (H, Replacement) = {

    // THIS MAY CAUSE A PROBLEM - the heap domain must know about all identifiers to construct correct replacements
    //    if(! variable.getType().isObject)
    //    // It does not modify the heap
    //      return (this.asInstanceOf[H], new Replacement)

    variable match {
      case x: VariableIdentifier =>
        expr match {
          case value: HeapIdSetDomain[I] =>
            (factory(_1.add(x, this.normalize(value)), _2), new Replacement)
          case _ =>
            val value = this.eval(expr)
            (factory(_1.add(x, this.normalize(value)), _2), new Replacement)
        }
      case x: I =>
        val value = this.eval(expr)

        // Brutschy: Following my understanding of the weak update implementation, we need the
        //           following distinction between summary nodes and non-summary nodes
        val heapEnv =
          if (x.representsSingleVariable)
            this._2.add(x, this.normalize(value))
          else
            this._2.add(x, this.get(x).add(this.normalize(value)))
        return (factory(_1, heapEnv), new Replacement);

      case x: HeapIdSetDomain[I] =>
        if (x.isTop)
          return (this.top(), new Replacement)
        var result = this._2
        val value = this.eval(expr)
        //TODO:Distinguish between definite and maybe
        for (addr <- x.value /*this.normalize(x).value*/ )
          result = result.add(addr, normalize(value).lub(this._2.get(addr)))
        return (factory(_1, result), new Replacement)
    }
  }

  override def setToTop(variable: Assignable) = variable match {
    case x: VariableIdentifier =>
      val newHeap = this.factory()
      (factory(_1.add(x, cod.top()), _2), new Replacement)
    case x: HeapIdSetDomain[I] =>
      var result = this._2
      for (addr <- x.value)
        result = result.add(addr, cod.top())
      (factory(_1, result), new Replacement)
    case x: I =>
      (factory(_1, _2.add(x, cod.top())), new Replacement)
  }


  override def removeVariable(variable: Assignable) = variable match {
    case x: VariableIdentifier =>
      (factory(_1.remove(x), _2), new Replacement)
    case x: HeapIdSetDomain[I] =>
      var result = this._2
      for (addr <- x.value)
        result = result.remove(addr)
      (factory(_1, result), new Replacement)
    case x: I =>
      (factory(_1, _2.remove(x)), new Replacement)
  }

  override def removeObject(objId: Assignable): (H, Replacement) = objId match {
    case id: I =>
      val d2n =
        if (id.representsSingleVariable) {
          _2.remove(id)
        } else _2
      (factory(_1, d2n), new Replacement())
    case _ => throw new SemanticException("Can only remove MaybeHeapIdSetDomain ids")
  }

  override def createObject(typ: Type, pp: ProgramPoint): (HeapIdSetDomain[I], H, Replacement) = {
    makeSummaryIfRequired(dom.createAddress(typ, pp))
  }

  def merge(rep: Replacement): H = {
    factory(_1.merge(rep), _2.merge(rep))
  }

  override def getFieldIdentifier(heapIdentifier: Assignable, name: String, typ: Type, pp: ProgramPoint): (HeapIdSetDomain[I], H, Replacement) = {
    (this.evalFieldAccess(heapIdentifier, name, typ), this.asInstanceOf[H], new Replacement)
  }


  def getSummaryCollectionIfExists(collection: Assignable): HeapIdSetDomain[I] = {
    def getSummaryCollection(a: Assignable): HeapIdSetDomain[I] = a match {
      case collection: I =>
        var newIds: HeapIdSetDomain[I] = new MaybeHeapIdSetDomain[I]()
        if (hasId(collection.toSummaryNode)) {
          newIds = newIds.add(collection.toSummaryNode)
        } else {
          newIds = newIds.add(collection)
        }

        newIds
      case _ => throw new SemanticException("This is not a collection identifier " + a.toString)
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getSummaryCollection(_))
  }

  override def getCollectionKeyByTuple(collectionTuple: Assignable): Assignable = {
    return this.dom.getCollectionKey(collectionTuple)
  }

  override def getCollectionValueByTuple(collectionTuple: Assignable): Assignable = {
    return this.dom.getCollectionValue(collectionTuple)
  }

  override def getCollectionTupleByKey(keyId: Assignable): Assignable = {
    return this.dom.getCollectionTupleByKey(keyId)
  }

  override def getCollectionTupleByValue(valueId: Assignable): Assignable = {
    return this.dom.getCollectionTupleByValue(valueId)
  }

  override def isSummaryCollection(collectionId: Assignable): Boolean = {
    var isSummary = false

    def f(a: Assignable): HeapIdSetDomain[I] = a match {
      case collectionId: CollectionIdentifier =>
        isSummary = isSummary || !collectionId.representsSingleVariable
        new MaybeHeapIdSetDomain[I]()
      case _ => throw new SemanticException("This is not a collection identifier " + a.toString)
    }
    resolveVariables(new MaybeHeapIdSetDomain[I](), collectionId, f(_))

    return isSummary
  }

  override def insertCollectionElementToApprox(collectionApprox: Assignable, pp: ProgramPoint) = {
    val collectionApproxId = collectionApprox.asInstanceOf[I]
    val collectionId = collectionApprox.asInstanceOf[FieldAndProgramPoint].p1.asInstanceOf[CollectionIdentifier]
    val keyType = collectionId.keyType
    val valueType = collectionId.valueType

    val (tupleIds, res, rep) = makeSummaryIfRequired(dom.createCollectionTuple(collectionApproxId, keyType, valueType, pp))
    //val (res, rep) = result.createVariable(tupleId, tupleId.getType())
    var result = res

    val newIds = result.heapEnv.get(collectionApproxId).add(tupleIds)
    result = result.set_2(result.heapEnv.remove(collectionApproxId).add(collectionApproxId, newIds))
    (tupleIds, result, rep)
  }

  override def removeCollectionElement(collectionTuple: Assignable) = collectionTuple match {
    case id: CollectionTupleIdentifier =>
      val approx = id.collectionApprox.asInstanceOf[I]
      val newTuples = this._2.get(approx).remove(id.asInstanceOf[I])
      factory(this._1, this._2.remove(approx).add(approx, newTuples))
    case _ => throw new SemanticException("This is not a collection tuple " + collectionTuple.toString)
  }

  override def getOriginalCollection(collection: Assignable): HeapIdSetDomain[I] = {

    def getOrigCollection(collection: Assignable) = collection match {
      case collectionId: CollectionIdentifier =>
        collectionId.originalCollectionTyp match {
          case Some(typ) =>
            val origCollection = dom.extractField(collectionId.asInstanceOf[I], "orig", typ)
            this.heapEnv.get(origCollection)
          case None =>
            new MaybeHeapIdSetDomain[I]().bottom()
        }
      case _ => throw new SemanticException("This is not a collection identifier " + collection)
    }
    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getOrigCollection(_))
  }


  override def getKeysCollection(collection: Assignable): HeapIdSetDomain[I] = {
    def getKeysCollection(collection: Assignable) = collection match {
      case collectionId: CollectionIdentifier =>
        collectionId.keyCollectionTyp match {
          case Some(typ) =>
            val keyCollection = dom.extractField(collectionId.asInstanceOf[I], "keys", typ)
            this.heapEnv.get(keyCollection)
          case None =>
            new MaybeHeapIdSetDomain[I]().bottom()
        }
      case _ => throw new SemanticException("This is not a collection identifier " + collection)
    }
    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getKeysCollection(_))
  }

  override def getCollectionTuples(collectionApprox: Assignable): HeapIdSetDomain[I] = {
    val collectionApproxId = collectionApprox.asInstanceOf[I]
    new MaybeHeapIdSetDomain[I]().add(this.heapEnv.get(collectionApproxId))
  }


  override def getCollectionKeys(collectionApprox: Assignable): HeapIdSetDomain[I] = {
    val collectionApproxId = collectionApprox.asInstanceOf[I]
    val tupleIds = new MaybeHeapIdSetDomain[I]().add(this.heapEnv.get(collectionApproxId))
    def f(a: Assignable) = new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionKey(a))
    HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), tupleIds, f(_))
  }

  override def getCollectionValues(collectionApprox: Assignable): HeapIdSetDomain[I] = {
    val collectionApproxId = collectionApprox.asInstanceOf[I]
    val tupleIds = new MaybeHeapIdSetDomain[I]().add(this.heapEnv.get(collectionApproxId))
    def f(a: Assignable) = new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionValue(a))
    HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), tupleIds, f(_))
  }

  override def getCollectionLength(collection: Assignable) = {
    def f(a: Assignable): HeapIdSetDomain[I] = new MaybeHeapIdSetDomain().convert(dom.getCollectionLength(a))
    resolveVariables(new MaybeHeapIdSetDomain(), collection, f(_))
  }

  override def getUnreachableHeap: Set[I] = {
    ReachabilityAnalysis.getUnreachableLocations(_1, _2) filter {
      case id: NonDeterminismSourceHeapId => false
      case _ => true
    }
  }

  private def makeNewIdOrReturnSummary(objectIdentifier: I): (HeapIdSetDomain[I], H, Replacement) = {

    // CASE 1) We already have a summary node for this, return the summary identifier
    if (hasId(objectIdentifier.toSummaryNode)) {
      (cod.convert(objectIdentifier.toSummaryNode), this.asInstanceOf[H], new Replacement)
    }

    // CASE 2) We do not have a not, return the non-summary-identifier. Change counter so that it is new
    var cur = objectIdentifier
    while (hasId(cur)) {
      cur = cur.setCounter(cur.getCounter + 1)
    }
    (cod.convert(cur), this.asInstanceOf[H], new Replacement)

  }

  protected def makeSummaryIfRequired(objectIdentifier: I): (HeapIdSetDomain[I], H, Replacement) = {
    // CASE 1) We already have a summary node for this, return the summary identifier
    if (hasId(objectIdentifier.toSummaryNode)) {

      (cod.convert(objectIdentifier.toSummaryNode), this.asInstanceOf[H], new Replacement)
    }

    // CASE 2) We do not have a not, return the non-summary-identifier
    else if (!hasId(objectIdentifier)) {

      (cod.convert(objectIdentifier), this.asInstanceOf[H], new Replacement)

    }

    // CASE 3) We have such a node already - convert it to a summary node and return the summary node
    else {

      // create a summary node and replace the old object identifier with the summary node. Also return the
      // replacement so that it can be replaced in the semantic domain, too

      def collectReachableNodes(node: I): Set[I] = {
        val oldNodesCurrentLevel = ids collect {
          case x: I if Some(node) == x.getReachableFromId => x
        }
        var oldNodes = Set[I](node)
        for (n <- oldNodesCurrentLevel) {
          oldNodes = oldNodes ++ collectReachableNodes(n)
        }

        oldNodes
      }

      val oldNodes = collectReachableNodes(objectIdentifier)

      val replacementMap = oldNodes.map({
        x: I => (Set(x.asInstanceOf[Identifier]), Set(x.toSummaryNode.asInstanceOf[Identifier]))
      }).toMap
      val replacement = new Replacement(new scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]]() ++ replacementMap)

      val createdObject = cod.convert(objectIdentifier.toSummaryNode)
      val newHeapDomain = this.merge(replacement)

      (createdObject, newHeapDomain, replacement)
    }
  }

  protected def resolveVariables[T <: Lattice[T]](fact: T, a: Assignable, f: Assignable => T): T = {
    a match {
      case id: VariableIdentifier => HeapIdSetFunctionalLifting.applyToSetHeapId(fact, this.normalize(_1.get(id)), f)
      case ids: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(fact, this.normalize(ids), f)
      case id: I => HeapIdSetFunctionalLifting.applyToSetHeapId(fact, this.normalize(new MaybeHeapIdSetDomain().convert(id)), f)
      case _ => f(a)
    }
  }

  override def assume(expr: Expression) =
    (this.asInstanceOf[H], new Replacement) //TODO: for now there is nothing about the heap structure

  def evalFieldAccess[S <: State[S]](expr: Assignable, field: String, typ: Type): HeapIdSetDomain[I] = expr match {
    case obj: VariableIdentifier => return extractField(this.get(obj), field, typ)

    case obj: HeapIdSetDomain[I] => {
      var result: HeapIdSetDomain[I] = cod.bottom();
      for (simplepp: I <- obj.value)
        result = result.lub(extractField(simplepp, field, typ))

      //If I don't have any information, I return a top identifier
      if (result.isBottom)
        return result.top()
      return result;
    }
    //case obj : I => return extractField(this.get(obj.asInstanceOf[I]), field, typ)
    case obj: I => return extractField(obj.asInstanceOf[I], field, typ)

    //case obj : SimpleProgramPointHeapIdentifier => return extractField(obj.asInstanceOf[I], field, typ)

    //case _ => return throw new Exception();//cod.top();
  }

  def extractField(obj: I, field: String, typ: Type): HeapIdSetDomain[I] = {
    var result: HeapIdSetDomain[I] = cod.bottom();
    for (id <- this.normalize(cod.convert(obj)).value)
      result = result.add(dom.extractField(id, field, typ));
    if (result.isBottom) return result.top()
    result
  }

  protected def normalize(id: HeapIdSetDomain[I]): HeapIdSetDomain[I] = {
    var result = id.factory();
    for (add <- id.value)
      if (add.isNormalized)
        result = result.add(add);
      else result = result.add(this._2.get(add));
    return result;
  }

  protected def extractField(obj: HeapIdSetDomain[I], field: String, typ: Type): HeapIdSetDomain[I] = {
    var result: HeapIdSetDomain[I] = cod.bottom();
    if (obj.isBottom) return result.bottom();
    if (obj.isTop) {
      //We manage the case in which we use the object to access a static object -> useful in Scala
      if (typ != null && typ.isStatic) {
        typ.isStatic;
        return result.add(dom.accessStaticObject(typ, obj.pp));
      }
    }
    val accessed = this.normalize(obj);
    for (node <- accessed.value)
      result = result.add(dom.extractField(node, field, typ));
    //If I don't have any information, I return a top identifier
    if (result.isBottom)
      return result.top();
    return result;
  }

  protected def eval[S <: State[S]](expr: Expression): HeapIdSetDomain[I] = expr match {
    case x: VariableIdentifier => this.get(x);
    case x: HeapIdSetDomain[I] => x; /*
      var initial=cod.bottom();
      for(addr <- x.value)
        initial=initial.lub(initial, this._2.get(addr));
      return initial;                               */
    case Constant("null", typ, pp) => return cod.convert(dom.getNullNode(pp));
    //TODO: correct?
    case x: I => return cod.convert(x);
    case x => return cod.top();
  }

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint,
                                 summary: Boolean): (I, H) = {
    val nondetId = dom.createNonDeterminismSource(typ, pp, summary)
    (nondetId, this)
  }

  def getNonDeterminismSource(pp: ProgramPoint, typ: Type): Identifier = {
    val allIds = ids
    val matchingIds = ids collect {
      case id@NonDeterminismSourceHeapId(_, idPP, _) if pp == idPP => id
    }
    if (matchingIds.size != 1) {
      throw new IllegalStateException(s"Non-deterministic source for $pp not found on heap")
    }
    val nonDetId = matchingIds.head.asInstanceOf[I]
    nonDetId
  }
}

//Approximates all the concrete references created at the same point of the program with a unique abstract reference
case class NonRelationalHeapDomain[I <: NonRelationalHeapIdentifier[I]](
                                                                         env: VariableEnv[I], heap: HeapEnv[I], cod: HeapIdSetDomain[I], dom: I)
  extends AbstractNonRelationalHeapDomain[I, NonRelationalHeapDomain[I]] {

  def this(cod: HeapIdSetDomain[I], dom: I) = {
    this(new VariableEnv(cod), new HeapEnv(cod), cod, dom)
  }

  override def getInitialState() = new NonRelationalHeapDomain(new VariableEnv(env.dom), new HeapEnv(heap.dom), cod, dom);

  def factory(a: VariableEnv[I], b: HeapEnv[I]) = new NonRelationalHeapDomain(a, b, cod.factory(), dom.factory())

  def getLabel(): String = "Heap Domain:" + dom.getLabel();

  override def createEmptyCollection(collTyp: Type, keyType: Type, valueType: Type, lengthTyp: Type, originalCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp: ProgramPoint): (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = {
    val (collections, heap, rep) = makeSummaryIfRequired(dom.createCollection(collTyp, keyType, valueType, lengthTyp, originalCollectionTyp, keyCollectionTyp, pp))

    var newHeap = heap

    for (coll <- collections.value) {
      val approxId = dom.getCollectionOverApproximation(coll)
      //if(!this._2.value.contains(approxId)) {
      val (assigned, _) = newHeap.assign(approxId, new MaybeHeapIdSetDomain[I](), null)
      newHeap = assigned
      //}
    }

    (collections, newHeap, rep)
  }

  override def insertCollectionTopElement(collection: Assignable, pp: ProgramPoint) = {
    this.insertCollectionElement(collection, pp)
  }

  override def insertCollectionElement(collection: Assignable, pp: ProgramPoint) = {
    var result = this

    var resultRep = new Replacement()

    def insertCollectionElement(collection: Assignable): HeapIdSetDomain[I] = {
      val keyType = collection.asInstanceOf[CollectionIdentifier].keyType
      val valueType = collection.asInstanceOf[CollectionIdentifier].valueType

      val collectionApproxId = this.dom.getCollectionOverApproximation(collection)
      val (tupleIds, newHeap, rep) = result.makeSummaryIfRequired(dom.createCollectionTuple(collectionApproxId, keyType, valueType, pp))
      result = newHeap
      resultRep = resultRep ++ rep

      val newIds = result.heapEnv.get(collectionApproxId).add(tupleIds)
      result = result.set_2(result.heapEnv.remove(collectionApproxId).add(collectionApproxId, newIds))

      return new MaybeHeapIdSetDomain[I]().add(tupleIds)
    }

    val tuples = resolveVariables(new MaybeHeapIdSetDomain[I](), collection, insertCollectionElement)
    (tuples, result, resultRep)
  }

  override def getCollectionOverApproximation(collection: Assignable): HeapIdSetDomain[I] = {
    def getApprox(a: Assignable): HeapIdSetDomain[I] = a match {
      case collectionId: CollectionIdentifier =>
        val id = this.dom.getCollectionOverApproximation(collectionId)
        new MaybeHeapIdSetDomain[I]().convert(id)
      case _ => throw new SemanticException("This is not a collection " + collection.toString)
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getApprox(_))
  }

  override def setCollectionToTop(collection: Assignable): NonRelationalHeapDomain[I] = {
    def setTop(collectionId: Assignable): NonRelationalHeapDomain[I] = setToTop(collectionId)._1

    val overApproxId = getCollectionOverApproximation(collection)
    HeapIdSetFunctionalLifting.applyToSetHeapId(bottom(), overApproxId, setTop)
  }


  override def getCollectionUnderApproximation(collection: Assignable): HeapIdSetDomain[I] = {
    return new MaybeHeapIdSetDomain[I]().bottom()
  }
}

// Must analysis for collection Elements
case class NonRelationalMustHeapDomain[I <: NonRelationalHeapIdentifier[I]](
                                                                             env: VariableEnv[I], heap: HeapEnv[I], cod: HeapIdSetDomain[I], dom: I)
  extends AbstractNonRelationalHeapDomain[I, NonRelationalMustHeapDomain[I]] {

  def this(cod: HeapIdSetDomain[I], dom: I) {
    this(new VariableEnv(cod), new HeapEnv(cod), cod, dom)
  }

  def getLabel(): String = "Must Heap Domain:" + dom.getLabel();

  def factory(a: VariableEnv[I], b: HeapEnv[I]) = new NonRelationalMustHeapDomain(a, b, cod.factory(), dom.factory())

  def getInitialState(): NonRelationalMustHeapDomain[I] = new NonRelationalMustHeapDomain(new VariableEnv(env.dom), new HeapEnv(heap.dom), cod, dom);

  override def setCollectionToTop(collection: Assignable): NonRelationalMustHeapDomain[I] = {
    def setTop(collectionId: Assignable): NonRelationalMustHeapDomain[I] = setToTop(collectionId)._1

    val overApproxId = getCollectionUnderApproximation(collection)
    HeapIdSetFunctionalLifting.applyToSetHeapId(bottom(), overApproxId, setTop)
  }


  override def createEmptyCollection(collTyp: Type, keyType: Type, valueType: Type, lengthTyp: Type, originalCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp: ProgramPoint): (HeapIdSetDomain[I], NonRelationalMustHeapDomain[I], Replacement) = {
    val (collections, heap, rep) = makeSummaryIfRequired(dom.createCollection(collTyp, keyType, valueType, lengthTyp, originalCollectionTyp, keyCollectionTyp, pp))

    var newHeap = heap
    for (coll <- collections.value) {
      val approxId = dom.getCollectionUnderApproximation(coll)
      val (assigned, _) = newHeap.assign(approxId, new TupleIdSetDomain[I](), null)
      newHeap = assigned
    }

    (collections, newHeap, rep)
  }

  override def insertCollectionTopElement(collection: Assignable, pp: ProgramPoint) = {
    (new MaybeHeapIdSetDomain[I]().bottom(), this, new Replacement)
  }

  override def getCollectionUnderApproximation(collection: Assignable): HeapIdSetDomain[I] = {
    def getApprox(a: Assignable): HeapIdSetDomain[I] = a match {
      case collectionId: CollectionIdentifier =>
        val id = this.dom.getCollectionUnderApproximation(collectionId)
        new MaybeHeapIdSetDomain[I]().convert(id)
      case _ => throw new SemanticException("This is not a collection " + collection.toString)
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getApprox(_))
  }

  override def getCollectionOverApproximation(collection: Assignable): HeapIdSetDomain[I] = {
    new MaybeHeapIdSetDomain[I]().bottom()
  }

  override def insertCollectionElement(collection: Assignable, pp: ProgramPoint) = {
    var result = this
    var resultRep = new Replacement()

    def insertCollectionElement(collection: Assignable): HeapIdSetDomain[I] = {
      val collectionId = collection.asInstanceOf[CollectionIdentifier]
      //if (!collectionId.representSingleVariable()) return new MaybeHeapIdSetDomain[I]()

      val keyType = collectionId.keyType
      val valueType = collectionId.valueType
      val collectionApproxId = this.dom.getCollectionUnderApproximation(collection)
      val (tupleIds, newHeap, rep) = makeSummaryIfRequired(dom.createCollectionTuple(collectionApproxId, keyType, valueType, pp))
      result = newHeap
      resultRep = resultRep ++ rep

      val newIds = result.heapEnv.get(collectionApproxId).add(tupleIds)
      result = result.set_2(result.heapEnv.remove(collectionApproxId).add(collectionApproxId, newIds))

      return new MaybeHeapIdSetDomain[I].add(tupleIds)
    }

    val tuples = resolveVariables(new MaybeHeapIdSetDomain[I](), collection, insertCollectionElement)
    (tuples, result, resultRep)
  }
}

// Combination of may and must analysis for collection elements
case class NonRelationalMayAndMustHeapDomain[I <: NonRelationalHeapIdentifier[I]](
                                                                                   heapMay: NonRelationalHeapDomain[I], heapMust: NonRelationalMustHeapDomain[I])
  extends CartesianProductDomain[NonRelationalHeapDomain[I], NonRelationalMustHeapDomain[I], NonRelationalMayAndMustHeapDomain[I]]
  with HeapDomain[NonRelationalMayAndMustHeapDomain[I], I]
  with HeapAnalysis[NonRelationalMayAndMustHeapDomain[I], I] {

  def _1 = heapMay

  def _2 = heapMust

  def get(key: VariableIdentifier): HeapIdSetDomain[I] = this._1.get(key).add(this._2.get(key))

  def get(key: HeapIdSetDomain[I]): HeapIdSetDomain[I] = this._1.get(key).add(this._2.get(key))

  def get(key: I): HeapIdSetDomain[I] = this._1.get(key).add(this._2.get(key))

  def createObject(typ: Type, pp: ProgramPoint): (HeapIdSetDomain[I], NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (ids1, heap1, rep1) = this._1.createObject(typ, pp)
    val (ids2, heap2, rep2) = this._2.createObject(typ, pp)

    (ids1.lub(ids2), new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  override def removeObject(obj: Assignable): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (heap1, rep1) = this._1.removeObject(obj)
    val (heap2, rep2) = this._2.removeObject(obj)
    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def createArray[S <: SemanticDomain[S]](length: Expression, typ: Type, pp: ProgramPoint, state: S): (HeapIdSetDomain[I], NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (ids1, heap1, rep1) = this._1.createArray(length, typ, pp, state)
    val (ids2, heap2, rep2) = this._2.createArray(length, typ, pp, state)

    (ids1.lub(ids2), new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def getArrayLength(arrayId: Assignable): (HeapIdSetDomain[I], NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (ids1, heap1, rep1) = this._1.getArrayLength(arrayId)
    val (ids2, heap2, rep2) = this._2.getArrayLength(arrayId)

    (ids1.lub(ids2), new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def getFieldIdentifier(objectIdentifier: Assignable, name: String, typ: Type, pp: ProgramPoint): (HeapIdSetDomain[I], NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    if (isCollectionMayVariable(objectIdentifier)) {
      val (ids1, heap1, rep1) = this._1.getFieldIdentifier(objectIdentifier, name, typ, pp)
      return (ids1, new NonRelationalMayAndMustHeapDomain[I](heap1, this._2), rep1)
    } else if (isCollectionMustVariable(objectIdentifier)) {
      val (ids2, heap2, rep2) = this._2.getFieldIdentifier(objectIdentifier, name, typ, pp)
      return (new MaybeHeapIdSetDomain[I]().add(ids2), new NonRelationalMayAndMustHeapDomain[I](this._1, heap2), rep2)
    } else {
      val (ids1, heap1, rep1) = this._1.getFieldIdentifier(objectIdentifier, name, typ, pp)
      //return (ids1, new NonRelationalMayAndMustHeapDomain[I](heap1, this._2), rep1)
      val (ids2, heap2, rep2) = this._2.getFieldIdentifier(objectIdentifier, name, typ, pp)
      return (ids1.glb(new MaybeHeapIdSetDomain[I]().add(ids2)), new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
    }
  }

  def endOfAssignment(): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (heap1, rep1) = this._1.endOfAssignment()
    val (heap2, rep2) = this._2.endOfAssignment()

    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier: Assignable, index: Expression, state: S, typ: Type): (HeapIdSetDomain[I], NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (ids1, heap1, rep1) = this._1.getArrayCell(arrayIdentifier, index, state, typ)
    val (ids2, heap2, rep2) = this._2.getArrayCell(arrayIdentifier, index, state, typ)

    (ids1.lub(ids2), new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def setToTop(variable: Assignable): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (heap1, rep1) = this._1.setToTop(variable)
    val (heap2, rep2) = this._2.setToTop(variable)

    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }


  override def setCollectionToTop(collection: Assignable): NonRelationalMayAndMustHeapDomain[I] = {
    val heap1 = this._1.setCollectionToTop(collection)
    val heap2 = this._2.setCollectionToTop(collection)
    new NonRelationalMayAndMustHeapDomain[I](heap1, heap2)
  }

  private def isCollectionMustVariable(variable: Assignable): Boolean = variable match {
    case FieldAndProgramPoint(_, "must", _, _) => true
    case CollectionTupleIdentifier(FieldAndProgramPoint(_, "must", _, _), _, _, _, _, _) => true
    case FieldAndProgramPoint(CollectionTupleIdentifier(FieldAndProgramPoint(_, "must", _, _), _, _, _, _, _), _, _, _) => true
    case _ => false
  }

  private def isCollectionMayVariable(variable: Assignable): Boolean = variable match {
    case FieldAndProgramPoint(_, "may", _, _) => true
    case CollectionTupleIdentifier(FieldAndProgramPoint(_, "may", _, _), _, _, _, _, _) => true
    case FieldAndProgramPoint(CollectionTupleIdentifier(FieldAndProgramPoint(_, "may", _, _), _, _, _, _, _), _, _, _) => true
    case _ => false
  }

  def assign[S <: SemanticDomain[S]](variable: Assignable, expr: Expression, state: S): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    if (isCollectionMustVariable(variable)) {
      val (mustHeap, mustRep) = this._2.assign(variable, expr, state)
      return (new NonRelationalMayAndMustHeapDomain[I](this._1, mustHeap), mustRep)
    } else if (isCollectionMayVariable(variable)) {
      val (mayHeap, mayRep) = this._1.assign(variable, expr, state)
      return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, this._2), mayRep)
    } else {
      val (mayHeap, mayRep) = this._1.assign(variable, expr, state)
      //return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, this._2), mayRep)
      val (mustHeap, mustRep) = this._2.assign(variable, expr, state)
      return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, mustHeap), mayRep ++ mustRep)
    }
  }

  def assignField(obj: Assignable, field: String, expr: Expression): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    if (isCollectionMustVariable(obj)) {
      val (mustHeap, mustRep) = this._2.assignField(obj, field, expr)
      return (new NonRelationalMayAndMustHeapDomain[I](this._1, mustHeap), mustRep)
    } else if (isCollectionMayVariable(obj)) {
      val (mayHeap, mayRep) = this._1.assignField(obj, field, expr)
      return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, this._2), mayRep)
    } else {
      val (mayHeap, mayRep) = this._1.assignField(obj, field, expr)
      //return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, this._2), mayRep)
      val (mustHeap, mustRep) = this._2.assignField(obj, field, expr)
      return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, mustHeap), mayRep ++ mustRep)
    }
  }

  def assignArrayCell[S <: SemanticDomain[S]](obj: Assignable, index: Expression, expr: Expression, state: S): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (heap1, rep1) = this._1.assignArrayCell(obj, index, expr, state)
    val (heap2, rep2) = this._2.assignArrayCell(obj, index, expr, state)

    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def setArgument(variable: Assignable, expr: Expression): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (heap1, rep1) = this._1.setArgument(variable, expr)
    val (heap2, rep2) = this._2.setArgument(variable, expr)

    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def assume(expr: Expression): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (heap1, rep1) = this._1.assume(expr)
    val (heap2, rep2) = this._2.assume(expr)

    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def createVariable(variable: Assignable, typ: Type): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    if (isCollectionMustVariable(variable)) {
      val (mustHeap, mustRep) = this._2.createVariable(variable, typ)
      return (new NonRelationalMayAndMustHeapDomain[I](this._1, mustHeap), mustRep)
    } else if (isCollectionMayVariable(variable)) {
      val (mayHeap, mayRep) = this._1.createVariable(variable, typ)
      return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, this._2), mayRep)
    } else {
      val (mayHeap, mayRep) = this._1.createVariable(variable, typ)
      //return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, this._2), mayRep)
      val (mustHeap, mustRep) = this._2.createVariable(variable, typ)
      return (new NonRelationalMayAndMustHeapDomain[I](mayHeap, mustHeap), mayRep.lub(mustRep))
    }
  }

  def createVariableForArgument(variable: Assignable, typ: Type, path: List[String]): (NonRelationalMayAndMustHeapDomain[I], Map[Identifier, List[String]], Replacement) = {
    val (heap1, map1, rep1) = this._1.createVariableForArgument(variable, typ, path)
    val (heap2, map2, rep2) = this._2.createVariableForArgument(variable, typ, path)

    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), map1 ++ map2, rep1.lub(rep2))
  }

  def removeVariable(variable: Assignable): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (heap1, rep1) = this._1.removeVariable(variable)
    val (heap2, rep2) = this._2.removeVariable(variable)

    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def backwardAssign(oldPreState: NonRelationalMayAndMustHeapDomain[I], variable: Assignable, expr: Expression): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (heap1, rep1) = this._1.backwardAssign(oldPreState._1, variable, expr)
    val (heap2, rep2) = this._2.backwardAssign(oldPreState._2, variable, expr)

    (new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def ids: Set[Identifier] = {
    val ids1 = this._1.ids
    val ids2 = this._2.ids
    return ids1 ++ ids2
  }

  def createEmptyCollection(collTyp: Type, keyType: Type, valueType: Type, lengthTyp: Type, originalCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp: ProgramPoint): (HeapIdSetDomain[I], NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (ids1, heap1, rep1) = this._1.createEmptyCollection(collTyp, keyType, valueType, lengthTyp, originalCollectionTyp, keyCollectionTyp, pp)
    val (ids2, heap2, rep2) = this._2.createEmptyCollection(collTyp, keyType, valueType, lengthTyp, originalCollectionTyp, keyCollectionTyp, pp)

    (ids1.lub(ids2), new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1.lub(rep2))
  }

  def getSummaryCollectionIfExists(collection: Assignable): HeapIdSetDomain[I] = {
    val ids1 = this._1.getSummaryCollectionIfExists(collection)
    val ids2 = this._2.getSummaryCollectionIfExists(collection)
    ids1.lub(ids2)
  }

  def insertCollectionTopElement(collection: Assignable, pp: ProgramPoint) = {
    val (ids1, heap1, rep1) = this._1.insertCollectionTopElement(collection, pp)
    val (ids2, heap2, rep2) = this._2.insertCollectionTopElement(collection, pp)

    (ids1.lub(ids2), new NonRelationalMayAndMustHeapDomain[I](heap1, heap2), rep1 ++ rep2)
  }

  def getOriginalCollection(collection: Assignable): HeapIdSetDomain[I] = this._1.getOriginalCollection(collection)

  def getKeysCollection(collection: Assignable): HeapIdSetDomain[I] = this._1.getKeysCollection(collection)

  def getCollectionKeyByTuple(collectionTuple: Assignable): Assignable = collectionTuple match {
    case id: CollectionTupleIdentifier =>
      val collectionApprox = id.collectionApprox
      collectionApprox match {
        case FieldAndProgramPoint(_, "may", _, _) =>
          return this._1.getCollectionKeyByTuple(collectionTuple)
        case FieldAndProgramPoint(_, "must", _, _) =>
          return this._2.getCollectionKeyByTuple(collectionTuple)
        case _ => throw new SemanticException("This is not a collection approximation identifier " + collectionApprox)
      }
    case _ => throw new SemanticException("This is not a collection tuple " + collectionTuple.toString)
  }

  def getCollectionValueByTuple(collectionTuple: Assignable): Assignable = collectionTuple match {
    case id: CollectionTupleIdentifier =>
      val collectionApprox = id.collectionApprox
      collectionApprox match {
        case FieldAndProgramPoint(_, "may", _, _) =>
          return this._1.getCollectionValueByTuple(collectionTuple)
        case FieldAndProgramPoint(_, "must", _, _) =>
          return this._2.getCollectionValueByTuple(collectionTuple)
        case _ => throw new SemanticException("This is not a collection approximation identifier " + collectionApprox)
      }
    case _ => throw new SemanticException("This is not a collection tuple " + collectionTuple.toString)
  }

  def getCollectionTupleByKey(keyId: Assignable): Assignable = {
    return this._1.getCollectionTupleByKey(keyId)
  }

  def getCollectionTupleByValue(valueId: Assignable): Assignable = {
    return this._1.getCollectionTupleByValue(valueId)
  }

  def isSummaryCollection(collectionId: Assignable): Boolean = {
    return this._1.isSummaryCollection(collectionId)
  }

  def getCollectionTuples(collectionApprox: Assignable): HeapIdSetDomain[I] = collectionApprox match {
    case FieldAndProgramPoint(_, "may", _, _) => this._1.getCollectionTuples(collectionApprox)
    case FieldAndProgramPoint(_, "must", _, _) => this._2.getCollectionTuples(collectionApprox)
    case _ => throw new SemanticException("This is not a collection approximation identifier " + collectionApprox)
  }

  def getCollectionOverApproximation(collection: Assignable): HeapIdSetDomain[I] = this._1.getCollectionOverApproximation(collection)

  def getCollectionUnderApproximation(collection: Assignable): HeapIdSetDomain[I] = this._2.getCollectionUnderApproximation(collection)

  def getCollectionKeys(collectionApprox: Assignable): HeapIdSetDomain[I] = collectionApprox match {
    case FieldAndProgramPoint(_, "may", _, _) => this._1.getCollectionKeys(collectionApprox)
    case FieldAndProgramPoint(_, "must", _, _) => this._2.getCollectionKeys(collectionApprox)
    case _ => throw new SemanticException("This is not a collection approximation identifier " + collectionApprox)
  }

  def getCollectionValues(collectionApprox: Assignable): HeapIdSetDomain[I] = collectionApprox match {
    case FieldAndProgramPoint(_, "may", _, _) => this._1.getCollectionValues(collectionApprox)
    case FieldAndProgramPoint(_, "must", _, _) => this._2.getCollectionValues(collectionApprox)
    case _ => throw new SemanticException("This is not a collection approximation identifier " + collectionApprox)
  }

  def insertCollectionElement(collection: Assignable, pp: ProgramPoint): (HeapIdSetDomain[I], NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (idsMay, heapMay, repMay) = this._1.insertCollectionElement(collection, pp)
    val (idsMust, heapMust, repMust) = this._2.insertCollectionElement(collection, pp)

    (idsMay.lub(idsMust), new NonRelationalMayAndMustHeapDomain[I](heapMay, heapMust), repMay ++ repMust)
  }

  def insertCollectionElementToApprox(collectionApprox: Assignable, pp: ProgramPoint) = collectionApprox match {
    case FieldAndProgramPoint(_, "may", _, _) =>
      val (ids, heap1, rep1) = this._1.insertCollectionElementToApprox(collectionApprox, pp)
      (ids, new NonRelationalMayAndMustHeapDomain[I](heap1, this._2), rep1)
    case FieldAndProgramPoint(_, "must", _, _) =>
      val (ids, heap2, rep2) = this._2.insertCollectionElementToApprox(collectionApprox, pp)
      (ids, new NonRelationalMayAndMustHeapDomain[I](this._1, heap2), rep2)
    case _ => throw new SemanticException("This is not a collection approximation identifier " + collectionApprox)
  }

  def removeCollectionElement(collectionTuple: Assignable): NonRelationalMayAndMustHeapDomain[I] = {
    val mayHeap = this._1.removeCollectionElement(collectionTuple)
    val mustHeap = this._2.removeCollectionElement(collectionTuple)
    new NonRelationalMayAndMustHeapDomain[I](mayHeap, mustHeap)
  }

  def getCollectionLength(collection: Assignable): HeapIdSetDomain[I] = this._1.getCollectionLength(collection)

  def getUnreachableHeap: Set[I] = {

    val unreachableHeap_1 = this._1.getUnreachableHeap
    val unreachableHeap_2 = this._2.getUnreachableHeap

    var unreachableHeap = unreachableHeap_1.intersect(unreachableHeap_2)
    unreachableHeap = unreachableHeap ++ (unreachableHeap_1.filter(isCollectionMayVariable(_)))
    unreachableHeap = unreachableHeap ++ (unreachableHeap_2.filter(isCollectionMustVariable(_)))

    return unreachableHeap
  }

  def getLabel(): String = {
    "MayAndMustDomain(" + this._1.getDomainLabel() + ")"
  }

  def parameters(): List[(String, Any)] = {
    this._1.parameters() ++ this._2.parameters()
  }

  def setParameter(label: String, value: Any) = {
    this._1.setParameter(label, value)
    this._2.setParameter(label, value)
  }

  def getProperties(): List[Property] = {
    this._1.getProperties()
  }

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = {
    this._1.getNativeMethodsSemantics() ++ this._2.getNativeMethodsSemantics()
  }

  def reset() = {
    this._1.reset()
    this._2.reset()
  }

  def factory(a: NonRelationalHeapDomain[I], b: NonRelationalMustHeapDomain[I]): NonRelationalMayAndMustHeapDomain[I] = {
    new NonRelationalMayAndMustHeapDomain[I](a, b)
  }

  def lubWithReplacement(other: NonRelationalMayAndMustHeapDomain[I]): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (lub1, rep1) = _1.lubWithReplacement(other._1)
    val (lub2, rep2) = _2.lubWithReplacement(other._2)

    (new NonRelationalMayAndMustHeapDomain[I](lub1, lub2), rep1.lub(rep2))
  }

  def glbWithReplacement(other: NonRelationalMayAndMustHeapDomain[I]): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (glb1, rep1) = _1.glbWithReplacement(other._1)
    val (glb2, rep2) = _2.glbWithReplacement(other._2)

    (new NonRelationalMayAndMustHeapDomain[I](glb1, glb2), rep1.lub(rep2))
  }

  def wideningWithReplacement(other: NonRelationalMayAndMustHeapDomain[I]): (NonRelationalMayAndMustHeapDomain[I], Replacement) = {
    val (widening1, rep1) = _1.wideningWithReplacement(other._1)
    val (widening2, rep2) = _2.wideningWithReplacement(other._2)

    (new NonRelationalMayAndMustHeapDomain[I](widening1, widening2), rep1.lub(rep2))
  }

  def getInitialState(): NonRelationalMayAndMustHeapDomain[I] = {
    val heap1 = this._1.getInitialState()
    val heap2 = this._2.getInitialState()
    new NonRelationalMayAndMustHeapDomain[I](heap1, heap2)
  }

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint,
                                 summary: Boolean): (I, NonRelationalMayAndMustHeapDomain[I]) = {
    val (id1, heap1) = this._1.createNonDeterminismSource(typ, pp, summary)

    (id1, new NonRelationalMayAndMustHeapDomain[I](heap1, _2))
  }

  def getNonDeterminismSource(pp: ProgramPoint, typ: Type): Identifier = {
    val id1 = this._1.getNonDeterminismSource(pp, typ)
    id1
  }
}

// Approximates all collection elements to one summary node
case class NonRelationalSummaryCollectionHeapDomain[I <: NonRelationalHeapIdentifier[I]](
                                                                                          env: VariableEnv[I], heap: HeapEnv[I], cod: HeapIdSetDomain[I], dom: I)
  extends AbstractNonRelationalHeapDomain[I, NonRelationalSummaryCollectionHeapDomain[I]] {

  def this(cod: HeapIdSetDomain[I], dom: I) {
    this(new VariableEnv(cod), new HeapEnv(cod), cod, dom)
  }

  def getLabel(): String = "Summary Collection Heap Domain:" + dom.getLabel();


  def factory(a: VariableEnv[I], b: HeapEnv[I]): NonRelationalSummaryCollectionHeapDomain[I] = {
    new NonRelationalSummaryCollectionHeapDomain[I](a, b, cod.factory(), dom.factory())
  }

  def getInitialState(): NonRelationalSummaryCollectionHeapDomain[I] = new NonRelationalSummaryCollectionHeapDomain(new VariableEnv(env.dom), new HeapEnv(heap.dom), cod, dom);

  override def createEmptyCollection(collTyp: Type, keyType: Type, valueType: Type, lengthTyp: Type, originalCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp: ProgramPoint): (HeapIdSetDomain[I], NonRelationalSummaryCollectionHeapDomain[I], Replacement) = {
    val (collections, heap, rep) = makeSummaryIfRequired(dom.createCollection(collTyp, keyType, valueType, lengthTyp, originalCollectionTyp, keyCollectionTyp, pp))

    var newHeap = heap
    for (coll <- collections.value) {
      val approxId = dom.getCollectionSummaryApproximation(coll)
      val (assigned, _) = newHeap.assign(approxId, new MaybeHeapIdSetDomain[I](), null)
      newHeap = assigned
    }

    (collections, newHeap, rep)
  }

  override def insertCollectionTopElement(collection: Assignable, pp: ProgramPoint) = {
    this.insertCollectionElement(collection, pp)
  }

  override def getCollectionUnderApproximation(collection: Assignable): HeapIdSetDomain[I] = {
    new MaybeHeapIdSetDomain[I]().bottom()
  }

  override def getCollectionOverApproximation(collection: Assignable): HeapIdSetDomain[I] = {
    def getApprox(a: Assignable): HeapIdSetDomain[I] = a match {
      case collectionId: CollectionIdentifier =>
        val id = this.dom.getCollectionSummaryApproximation(collectionId)
        new MaybeHeapIdSetDomain[I]().convert(id)
      case _ => throw new SemanticException("This is not a collection " + collection.toString)
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getApprox(_))
  }

  override def insertCollectionElement(collection: Assignable, pp: ProgramPoint) = {
    var result = this
    var resultRep = new Replacement()

    def insertCollectionElement(collection: Assignable): HeapIdSetDomain[I] = {
      val collectionId = collection.asInstanceOf[CollectionIdentifier]

      val keyType = collectionId.keyType
      val valueType = collectionId.valueType

      val collectionApproxId = this.dom.getCollectionSummaryApproximation(collectionId)
      val (tupleIds, newHeap, rep) = makeSummaryIfRequired(dom.createCollectionSummaryTuple(collectionApproxId, keyType, valueType))
      result = newHeap
      resultRep = resultRep ++ rep

      val newIds = result.heapEnv.get(collectionApproxId).add(tupleIds)
      result = result.set_2(result.heapEnv.remove(collectionApproxId).add(collectionApproxId, newIds))

      new MaybeHeapIdSetDomain[I]().add(tupleIds)
    }

    val tuples = resolveVariables(new MaybeHeapIdSetDomain[I](), collection, insertCollectionElement(_))
    (tuples, result, resultRep)
  }
}

case class TopHeapIdentifier(typ: Type, pp: ProgramPoint)
  extends NonRelationalHeapIdentifier[TopHeapIdentifier] {

  override def getArrayCell(array: Assignable, index: Expression) = this

  override def getArrayLength(array: Assignable) = this

  override def getLabel() = "Top"

  override def getNullNode(pp: ProgramPoint) = this

  override def getField: Option[String] = None

  override def isNormalized(): Boolean = true

  override def representsSingleVariable = false

  override def getName = "#abstractReference#"

  override def equals(o: Any) = o match {
    case x: TopHeapIdentifier => true
    case _ => false
  }

  override def factory() = this

  override def createAddress(typ: Type, pp: ProgramPoint) = this

  override def createAddressForArgument(typ: Type, pp: ProgramPoint) = this

  override def extractField(obj: TopHeapIdentifier, field: String, typ: Type) = this

  override def accessStaticObject(typ: Type, pp: ProgramPoint) = this

  override def hashCode(): Int = 0

  override def toSummaryNode: TopHeapIdentifier = this

  override def toNonSummaryNode: TopHeapIdentifier = this

  override def getReachableFromId: Option[TopHeapIdentifier] = None

  override def hasMultipleAccessPaths = false

  override def getCounter = 0

  override def setCounter(c: Int) = this

  override def createCollection(collTyp: Type, keyType: Type, valueType: Type, lengthTyp: Type, origCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp: ProgramPoint) = this

  override def getCollectionOverApproximation(collection: Assignable) = this

  override def getCollectionUnderApproximation(collection: Assignable) = this

  override def getCollectionSummaryApproximation(collection: Assignable) = this

  override def createCollectionSummaryTuple(collectionApprox: Assignable, keyType: Type, valueType: Type) = this

  override def createCollectionTuple(collectionApprox: Assignable, keyType: Type, valueType: Type, pp: ProgramPoint) = this

  override def createCollectionTuple(collectionApprox: Assignable, keyType: Type, valueType: Type, pps: Set[ProgramPoint]) = this

  override def createCollectionTuple(collectionTuple1: Assignable, collectionTuple2: Assignable) = this

  override def getCollectionTupleByKey(collectionKey: Assignable) = this

  override def getCollectionTupleByValue(collectionValue: Assignable) = this

  override def getCollectionLength(collection: Assignable) = this

  override def getCollectionKey(collectionTuple: Assignable) = this

  override def getCollectionValue(collectionTuple: Assignable) = this

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, multiple: Boolean): TopHeapIdentifier = this
}
