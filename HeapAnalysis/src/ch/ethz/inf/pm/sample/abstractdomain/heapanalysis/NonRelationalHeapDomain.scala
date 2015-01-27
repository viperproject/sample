package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample._
import abstractdomain._
import abstractdomain.Constant
import abstractdomain.VariableIdentifier
import oorepresentation._
import property.Property
import util.HeapIdSetFunctionalLifting
import scala.collection.mutable

object NonRelationalHeapDomainSettings {
  var unsoundEntryState: Boolean = true
  var maxInitialNodes: Int = 5
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

    val res = left._lub(right)
    (res, repSummaries)
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

    // Also convert nodes that refer to summary nodes (fields of summary nodes)
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

    // Also convert nodes that refer to summary nodes (fields of summary nodes)
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

  def ids: Set[Identifier] = this.getVariables ++ this.getAddresses

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

  def getVariables = map.keySet

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

    // Also convert nodes that refer to summary nodes (fields of summary nodes)
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

    // Also convert nodes that refer to summary nodes (fields of summary nodes)
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
  def getLabel(): String

  def createAddress(typ: Type, pp: ProgramPoint): I

  def createAddressForArgument(typ: Type, p: ProgramPoint): I

  def extractField(obj: I, field: String, typ: Type): I

  def getArrayCell(array: Assignable, index: Expression): I

  def getArrayLength(array: Assignable): I

  def createArray(length: Expression, typ: Type, p: ProgramPoint): I = this.createAddress(typ, p)

  def accessStaticObject(typ: Type, p: ProgramPoint): I

  def getNullNode(p: ProgramPoint): I

  def isNormalized(): Boolean

  def factory(): I

  def toSummaryNode: I

  def toNonSummaryNode: I

  def getReachableFromId: Option[I]

  def getCounter: Int

  def setCounter(c: Int): I

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

  protected var alreadyInitialized: Set[I] = Set.empty[I]
  protected var fieldsInitialized: Set[I] = Set.empty[I]

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
    (HeapIdSetDomain.MayBe.Bottom().convert(dom.getArrayCell(arrayIdentifier, index)), this.asInstanceOf[H], new Replacement)

  override def createArray[S <: SemanticDomain[S]](length: Expression, typ: Type, pp: ProgramPoint, state: S) =
    (HeapIdSetDomain.MayBe.Bottom().convert(dom.createArray(length, typ, pp)), this.asInstanceOf[H], new Replacement)

  override def getArrayLength(id: Assignable) = (HeapIdSetDomain.MayBe.Bottom().convert(dom.getArrayLength(id)), this.asInstanceOf[H], new Replacement)

  override def assignArrayCell[S <: SemanticDomain[S]](obj: Assignable, index: Expression, expr: Expression, state: S) = {
    var result = this.bottom()
    val ids = this.getArrayCell(obj, index, state, expr.typ)._1
    for (id <- ids.value)
      result = result.lub(assign(id, expr, null)._1)
    (result, new Replacement)
  }

  override def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil

  def getDomainLabel(): String = dom.getLabel()

  override def parameters(): List[(String, Any)] = List(("UnsoundEntryState", true), ("MaxEntryNodes", 10))

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

  def get(key: I): HeapIdSetDomain[I] = this._2.get(key)

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
        var (result, r) = this.createVariable(variable, typ)
        //r will be always empty, so I ignore it
        var ids: Map[Identifier, List[String]] = Map.empty[Identifier, List[String]]
        alreadyInitialized = Set.empty[I]
        this.initializeObject(x, dom.createAddressForArgument(typ, x.pp), typ, result, path ::: variable.toString :: Nil)
      }
      else {
        var result = Map.empty[Identifier, List[String]]
        result = result + ((x, variable.toString :: Nil))
        (factory(this._1.add(x, cod.bottom()), this._2), result, new Replacement)
      }
    case x: HeapIdentifier[I] => {
      throw new Exception("This should not happen!")
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
      val c = typ.possibleFields
      for (field <- c) {
        val adds = cod.convert(dom.createAddressForArgument(field.typ, x.pp))
        //I can ignore newHeap since it's equal to initial as it is not changed by getFieldIdentifier
        //in the same way I ignore rep
        val (fieldAdd, newHeap, rep) = result.getFieldIdentifier(obj, field.getName, field.typ, field.pp)
        for (id: I <- fieldAdd.value) {
          result = factory(result._1, result._2.add(id, adds))
          ids = ids + ((id, path ::: field.getName :: Nil))
          val r = initializeObject(id, id, id.typ, result, path ::: field.getName :: Nil)
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
      case id: I => HeapIdSetFunctionalLifting.applyToSetHeapId(fact, this.normalize(HeapIdSetDomain.MayBe.Bottom().convert(id)), f)
      case _ => f(a)
    }
  }

  override def assume(expr: Expression) =
    (this.asInstanceOf[H], new Replacement) //TODO: for now there is nothing about the heap structure

  def evalFieldAccess[S <: State[S]](expr: Assignable, field: String, typ: Type): HeapIdSetDomain[I] = expr match {
    case obj: VariableIdentifier => return extractField(this.get(obj), field, typ)

    case obj: HeapIdSetDomain[I] => {
      var result: HeapIdSetDomain[I] = cod.bottom()
      for (simplepp: I <- obj.value)
        result = result.lub(extractField(simplepp, field, typ))

      //If I don't have any information, I return a top identifier
      if (result.isBottom)
        return result.top()
      return result
    }
    //case obj : I => return extractField(this.get(obj.asInstanceOf[I]), field, typ)
    case obj: I => return extractField(obj.asInstanceOf[I], field, typ)

    //case obj : SimpleProgramPointHeapIdentifier => return extractField(obj.asInstanceOf[I], field, typ)

    //case _ => return throw new Exception();//cod.top();
  }

  def extractField(obj: I, field: String, typ: Type): HeapIdSetDomain[I] = {
    var result: HeapIdSetDomain[I] = cod.bottom()
    for (id <- this.normalize(cod.convert(obj)).value)
      result = result.add(dom.extractField(id, field, typ))
    if (result.isBottom) return result.top()
    result
  }

  protected def normalize(id: HeapIdSetDomain[I]): HeapIdSetDomain[I] = {
    var result = id.factory()
    for (add <- id.value)
      if (add.isNormalized)
        result = result.add(add)
      else result = result.add(this._2.get(add))
    return result
  }

  protected def extractField(obj: HeapIdSetDomain[I], field: String, typ: Type): HeapIdSetDomain[I] = {
    var result: HeapIdSetDomain[I] = cod.bottom()
    if (obj.isBottom) return result.bottom()
    if (obj.isTop) {
      //We manage the case in which we use the object to access a static object -> useful in Scala
      if (typ != null && typ.isStatic) {
        typ.isStatic
        return result.add(dom.accessStaticObject(typ, obj.pp))
      }
    }
    val accessed = this.normalize(obj)
    for (node <- accessed.value)
      result = result.add(dom.extractField(node, field, typ))
    //If I don't have any information, I return a top identifier
    if (result.isBottom)
      return result.top()
    return result
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

case class NonRelationalHeapDomain[I <: NonRelationalHeapIdentifier[I]](
                                                                                          env: VariableEnv[I], heap: HeapEnv[I], cod: HeapIdSetDomain[I], dom: I)
  extends AbstractNonRelationalHeapDomain[I, NonRelationalHeapDomain[I]] {

  def this(cod: HeapIdSetDomain[I], dom: I) {
    this(new VariableEnv(cod), new HeapEnv(cod), cod, dom)
  }

  def getLabel(): String = "Heap Domain:" + dom.getLabel()


  def factory(a: VariableEnv[I], b: HeapEnv[I]): NonRelationalHeapDomain[I] = {
    new NonRelationalHeapDomain[I](a, b, cod.factory(), dom.factory())
  }

  def getInitialState(): NonRelationalHeapDomain[I] = new NonRelationalHeapDomain(new VariableEnv(env.dom), new HeapEnv(heap.dom), cod, dom)

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

  override def getCounter = 0

  override def setCounter(c: Int) = this

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, multiple: Boolean): TopHeapIdentifier = this
}
