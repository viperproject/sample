package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.{SystemParameters, ToStringUtilities}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.util.{AccumulatingTimer, MapUtil}
import ch.ethz.inf.pm.td.analysis.TouchField

import scala.collection.immutable.Set

case class HeapIdentifier(pp: ProgramPoint, typ:Type, summary:Boolean, unique:Int) extends Identifier {
  override def getName: String = pp + (if (summary) "Σ" else "") + "[v"+unique+"]" + typ.toString
  override def getField: Option[String] = None
  override def representsSingleVariable: Boolean = !summary
}

case class FieldIdentifier(o:HeapIdentifier,f:String,typ:Type) extends Identifier {
  override def pp:  ProgramPoint = o.pp
  override def getName: String = o + "." + f
  override def getField: Option[String] = Some(f)
  override def representsSingleVariable: Boolean = o.representsSingleVariable
}

/**
 * A new state for TouchDevelop
 */
trait TouchState [S <: SemanticDomain[S], T <: TouchState[S, T]]
  extends SimpleState[T]
  with StateWithCollectionStubs[T]
  with StateWithBackwardAnalysisStubs[T]
{
  self:T =>

  val forwardMay:        Map[Identifier,Set[HeapIdentifier]]
  val forwardMust:       Map[Identifier,Set[HeapIdentifier]]
  val backwardMay:       Map[HeapIdentifier,Set[Identifier]]
  val versions:          Map[(ProgramPoint,Type),Seq[HeapIdentifier]]
  val valueState:        S
  val expr:              ExpressionSet
  val isTop:             Boolean

  def copy (
    forwardMay:        Map[Identifier,Set[HeapIdentifier]] = forwardMay,
    forwardMust:       Map[Identifier,Set[HeapIdentifier]] = forwardMust,
    backwardMay:       Map[HeapIdentifier,Set[Identifier]] = backwardMay,
    versions:          Map[(ProgramPoint,Type),Seq[HeapIdentifier]] = versions,
    valueState:        S = valueState,
    expr:              ExpressionSet = expr,
    isTop:             Boolean = isTop
  ):T = {

    val res = factory(forwardMay,forwardMust,backwardMay,versions,valueState,expr,isTop)

    // Invariants
    if (SystemParameters.DEBUG) {
      val isBottom = res.isBottom
      val isLessEqualBottom = res.lessEqual(res.bottom())
      val resIds = res.ids

      assert((isBottom && isLessEqualBottom) || (!isBottom && !isLessEqualBottom))
      assert(res.forwardMay.keySet.find(_.isInstanceOf[HeapIdentifier]).isEmpty)
      assert(res.forwardMust.keySet.find(_.isInstanceOf[HeapIdentifier]).isEmpty)
      assert(res.valueState.ids.find(_.isInstanceOf[HeapIdentifier]).isEmpty)

      // ASSERT FOR EVERY HEAP IDENTIFIER, THAT ONLY EITHER A SUMMARY OR A NONSUMMARY EXISTS
      for (id <- resIds) id match {
        case h:HeapIdentifier => assert(!(resIds.contains(h) && resIds.contains(h.copy(summary = !h.summary))))
        case f:FieldIdentifier => assert(!(resIds.contains(f) && resIds.contains(f.copy(o = f.o.copy(summary = !f.o.summary)))))
        case _ => ()
      }

      // forward May corresponds to backward May
      for ((obj,pointers) <- res.backwardMay; pointer <- pointers) {
        assert (res.forwardMay.getOrElse(pointer,Set.empty).contains(obj))
      }
      for ((pointer,objects) <- res.forwardMay; obj <- objects) {
        assert (res.backwardMay.getOrElse(obj,Set.empty).contains(pointer))
      }

    }

    res
  }

  def factory (
    forwardMay:        Map[Identifier,Set[HeapIdentifier]] = Map.empty,
    forwardMust:       Map[Identifier,Set[HeapIdentifier]] = Map.empty,
    backwardMay:       Map[HeapIdentifier,Set[Identifier]] = Map.empty,
    versions:          Map[(ProgramPoint,Type),Seq[HeapIdentifier]] = Map.empty,
    valueState:        S = valueState.factory(),
    expr:              ExpressionSet = ExpressionFactory.unitExpr,
    isTop:             Boolean = false
  ):T

  /** Creates a variable given a `VariableIdentifier`.
    * Implementations can already assume that this state is non-bottom.
    */
  override def createVariable(va: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    copy(
      forwardMay,// + (va -> Set.empty),
      forwardMust,// + (va -> Set.empty),
      backwardMay,
      versions,
      valueState.createVariable(va)
    )//.garbageCollect(forwardMust.getOrElse(va,Set.empty).toList).canonicalizeEnvironment
  }

  /** Removes the given variable.
    * Implementations can assume this state is non-bottom
    */
  override def removeVariable(va: VariableIdentifier): T = {
    val result = copy(
      forwardMay - va,
      forwardMust - va,
      backwardMay.map( x => x._1 -> (x._2 - va) ),
      versions,
      valueState.removeVariable(va)
    )
    val result2 = result.garbageCollect(forwardMay.getOrElse(va,Set.empty).toList)
    val result3 = result2.canonicalizeEnvironment

    if (SystemParameters.DEBUG) {
      val ids = result3.ids
      assert(!ids.contains(va))
    }

    // Garbage collection invariant
    if (SystemParameters.DEBUG) {
      for ( (k,v) <- result3.backwardMay ) {
        assert(v.nonEmpty)
      }
    }

    result3
  }

  /** Returns a new state whose `ExpressionSet` holds the value of the given field.
    * Implementations can already assume that this state is non-bottom.
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): T = obj match {
    case h:HeapIdentifier =>
      copy(
        expr = new ExpressionSet(typ,SetDomain.Default(Set(FieldIdentifier(h,field,typ))))
      )
    case a:Identifier =>
      val objects = forwardMay.getOrElse(a,Set.empty)
      copy(
        expr = new ExpressionSet(typ,SetDomain.Default(objects.map(FieldIdentifier(_,field,typ))))
      )
    case _ => bottom()
  }

  /** TODO: Is this enough?
    *
    * Returns May / Must information
    *
    * */
  def getFieldValueWhere(obj: Expression, field: String, typ: Type, filter:(HeapIdentifier,T) => Boolean): (Set[HeapIdentifier],Set[HeapIdentifier]) = {
    assert (typ.isObject)

    val objs = obj match {
      case h:HeapIdentifier => Set(h)
      case a:Identifier => forwardMay.getOrElse(a,Set.empty)
    }

    val valsMay = (for (o <- objs) yield {
      val fieldTargets = forwardMay.getOrElse(FieldIdentifier(o,field,typ),Set.empty)
      fieldTargets.filter{filter(_,this)}
    }).flatten

    val valsMust = (for (o <- objs) yield {
      val fieldTargets = forwardMust.getOrElse(FieldIdentifier(o,field,typ),Set.empty)
      fieldTargets.filter{filter(_,this)}
    }).flatten

    (valsMay,valsMust)
  }

  def getFieldValueWhere(objSet: ExpressionSet, field: String, typ: Type, filter:(HeapIdentifier,T) => Boolean): (Set[HeapIdentifier],Set[HeapIdentifier]) = {
    val (left,right) = objSet.getSetOfExpressions.map(getFieldValueWhere(_, field, typ, filter)).unzip
    val (newLeft,newRight) = (left.flatten,right.flatten)
    (newLeft,newRight)
  }

  /** Assumes an expression.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assume(cond: Expression): T = {
    val result = copy(
      valueState = valueState.assume(cond)
    )
    result
  }

  /** Assigns an expression to a variable.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assignVariable(left: Expression, right: Expression): T = left match {
    case leftVariable:VariableIdentifier =>
      if (leftVariable.representsSingleVariable) {
        // strong update
        right match {
          case rightHeap: HeapIdentifier => strongUpdateReference(leftVariable, rightHeap)
          case rightIdentifier: Identifier => strongUpdateAlias(leftVariable, rightIdentifier)
          case _ => strongUpdateValue(leftVariable,right)
        }
      } else {
        // weak update
        right match {
          case rightHeap: HeapIdentifier => weakUpdateReference(leftVariable, rightHeap)
          case rightIdentifier: Identifier => weakUpdateAlias(leftVariable, rightIdentifier)
          case _ => weakUpdateValue(leftVariable,right)
        }
      }
    case leftField:FieldIdentifier =>
      assignField(leftField.o,leftField.f,right)
  }

  /** Assigns an expression to a field.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assignField(obj: Expression, field: String, right: Expression): T = obj match {
    case leftHeap:HeapIdentifier =>
      val leftField = fieldFromString(leftHeap,field)
      if (leftHeap.representsSingleVariable) {
        // strong update
        right match {
          case rightHeap: HeapIdentifier => strongUpdateReference(leftField, rightHeap)
          case rightIdentifier: Identifier => strongUpdateAlias(leftField, rightIdentifier)
          case _ => strongUpdateValue(leftField,right)
        }
      } else {
        // weak update
        right match {
          case rightHeap:HeapIdentifier   => weakUpdateReference(leftField,rightHeap)
          case rightIdentifier:Identifier => weakUpdateAlias(leftField,rightIdentifier)
          case _ => weakUpdateValue(leftField,right)
        }
      }
    case leftIdentifier:Identifier =>
      forwardMay.get(leftIdentifier) match {
        case Some(set) if set.nonEmpty =>
          Lattice.bigLub(set.map{assignField(_,field,right)})
        case _ =>
          println("assigning to something that is empty - not a good idea")
          this
      }
    case _ => bottom()
  }

  /**
   * Removes the current expression
   *
   * @return The abstract state after removing the current expression
   */
  override def removeExpression(): T = copy(expr = ExpressionFactory.unitExpr)

  /**
   * Removes all variables satisfying filter
   */
  override def pruneVariables(filter: (Identifier) => Boolean): T = {

    var cur = this
    for (id <- ids) {
      if (filter(id)) {
        id match {
          case v:VariableIdentifier =>
            cur = cur.removeVariable(v)
        }
      }
    }
    cur

  }

  /**
   * Evaluates a numerical constant
   *
   * @param value The string representing the numerical constant
   * @param typ The type of the numerical constant
   * @param pp The program point that contains the constant
   * @return The abstract state after the evaluation of the constant, that is, the state that contains an expression representing this constant
   */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    this.setExpression(ExpressionSet(new Constant(value, typ, pp)))
  }

  /**
   * Creates an object
   *
   * @param typ The dynamic type of the created object
   * @param pp The point of the program that creates the object
   * @return The abstract state after the creation of the object
   */
  override def createObject(typ: Type, pp: ProgramPoint): T = {
    val oldVersions = versions.getOrElse((pp,typ),Seq.empty[HeapIdentifier])
    val unique = oldVersions.lastOption match {
      case Some(x) => x.unique+1
      case None => 0
    }
    val id = HeapIdentifier(pp,typ,summary = false, unique)
    val newVersions = versions.getOrElse((pp,typ),Seq.empty[HeapIdentifier]) :+ id

    val fieldIdentifiers = for (field <- typ.representedFields) yield {
      FieldIdentifier(id,field.getField.get,field.typ).asInstanceOf[Identifier]
    }

    val fieldIdentifierObjects = fieldIdentifiers.filter(_.typ.isObject)
    val fieldAssignment = fieldIdentifierObjects.map( _ -> Set.empty[HeapIdentifier] )

    val res = copy(
      forwardMay = forwardMay ++ fieldAssignment,
      forwardMust = forwardMust ++ fieldAssignment,
      versions = versions + ((pp,typ) -> newVersions),
      valueState = valueState.createVariables(fieldIdentifiers),
      expr = new ExpressionSet(typ).add(id)
    )

    res
  }

  /**
   * Sets the current expression
   *
   * @param expr The current expression
   * @return The abstract state after changing the current expression with the given one
   */
  override def setExpression(expr: ExpressionSet): T = {
    copy(expr = expr)
  }

  /**
   * Gets the value of a variable
   *
   * @param id The variable to access
   * @return The abstract state obtained after accessing the variable, that is, the state that contains as expression the symbolic representation of the value of the given variable
   */
  override def getVariableValue(id: Assignable): T = id match {
    case i: Identifier => copy(expr = new ExpressionSet(id.typ).add(i))
  }

  /**
   * Returns the top value of the lattice
   *
   * @return The top value, that is, a value x that is greater or equal than any other value
   */
  override def top(): T = factory(valueState = valueState.top(), expr = expr.top(), isTop = true)

  /**
   * Returns the bottom value of the lattice
   *
   * @return The bottom value, that is, a value x that is less or equal than any other value
   */
  override def bottom(): T = factory(valueState = valueState.bottom(), expr = expr.bottom(), isTop = false)


  /**
   * Computes the upper bound of two elements
   *
   * @param other The other value
   * @return The least upper bound, that is, an element that is greater or equal than the two arguments
   */
  override def lub(other: T): T = {

    val (left,right) = adaptEnvironments(this,other)
    val result = factory(
      MapUtil.mapToSetUnion(left.forwardMay,right.forwardMay),
      MapUtil.mapToSetIntersection(left.forwardMust,right.forwardMust),
      MapUtil.mapToSetUnion(left.backwardMay,right.backwardMay),
      MapUtil.mapToSeqKeepLonger(left.versions,right.versions),
      left.valueState.lub(right.valueState),
      left.expr.lub(right.expr),
      left.isTop || right.isTop
    )

    // Garbage collection invariant
    if (SystemParameters.DEBUG) {
      for ( (k,v) <- result.backwardMay ) {
        assert(v.nonEmpty)
      }
    }

    result
  }

  /**
   * Computes widening of two elements
   *
   * @param other The new value
   * @return The widening of <code>left</code> and <code>right</code>
   */
  override def widening(other: T): T = {

    val (left,right) = adaptEnvironments(this,other)

    val widened = factory(
      MapUtil.mapToSetUnion(left.forwardMay,right.forwardMay),
      MapUtil.mapToSetIntersection(left.forwardMust,right.forwardMust),
      MapUtil.mapToSetUnion(left.backwardMay,right.backwardMay),
      MapUtil.mapToSeqKeepLonger(left.versions,right.versions),
      left.valueState.widening(right.valueState),
      left.expr.widening(right.expr),
      left.isTop || right.isTop
    )

    // Cut sequences and merge to summary nodes
   val replacement = new Replacement()
    for ((pp,typ) <- widened.versions.keys) {
      val versions = widened.versions.getOrElse((pp,typ),Seq.empty)
      if (versions.length > 2) {

        // Merge objects
        val summaryID = HeapIdentifier(pp,typ,summary = true,versions.head.unique)
        val objectsToBeMerged = versions.take(versions.length - 1).toSet[Identifier]
        replacement.value += (objectsToBeMerged -> Set[Identifier](summaryID))

        // Merge fields
        val fieldsToBeMerged = for (field <- typ.representedFields) {
          val summaryField = FieldIdentifier(summaryID,field.getField.get,field.typ)
          val fieldsToBeMerged = objectsToBeMerged.map{x => FieldIdentifier(x.asInstanceOf[HeapIdentifier],field.getField.get,field.typ)}.toSet[Identifier]
          replacement.value += (fieldsToBeMerged -> Set[Identifier](summaryField))
        }

      }
    }

    val result = widened.merge(replacement).canonicalizeEnvironment

    // Garbage collection invariant
    if (SystemParameters.DEBUG) {
      for ( (k,v) <- result.backwardMay ) {
        assert(v.nonEmpty)
      }
    }

    result
  }

  /**
   * Returns true iff <code>this</code> is less or equal than <code>r</code>
   *
   * @param r The value to compare
   * @return true iff <code>this</code> is less or equal than <code>r</code>
   */
  override def lessEqual(r: T): Boolean = {

    if (isBottom || r.isTop) return true
    if (r.isBottom || isTop) return false

    val (left,right) = adaptEnvironments(this,r)
    if (!MapUtil.mapToSetContainment(left.forwardMay,right.forwardMay)) return false
    if (!MapUtil.mapToSetContainment(right.forwardMust,left.forwardMust)) return false
    if (!MapUtil.mapToSetContainment(left.backwardMay,right.backwardMay)) return false
    // no check on versions. however, this should be fine?
    if (!left.valueState.lessEqual(right.valueState)) return false
    if (!left.expr.lessEqual(right.expr)) return false

    return true
  }

  override def isBottom:Boolean = {

    if (isTop) return false
//    if (forwardMay.exists( _._2.isEmpty))
//      return true
    if (valueState.isBottom) return true
    return false

  }


  def mergeForwards(rep:Replacement,map:Map[Identifier,Set[HeapIdentifier]],revMap:Map[HeapIdentifier,Set[Identifier]]): Map[Identifier,Set[HeapIdentifier]] = {

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.mergeForwards")

    if (rep.isEmpty()) return map
    var result = map
    for ((left,right) <- rep.value) {
      // compute the lub of everything on the left side
      val lub = left.foldLeft(Set.empty[HeapIdentifier])((s,l) => l match { case a:Identifier => s ++ map.getOrElse(a,Set.empty); case _ => s })
      // assign it to everything on the right side
      if (lub.nonEmpty) {
        for (r <- right) {
          r match {
            case a: HeapIdentifier => ()
            case a: Identifier => result = result + (a -> lub)
            case _ => ()
          }
        }
      }
    }

    // remove everything on the left side in the keys of the map
    val toBeRemoved = rep.value.keySet.flatten -- rep.value.values.flatten
    result = result -- toBeRemoved

    // perform replacements on the right side
    for ((l,r) <- rep.value) {
      val toBeReplacedSet = l.view.collect { case x:HeapIdentifier => x }.toSet
      val byThis = r.view.collect { case x:HeapIdentifier => x }.toSet

      // look it up using the reverse map
      for (toBeReplaced <- toBeReplacedSet; pointer <- revMap.getOrElse(toBeReplaced,Set.empty); if !toBeRemoved.contains(pointer)) {
        result = result + (pointer -> ((result.getOrElse(pointer,Set.empty) -- toBeReplacedSet) ++ byThis))
      }
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.mergeForwards")

    result
  }

  def mergeBackwards(rep:Replacement,map:Map[HeapIdentifier,Set[Identifier]],revMap:Map[Identifier,Set[HeapIdentifier]]): Map[HeapIdentifier,Set[Identifier]] = {

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.mergeBackwards")

    if (rep.isEmpty()) return map
    val mapKeys = map.keySet.toSet[Identifier]
    var result = map
    for ((left,right) <- rep.value if (left intersect mapKeys).nonEmpty) {
      // compute the lub of everything on the left side
      val lub = left.foldLeft(Set.empty[Identifier])((s,l) => l match { case a:HeapIdentifier => s ++ map.getOrElse(a,Set.empty); case _ => s })
      // assign it to everything on the right side
      for (r <- right) {
        r match {
          case a:HeapIdentifier => result = result + (a -> lub)
          case _ => ()
        }
      }
    }

    // remove everything on the left side in the keys of the map
    val toBeRemoved = rep.value.keySet.flatten.collect { case x:HeapIdentifier => x } -- rep.value.values.flatten.collect { case x:HeapIdentifier => x }
    result = result -- toBeRemoved

    // perform replacements on the right side
    for ((l,r) <- rep.value) {
      val toBeReplacedSet = l.view.filter { case x:HeapIdentifier => false; case _ => true }.toSet
      val byThis = r.view.filter { case x:HeapIdentifier => false; case _ => true }.toSet

      // look it up using the reverse map
      for (toBeReplaced <- toBeReplacedSet; pointer <- revMap.getOrElse(toBeReplaced,Set.empty); if !toBeRemoved.contains(pointer)) {
        result = result + (pointer -> ((result.getOrElse(pointer,Set.empty) -- toBeReplacedSet) ++ byThis))
      }
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.mergeBackwards")

    result
  }


  def mergeVersions(rep: Replacement, map: Map[(ProgramPoint,Type), Seq[HeapIdentifier]]): Map[(ProgramPoint,Type), Seq[HeapIdentifier]] = {


    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.mergeVersions")

    var cur = map
    for ((ls,rs) <- rep.value) {
      for (l <- ls) {

        l match {

          case h:HeapIdentifier =>

            // replace all, but only add one copy of the right side
            val versions = cur.get((h.pp,h.typ)).get // must exist
            var already = false // ugly, works.
            val newVersions = versions.foldRight(Seq.empty[HeapIdentifier])(
              (a,b) =>
                if (ls.contains(a)) {
                  if (already) b
                  else {
                    already = true
                    rs.toSeq.sortBy( _.toString ).map(_.asInstanceOf[HeapIdentifier]) ++: b // tooo slow
                  }
                } else a +: b
            )
            cur = cur + ((l.pp,l.typ) -> newVersions)

          case _ => ()

        }

      }
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.mergeVersions")

    cur
  }

  def merge(r:Replacement):T = {

    if (r.isEmpty()) return this

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.merge")

    val result = copy(
      mergeForwards(r,forwardMay,backwardMay),
      mergeForwards(r,forwardMust,backwardMay),
      mergeBackwards(r,backwardMay,forwardMay),
      mergeVersions(r,versions),
      valueState.merge(r),
      expr.merge(r)
    )

    // DEBUG CODE
    if (SystemParameters.DEBUG) {
      val removedIdentifiers = r.removedIdentifiers
      val addedIdentifiers = r.addedIdentifiers
      val ids = result.ids
      val remaining = ids intersect removedIdentifiers
      val nonAdded = addedIdentifiers -- ids
      assert (remaining.isEmpty)
      assert (nonAdded.isEmpty)
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.merge")

    result
  }

  override def pruneUnreachableHeap(): T = this   // garbage collection is immediate
  override def before(pp: ProgramPoint): T = this // nothing to be done
  override def factory(): T = factory(valueState = valueState.factory())
  override def glb(other: T): T = ???
  override def setVariableToTop(varExpr: Expression): T = ???
  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = ???
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): T = ???
  override def throws(t: ExpressionSet): T = ???

  /** Prune all objects that have no references, starting from startNode */
  def garbageCollect(nodes: List[HeapIdentifier]): T = {

    if (nodes.isEmpty) return this

    def removeObject(node:HeapIdentifier): T = {
      val fields = fieldsOf(node).toSet[Identifier]
      copy(
        (forwardMay -- fields).map { x => x._1 -> (x._2 - node) },
        (forwardMust -- fields).map { x => x._1 -> (x._2 - node) },
        (backwardMay - node).map { x => x._1 -> (x._2 -- fields) },
        {
          val newVersions = versions.getOrElse((node.pp,node.typ),Seq.empty).filter( _ != node )
          if (newVersions.nonEmpty)
            versions + ((node.pp,node.typ) -> newVersions)
          else
            versions - ((node.pp,node.typ))
        },
        valueState.removeVariables(fields + node)
      )
    }

    val result = nodes match {
      case x :: xs =>
        if (backwardMay.getOrElse(x, Set.empty).isEmpty) {
          removeObject(x).garbageCollect(
            (x.typ.representedFields map {
              case field : TouchField  => forwardMay.getOrElse(FieldIdentifier(x,field.getField.get,field.typ),Set.empty)
            }).flatten.toList ::: xs
          )
        } else { garbageCollect(xs) }
      case Nil => this
    }

    if (SystemParameters.DEBUG) {
        val ids = result.ids
        for (n <- nodes) {
          if (result.backwardMay.getOrElse(n, Set.empty).isEmpty) {
            // unreachable
            assert(!ids.contains(n))
            for (m <- fieldsOf(n)) {
              assert(!ids.contains(m))
            }
          }
        }
    }

    result
  }

  def fieldsOf(node:HeapIdentifier):Set[FieldIdentifier] = {
    node.typ.representedFields map {
      case field : TouchField  => FieldIdentifier(node,field.getField.get,field.typ)
    }
  }

  def fieldFromString(node:HeapIdentifier, field:String):FieldIdentifier = {
    val tField = node.typ.representedFields.find{_.getField.get == field}.get
    FieldIdentifier(node,tField.getField.get,tField.typ)
  }


  def strongUpdateAlias(left:Identifier, right:Identifier): T = {
    // copy references from other variable/field
    val newObjectsMay =  forwardMay.getOrElse (right,Set.empty)
    val newObjectsMust = forwardMust.getOrElse(right,Set.empty)
    copy(
      forwardMay +  (left -> newObjectsMay),
      forwardMust + (left -> newObjectsMust),
      (backwardMay map { x => x._1 -> (x._2 - left) }) ++ (newObjectsMay map { x => x -> (backwardMay.getOrElse(x,Set.empty) + left)}),
      versions,
      valueState.assign(left,right),
      ExpressionFactory.unitExpr
    ).garbageCollect(forwardMay.getOrElse(left,Set.empty).toList).canonicalizeEnvironment
  }


  def strongUpdateReference(left:Identifier, right:HeapIdentifier): T = {
    // reference object
    val afterUpdate = this.copy(
      forwardMay +  (left -> Set(right)),
      forwardMust + (left -> Set(right)),
      (backwardMay map { x => x._1 -> (x._2 - left) }) + (right -> (backwardMay.getOrElse(right,Set.empty) + left)),
      versions,
      valueState.assign(left,ValidExpression(left.typ,left.pp)),
      ExpressionFactory.unitExpr
    )
    val afterGarbageCollect = afterUpdate.garbageCollect(forwardMay.getOrElse(left,Set.empty).toList)
    val canonicalized = afterGarbageCollect.canonicalizeEnvironment
    canonicalized
  }

  def strongUpdateValue(left:Identifier, right:Expression): T = {
    // reference object
    copy(
      forwardMay,
      forwardMust,
      backwardMay,
      versions,
      valueState.assign(left,right),
      ExpressionFactory.unitExpr
    ) // TODO: Correct?
  }

  def weakUpdateReference(left:Identifier, right:HeapIdentifier): T = {
    // reference object
    copy(
      forwardMay +  (left -> (forwardMay.getOrElse (left,Set.empty) + right)),
      forwardMust + (left -> (forwardMust.getOrElse(left,Set.empty) + right)),
      backwardMay + (right -> (backwardMay.getOrElse(right,Set.empty) + left)),
      versions,
      valueState.assign(left,ValidExpression(left.typ,left.pp)).lub(valueState),
      ExpressionFactory.unitExpr
    )
    // nothing may become unreachable
  }

  def weakUpdateAlias(left:Identifier, right:Identifier): T = {
    // copy references from other variable/field
    val newObjectsMay =  forwardMay.getOrElse (right,Set.empty)
    val newObjectsMust = forwardMust.getOrElse(right,Set.empty)
    copy(
      forwardMay +  (left -> (forwardMay.getOrElse (left,Set.empty) ++ newObjectsMay)),
      forwardMust + (left -> (forwardMust.getOrElse(left,Set.empty) ++ newObjectsMust)),
      backwardMay ++ (newObjectsMay map { x => x -> (backwardMay.getOrElse(x,Set.empty) + left)}),
      versions,
      valueState.assign(left,right).lub(valueState),
      ExpressionFactory.unitExpr
    )
    // nothing may become unreachable
  }

  def weakUpdateValue(left:Identifier, right:Expression): T = {
    // reference object
    copy(
      forwardMay,
      forwardMust,
      backwardMay,
      versions,
      valueState.assign(left,right).lub(valueState),
      ExpressionFactory.unitExpr
    ) // TODO: Correct?
  }

  def ids:Set[Identifier] = {
    forwardMay.keySet ++ forwardMay.values.flatten ++
    backwardMay.keySet ++ backwardMay.values.flatten ++
    forwardMust.keySet ++ forwardMust.values.flatten ++
    versions.values.flatten ++
    valueState.ids
  }

  /**
   * Makes sure that versions are called according to their indexes in the list
   */
  def canonicalizeEnvironment:T = {

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.canonicalizeEnvironment")

    var curState = this

    for ((k, v) <- versions) {
      for ((x, i) <- v.view.zipWithIndex) {
        if (x.unique != i) {
          val rep = new Replacement(isPureRenaming = true)
          val newObject = x.copy(unique = i)
          rep.value(Set(x)) = Set(newObject.asInstanceOf[Identifier])
          for (field <- x.typ.representedFields) {
            val from = FieldIdentifier(x, field.getField.get, field.typ)
            val to = FieldIdentifier(newObject, field.getField.get, field.typ)
            assert(!curState.ids.contains(to))
            rep.value(Set(from).toSet[Identifier]) = Set(to).toSet[Identifier]
          }
          curState = curState.merge(rep)
        }
      }
    }

    if (SystemParameters.DEBUG) {
      for ( (k,v) <- curState.versions ) {
        for ((x, i) <- v.view.zipWithIndex) {
          assert (x.unique == i)
        }
      }
    }

    // Garbage collection invariant
    if (SystemParameters.DEBUG) {
      for ( (k,v) <- curState.backwardMay ) {
        assert(v.nonEmpty)
      }
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.canonicalizeEnvironment")

    curState

  }

  /**
   * Makes sure left is summary iff right is summary
   *
   * precondition: l and r canonicalized!
   */
  def adaptSummaryNodes(l:T,r:T): (T,T) = {

    val replaceLeft  = new Replacement(isPureRenaming = true)
    val replaceRight = new Replacement(isPureRenaming = true)

    for (
      pp <- l.versions.keySet intersect r.versions.keySet;
      (left,right) = (l.versions.get(pp).get,r.versions.get(pp).get)
      if left.nonEmpty && right.nonEmpty
    ) {

      if (left.head.summary && !right.head.summary) {
        replaceRight.value(Set[Identifier](right.head)) = Set[Identifier](left.head)
        for (f <- fieldsOf(right.head)) replaceRight.value(Set[Identifier](f)) = Set[Identifier](f.copy(o = left.head))
      } else if (!left.head.summary && right.head.summary) {
        replaceLeft.value(Set[Identifier](left.head)) = Set[Identifier](right.head)
        for (f <- fieldsOf(left.head)) replaceLeft.value(Set[Identifier](f)) = Set[Identifier](f.copy(o = right.head))
      }

    }

    (l.merge(replaceLeft),r.merge(replaceRight))

  }

  def adaptEnvironments(l:T,r:T): (T,T) = {
    adaptSummaryNodes(l.canonicalizeEnvironment,r.canonicalizeEnvironment)
  }

  override def toString: String = {

    "Points-To:\n" +
      ToStringUtilities.indent(forwardMay.map {
        case (a, b) =>
          val must = forwardMust.getOrElse(a, Set.empty)
          a.toString + " -> " + b.map { x => x.toString + (if (must.contains(x)) " (must)" else " (may)")}.mkString(", ")
      }.mkString("\n"))  +
    "\nPointed-By:\n" +
      ToStringUtilities.indent(backwardMay.map {
        case (a, b) =>
          a.toString + " -> " + b.map { x => x.toString }.mkString(", ")
      }.mkString("\n")) +
    "\nVersions:\n" +
      ToStringUtilities.indent(versions.map {
        case (a, b) =>
          a.toString + " -> " + b.map { x => x.unique + (if (x.summary) "Σ" else "") }.mkString(", ")
      }.mkString("\n")) +
    "\nValues:\n" +
      ToStringUtilities.indent(valueState.toString) +
    "\nExpression: " + expr.toString

  }


  /**
   * Graph-like interface
   */
  lazy val vertices:Set[Identifier] = forwardMay.map{ case (a,b) => b.toSet[Identifier] + a}.flatten.toSet

  lazy val edges:Set[(Identifier,String,Identifier)] = {
    (forwardMay.map{ case (a,b) => b.map((a,"may",_)) }.flatten ++
    forwardMust.map{ case (a,b) => b.map((a,"must",_))}.flatten ++
    backwardMay.map{ case (a,b) => b.map((a,"back",_))}.flatten ++
      (for (v <- vertices) yield { v match {
        case x:FieldIdentifier => Some((x.o,"field",x))
        case _ => None
      }}).flatten).toSet
  }

}

object TouchState {
  
  case class Default[S <: SemanticDomain[S]](
                                              forwardMay:        Map[Identifier,Set[HeapIdentifier]] = Map.empty,
                                              forwardMust:       Map[Identifier,Set[HeapIdentifier]] = Map.empty,
                                              backwardMay:       Map[HeapIdentifier,Set[Identifier]] = Map.empty,
                                              versions:          Map[(ProgramPoint,Type),Seq[HeapIdentifier]] = Map.empty,
                                              valueState:        S,
                                              expr:              ExpressionSet = ExpressionFactory.unitExpr,
                                              isTop:             Boolean = false
                                            )
    extends TouchState[S, Default[S]] {

    def factory (
                  forwardMay:        Map[Identifier,Set[HeapIdentifier]] = Map.empty,
                  forwardMust:       Map[Identifier,Set[HeapIdentifier]] = Map.empty,
                  backwardMay:       Map[HeapIdentifier,Set[Identifier]] = Map.empty,
                  versions:          Map[(ProgramPoint,Type),Seq[HeapIdentifier]] = Map.empty,
                  valueState:        S,
                  expr:              ExpressionSet = ExpressionFactory.unitExpr,
                  isTop:             Boolean = false
                ):Default[S] =
      Default(forwardMay, forwardMust, backwardMay, versions, valueState, expr, isTop)

  }
}
