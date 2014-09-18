package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Type, ProgramPoint}
import ch.ethz.inf.pm.sample.util.MapUtil
import ch.ethz.inf.pm.td.analysis.TouchField

case class HeapIdentifier(pp: ProgramPoint, typ:Type, summary:Boolean, unique:Int) extends Identifier {
  override def getName: String = pp + typ.toString + (if (summary) "Σ" else "") + unique
  override def getField: Option[String] = None
  override def representsSingleVariable: Boolean = !summary
}

case class FieldIdentifier(o:HeapIdentifier,f:String,typ:Type) extends Identifier {
  override def pp:  ProgramPoint = o.pp
  override def getName: String = o + f + typ.toString
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
  val versions:          Map[ProgramPoint,Seq[HeapIdentifier]]
  val valueState:        S
  val expr:              ExpressionSet
  val isTop:             Boolean

  def copy (
    forwardMay:        Map[Identifier,Set[HeapIdentifier]] = forwardMay,
    forwardMust:       Map[Identifier,Set[HeapIdentifier]] = forwardMust,
    backwardMay:       Map[HeapIdentifier,Set[Identifier]] = backwardMay,
    versions:          Map[ProgramPoint,Seq[HeapIdentifier]] = versions,
    valueState:        S = valueState,
    expr:              ExpressionSet = expr,
    isTop:             Boolean = isTop
  ):T = factory(forwardMay,forwardMust,backwardMay,versions,valueState,expr,isTop)

  def factory (
    forwardMay:        Map[Identifier,Set[HeapIdentifier]] = Map.empty,
    forwardMust:       Map[Identifier,Set[HeapIdentifier]] = Map.empty,
    backwardMay:       Map[HeapIdentifier,Set[Identifier]] = Map.empty,
    versions:          Map[ProgramPoint,Seq[HeapIdentifier]] = Map.empty,
    valueState:        S = valueState.factory(),
    expr:              ExpressionSet = ExpressionFactory.unitExpr,
    isTop:             Boolean = false
  ):T

  /** Creates a variable given a `VariableIdentifier`.
    * Implementations can already assume that this state is non-bottom.
    */
  override def createVariable(va: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    copy(
      forwardMay + (va -> Set.empty),
      forwardMust + (va -> Set.empty),
      backwardMay,
      versions,
      valueState.createVariable(va)
    ).garbageCollect(forwardMust.getOrElse(va,Set.empty).toList).canonicalizeEnvironment
  }

  /** Removes the given variable.
    * Implementations can assume this state is non-bottom
    */
  override def removeVariable(va: VariableIdentifier): T = {
    copy(
      forwardMay - va,
      forwardMust - va,
      backwardMay.map( x => x._1 -> (x._2 - va) ),
      versions,
      valueState.removeVariable(va)
    ).garbageCollect(forwardMay.getOrElse(va,Set.empty).toList).canonicalizeEnvironment
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
      val objs = forwardMay.getOrElse(a,Set.empty)
      copy(
        expr = new ExpressionSet(typ,SetDomain.Default(objs.map(FieldIdentifier(_,field,typ))))
      )
}

  /** Assumes an expression.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assume(cond: Expression): T = {
    copy(
      valueState = valueState.assume(cond)
    )
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
   * @param fields If this is defined, the given fields will be created instead of the types fields (e.g. for reducing
   *               the set of initialized fields)
   * @return The abstract state after the creation of the object
   */
  override def createObject(typ: Type, pp: ProgramPoint, fields: Option[Set[Identifier]]): T = {
    val oldVersions = versions.getOrElse(pp,Seq.empty[HeapIdentifier])
    val unique = oldVersions.lastOption match {
      case Some(x) => x.unique+1
      case None => 0
    }
    val id = HeapIdentifier(pp,typ,summary = false, unique)
    val newVersions = versions.getOrElse(pp,Seq.empty[HeapIdentifier]) :+ id
    copy(
      versions = versions + (pp -> newVersions),
      expr = new ExpressionSet(typ).add(id)
    )
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
  override def top(): T = factory(valueState = valueState.top(),isTop = true)

  /**
   * Returns the bottom value of the lattice
   *
   * @return The bottom value, that is, a value x that is less or equal than any other value
   */
  override def bottom(): T = factory(valueState = valueState.bottom(),isTop = false)


  /**
   * Computes the upper bound of two elements
   *
   * @param other The other value
   * @return The least upper bound, that is, an element that is greater or equal than the two arguments
   */
  override def lub(other: T): T = {

    val (left,right) = adaptEnvironments(this,other)
    factory(
      MapUtil.mapToSetUnion(left.forwardMay,right.forwardMay),
      MapUtil.mapToSetIntersection(left.forwardMust,right.forwardMust),
      MapUtil.mapToSetUnion(left.backwardMay,right.backwardMay),
      MapUtil.mapToSeqKeepLonger(left.versions,right.versions),
      left.valueState.lub(right.valueState),
      left.expr.lub(right.expr),
      left.isTop || right.isTop
    )

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
    for (pp <- widened.versions.keys) {
      val versions = widened.versions.getOrElse(pp,Seq.empty)
      if (versions.length > 3) {
        val id = HeapIdentifier(pp,versions.head.typ,summary = true,versions.head.unique)
        replacement.value(versions.take(versions.length - 3).toSet) = Set[Identifier](id)
      }
    }
    widened.merge(replacement)

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
    //if (forwardMay.exists( _._2.isEmpty)) return true TODO: Add again
    if (valueState.isBottom) return true
    return false

  }

  def merge[A <: Identifier,B <: Identifier](rep:Replacement,map:Map[A,Set[B]]): Map[A,Set[B]] = {

    if (rep.isEmpty()) return map
    var result = map
    for ((left,right) <- rep.value) {
      // compute the lub of everything on the left side
      val lub = left.foldLeft(Set.empty[B])((s,l) => l match { case a:A => s ++ map.getOrElse(a,Set.empty); case _ => s })
      // assign it to everything on the right side
      for (r <- right) {
        r match {
          case a:A => result = result + (a -> lub)
          case _ => ()
        }
      }
    }

    // remove everything on the left side in the keys of the map
    result = result -- rep.keySet().flatten.collect { case x:A => x }.toSet

    result = for ((a,b) <- result) yield {
      var set = b
      for ((c,d) <- rep.value) {
        val cB = c.collect { case x:B => x }.toSet
        val dB = d.collect { case x:B => x }.toSet // FIXME: Inefficient!!
        if ((set intersect cB).nonEmpty) {
          set = set -- cB ++ dB
        }
      }
      a -> set
    }


    result
  }

  def merge(r:Replacement):T = {
    copy(
      merge(r,forwardMay),
      merge(r,forwardMust),
      merge(r,backwardMay),
      versions, // TODO TODO TODO
      valueState.merge(r),
      expr.merge(r)
    )
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

    def removeObject(node:HeapIdentifier): T = {
      val fields = fieldsOf(node)
      copy(
        (forwardMay -- fields).map { x => x._1 -> (x._2 - node) },
        (forwardMust -- fields).map { x => x._1 -> (x._2 - node) },
        (backwardMay - node).map { x => x._1 -> (x._2 -- fields) },
        versions.map { x => x._1 -> x._2.filterNot( _ == node) },
        valueState.removeVariables(fields + node)
      )
    }

    nodes match {
      case x :: xs =>
        if (backwardMay.getOrElse(x, Set.empty).isEmpty) {
          removeObject(x).garbageCollect(
            (x.typ.possibleFields map {
              case field : TouchField  => forwardMay.getOrElse(FieldIdentifier(x,field.getName,field.typ),Set.empty)
            }).flatten.toList ::: xs
          )
        } else { garbageCollect(xs) }
      case Nil => this
    }

  }

  def fieldsOf(node:HeapIdentifier):Set[Identifier] = {
    node.typ.possibleFields map {
      case field : TouchField  => FieldIdentifier(node,field.getName,field.typ)
    }
  }

  def fieldFromString(node:HeapIdentifier, field:String):FieldIdentifier = {
    val tField = node.typ.possibleFields.find{_.getName == field}.get
    FieldIdentifier(node,tField.getName,tField.typ)
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
    copy(
      forwardMay +  (left -> Set(right)),
      forwardMust + (left -> Set(right)),
      (backwardMay map { x => x._1 -> (x._2 - left) }) + (right -> (backwardMay.getOrElse(right,Set.empty) + left)),
      versions,
      valueState.assign(left,ValidExpression(left.typ,left.pp)),
      ExpressionFactory.unitExpr
    ).garbageCollect(forwardMay.getOrElse(left,Set.empty).toList).canonicalizeEnvironment
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
      forwardMay.keySet
  }

  /**
   * Makes sure that versions are called according to their indexes in the list
   */
  def canonicalizeEnvironment:T = {

    val rep  = new Replacement(isPureRenaming = true)
    for ( (k,v) <- versions ) {
      for ((x,i) <- v.view.zipWithIndex) {
        if (x.unique != i) {
          rep.value(Set(x)) = Set(x.copy(unique = i).asInstanceOf[Identifier])
        }
      }
    }
    this.merge(rep)

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
        replaceRight.value(Set(right.head)) = Set(left.head.asInstanceOf[Identifier])
      } else if (!left.head.summary && right.head.summary) {
        replaceLeft.value(Set(left.head)) = Set(right.head.asInstanceOf[Identifier])
      }

    }

    (l.merge(replaceLeft),r.merge(replaceRight))

  }

  def adaptEnvironments(l:T,r:T): (T,T) = {
    adaptSummaryNodes(l.canonicalizeEnvironment,r.canonicalizeEnvironment)
  }

//  def adaptEnvironments(l:T,r:T): (T,T) = {
//
//    val replaceLeft  = new Replacement(isPureRenaming = true)
//    val replaceRight = new Replacement(isPureRenaming = true)
//
//    for (pp <- l.versions.keys ++ r.versions.keys) {
//
//      val left = l.versions.getOrElse(pp,Seq.empty)
//      val right = r.versions.getOrElse(pp,Seq.empty)
//
//      if (right.isEmpty || right.head.representsSingleVariable ||
//        (left.nonEmpty && !left.head.representsSingleVariable)) {
//
//        // give preference to left names
//        for ((a,b) <- left zip right) {
//          if (a != b)
//            replaceRight.value(Set(b)) = Set[Identifier](a)
//        }
//
//      } else {
//
//        // give preference to left names
//        for ((a,b) <- left zip right) {
//          if (a != b)
//            replaceLeft.value(Set(a)) = Set[Identifier](b)
//        }
//
//      }
//
//    }
//
//    (l.merge(replaceLeft),r.merge(replaceRight))
//
//  }

}

object TouchState {
  
  case class Default[S <: SemanticDomain[S]](
                                              forwardMay:        Map[Identifier,Set[HeapIdentifier]] = Map.empty,
                                              forwardMust:       Map[Identifier,Set[HeapIdentifier]] = Map.empty,
                                              backwardMay:       Map[HeapIdentifier,Set[Identifier]] = Map.empty,
                                              versions:          Map[ProgramPoint,Seq[HeapIdentifier]] = Map.empty,
                                              valueState:        S,
                                              expr:              ExpressionSet = ExpressionFactory.unitExpr,
                                              isTop:             Boolean = false
                                            )
    extends TouchState[S, Default[S]] {

    def factory (
                  forwardMay:        Map[Identifier,Set[HeapIdentifier]] = Map.empty,
                  forwardMust:       Map[Identifier,Set[HeapIdentifier]] = Map.empty,
                  backwardMay:       Map[HeapIdentifier,Set[Identifier]] = Map.empty,
                  versions:          Map[ProgramPoint,Seq[HeapIdentifier]] = Map.empty,
                  valueState:        S,
                  expr:              ExpressionSet = ExpressionFactory.unitExpr,
                  isTop:             Boolean = false
                ):Default[S] =
      Default(forwardMay, forwardMust, backwardMay, versions, valueState, expr, isTop)

  }
}
