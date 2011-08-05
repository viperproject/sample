package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain._

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.property.Property
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}

/**
 * <code>TVSHeap</code> is a HeapDomain that uses TVLA to analyse the heap.
 */
class TVSHeap extends HeapDomain[TVSHeap, NodeName] {

  def this(other: TVSHeap) {
    this()
    structures = other.structures
    variables = other.variables
    tempVariables = other.tempVariables
    isTop = other.isTop
    isBottom = other.isBottom
    ppCreates = other.ppCreates
    fields = other.fields
  }


  //================================================================================
  // Heap State
  //================================================================================

  /**
   * Set of three-valued structures
   */
  var structures: Set[TVS[NodeName]] = Set(new TVS)

  /**
   * Normal program variable identifiers
   */
  var variables: Set[VariableIdentifier] = Set.empty


  /**
   * Temporary variable identifiers
   */
  var tempVariables: Set[TemporaryVariableIdentifier] = Set.empty

  /**
   * All field names seen by the heap domain
   */
  var fields: Set[String] = Set.empty

  var ppCreates: Map[ProgramPoint, Int] = Map.empty


  /**
   * Holds when state is T
   */
  var isTop: Boolean = false

  /**
   * Holds when state is _|_
   */
  var isBottom: Boolean = false



  /**
   * To pretty-print the heap state
   */
  override def toString = {
    if (isTop) "T"
    else if (isBottom) "_|_"
    else {
      "Variables: " + variables.mkString(",") + "\n" +
        "Temporaries: " + tempVariables.mkString(",") + "\n" +
        "Structures:\n" + structures.mkString("\n")
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: TVSHeap =>
      (this eq that) ||
        (this.variables == that.variables &&
          this.tempVariables == that.tempVariables &&
          this.structures == that.structures &&
          this.isBottom == that.isBottom &&
          this.isTop == that.isTop &&
          this.fields == that.fields)
    case _ => false
  }

  override def hashCode(): Int =
    41 * (41 * (41 + variables.hashCode) + tempVariables.hashCode) + structures.hashCode()



  //================================================================================
  // Heap Operations
  //================================================================================


  /**
   * Creates an object of type typ on the heap and return a heap identifier representing that
   * object.
   */
  def createObject(typ: Type, pp: ProgramPoint): (TVSHeapIDSet, TVSHeap, Replacement) = {

    // create temporary variable which afterwards points to the created heap node
    val tempheap = new TVSHeap(this)
    val tempvar = tempheap.addTemporaryVariable()

    // if the object has a new fields, we add the necessary predicates
    for (f <- (typ.getPossibleFields map {
      _.getName()
    })) {
      if (!fields.contains(f)) {
        tempheap.structures = this.structures.map(_.addField("field_" + f))
        tempheap.fields += f
      }
    }

    val tvp = new TVP(tempheap)
    tvp.newPP = Some(pp)
    tvp.newPPNum = ppCreates.getOrElse(pp, 0)
    tvp.addAction(new CreateObject(tempvar.toString()))
    val (newheap, repl) = tvp.execute()
    newheap.ppCreates += pp -> (tvp.newPPNum + 1)

    (newheap.extractHeapId(tempvar.toString(), typ), newheap, repl)
  }

  /**
   * Return an identifier for objectid.fieldname, where objectid is the target of the field access
   * and may be a variable or a TVSHeapIDSet obtained previously (e.g  (x.n).fieldname)
   *
   * In some (or all) structures this may return an empty set if the field is null.
   */
  def getFieldIdentifier(target: Assignable, fieldname: String, typ: Type, pp: ProgramPoint): (TVSHeapIDSet, TVSHeap, Replacement) = {
    target match {
      case variable: VariableIdentifier =>
        val tempheap = new TVSHeap(this)
        val tempvar = tempheap.addTemporaryVariable()
        val tvp = new TVP(tempheap)
        tvp.addAction(new ExtractField(tempvar.toString, variable.toString, tvp.encodeFieldname(fieldname)))
        val (newheap, repl) = tvp.execute()

        val heapidset = newheap.extractHeapId(tempvar.toString, typ)
        (heapidset, newheap, repl)

      case heapidset: TVSHeapIDSet =>
        val tempheap = new TVSHeap(this)
        val targetTemporary = tempheap.addTemporaryVariable()
        val tvp = new TVP(tempheap)
        tvp.addAction(new ExtractField(targetTemporary.toString, heapidset.pointedBy, tvp.encodeFieldname(fieldname)))

        val (newheap, repl) = tvp.execute()
        val newheapidset = newheap.extractHeapId(targetTemporary.toString, typ)

        (newheapidset, newheap, new Replacement)
      case _ => throw new Exception()
    }
  }

  /**
   * Create a TVSHeapIDSet containing the nodes pointed to by a temporary variable tempvar
   */
  private def extractHeapId(tempvar: String, typ: Type): TVSHeapIDSet = {
    val ids: Set[NodeName] = structures.flatMap(_.programVariables(tempvar).value)
    val heapidset = new TVSHeapIDSet(tempvar)
    heapidset.value = ids
    heapidset
  }

  /**
   * Add a temporary program variable (only visible to this heap domain)
   * NOTE: modifies the heap state. Only use on new heap instances
   */
  private def addTemporaryVariable(): TemporaryVariableIdentifier = {
    val temp = new TemporaryVariableIdentifier("temp_" + tempVariables.size, SystemParameters.typ, null)
    tempVariables = tempVariables + temp
    structures = this.structures.map(_.addVariable(temp.name))
    temp
  }

  /**
   * Create a heap node for a numerical field. This happens during the assignment
   * of a numerical expression to a field (e.g. x.i = 0)
   */
  private def createNumericField(obj: Assignable, field: String, typ: Type, pp: ProgramPoint): (TVSHeap, Replacement) = {
    val target = obj match {
      case lhs: VariableIdentifier => lhs
      case lhs: TVSHeapIDSet => lhs.pointedBy
      case _ => throw new Exception("invalid target")
    }

    val tempheap = new TVSHeap(this)
    val tempvar = tempheap.addTemporaryVariable()

    val tvp = new TVP(tempheap)
    tvp.newPP = Some(pp)
    tvp.newPPNum = ppCreates.getOrElse(pp, 0)
    tvp.addAction(new CreateObject(tempvar.toString))
    tvp.addAction(new SetFieldNull(target.toString, tvp.encodeFieldname(field)))
    tvp.addAction(new SetField(target.toString, tvp.encodeFieldname(field), tempvar.toString))
    val (newheap, repl) = tvp.execute()
    newheap.ppCreates += pp -> (tvp.newPPNum + 1)

    (newheap, repl)
  }

  /**
   * Assignment to a field
   */
  def assignField(obj: Assignable, field: String, expr: Expression): (TVSHeap, Replacement) = {

    // we represent numerical fields in the heap explicitly. createObject was never invoked for them before,
    // so we create a corresponding object now if the expr is numerical.
    if (expr.getType.getName == "Int" || expr.getType.getName == "String") {
      return createNumericField(obj, field, expr.getType, expr.getProgramPoint)
    }

    val target = obj match {
      case lhs: VariableIdentifier => lhs
      case lhs: TVSHeapIDSet => lhs.pointedBy
      case _ => throw new Exception("assignField: invalid target")
    }

    val tvp = new TVP(this)

    val assignActions = expr match {
    // x.n = y
      case rhs: VariableIdentifier => List(
        new SetFieldNull(target.toString, tvp.encodeFieldname(field)),
        new SetField(target.toString, tvp.encodeFieldname(field), rhs.toString)
      )
      // x.n = y.n  or x.n = new Foo
      case rhs: TVSHeapIDSet => List(
        new SetFieldNull(target.toString, tvp.encodeFieldname(field)),
        new SetField(target.toString, tvp.encodeFieldname(field), rhs.pointedBy)
      )
      // x.n = null
      case rhs: Constant =>
        List(new SetFieldNull(target.toString, tvp.encodeFieldname(field)))
      case _ => throw new Exception("assignField: Invalid rhs")
    }

    for (a <- assignActions)
      tvp.addAction(a)

    val (newheap, repl) = tvp.execute()

    (newheap, repl)
  }

  /**
   * After an assignment, we set all temporary variables to null and discard them.
   */
  def endOfAssignment(): (TVSHeap, Replacement) = {
    if (tempVariables.isEmpty)
      return (this, new Replacement)

    val tvp = new TVP(this)

    for (t <- tempVariables) {
      tvp.addAction(new SetVariableNull(t.toString))
    }

    val (newheap, repl) = tvp.execute()

    // temporaries are all discarded in order to have a bounded heap
    for (t <- tempVariables) {
      newheap.structures = newheap.structures.map(_.removeVariable(t.name))
    }
    newheap.tempVariables = Set.empty

    (newheap, repl)
  }

  /**
   * Variable assignment
   *
   * @param variable the LHS of the the assignment
   * @param expr the RHS of the assignment
   */
  def assign[S <: SemanticDomain[S]](variable: Assignable, expr: Expression, state: S): (TVSHeap, Replacement) = {
    // only consider assignments which change the heap
    // we consider strings not as objects since they are immutable
    if (!variable.getType.isObject || variable.getType().getName().equals("String")) return (this, new Replacement)

    val tvp = new TVP(this)
    val assignAction = variable match {
    // x = ...
      case lhs: VariableIdentifier =>
        expr match {
        // x = y
          case rhs: VariableIdentifier =>
            new CopyVariable(lhs.toString, rhs.toString)

          // x = y.n or x = new Foo
          case rhs: TVSHeapIDSet =>
            new CopyVariable(lhs.toString, rhs.pointedBy)

          // x = null
          case rhs: Constant =>
            if (rhs.constant == "null") {
              new SetVariableNull(lhs.toString)
            } else {
              throw new Exception("Assignment: Trying to assign non-null constant")
            }
          case _ => throw new Exception("Assignment: Invalid rhs")
        }
      case _ => throw new Exception("assign: lhs must be variable")
    }

    tvp.addAction(assignAction)
    val (newheap, repl) = tvp.execute()

    (newheap, repl)
  }


  /**
   * For convenience, parameters for a few common data structures could be initialized here.
   * The structure is decided by looking at the type and comparing it with the hard-coded types here.
   * For now, we just support an acyclic, singly-linked list "AcyclicList"
   */
  def createVariableForParameter(variable: Assignable, typ: Type, path: List[String]): (TVSHeap, Map[Identifier, List[String]], Replacement) = {
    variable match {
      case v: TVSHeapIDSet => (this, Map(), new Replacement)
      case v: VariableIdentifier =>
        if (typ.getName == "AcyclicList")
          createAcyclicListParameter(v)
        else
          (createVariable(v, typ)._1, Map(), new Replacement)
    }
  }

  /**
   * Initialization for a parameter of type AcyclicList.
   * We assemble all predicates manually...
   */
  private def createAcyclicListParameter(v: VariableIdentifier): (TVSHeap, Map[Identifier, List[String]], Replacement) = {
    val nextField = "n"
    val encodedField = "field_" + nextField
    val pp = v.getProgramPoint()
    var newStructs: Set[TVS[NodeName]] = Set.empty
    var idPath: Map[Identifier, List[String]] = Map.empty
    for (tvs <- structures) {
      var newtvs = tvs.copy
      // create two nodes
      val n1 = PPHeapID(Set((pp, 0)), 0)
      val n2 = PPHeapID(Set((pp, 1)), 0)
      newtvs.nodes = n1 :: n2 :: tvs.nodes
      idPath += n1 -> Nil
      idPath += n2 -> Nil
      // make second one summarized
      newtvs.summarization = tvs.summarization + (n2 -> Kleene.Unknown)
      // create program variable and let it point to first node
      newtvs.programVariables += v.name -> new ProgramVariablePredicate(v.name, Some(n1))
      // set fields
      if (!fields.contains(nextField))
        newtvs = newtvs.addField(encodedField)
      var f = newtvs.fields(encodedField)
      f = f.setNeighbour(n1, n2, Kleene.Unknown)
      f = f.setNeighbour(n2, n2, Kleene.Unknown)
      newtvs.fields += encodedField -> f
      // reachability
      val t = newtvs.transitiveReachability("t[" + encodedField + "]")
      val tv = List((n1, n1) -> Kleene.True, (n1, n2) -> Kleene.True, (n2, n2) -> Kleene.Unknown)
      newtvs.transitiveReachability += t.n -> new BinaryPredicate(t.n, t.values ++ tv)
      val rn = "r[" + encodedField + "," + v.name + "]"
      val rv = List(n1 -> Kleene.True, n2 -> Kleene.True)
      newtvs.variableReachability += rn -> new UnaryPredicate(rn, rv.toMap)
      newStructs += newtvs
    }
    val newheap = new TVSHeap(this)
    newheap.variables += v
    newheap.fields += nextField
    newheap.structures = newStructs
    newheap.ppCreates += pp -> 2
    (newheap, idPath, new Replacement)
  }


  /**
   * Create a new variable
   */
  def createVariable(variable: Assignable, typ: Type): (TVSHeap, Replacement) = {
    variable match {
      case v: TVSHeapIDSet => (this, new Replacement)
      case v: VariableIdentifier =>
        if (v.typ.isObject) {
          // we don't care about numerical variables
          val newheap = new TVSHeap(this)
          newheap.structures = this.structures.map(_.addVariable(v.name))
          newheap.variables += v
          return (newheap, new Replacement)
        } else (this, new Replacement)

    }
  }

  /**
   * Remove an existing variable
   */
  def removeVariable(variable: Assignable): (TVSHeap, Replacement) = {
    variable match {
      case v: TVSHeapIDSet => (this, new Replacement)
      case v: VariableIdentifier =>
        val newheap = new TVSHeap(this)
        newheap.structures = this.structures.map(_.removeVariable(v.name))
        newheap.variables -= v
        (newheap, new Replacement)
    }
  }

  def factory() = new TVSHeap

  /**
   * Create a bottom "_|_" TVSHeap
   */
  def bottom(): TVSHeap = {
    val result = new TVSHeap
    result.isBottom = true
    result
  }

  /**
   * Create a top "T" TVSHeap
   */
  def top(): TVSHeap = {
    val result = new TVSHeap
    result.isTop = true
    result
  }

  /**
   * Some support for a <= relation on heap states
   */
  def lessEqual(r: TVSHeap): Boolean = {
    if (this.isBottom) return true;
    if (r.isTop) return true;

    this.equals(r)
  }

  /**
   * Least upper bound of heap states left and right. Produces a Replacement to be used
   * by the semantic domain.
   */
  def lubWithReplacement(left: TVSHeap, right: TVSHeap): (TVSHeap, Replacement) = {
    if (left.isTop || right.isTop) return (top(), new Replacement)
    if (left.isBottom) return (right, new Replacement)
    if (right.isBottom) return (left, new Replacement)
    if (left == right) return (left, new Replacement)

    val tempheap = new TVSHeap
    tempheap.variables = left.variables union right.variables
    tempheap.tempVariables = left.tempVariables union right.tempVariables
    tempheap.structures = left.structures union right.structures
    tempheap.fields = left.fields union right.fields
    tempheap.ppCreates = MapUtil.mergeMaps(left.ppCreates, right.ppCreates)(math.max)

    val tvp = new TVP(tempheap)
    tvp.addAction(new Lub())
    val (newheap, repl) = tvp.execute()

    (newheap, repl)
  }

  /**
   * Some support for a greatest lower bound. Only the obvious cases are handled.
   * There seems to be no support in TVLA for this notion.
   */
  def glbWithReplacement(left: TVSHeap, right: TVSHeap): (TVSHeap, Replacement) = {
    if (left.isBottom || right.isBottom)  (bottom(), new Replacement)
    else if (left.isTop)  (right, new Replacement)
    else if (right.isTop)  (left, new Replacement)
    else if (left == right)  (left, new Replacement())
    else throw new Exception("Cannot compute glb on heaps")
  }


  /**
   * Assume allows assumptions to be made in branches of the control flow.
   * It calls TVLA to eliminate structures that are invalid under the assumption.
   */
  def assume(expr: Expression) = {

    val action: Option[TVPAction] =
      expr match {
        // ** null comparison ** //
        // !(x == null)
        case NegatedBooleanExpression(BinaryArithmeticExpression(v: VariableIdentifier, Constant("null", _, _), ArithmeticOperator.==, typ)) =>
          Some(new AssumeVariableNotNull(v.toString))
        // !(x != null)
        case NegatedBooleanExpression(BinaryArithmeticExpression(v: VariableIdentifier, Constant("null", _, _), ArithmeticOperator.!=, typ)) =>
          Some(new AssumeVariableNull(v.toString))
        // x == null
        case BinaryArithmeticExpression(v: VariableIdentifier, Constant("null", _, _), ArithmeticOperator.==, typ) =>
          Some(new AssumeVariableNull(v.toString))
        // x != null
        case BinaryArithmeticExpression(v: VariableIdentifier, Constant("null", _, _), ArithmeticOperator.!=, typ) =>
          Some(new AssumeVariableNotNull(v.toString))

        // ** reference comparison ** //
        // x == y
        case ReferenceComparisonExpression(v1: VariableIdentifier, v2: VariableIdentifier, ArithmeticOperator.==, typ) =>
          Some(new AssumeVariableEqual(v1.toString, v2.toString))
        // x != y
        case ReferenceComparisonExpression(v1: VariableIdentifier, v2: VariableIdentifier, ArithmeticOperator.!=, typ) =>
          Some(new AssumeVariableNotEqual(v1.toString, v2.toString))
        // !(x == y)
        case NegatedBooleanExpression(ReferenceComparisonExpression(v1: VariableIdentifier, v2: VariableIdentifier, ArithmeticOperator.==, typ)) =>
          Some(new AssumeVariableNotEqual(v1.toString, v2.toString))
        // !(x != y)
        case NegatedBooleanExpression(ReferenceComparisonExpression(v1: VariableIdentifier, v2: VariableIdentifier, ArithmeticOperator.!=, typ)) =>
          Some(new AssumeVariableEqual(v1.toString, v2.toString))

        // TODO: ReferenceComparisonExpressions above are not created in AST - fix Sample. for now, use arithmetic expressions below
        // x == y
        case BinaryArithmeticExpression(v1: VariableIdentifier, v2: VariableIdentifier, ArithmeticOperator.==, typ) =>
          Some(new AssumeVariableEqual(v1.toString, v2.toString))
        // x != y
        case BinaryArithmeticExpression(v1: VariableIdentifier, v2: VariableIdentifier, ArithmeticOperator.!=, typ) =>
          Some(new AssumeVariableNotEqual(v1.toString, v2.toString))
        // !(x == y)
        case NegatedBooleanExpression(BinaryArithmeticExpression(v1: VariableIdentifier, v2: VariableIdentifier, ArithmeticOperator.==, typ)) =>
          Some(new AssumeVariableNotEqual(v1.toString, v2.toString))
        // !(x != y)
        case NegatedBooleanExpression(BinaryArithmeticExpression(v1: VariableIdentifier, v2: VariableIdentifier, ArithmeticOperator.!=, typ)) =>
          Some(new AssumeVariableEqual(v1.toString, v2.toString))

        case _ =>
          None
      }

    // only execute something if any of the cases above hold
    if (action.isDefined) {
      val tvp = new TVP(this)
      tvp.addAction(action.get)
      val result = tvp.execute()
      result
    } else {
      (this, new Replacement)
    }
  }

  def getIds(): Set[Identifier] = null


  // not implemented
  def backwardAssign(variable: Assignable, expr: Expression) = throw new NotImplementedException("not implemented yet")
  def backwardAccess(field: Assignable) = throw new NotImplementedException("not implemented yet")
  def setParameter(variable: Assignable, expr: Expression) = throw new NotImplementedException("not implemented yet")
  def assignArrayCell[S <: SemanticDomain[S]](obj: Assignable, index: Expression, expr: Expression, state: S) = null
  def wideningWithReplacement(left: TVSHeap, right: TVSHeap): (TVSHeap, Replacement) = (left, new Replacement)
  def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier: Assignable, index: Expression, state: S,
        typ: Type): (DefiniteHeapIdSetDomain[NodeName], TVSHeap, Replacement) = throw new NotImplementedException("not implemented yet")

  // methods required by Analysis trait
  override def reset() {}
  override def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil
  override def setParameter(label: String, value: Any) {}
  override def parameters(): List[(String, Any)] = Nil
  def setToTop(variable: Assignable) = (this, new Replacement)
  override def getProperties(): Set[Property] = Set.empty
  override def getLabel(): String = "TVS heap domain"
}



/**
 * TVSHeapIDSet contains a set of definite heap locations.
 * Additionally, it carries the name of a temporary that may be used later to access the
 * nodes in the TVS again.
 */
class TVSHeapIDSet(val pointedBy: String) extends DefiniteHeapIdSetDomain[NodeName]

/**
 * Used to make a distinction between temporaries and normal variables
 */
class TemporaryVariableIdentifier(name: String, typ1: Type, pp: ProgramPoint) extends VariableIdentifier(name, typ1, pp)

class NotImplementedException(message: String) extends Exception(message)