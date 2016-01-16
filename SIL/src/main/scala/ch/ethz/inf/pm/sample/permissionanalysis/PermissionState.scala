package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{DoubleInterval, BoxedNonRelationalNumericalDomain, Apron}
import ch.ethz.inf.pm.sample.execution.{AbstractCFGState, SimpleAnalysis, AnalysisResult, EntryStateBuilder}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{DefaultSilConverter, SilAnalysisRunner}
import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

/** Path to a location.
  *
  * A path is a sequence starting with a variable and followed by field identifiers (e.g., x.f).
  *
  * @param p the list of strings forming the path
  * @author Caterina Urban
  */
class Path(val p : List[String]) {
  override def toString : String = this.p.mkString(".")
  override def equals(a: Any) : Boolean = a match {
    case x: Path => this.p.corresponds(x.p)(_ equals _)
    case _ => false
  }
}

/** Symbolic value.
  *
  * We introduce a symbolic value for each location
  * and each possible occurrence of an access permission in a pre- or post-condition.
  *
  * @param path the path for which we specify the access permission
  * @author Caterina Urban
  */
sealed abstract class SymbolicValue(var path : Path) {
  def setPath(p : Path) : SymbolicValue = { path=p; this }
//    if (path == null) {
//      path=p; this
//    } else throw new RuntimeException("The path of the symbolic value is already initialized.")
  def factory() : SymbolicValue
}

/** Symbolic precondition value. */
case class SymbolicPrecondition(p: Path) extends SymbolicValue(p) {
  override def toString : String = "acc(" + path.toString() + ")"
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicPrecondition => path.equals(x.path)
    case _ => false
  }
  override def factory() : SymbolicValue = new SymbolicPrecondition(p)
}

/** Counted symbolic value.
  *
  * It is either a monomial `n * s` or a constant `n` (with `null` symbolic value).
  * Monomials are used to refer within a program to occurrences of access permissions in pre- or post-conditions.
  * Constants are used to express the access permissions available within a program.
  *
  * @param n the number of times the symbolic value is taken into account
  * @param s symbolic value taken into account
  * @author Caterina Urban
  */
case class CountedSymbolicValue(n : Double, s : SymbolicValue) {

  /** Custom constructor(s). */
  def this(n: Double) = this(n, null)

  override def equals(a : Any) : Boolean = a match {
    case x: CountedSymbolicValue => (this.n equals x.n) && this.sameSymbolicValue(x)
    case _ => false
  }
  def sameSymbolicValue(a : CountedSymbolicValue) : Boolean = {
    if (this.s == null && a.s == null) return true
    if (this.s == null || a.s == null) return false
    this.s.equals(a.s)
  }

  /** Least upper bound of counted symbolic values. */
  def lub(b : CountedSymbolicValue) = {
    assert(this.sameSymbolicValue(b))
    CountedSymbolicValue(Math.min(this.n, b.n), this.s)
  }
  /** Greatest lower bound of counted symbolic values. */
  def glb(b : CountedSymbolicValue) = {
    assert(this.sameSymbolicValue(b))
    CountedSymbolicValue(Math.max(this.n, b.n), this.s)
  }

  /** Addition of counted symbolic values. */
  def +(b : CountedSymbolicValue) = {
    assert(this.sameSymbolicValue(b))
    CountedSymbolicValue(this.n+b.n, this.s)
  }
  /** Subtraction of counted symbolic values. */
  def -(b : CountedSymbolicValue) = {
    assert(this.sameSymbolicValue(b))
    CountedSymbolicValue(this.n-b.n, this.s)
  }

  override def toString = s match {
    case null => this.n.toString
    case k => this.n.toString + "*" + this.s.toString
  }
}

/** Symbolic permission.
  *
  * @author Caterina Urban
  */
class SymbolicPermission extends Lattice[SymbolicPermission] {

  /** Custom constructor(s). */
  def this(v: CountedSymbolicValue) = {
    this(); this.value = if (v.n != 0) this.value + v else this.value
  }
  def this(s: Set[CountedSymbolicValue]) = {
    this(); this.value = s.filter((el) => el.n != 0)
  }

  /** Set of counted symbolic values. */
  var value : Set[CountedSymbolicValue] = Set[CountedSymbolicValue]()

  private def addCountedSymbolicValue(s: Set[CountedSymbolicValue], v: CountedSymbolicValue): Set[CountedSymbolicValue] = {
    var added = false
    val sym: Set[CountedSymbolicValue] = s.map(
      // adding the counted symbolic value with an existing element with the same symbolic value
      (el) => if (el.sameSymbolicValue(v)) { added = true; el + v } else el
    )
    // adding the counted symbolic value in case there is no element with the same symbolic value
    if (!added && v.n != 0) sym + v else sym
  }
  /** Adds a counted symbolic value to the current set of counted symbolic values. */
  def +(v: CountedSymbolicValue): SymbolicPermission = {
    val sym: Set[CountedSymbolicValue] = addCountedSymbolicValue(this.value,v)
    new SymbolicPermission(sym)
  }
  /** Adds a symbolic permission with the current symbolic permission. */
  def ++(v: SymbolicPermission): SymbolicPermission = {
    val sym: Set[CountedSymbolicValue] = v.value.foldLeft(this.value)(addCountedSymbolicValue)
    new SymbolicPermission(sym)
  }

  private def subCountedSymbolicValue(s: Set[CountedSymbolicValue], v: CountedSymbolicValue): Set[CountedSymbolicValue] = {
    var subtracted = false
    val sym: Set[CountedSymbolicValue] = s.map(
      // subtracting the counted symbolic value from an existing element with the same symbolic value
      (s) => if (s.sameSymbolicValue(v)) { subtracted = true; s - v } else s
    )
    // adding the negated counted symbolic value in case there is no element with the same symbolic value
    if (!subtracted && v.n != 0) sym + CountedSymbolicValue(-v.n,v.s) else sym
    // if (!subtracted) throw new IllegalArgumentException("Trying to give away without owning a permission!") else sym
  }
  /** Subtracts a counted symbolic value from the current set of counted symbolic values. */
  def -(v : CountedSymbolicValue) : SymbolicPermission= {
    val sym: Set[CountedSymbolicValue] = subCountedSymbolicValue(this.value,v)
    new SymbolicPermission(sym)
  }
  /** Subtracts a symbolic permission from the current symbolic permission. */
  def --(v: SymbolicPermission): SymbolicPermission = {
    val sym: Set[CountedSymbolicValue] = v.value.foldLeft(this.value)(subCountedSymbolicValue)
    new SymbolicPermission(sym)
  }

  /** Returns the bottom value of the lattice. */
  override def bottom(): SymbolicPermission = ???
  /** Returns a new instance of the lattice. */
  override def factory(): SymbolicPermission = ???
  /** Returns the top value of the lattice. */
  override def top(): SymbolicPermission = ???

  /** Checks whether the given domain element is equivalent to bottom. */
  override def isBottom: Boolean = false
  /** Returns true if and only if `this` is less than or equal to `other`. */
  override def lessEqual(other: SymbolicPermission): Boolean = ???
  /** Checks whether the given domain element is equivalent to top. */
  override def isTop: Boolean = ???

  /** Computes the greatest lower bound of two elements. */
  override def glb(other: SymbolicPermission): SymbolicPermission = ???
  /** Computes the least upper bound of two elements. */
  override def lub(other: SymbolicPermission): SymbolicPermission = {
    val sym = this.value union other.value.map(
      (s) => this.value.foldLeft(s)(
        (el, v) => if (s sameSymbolicValue v) el lub v else el
      )
    )
    new SymbolicPermission(sym)
  }
  /** Computes the widening of two elements. */
  override def widening(other: SymbolicPermission): SymbolicPermission = this lub other

  override def toString : String = if (isBottom) "⊥" else if (value.isEmpty) "0" else value.mkString("", " + ", "")
}

/** Permission Inference State.
  *
  * Note that each abstract state must include an `ExpressionSet`!
  * It is accessed during the analysis to retrieve the result of each statement!!
  *
  * @author Caterina Urban
  */
case class PermissionState(heapNum: PointsToNumericalState,
                           // map from a `Identifier` to its `SymbolicPermission`
                           idToSym: Map[Identifier,SymbolicPermission])
  extends SimplePermissionState[PermissionState]
  with StateWithBackwardAnalysisStubs[PermissionState]
  with LazyLogging
{
  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): PermissionState = {
    logger.debug("*** assignField(" + obj.toString + "; " + field.toString + "; " + right.toString + ")")

    obj match {
      case obj: FieldIdentifier =>
        val sym = idToSym(obj) // retrieve the associated symbolic permission
        // create constraint to ensure write permissions
        val c = PermissionSolver.permissionType.ensureWrite(PermissionSolver.convertSymbolicPermission(sym))
        // add constraint to solver
        PermissionSolver.addConstraint(c)
        // ensure read permissions for all field identifiers in the right-hand side
        for (id <- right.ids.getNonTop) {
          id match {
            case id: FieldIdentifier =>
              val s = idToSym(id) // retrieve the associated symbolic permission
              // create constraint to ensure read permissions
              val c = PermissionSolver.permissionType.ensureRead(PermissionSolver.convertSymbolicPermission(s))
              // add constraint to solver
              PermissionSolver.addConstraint(c)
            case _ => // nothing to be done
          }
        }
        // return the current state with updated heapNum
        this.copy(heapNum = heapNum.assignField(obj, field, right))
      case _ => throw new IllegalArgumentException("A field assignment must occur via a FieldIdentifier.")
    }
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): PermissionState = {
    logger.debug("*** assignVariable(" + x.toString + "; " + right.toString + ")")

    // ensure read permissions for all field identifiers in the right-hand side
    for (id <- right.ids.getNonTop) {
      id match {
        case id: FieldIdentifier =>
          val s = idToSym(id) // retrieve the associated symbolic permission
          // create constraint to ensure read permissions
          val c = PermissionSolver.permissionType.ensureRead(PermissionSolver.convertSymbolicPermission(s))
          // add constraint to solver
          PermissionSolver.addConstraint(c)
        case _ => // nothing to be done
      }
    }
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.assignVariable(x, right))
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): PermissionState = {
    logger.debug("*** assume(" + cond.toString + ")")

    // ensure read permissions for all field identifiers in the condition
    for (id <- cond.ids.getNonTop) {
      id match {
        case id: FieldIdentifier =>
          val s = idToSym(id) // retrieve the associated symbolic permission
          // create constraint to ensure read permissions
          val c = PermissionSolver.permissionType.ensureRead(PermissionSolver.convertSymbolicPermission(s))
          // add constraint to solver
          PermissionSolver.addConstraint(c)
        case _ => // nothing to be done
      }
    }
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.assume(cond))
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): PermissionState = {
    logger.debug("*** before(" + pp.toString + ")")

    this  // return the current state without modification
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): PermissionState = {
    logger.debug("*** bottom()")

//    // return a new state with bottom exprSet and empty refToObj map
//    PermissionState(exprSet.bottom(),refToObj.empty,objFieldToObj.empty,numDom.bottom())

    this.copy(heapNum = heapNum.bottom())
  }

  /** Creates an object at allocation site.
    *
    * Invoked by calls to `new()`.
    *
    * @param typ The dynamic type of the created object
    * @param pp The allocation site of the object
    * @return The abstract state with updated exprSet after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** createObject(" + typ.toString + "; " + pp.toString + ")")
    
//    val obj = HeapIdentifier(typ,pp) // create new Obj
//    val objFieldToObjmap = objFieldToObj + (obj -> Map[String,Set[HeapIdentifier]]()) // add key to objFieldToObj map
//    // return the current state with updated exprSet and updated objFieldToObj map
//    this.copy(exprSet = ExpressionSet(obj), objFieldToObj = objFieldToObjmap)

    this.copy(heapNum = heapNum.createObject(typ, pp))
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    * Invoked by variable declarations (`var x : Ref`, ...)
    *
    * @param x The name of the variable
    * @param typ The static type of the variable
    * @param pp The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** createVariable(" + x.toString + "; " + typ.toString + "; " + pp.toString + ")")

    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.createVariable(x, typ, pp))
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): PermissionState = {
    logger.debug("*** createVariableForArgument(" + x.toString + "; " + typ.toString + ")")

    if (typ.isObject) { // the variable to be created is a `Ref`

      // method Foo(x: Ref, ...) { ... }
      //
      // heap.refToObj + (x -> o)
      // idToSym + (o -> acc(x))

      val obj = HeapIdentifier(typ,x.pp) // create new Obj
      // create new SymbolicPermission (with SymbolicPrecondition as CountedSymbolicValue)
      val pre = new SymbolicPrecondition(new Path(List(x.getName)))
      val sym = new SymbolicPermission(new CountedSymbolicValue(1,pre))
      // add key to idToSym map
      val idToSymmap = idToSym + (obj -> sym)
      // return the current state with updated heapNum and updated idToSym
      this.copy(heapNum = heapNum.createVariableForArgument(x, typ), idToSym = idToSymmap)
    } else { // the variable to be created is not a `Ref`
      // return the current state with updated heapNum
      this.copy(heapNum = heapNum.createVariableForArgument(x, typ))
    }
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ The type of the numerical constant
    * @param pp The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** evalConstant(" + value + "; " + typ.toString + "; " + pp.toString + ")")

    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.evalConstant(value, typ, pp))
  }

  /** Exhales permissions. */
  override def exhale(acc: Expression) : PermissionState = {
    logger.debug("*** exhale(" + acc.toString + "): implement me!")

    acc match {
      case acc: PermissionExpression => //TODO: handle permission levels different than the full permission level
        acc.id match {
          case id: FieldIdentifier =>
            // retrieve the symbolic permission associated with the permission expression
            val sym: SymbolicPermission = idToSym(id)
            // create a counted symbolic value to add to the symbolic permission
            val v: CountedSymbolicValue = new CountedSymbolicValue(acc.p.toString.toDouble)
            // update key in idToSym map
            val idToSymmap = idToSym + (id -> (sym - v))

            // create constraint to ensure proper exhale
            // val c = new Geq(PermissionSolver.convertSymbolicPermission(sym), PermissionSolver.convertCountedSymbolicValue(v))
            // add constraint to solver
            // PermissionSolver.addConstraint(c)

            // return the current state with updated idToSym
            this.copy(idToSym = idToSymmap)
          case _ => throw new IllegalArgumentException("A permission exhale must occur via a FieldIdentifier")
        }
      case _ =>
        val asserted: PermissionState = this.setExpression(ExpressionSet(acc))
        assert(asserted.testFalse() lessEqual this.bottom())
        this.assume(acc)
    }
  }

  /** The current expression.
    *
    * Invoked after each statement to retrieve its result.
    */
  override def expr: ExpressionSet = {
    logger.debug("*** expr: " + this.heapNum.exprSet.toString)
    
    this.heapNum.exprSet // return exprSet
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    *
    * @todo implement me!
    */
  override def factory(): PermissionState = {
    logger.debug("*** factory(): implement me!")
    
    this.copy(heapNum = heapNum.factory())
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object from which the field access is performed
    * @param field the name of the field to be accessed
    * @param typ the type of the field to be accessed
    * @return The abstract state obtained after the field access, that is, a new state whose `ExpressionSet`
    *         holds the objects referenced by the access path (up to the given field excluded).
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): PermissionState = {
    logger.debug("*** getFieldValue(" + obj.toString + "; " + field + "; " + typ.toString + ")")

    obj match {
      case obj:AccessPathIdentifier =>
        // call getFieldValue on heapNum
        val heap = heapNum.getFieldValue(obj, field, typ)

        // x.f
        // heap.refToObj: x -> {o1, ...}, ...
        // idToSym: o1 -> acc(x), ...
        //
        // idToSym: o1 -> acc(x), o1.f -> acc(x.f), ...

        val path = obj.stringPath // path to evaluate
        // evaluate path into the set of objects referenced by it (up to the given field excluded)
        val objSet = heapNum.evaluatePath(path,heapNum.objFieldToObj)

        // update idToSym
        val idToSymmap = objSet.foldLeft(idToSym)(
          (m: Map[Identifier,SymbolicPermission],o : HeapIdentifier) => {
            val fld = FieldIdentifier(o,field,typ) // create new FieldIdentifier
            // retrieve the associated symbolic permission
            val sym = getSymbolicPermission(fld)
            // add key to idToSym map
            m + (fld -> sym)
          }
        )

        // return the current state with updated heapNum and updated idToSym
        this.copy(heapNum = heap, idToSym = idToSymmap)

      case _ => throw new IllegalArgumentException("A field access must occur via an AccessPathIdentifier")
    }
  }

  /** Retrieves the symbolic permission associated with a field identifier, or creates a new symbolic permission
    * from the symbolic permission associated with the corresponding heap identifier.
    *
    * @param id the field identifier used for the retrieval
    * @return the symbolic permission associated with the field identifier
    */
  private def getSymbolicPermission(id: FieldIdentifier) : SymbolicPermission = idToSym.get(id) match {
    case Some(s) => s
    case None =>
      val obj = id.obj // retrieve the heap identifier
      val perm = idToSym(obj) // retrieve the associated symbolic permission
      // update the path of the counted symbolic values
      val csv = perm.value.map(
        (s) => CountedSymbolicValue(s.n, s.s.factory().setPath(new Path(s.s.path.p ::: List[String](id.field))))
      )
      // create and return new SymbolicPermission
      new SymbolicPermission(csv)
  }

  /** Gets the value of a variable.
    *
    * Invoked by variable declarations (`var x : Ref`) and variable assignments (`x = ...`, `... = x`)
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): PermissionState = {
    logger.debug("*** getVariableValue(" + id.toString + ")")

    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.getVariableValue(id))
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    *
    * @todo implement me!
    */
  override def glb(other: PermissionState): PermissionState = {
    logger.debug("*** glb(" + other.repr + "): implement me!")
    
    this.copy(heapNum = heapNum glb other.heapNum)
  }

  /** Inhales permissions. */
  override def inhale(acc: Expression) : PermissionState = {
    logger.debug("*** inahle(" + acc.toString + ")")

    acc match {
      case acc: PermissionExpression => //TODO: handle permission levels different than the full permission level
        acc.id match {
          case id: FieldIdentifier =>
            // retrieve the symbolic permission associated with the permission expression
            val sym: SymbolicPermission = idToSym(id)
            // create a counted symbolic value to add to the symbolic permission
            val v: CountedSymbolicValue = new CountedSymbolicValue(acc.p.toString.toDouble)
            // update key in idToSym map
            val idToSymmap = idToSym + (id -> (sym + v))

            // create constraint to ensure proper inhale
            // val m = new CountedSymbolicValue(PermissionSolver.permissionType.maxLevel)
            // val c = new Geq(PermissionSolver.convertCountedSymbolicValue(m), PermissionSolver.convertSymbolicPermission(sym + v))
            // add constraint to solver
            // PermissionSolver.addConstraint(c)

            // return the current state with updated idToSym
            this.copy(idToSym = idToSymmap)
          case _ => throw new IllegalArgumentException("A permission inhale must occur via a FieldIdentifier")
        }
      case _ => this.assume(acc)
    }
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return `true` if and only if the state is equivalent to bottom
    *
    * @todo implement me!
    */
  override def isBottom: Boolean = {
    logger.debug("*** isBottom: implement me!")

    heapNum.isBottom
  }

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return `true` if and only if the state is equivalent to top
    *
    * @todo implement me!
    */
  override def isTop: Boolean = {
    logger.debug("*** isTop: implement me!")
    
    heapNum.isTop
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return `true` if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: PermissionState): Boolean = {
    logger.debug("*** lessEqual(" + other.repr + ")")

//    val exp = this.exprSet.lessEqual(other.exprSet) // test the exprSets
//    val refToObjmap = this.refToObj.forall {
//      case (k: VariableIdentifier,v: Set[HeapIdentifier]) => v subsetOf other.refToObj.getOrElse(k,Set[HeapIdentifier]())
//    } // test the refToObjs
//    val objFieldToObjmap = this.objFieldToObj.forall {
//      case (o: HeapIdentifier, m: Map[String,Set[HeapIdentifier]]) => m.forall {
//        case (f: String, s: Set[HeapIdentifier]) =>
//          s subsetOf other.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(f,Set[HeapIdentifier]())
//      }
//    } // test the objFieldToObjs
//    val num = this.numDom.lessEqual(other.numDom) // test the numDoms
//    exp && refToObjmap && objFieldToObjmap && num

    heapNum lessEqual other.heapNum
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: PermissionState): PermissionState = {
    logger.debug("*** lub(" + other.repr + ")")

//    val exp = this.exprSet lub other.expr // join the exprSets
//    val refToObjmap = this.refToObj ++ other.refToObj.map {
//      case (k: VariableIdentifier,v: Set[HeapIdentifier]) => k -> (this.refToObj.getOrElse(k,Set[HeapIdentifier]()) ++ v)
//    } // merge the refToObjs
//    val objFieldToObjmap = this.objFieldToObj ++ other.objFieldToObj.map {
//      case (o: HeapIdentifier,m: Map[String,Set[HeapIdentifier]]) => o ->
//        (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()) ++ other.objFieldToObj(o).map {
//          case (s: String, v: Set[HeapIdentifier]) => s ->
//            (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(s,Set[HeapIdentifier]()) ++ v)
//        })
//    } // merge the objFieldToObjmap
//    val num = this.numDom lub other.numDom // join the numDoms
//
//    // return the current state with updated exprSet, updated refToObj, updated objFieldToObjmap and updated numDom
//    this.copy(exprSet = exp, refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)

    this.copy(heapNum = heapNum lub other.heapNum)
  }

  /** Performs abstract garbage collection.
    *
    * @todo implement me!
    */
  override def pruneUnreachableHeap(): PermissionState = {
    logger.debug("*** pruneUnreachableHeap(): implement me!")

    this.copy(heapNum = heapNum.pruneUnreachableHeap())
  }

  /** Removes all variables satisfying filter.
    *
    * @todo implement me!
    */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): PermissionState = {
    logger.debug("*** pruneVariables(" + filter.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.pruneVariables(filter))
  }

  /** Removes the current expression.
    *
    * @return The abstract state obtained after removing the current expression
    */
  override def removeExpression(): PermissionState = {
    logger.debug("*** removeExpression()")

    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.removeExpression())
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    *
    * @todo implement me!
    */
  override def removeVariable(varExpr: VariableIdentifier): PermissionState = {
    logger.debug("*** removeVariable(" + varExpr.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.removeVariable(varExpr))
  }

  /** The default state string representation.
    *
    * @return the default string representation of the current state
    */
  private def repr: String = {
    "PermissionState(" +
      "PointsToNumericalState(" +
      heapNum.exprSet.toString + ", " +
      heapNum.refToObj.toString + ", " +
      heapNum.objFieldToObj.toString + ", " +
      heapNum.numDom.toString + ")" +
    ")"
  }

  /** Assigns an expression to an argument.
    *
    * @param x The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    *
    * @todo implement me!
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): PermissionState = {
    logger.debug("*** setArgument(" + x.toString + "; " + right.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.setArgument(x, right))
  }

  /** Sets the current expression.
    *
    * Invoked after statements that do not have results (like assignments to variables and fields).
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): PermissionState = {
    logger.debug("*** setExpression(" + expr.toString + ")")

    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.setExpression(expr))
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    *
    * @todo implement me!
    */
  override def setVariableToTop(varExpr: Expression): PermissionState = {
    logger.debug("*** setVariableToTop(" + varExpr.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.setVariableToTop(varExpr))
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    *
    * @todo implement me!
    */
  override def throws(t: ExpressionSet): PermissionState = {
    logger.debug("*** throws(" + t.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.throws(t))
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    *
    * @todo implement me!
    */
  override def top(): PermissionState = {
    logger.debug("*** top(): implement me!")
    
    this.copy(heapNum = heapNum.top())
  }

  /** The state string representation.
    *
    * @return the string representation of the current state
    */
  override def toString: String =
    "PermissionState(\n" +
    "\theapNum: " + heapNum.repr + "\n" +
    "\tidToSym: " + idToSym.toString + "\n" +
    ")"

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    *
    * @todo implement me!
    */
  override def widening(other: PermissionState): PermissionState = {
    logger.debug("*** widening(" + other.repr + ")")

    //    val exp = this.exprSet widening other.expr // widen the exprSets
    //    val refToObjmap = this.refToObj ++ other.refToObj.map {
    //        case (k: VariableIdentifier,v: Set[HeapIdentifier]) => k -> (this.refToObj.getOrElse(k,Set[HeapIdentifier]()) ++ v)
    //      } // merge the refToObjs
    //    val objFieldToObjmap = this.objFieldToObj ++ other.objFieldToObj.map {
    //        case (o: HeapIdentifier,m: Map[String,Set[HeapIdentifier]]) => o ->
    //          (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()) ++ other.objFieldToObj(o).map {
    //            case (s: String, v: Set[HeapIdentifier]) => s ->
    //              (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(s,Set[HeapIdentifier]()) ++ v)
    //          })
    //      } // merge the objFieldToObjmap
    //    val num = this.numDom widening other.numDom // widen the numDoms
    //
    //    // return the current state with updated exprSet, updated refToObj, updated objFieldToObjmap and updated numDom
    //    this.copy(exprSet = exp, refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)
    //  }

    this.copy(heapNum = heapNum widening other.heapNum)
  }
}

/** Builds permission analysis entry states for given method declarations. */
object PermissionEntryStateBuilder extends EntryStateBuilder[PermissionState] {
  override def topState: PermissionState = PermissionState(
    PointsToNumericalState(ExpressionSet(),
    Map[VariableIdentifier,Set[HeapIdentifier]](),
    Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]](),
    new BoxedNonRelationalNumericalDomain[DoubleInterval](DoubleInterval.Top)), //TODO: Apron.Polyhedra.Bottom.factory()
    // map from a `Identifier` to its `SymbolicPermission`
    Map[Identifier,SymbolicPermission]())
}

class PermissionAnalysis extends SimpleAnalysis[PermissionState](PermissionEntryStateBuilder) {

  override def analyze(method: MethodDeclaration): AnalysisResult[PermissionState] = {
    val result = analyze(method, entryStateBuilder.build(method))

    val solution = PermissionSolver.solve(PermissionSolver.getConstraints)
    println("\nResult: ")
    for ((s,v) <- solution) {
      if (v >= 1)
        println("acc(" + s.path.toString + ")")
      else if (v > 0)
        println("acc(" + s.path.toString + ", " + PermissionSolver.doubleToRational(v) + ")")
    }

    //val cfg = result.cfgState

    //if (cfg.cfg.initialBlockInLoop(0)) println("0 is loop")
    //if (cfg.cfg.initialBlockInLoop(1)) println("1 is loop")
    //if (cfg.cfg.initialBlockInLoop(2)) println("2 is loop")
    //if (cfg.cfg.initialBlockInLoop(3)) println("3 is loop")
    //if (cfg.cfg.initialBlockInLoop(4)) println("4 is loop")
    //println(cfg.blockStates)
    //println("states: " + cfg.blockStates.mapValues[PermissionState](
    //  (l) => l.last.head
    //))

    result
  }
}

/** Runs the Access Permission Inference analysis. */
object PermissionAnalysisRunner extends SilAnalysisRunner[PermissionState] {

  /** The analysis to be run. */
  val analysis = new PermissionAnalysis

  /** Extends a sil.Program with permissions inferred by the PermissionAnalysis. */
  def extendProgram(prog: sil.Program, results: List[AnalysisResult[PermissionState]]): sil.Program = {
    // map of method names to control flow graphs
    val methodNameToCfgState = results.map(result => result.method.name.toString -> result.cfgState).toMap
    // extending program methods
    val extMethods = prog.methods.map(
      method => methodNameToCfgState.get(method.name) match {
        case Some(cfgState) => extendMethod(prog, method, cfgState)
        case None => method
      }
    )
    // building the extended program
    prog.copy(methods = extMethods)(prog.pos, prog.info)
  }

  /** Extends a sil.Method with permissions inferred by the PermissionAnalysis. */
  def extendMethod(prog: sil.Program, method: sil.Method, cfgState: AbstractCFGState[PermissionState]): sil.Method = {

    var entryState = cfgState.entryState()
    var exitState = cfgState.exitState()

    // removing all return variables from the entry state, since we cannot refer to them in the precondition
    val returnVarIds = method.formalReturns.map(DefaultSilConverter.convert).map(_.variable.id)
    entryState = returnVarIds.foldLeft(entryState)(_.removeVariable(_))

    // removing all local variables from the exit state, since we cannot refer to them in the postcondition
    val localVarIds = method.locals.map(DefaultSilConverter.convert).map(_.variable.id)
    exitState = localVarIds.foldLeft(exitState)(_.removeVariable(_))

    method
  }

  /** Runs the permission analysis on each method within a program. */
  override protected def _run(): List[AnalysisResult[PermissionState]] = {
    prepareContext()
    val results = methodsToAnalyze.map(analysis.analyze)

    // extending program with inferred permission
    val extProgram = extendProgram(DefaultSilConverter.prog,results)
    println("\nExtended Program:\n" + extProgram)

    results
  }

  override def toString = "Access Permission Inference Analysis"
}
