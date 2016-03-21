package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron.Polyhedra
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{NumericalDomain, DoubleInterval, BoxedNonRelationalNumericalDomain, Apron}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{RefType, sample, SilAnalysisRunner, DefaultSilConverter}
import ch.ethz.inf.pm.sample.oorepresentation._
import com.typesafe.scalalogging.LazyLogging
import viper.silver.ast.SourcePosition
import viper.silver.{ast => sil}
import ch.ethz.inf.pm.sample.execution.{Analysis}

/** Path to a location.
  *
  * A path is a sequence starting with a variable and followed by field identifiers (e.g., x.f).
  * Note that, for efficiency the path is stored in reverse order.
  *
  * @param p the list of strings forming the path
  * @author Caterina Urban
  */
class Path(val p: List[String], val t: List[Type], val l: List[ProgramPoint]) {
  override def toString : String = this.p.reverse.mkString(".")
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
sealed abstract class SymbolicValue(var path: Path) {
  def setPath(p: Path): SymbolicValue = { path=p; this }
  def factory(): SymbolicValue
  override def toString : String = "acc(" + path.toString() + ")"
}

/** Symbolic precondition value. */
case class SymbolicPrecondition(p: Path) extends SymbolicValue(p) {
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicPrecondition => path.equals(x.path)
    case _ => false
  }
  override def factory() : SymbolicValue = SymbolicPrecondition(p)
}

/** Symbolic access permission value. */
case class SymbolicAccess(p: Path) extends SymbolicValue(p) {
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicAccess => path.equals(x.path)
    case _ => false
  }
  override def factory(): SymbolicValue = SymbolicAccess(p)
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
case class CountedSymbolicValue(n: Double, s: SymbolicValue) {

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

  def evaluate(result: Map[SymbolicValue, Double]): Double = n * result.getOrElse(s,0.0)

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

  def evaluate(result: Map[SymbolicValue, Double]): Double = value.foldLeft(0.0)(_ + _.evaluate(result))

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
  * @tparam N the numerical domain
  * @tparam T the pointsto+numerical state
  * @tparam S the permission state
  * @author Caterina Urban
  */
trait PermissionState[N <: NumericalDomain[N], T <: PointsToNumericalState[N,T], S <: PermissionState[N,T,S]]
  extends SimplePermissionState[S]
  with StateWithBackwardAnalysisStubs[S]
  with LazyLogging
{
  this: S =>

  // points-to+numerical state
  def heapNum: T
  // map from field identifiers to their associated set of symbolic permissions
  def idToSym: Map[FieldIdentifier, Set[SymbolicPermission]]
  // map from field identifiers to their currently associated set of symbolic permissions
  def idToPerm: Map[FieldIdentifier, Set[SymbolicPermission]]
  
  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): S = {
    logger.info("*** assignField(" + obj.toString + "; " + field.toString + "; " + right.toString + ")")

    obj match {
      case obj: FieldIdentifier =>
        //val idToSymmap = if (obj.obj.representsSingleVariable) { // strong update
        //  idToSym - obj // remove information about permissions of the field identifier
        //} else { idToSym }
        // add constraints to the solver
        for (sym <- idToPerm(obj)) { // for all currently associated symbolic permissions...
          // create constraint to ensure write permissions
          val c = PermissionSolver.permissionType.ensureWrite(PermissionSolver.convertSymbolicPermission(sym))
          // add constraint to solver
          PermissionSolver.addConstraint(c)
        }
        // ensure read permissions for all field identifiers in the right-hand side
        for (id <- right.ids.getNonTop) {
          id match {
            case id: FieldIdentifier =>
              for (s <- idToPerm(id)) { // for all currently associated symbolic permissions...
                // create constraint to ensure read permissions
                val c = PermissionSolver.permissionType.ensureRead(PermissionSolver.convertSymbolicPermission(s))
                // add constraint to solver
                PermissionSolver.addConstraint(c)
              }
            case _ => // nothing to be done
          }
        }
        // return the current state with updated heapNum //and updated idToSym
        this.copy(heapNum = heapNum.assignField(obj, field, right)) //idToSym = idToSymmap
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
  override def assignVariable(x: Expression, right: Expression): S = {
    logger.info("*** assignVariable(" + x.toString + "; " + right.toString + ")")

    // ensure read permissions for all field identifiers in the right-hand side
    for (id <- right.ids.getNonTop) {
      id match {
        case id: FieldIdentifier =>
          for (s <- idToPerm.getOrElse(id,Set[SymbolicPermission]())) { // for all currently associated symbolic permissions...
            // create constraint to ensure read permissions
            val c = PermissionSolver.permissionType.ensureRead(PermissionSolver.convertSymbolicPermission(s))
            // add constraint to solver
            PermissionSolver.addConstraint(c)
          }
        case _ => // nothing to be done
      }
    }
    // return the current state with updated heapNum and possibly updated idToSym
    this.copy(heapNum = heapNum.assignVariable(x, right))
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): S = {
    logger.info("*** assume(" + cond.toString + ")")

    // ensure read permissions for all field identifiers in the right-hand side
    for (id <- cond.ids.getNonTop) {
      id match {
        case id: FieldIdentifier =>
          for (s <- idToPerm(id)) { // for all currently associated symbolic permissions...
          // create constraint to ensure read permissions
          val c = PermissionSolver.permissionType.ensureRead(PermissionSolver.convertSymbolicPermission(s))
            // add constraint to solver
            PermissionSolver.addConstraint(c)
          }
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
  override def before(pp: ProgramPoint): S = {
    logger.info("\n*** before(" + pp.toString + "): " + this.repr)
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.before(pp))
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): S = {
    // logger.info("*** bottom()")

//    // return a new state with bottom exprSet and empty refToObj map
//    PermissionState(exprSet.bottom(),refToObj.empty,objFieldToObj.empty,numDom.bottom())

    this.copy(heapNum = heapNum.bottom())
  }

  def copy(heapNum: T = heapNum, idToSym: Map[FieldIdentifier,Set[SymbolicPermission]] = idToSym,
           idToPerm: Map[FieldIdentifier,Set[SymbolicPermission]] = idToPerm): S

  /** Creates an object at allocation site.
    *
    * Invoked by calls to `new()`.
    *
    * @param typ The dynamic type of the created object
    * @param pp The allocation site of the object
    * @return The abstract state with updated exprSet after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): S = {
    logger.info("*** createObject(" + typ.toString + "; " + pp.toString + ")")
    
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
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): S = {
    logger.info("*** createVariable(" + x.toString + "; " + typ.toString + "; " + pp.toString + ")")

    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.createVariable(x, typ, pp))
  }

  /** Creates a variable for a method argument.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): S = {
    logger.info("*** createVariableForArgument(" + x.toString + "; " + typ.toString + ")")

    if (typ.isObject) { // the variable to be created is a `Ref`
      val obj = HeapIdentifier(typ, x.pp) // create new heap object
      val sumObj = HeapIdentifier(typ, DummyProgramPoint).setSummary(true) // create new summary heap object
      var idToSymmap = idToSym // update idToSym map...
      for (f <- heapNum.fieldSet) { // for each field declared in the program...
        // create a field identifier for the heap object
        val fld = FieldIdentifier(obj,f._2,f._1)
        // create a field identifier for the summary heap object
        val sumFld = FieldIdentifier(sumObj,f._2,f._1)
        // create new symbolic access precondition for the path formed by the variable being created and the current field
        val pre = SymbolicPrecondition(new Path(List(f._2, x.name), List(f._1, x.typ), List(x.pp, x.pp)))
        val sym = new SymbolicPermission(CountedSymbolicValue(1,pre))
        // add keys to idToSym map
        idToSymmap = idToSymmap + (fld -> Set[SymbolicPermission](sym))
        // note that the field identifier corresponding to the summary heap object could already be in the idToSym map
        idToSymmap = idToSymmap + (sumFld -> (idToSymmap.getOrElse(sumFld,Set[SymbolicPermission]()) + sym))
      }
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
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): S = {
    // logger.info("*** evalConstant(" + value + "; " + typ.toString + "; " + pp.toString + ")")

    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.evalConstant(value, typ, pp))
  }

  /** Exhales permissions. */
  override def exhale(acc: Expression) : S = {
    logger.info("*** exhale(" + acc.toString + "): implement me!")

    //acc match {
    //  case acc: PermissionExpression => //TODO: handle permission levels different than the full permission level
    //    acc.id match {
    //      case id: FieldIdentifier =>
    //        // retrieve the symbolic permission associated with the permission expression
    //        val sym: SymbolicPermission = idToSym()
    //        // create a counted symbolic value to add to the symbolic permission
    //        val v: CountedSymbolicValue = new CountedSymbolicValue(acc.p.toString.toDouble)
    //        // update key in idToSym map
    //        val idToSymmap = idToSym + (id -> (sym - v))
    //
    //        // create constraint to ensure proper exhale
    //        // val c = new Geq(PermissionSolver.convertSymbolicPermission(sym), PermissionSolver.convertCountedSymbolicValue(v))
    //        // add constraint to solver
    //        // PermissionSolver.addConstraint(c)
    //
    //        // return the current state with updated idToSym
    //        this.copy(idToSym = idToSymmap)
    //      case _ => throw new IllegalArgumentException("A permission exhale must occur via a FieldIdentifier")
    //    }
    //  case _ =>
    //    val asserted: S = this.setExpression(ExpressionSet(acc))
    //    assert(asserted.testFalse() lessEqual this.bottom())
    //    this.assume(acc)
    //}
    this
  }

  /** The current expression.
    *
    * Invoked after each statement to retrieve its result.
    */
  override def expr: ExpressionSet = {
    // logger.info("*** expr: " + this.heapNum.exprSet.toString)
    
    this.heapNum.exprSet // return exprSet
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    * @todo implement me!
    */
  override def factory(): S = {
    // logger.info("*** factory(): implement me!")
    
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
  override def getFieldValue(obj: Expression, field: String, typ: Type): S = {
    logger.info("*** getFieldValue(" + obj.toString + "; " + field + "; " + typ.toString + ")")

    def extendSym(sym: SymbolicPermission, field: String, typ: Type, pp: ProgramPoint): SymbolicPermission = {
      val csv = sym.value.map(s => { // for each symbolic value within the symbolic permission...
        val p: Path = s.s.path // retrieve the current path
        val v = s.s.factory().setPath(new Path(field::p.p, typ::p.t, pp::p.l)) // extend the current path
        CountedSymbolicValue(s.n, v) // create a new symbolic value
      })
      new SymbolicPermission(csv) // create and return extended symbolic permission
    }

    obj match {
      case pathId:AccessPathIdentifier => // access path evaluation...
        val ids: List[Identifier] = pathId.path // list of identifiers forming the path
        val head = ids.head // first identifier on the path
        val tail = ids.tail // all identifiers on the path except the first
        val last = ids.last // last identifier on the path

        val objs = Set[HeapIdentifier]() // initially empty set of heap objects
        // set of heap objects pointed to by the path head
        val rcvs: Set[HeapIdentifier] = heapNum.refToObj(head.asInstanceOf[VariableIdentifier]) - NullHeapIdentifier
        val perm = Set[SymbolicPermission]() // initially empty symbolic permission
        // evaluation of the path
        val eval = tail.foldLeft((objs, rcvs, perm))((curr, next) => {
          // for each following path segment...
          val currc: Set[HeapIdentifier] = curr._1 // set of all heap objects collected so far
          val curro: Set[HeapIdentifier] = curr._2 // current set of receiver heap objects
          val currp: Set[SymbolicPermission] = curr._3 // set of all symbolic permission collected so far
          //
          val nextc = currc ++ curro // next set of collected heap objects
          var nexto = Set[HeapIdentifier]() // next set of receiver heap object
          // new set of collected symbolic permissions
          var newp: Set[SymbolicPermission] = idToSym.foldLeft(Set[SymbolicPermission]())(_ ++ _._2)
          for (o: HeapIdentifier <- curro) {
            // add newly reached heap objects
            nexto = nexto ++ heapNum.objFieldToObj.
              getOrElse[Map[String, Set[HeapIdentifier]]](o, Map[String, Set[HeapIdentifier]]()).
              getOrElse[Set[HeapIdentifier]](next.getName, Set[HeapIdentifier]())
            // create a field identifier for the heap object
            val fld = FieldIdentifier(o, next.getName, next.typ)
            // remove unseen symbolic permissions
            newp = newp intersect idToSym.getOrElse[Set[SymbolicPermission]](fld, Set[SymbolicPermission]())
          }
          val nextp = if ((currc intersect curro).nonEmpty) {
            // if a current heap object was already seen
            currp ++ newp.foldLeft[Set[SymbolicPermission]](Set[SymbolicPermission]())(
              // extend the paths of the symbolic permission in the new set of collected symbolic permissions
              (ps, p) => ps + extendSym(p, next.getName, next.typ, next.pp)
            )
          } else {
            // no current heap object was already seen
            currp ++ newp // add the new set of collected symbolic permission to the current set
          }
          // return the partial result of the evaluation
          if (next.typ.isObject) (nextc, nexto - NullHeapIdentifier, nextp) else (nextc, curro, nextp)
        })

        // update idToPerm map
        var idToPermmap = idToPerm
        for (o: HeapIdentifier <- eval._2) { // for each resulting receiver heap object...
          // create a field identifier for the heap object
          val fld = FieldIdentifier(o,last.getName,last.typ)
          // add key to idToPerm map
          idToPermmap = idToPermmap + (fld -> eval._3) // resulting set of collected symbolic permissions
        }

        // return the current state with updated heapNum and updated idToPerm
        this.copy(heapNum = heapNum.getFieldValue(pathId, field, typ), idToPerm = idToPermmap)

      case _ => throw new IllegalArgumentException("A field access must occur via an AccessPathIdentifier")
    }
  }

  ///** Retrieves the symbolic permission associated with a field identifier, or creates a new symbolic permission
  //  * from the symbolic permission associated with the corresponding heap identifier.
  //  *
  //  * @param id the field identifier used for the retrieval
  //  * @return the symbolic permission associated with the field identifier
  //  */
  //private def getSymbolicPermission(id: FieldIdentifier): SymbolicPermission = idToSym.get(id) match {
  //  case Some(s) => s
  //  case None =>
  //    val obj = id.obj // retrieve the heap identifier
  //    val perm = idToSym() // retrieve the associated symbolic permission
  //    // update the path of the counted symbolic values
  //    val csv = perm.value.map(s => {
  //      val v = s.s.factory().setPath(new Path(id.field :: s.s.path.p, id.typ :: s.s.path.t, id.pp :: s.s.path.l))
  //      CountedSymbolicValue(s.n, v)
  //    })
  //    // create and return new SymbolicPermission
  //    new SymbolicPermission(csv)
  //}

  /** Gets the value of a variable.
    *
    * Invoked by variable declarations (`var x : Ref`) and variable assignments (`x = ...`, `... = x`)
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): S = {
    logger.info("*** getVariableValue(" + id.toString + ")")

    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.getVariableValue(id))
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    * @todo implement me!
    */
  override def glb(other: S): S = {
    logger.info("*** glb(" + other.repr + "): implement me!")
    
    ???
  }

  /** Inhales permissions. */
  override def inhale(acc: Expression) : S = {
    logger.info("*** inahle(" + acc.toString + ")")

    acc match {
      case acc: PermissionExpression => //TODO: handle permission levels different than the full permission level
        acc.id match {
          case id: FieldIdentifier =>
            println("INHALE: " + idToSym(id))
    //        if (id.obj.representsSingleVariable) {
    //          // retrieve the symbolic permission associated with the permission expression
    //          val sym: SymbolicPermission = idToSym()
    //          // create a counted symbolic value to add to the symbolic permission
    //          val v: CountedSymbolicValue = new CountedSymbolicValue(acc.p.toString.toDouble)
    //          // update key in idToSym map
    //          val idToSymmap = idToSym + (id -> (sym + v))
    //
    //          // create constraint to ensure proper inhale
    //          // val m = new CountedSymbolicValue(PermissionSolver.permissionType.maxLevel)
    //          // val c = new Geq(PermissionSolver.convertCountedSymbolicValue(m), PermissionSolver.convertSymbolicPermission(sym + v))
    //          // add constraint to solver
    //          // PermissionSolver.addConstraint(c)
    //
    //          // return the current state with updated idToSym
    //          this.copy(idToSym = idToSymmap)
    //        } else { this } // no inhale on summary nodes

            //// add constraints to the solver
            //for (sym <- idToPerm(obj)) { // for all currently associated symbolic permissions...
            //// create constraint to ensure write permissions
            //val c = PermissionSolver.permissionType.ensureWrite(PermissionSolver.convertSymbolicPermission(sym))
            //  // add constraint to solver
            //  PermissionSolver.addConstraint(c)3
            //}

          case _ => throw new IllegalArgumentException("A permission inhale must occur via a FieldIdentifier")
        }
      case _ => this.assume(acc)
    }
    this
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return `true` if and only if the state is equivalent to bottom
    * @todo implement me!
    */
  override def isBottom: Boolean = {
    // logger.info("*** isBottom: implement me!")

    heapNum.isBottom
  }

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return `true` if and only if the state is equivalent to top
    * @todo implement me!
    */
  override def isTop: Boolean = {
    // logger.info("*** isTop: implement me!")
    
    heapNum.isTop
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return `true` if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: S): Boolean = {
    // logger.info("*** lessEqual(" + other.repr + ")")

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
  override def lub(other: S): S = {
    // logger.info("*** lub(" + other.repr + ")")

    val idToSymmap = this.idToSym.filterKeys(k => !other.idToSym.contains(k)) ++ other.idToSym.map {
      case (k: FieldIdentifier,v: Set[SymbolicPermission]) => k -> (v ++ this.idToSym.getOrElse(k,Set[SymbolicPermission]()))
    } // merge the itToSyms
    val idToPermmap = this.idToPerm.filterKeys(k => !other.idToPerm.contains(k)) ++ other.idToPerm.map {
      case (k: FieldIdentifier,v: Set[SymbolicPermission]) => k -> (v ++ this.idToPerm.getOrElse(k,Set[SymbolicPermission]()))
    } // merge the itToPerm

    // return the current state with updated heapNum, updated idToSym and updated idToPerm
    this.copy(heapNum = heapNum lub other.heapNum, idToSym = idToSymmap, idToPerm = idToPermmap)
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): S = {
    // logger.info("*** pruneUnreachableHeap()")
    this.copy(heapNum = heapNum.pruneUnreachableHeap())
  }

  /** Removes all variables satisfying filter.
    *
    * @todo implement me!
    */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): S = {
    logger.info("*** pruneVariables(" + filter.toString + "): implement me!")
    
    ???
  }

  /** Removes the current expression.
    *
    * @return The abstract state obtained after removing the current expression
    */
  override def removeExpression(): S = {
    // logger.info("*** removeExpression()")
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.removeExpression())
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    * @todo implement me!
    */
  override def removeVariable(varExpr: VariableIdentifier): S = {
    logger.info("*** removeVariable(" + varExpr.toString + "): implement me!")
    
    ???
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
      heapNum.numDom.toString + "), " +
      idToSym.toString + ", "
      idToPerm.toString + ")"
  }

  /** Assigns an expression to an argument.
    *
    * @param x The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    * @todo implement me!
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): S = {
    logger.info("*** setArgument(" + x.toString + "; " + right.toString + "): implement me!")
    
    ???
  }

  /** Sets the current expression.
    *
    * Invoked after statements that do not have results (like assignments to variables and fields).
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): S = {
    // logger.info("*** setExpression(" + expr.toString + ")")
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.setExpression(expr))
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    * @todo implement me!
    */
  override def setVariableToTop(varExpr: Expression): S = {
    logger.info("*** setVariableToTop(" + varExpr.toString + "): implement me!")
    
    ???
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    * @todo implement me!
    */
  override def throws(t: ExpressionSet): S = {
    logger.info("*** throws(" + t.toString + "): implement me!")
    
    ???
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    * @todo implement me!
    */
  override def top(): S = {
    // logger.info("*** top(): implement me!")
    
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
    "\tidToPerm: " + idToPerm.toString + "\n" +
    ")"

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    * @todo implement me!
    */
  override def widening(other: S): S = {
    logger.info("*** widening(" + other.repr + ")")

    val idToSymmap = this.idToSym.filterKeys(k => !other.idToSym.contains(k)) ++ other.idToSym.map {
      case (k: FieldIdentifier,v: Set[SymbolicPermission]) => k -> (v ++ this.idToSym.getOrElse(k,Set[SymbolicPermission]()))
    } // merge the itToSyms
    val idToPermmap = this.idToPerm.filterKeys(k => !other.idToPerm.contains(k)) ++ other.idToPerm.map {
      case (k: FieldIdentifier,v: Set[SymbolicPermission]) => k -> (v ++ this.idToPerm.getOrElse(k,Set[SymbolicPermission]()))
    } // merge the itToPerm

    // return the current state with updated heapNum, updated idToSym and updated idToPerm
    this.copy(heapNum = heapNum widening other.heapNum, idToSym = idToSymmap, idToPerm = idToPermmap)
  }
}

/** Permission Inference State using Intervals.
  *
  * @param heapNum pointsto+intervals state
  * @param idToSym map from an `Identifier` to its `SymbolicPermission`
  * @author Caterina Urban
  */
case class PermissionIntervalsState(heapNum: PointsToIntervalsState,
                                    idToSym: Map[FieldIdentifier,Set[SymbolicPermission]],
                                    idToPerm: Map[FieldIdentifier,Set[SymbolicPermission]])
  extends PermissionState[BoxedNonRelationalNumericalDomain[DoubleInterval],PointsToIntervalsState,PermissionIntervalsState] {
  override def copy(heapNum: PointsToIntervalsState,
                    idToSym: Map[FieldIdentifier, Set[SymbolicPermission]],
                    idToPerm: Map[FieldIdentifier, Set[SymbolicPermission]]): PermissionIntervalsState =
    PermissionIntervalsState(heapNum, idToSym, idToPerm)
}

/** Permission Inference State using Polyhedra.
  *
  * @param heapNum pointsto+polyhedra state
  * @param idToSym map from an `Identifier` to its `SymbolicPermission`
  * @author Caterina Urban
  */
case class PermissionPolyhedraState(heapNum: PointsToPolyhedraState,
                                    idToSym: Map[FieldIdentifier, Set[SymbolicPermission]],
                                    idToPerm: Map[FieldIdentifier, Set[SymbolicPermission]])
  extends PermissionState[Apron.Polyhedra,PointsToPolyhedraState,PermissionPolyhedraState] {
  override def copy(heapNum: PointsToPolyhedraState,
                    idToSym: Map[FieldIdentifier, Set[SymbolicPermission]],
                    idToPerm: Map[FieldIdentifier, Set[SymbolicPermission]]): PermissionPolyhedraState =
    PermissionPolyhedraState(heapNum, idToSym, idToPerm)
}

/** Permission Inference entry states for given method declarations.
  *
  * @tparam N the numerical domain
  * @tparam T the pointsto+numerical state
  * @tparam S the permission state
  * @author Caterina Urban
  */
trait PermissionEntryStateBuilder[N <: NumericalDomain[N],
  T <: PointsToNumericalState[N,T], S <: PermissionState[N,T,S]] extends EntryStateBuilder[S] {

  protected var fields: Set[(Type,String)] = Set[(Type,String)]()

  override def build(method: MethodDeclaration): S = {
    fields = Set[(Type,String)]()
    for(f <- method.classDef.fields) {
      fields = fields + ((f.typ, f.variable.toString))
    }
    method.initializeArgument[S](topState.copy(heapNum = topState.heapNum.copy(fieldSet = fields)))
  }

}

/** Permission Inference entry states using Intervals for given method declarations.
  *
  * @author Caterina Urban
  */
object PermissionIntervalsEntryStateBuilder
  extends PermissionEntryStateBuilder[BoxedNonRelationalNumericalDomain[DoubleInterval],PointsToIntervalsState,PermissionIntervalsState] {
  override def topState: PermissionIntervalsState = PermissionIntervalsState(
    PointsToIntervalsEntryStateBuilder.topState, // top points-to+intervals state
    // map from an identifier to its symbolic permission`
    Map[FieldIdentifier,Set[SymbolicPermission]](),
    // map from an identifier to its currently associated set of symbolic permissions
    Map[FieldIdentifier,Set[SymbolicPermission]]())
}

/** Permission Inference entry states using Polyhedra for given method declarations.
  *
  * @author Caterina Urban
  */
object PermissionPolyhedraEntryStateBuilder
  extends PermissionEntryStateBuilder[Apron.Polyhedra,PointsToPolyhedraState,PermissionPolyhedraState] {
  override def topState: PermissionPolyhedraState = PermissionPolyhedraState(
    PointsToPolyhedraEntryStateBuilder.topState, // top points-to+polyhedra state
    // map from an identifier to its symbolic permission`
    Map[FieldIdentifier,Set[SymbolicPermission]](),
    // map from an identifier to its currently associated set of symbolic permissions
    Map[FieldIdentifier,Set[SymbolicPermission]]())
}

/** Permission Inference.
  *
  * @tparam N the numerical domain
  * @tparam T the pointsto+numerical state
  * @tparam S the permission state
  * @author Caterina Urban
  */
trait PermissionInference[N <: NumericalDomain[N], T <: PointsToNumericalState[N,T], S <: PermissionState[N,T,S]]
  extends Analysis[S] {

  // map between method names and constraint solutions
  var permissions = Map[String, Map[SymbolicValue,Double]]()

  def entryStateBuilder: PermissionEntryStateBuilder[N,T,S]

  override def analyze(method: MethodDeclaration): AnalysisResult[S] = {
    // analyze the method
    val result = analyze(method, entryStateBuilder.build(method))
    // solve the accumulated constraints
    val solution = PermissionSolver.solve(PermissionSolver.getConstraints)
    println("\nResult: ")
    for ((s,v) <- solution) {
      if (v >= 1)
        println("acc(" + s.path.toString + ")")
      else if (v > 0)
        println("acc(" + s.path.toString + ", " + PermissionSolver.doubleToRational(v) + ")")
    }
    // add entry to the map between method names and constraint solutions
    permissions = permissions + (method.name.toString -> solution)
    // clear the accumulated constraints (for the analysis of the next method)
    PermissionSolver.emptyConstraints()
    result  // return the result of the analysis
  }

}

/** Permission Inference using Intervals.
  *
  * @author Caterina Urban
  */
object PermissionIntervalsAnalysis
  extends PermissionInference[BoxedNonRelationalNumericalDomain[DoubleInterval],
  PointsToIntervalsState, PermissionIntervalsState] {
  override def entryStateBuilder = PermissionIntervalsEntryStateBuilder
}

/** Permission Inference using Polyhedra.
  *
  * @author Caterina Urban
  */
object PermissionPolyhedraAnalysis
  extends PermissionInference[Apron.Polyhedra, PointsToPolyhedraState, PermissionPolyhedraState] {
  override def entryStateBuilder = PermissionPolyhedraEntryStateBuilder
}

/** Runs the Permission Inference.
  *
  * @tparam N the numerical domain
  * @tparam T the pointsto+numerical state
  * @tparam S the permission state
  * @author Caterina Urban
  */
trait PermissionInferenceRunner[N <: NumericalDomain[N], T <: PointsToNumericalState[N,T], S <: PermissionState[N,T,S]]
  extends SilAnalysisRunner[S] {

  /** The analysis to be run. */
  val analysis : PermissionInference[N,T,S]

  /** Extends a sil.Program with permissions inferred by the PermissionAnalysis. */
  def extendProgram(prog: sil.Program, results: List[AnalysisResult[S]]): sil.Program = {
    // map of method names to control flow graphs
    val methodNameToCfgState = results.map(result => result.method.name.toString -> result.cfgState).toMap
    // extending program methods
    val extMethods = prog.methods.map(
      method => methodNameToCfgState.get(method.name) match {
        case Some(cfgState) => extendMethod(method, cfgState)
        case None => method
      }
    )
    // building the extended program
    prog.copy(methods = extMethods)(prog.pos, prog.info)
  }

  /** Extends a sil.Method with permissions inferred by the PermissionAnalysis. */
  def extendMethod(method: sil.Method, cfgState: AbstractCFGState[S]): sil.Method = {

    // retrieve the result of the analysis at the method entry
    val pre = cfgState.entryState()
    println("PRE: " + pre)

    // update the method precondition
    var precondition: Seq[sil.Exp] = method.pres
    // add non-nullness preconditions
    //for ((id: Identifier,sym: SymbolicPermission) <- pre.idToSym) {
    //  // for each pair of identifier and symbolic permission...
    //  id match {
    //    case id: HeapIdentifier => // we only care about HeapIdentifiers
    //      // retrieve the paths leading to the heap identifier of the field identifier
    //      val paths = pre.heapNum.pathFromObj(id)
    //      for ((x,_) <- paths) { // for all retrieved paths...
    //        // adding non-nullness precondition to the method preconditions
    //        val v = sym.evaluate(analysis.permissions.getOrElse(method.name.toString, Map[SymbolicValue, Double]()))
    //        if (v > 0) {
    //          val left = sil.LocalVar(x.toString)(typToSilver(x.typ), ppToSilver(x.pp))
    //          precondition = precondition ++ Seq[sil.Exp](sil.NeCmp(left,sil.NullLit()())())
    //        }
    //      }
    //    case _ => // nothing to be done
    //  }
    //}
    // add access preconditions
    for ((s,v) <- analysis.permissions.getOrElse(method.name.toString, Map[SymbolicValue, Double]())) {
      s match {
        case s: SymbolicPrecondition => // we only care about preconditions
          // extracting all information from the path of the symbolic value
          val ids: List[(String, Type, ProgramPoint)] = (s.path.p, s.path.t, s.path.l).zipped.toList.reverse
          if (ids.length > 1) {
            // creating the corresponding field access
            val fst = sil.LocalVar(ids.head._1)(typToSilver(ids.head._2), ppToSilver(ids.head._3))
            val snd = sil.FieldAccess(fst, sil.Field(ids.tail.head._1, typToSilver(ids.tail.head._2))())()
            val acc: sil.FieldAccess = ids.tail.tail.foldLeft[sil.FieldAccess](snd)(
              (exp, fld) => sil.FieldAccess(exp, sil.Field(fld._1, typToSilver(fld._2))())()
            )
            // adding access permission to the method precondition
            if (v == 1) {
              val perm = sil.FieldAccessPredicate(acc, sil.FullPerm()())()
              precondition = precondition ++ Seq[sil.Exp](perm)
            } else if (v > 0) {
              val (num, den) = PermissionSolver.doubleToRational(v)
              //val perm = sil.FieldAccessPredicate(acc, sil.WildcardPerm()())()
              val perm = sil.FieldAccessPredicate(acc, sil.FractionalPerm(sil.IntLit(num)(), sil.IntLit(den)())())()
              precondition = precondition ++ Seq[sil.Exp](perm)
            }
          }
        case _ => // nothing to be done
      }
    }

    // update the method body
    val body = extendStmt(method.body, method, cfgState)

    // update the method postcondition
    // TODO

    // return the method with updated precondition, updated body and updated postcondition
    method.copy(_pres = precondition, _body = body)(method.pos, method.info)
  }

  def extendStmt(stmt: sil.Stmt, method: sil.Method, cfgState: AbstractCFGState[S]): sil.Stmt =
    stmt match {
      case stmt: sil.If => stmt

      case stmt: sil.NewStmt =>
        val pos = DefaultSilConverter.convert(stmt.pos)
        println("POS: " + pos)
        val cfgPositions = cfgState.cfg.nodes.zipWithIndex.flatMap({
          case (stmts, blockIdx) => stmts.zipWithIndex.flatMap({
            case (stmt, stmtIdx) =>
              if (stmt.getPC() == pos) Some(CFGPosition(blockIdx, stmtIdx))
              else None
          })
        })
        println("WHAT IS THIS: " + cfgPositions)

        val lhs = stmt.lhs
        stmt

        // TODO: wip
      case stmt: sil.Seqn =>
        val seq: Seq[sil.Stmt] = stmt.ss.foldRight(Seq[sil.Stmt]())(
          (s: sil.Stmt, ss: Seq[sil.Stmt]) => ss.+:(extendStmt(s,method,cfgState))
        )
        sil.Seqn(seq)(stmt.pos,stmt.info)

      case stmt: sil.While =>
        // retrieve the position of the loop head
        val pos = DefaultSilConverter.convert(stmt.cond.pos)
        // retrieve the block index and the statement index within the block of the loop head
        val cfgPositions = cfgState.cfg.nodes.zipWithIndex.flatMap({
          case (stmts, blockIdx) => stmts.zipWithIndex.flatMap({
            case (stmt, stmtIdx) =>
              if (stmt.getPC() == pos) Some(CFGPosition(blockIdx, stmtIdx)) else None
          })
        })
        // retrieve the result of the analysis at the loop head
        val pre = cfgState.preStateAt(cfgPositions.head)
        // update the method loop invariants
        var invariants: Seq[sil.Exp] = stmt.invs

        // add access permissions
        for ((id: FieldIdentifier,sym: Set[SymbolicPermission]) <- pre.idToSym) {
          // for each pair of identifier and set of symbolic permissions...
          val paths = pre.heapNum.pathFromObj(id.obj) // retrieve the paths leading to the receiver of the field identifier
          // select the shortest paths among the retrieved paths
          var shortest = Set[(VariableIdentifier,List[String])]()
          if (paths.size > 0) { // if there is at least one retrieved path
            // select the first path as the shortest
            shortest = Set[(VariableIdentifier,List[String])](paths.head)
            for ((x,p) <- paths.tail) { // for all remaining paths
              if (p.size < shortest.head._2.size) { // the current path is shorter than the ones collected so far
                // start collecting shorter paths
                shortest = Set[(VariableIdentifier,List[String])]((x,p))
              } else if (p.size == shortest.head._2.size) { // the current path is as long as the one collected so far
                shortest = shortest + ((x,p)) // add the current path to the collection
              }
            }
          }
          for ((x,p) <- shortest) { // for all retrieved shortest paths...
            // creating the corresponding field access
            val typ = typToSilver(x.typ)
            val fst = sil.LocalVar(x.toString)(typ, ppToSilver(x.pp))
            val ids = (id.field::p).reverse // note that the paths are stored in reverse order for efficiency
            val snd = sil.FieldAccess(fst, sil.Field(ids.head, typ)())()
            val acc: sil.FieldAccess = ids.tail.foldLeft[sil.FieldAccess](snd)(
              (exp, fld) => sil.FieldAccess(exp, sil.Field(fld, typ)())()
            )
            // adding access permission to the loop invariants
            val v = sym.head.evaluate(analysis.permissions.getOrElse(method.name.toString, Map[SymbolicValue, Double]()))
            if (v >= 1) {
              val perm = sil.FieldAccessPredicate(acc, sil.FullPerm()())()
              invariants = invariants ++ Seq[sil.Exp](perm)
            } else if (v > 0) {
              val (num, den) = PermissionSolver.doubleToRational(v)
              val perm = sil.FieldAccessPredicate(acc, sil.FractionalPerm(sil.IntLit(num)(), sil.IntLit(den)())())()
              invariants = invariants ++ Seq[sil.Exp](perm)
            }
          }
        }

        // add equalities between paths within the heap
        var eqSet = Set[sil.Exp]() // current set of equalities to add
        for (obj: HeapIdentifier <- pre.heapNum.objFieldToObj.keys) { // for all heap objects...
        val paths = pre.heapNum.pathFromObj(obj) // retrieve paths leading to the given heap object
          if (paths.size > 1) { // if there are at least two different paths...
          var curr = paths.head // first path
          var currIds = curr._2.drop(1).foldLeft(pre.heapNum.refToObj(curr._1))(
              (set,next) => // next path segment
                set.foldLeft(Set[HeapIdentifier]())(
                  (s,obj) => s ++ pre.heapNum.objFieldToObj.getOrElse(obj,Map[String,Set[HeapIdentifier]]()).
                    getOrElse(next,Set[HeapIdentifier]())
                )
            ) // evaluate the first path
            for (p <- paths - curr) { // second path
            // evaluate the second path
            val pIds = p._2.drop(1).foldLeft(pre.heapNum.refToObj(p._1))(
                (set,next) => // next path segment
                  set.foldLeft(Set[HeapIdentifier]())(
                    (s,obj) => s ++ pre.heapNum.objFieldToObj.getOrElse(obj,Map[String,Set[HeapIdentifier]]()).
                      getOrElse(next,Set[HeapIdentifier]())
                  )
              )
              if (currIds == pIds) {
                // creating the field access corresponding to the first path
                val currTyp = typToSilver(curr._1.typ)
                val currFst = sil.LocalVar(curr._1.toString)(currTyp, ppToSilver(curr._1.pp))
                var currExp: sil.Exp = currFst
                if (curr._2.size > 1) {
                  val currNms = curr._2.tail.reverse // note that the paths are stored in reverse order for efficiency
                  val currSnd = sil.FieldAccess(currFst, sil.Field(currNms.head, currTyp)())()
                  val currAcc: sil.FieldAccess = currNms.tail.foldLeft[sil.FieldAccess](currSnd)(
                    (exp, fld) => sil.FieldAccess(exp, sil.Field(fld, currTyp)())()
                  )
                  currExp = currAcc
                }
                // creating the field access corresponding to the second path
                val pTyp = typToSilver(p._1.typ)
                val pFst = sil.LocalVar(p._1.toString)(pTyp, ppToSilver(p._1.pp))
                var pExp: sil.Exp = pFst
                if (p._2.size > 1) {
                  val pNms = p._2.tail.reverse // note that the paths are stored in reverse order for efficiency
                  val pSnd = sil.FieldAccess(pFst, sil.Field(pNms.head, pTyp)())()
                  val pAcc: sil.FieldAccess = pNms.tail.foldLeft[sil.FieldAccess](pSnd)(
                    (exp, fld) => sil.FieldAccess(exp, sil.Field(fld, pTyp)())())
                  pExp = pAcc
                }
                // adding equality to invariants
                eqSet = eqSet + sil.EqCmp(currExp,pExp)()
              }
              curr = p; currIds = pIds
            }
          }
        }
        for (eq <- eqSet) { invariants = invariants ++ Seq[sil.Exp](eq) }

        sil.While(stmt.cond, invs = invariants, stmt.locals, body = extendStmt(stmt.body,method,cfgState))(stmt.pos,stmt.info)

      case _ => println(stmt.getClass); stmt
    }

  // convert sample.ProgramPoint to sil.ProgramPoint
  def ppToSilver(pp: ProgramPoint): sil.Position = pp match {
    case sample.DummyProgramPoint => sil.NoPosition
    case sample.WrappedProgramPoint(pos) => pos.asInstanceOf[SourcePosition]
  }

  // convert sample.Type to sil.Type
  def typToSilver(typ: Type): sil.Type = typ match {
    case sample.IntType => sil.Int
    case sample.BoolType => sil.Bool
    case sample.RefType(_) => sil.Ref
  }

  override def main(args: Array[String]) {
    // run the analysis and the permission inference
    val results = run(new File(args(0)).toPath)
    // extending program with inferred permission
    val out = extendProgram(DefaultSilConverter.prog,results)
    println("\nExtended Program:\n" + out)
    // create a file with the extended program
    val outName = args(0).split('.')(0) + "X.sil"
    val pw = new PrintWriter(new File(outName))
    pw.write(out.toString)
    pw.close
  }

  override def toString = "Access Permission Inference Analysis"
}

/** Runs the Permission Inference using Intervals.
  *
  * @author Caterina Urban
  */
object PermissionIntervalsAnalysisRunner
  extends PermissionInferenceRunner[BoxedNonRelationalNumericalDomain[DoubleInterval], PointsToIntervalsState, PermissionIntervalsState] {
  override val analysis = PermissionIntervalsAnalysis
  override def toString = "Permission-Intervals Analysis"
}

/** Runs the Permission Inference using Polyhedra.
  *
  * @author Caterina Urban
  */
object PermissionPolyhedraAnalysisRunner
  extends PermissionInferenceRunner[Apron.Polyhedra, PointsToPolyhedraState, PermissionPolyhedraState] {
  override val analysis = PermissionPolyhedraAnalysis
  override def toString = "Permission-Polyhedra Analysis"
}
