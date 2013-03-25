package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._

import apron._
import collection.mutable.ListBuffer
import ch.ethz.inf.pm.sample.abstractdomain.ReferenceComparisonExpression
import ch.ethz.inf.pm.sample.abstractdomain.UnaryArithmeticExpression
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.BinaryBooleanExpression
import ch.ethz.inf.pm.sample.abstractdomain.NegatedBooleanExpression
import ch.ethz.inf.pm.sample.abstractdomain.BinaryNondeterministicExpression

object ApronInstanceCounter {

  def inc(location:Integer,number:Integer = 1) {
    count.get(location) match {
      case Some(x) => count(location) = x + number
      case None => count(location) = number
    }
  }

  def reset() {
    count = scala.collection.mutable.Map.empty[Integer,Integer]
  }

  def print() {
    println("Apron Counts: "+(for ((a,b) <- count.seq.toList.sortBy(_._2)) yield {
      a+" -> "+b
    }).mkString(" | "))
  }

  var count = scala.collection.mutable.Map.empty[Integer,Integer]

}


class ApronInterface(state: Option[Abstract1], val domain: Manager, isPureBottom:Boolean = false, isPureTop:Boolean = false) extends RelationalNumericalDomain[ApronInterface] {

  /**
   * This returns an unusable object, which is neither top nor bottom nor has a state.
   * After using the factory, call bottom or top on it to create a usable state.
   */
  override def factory(): ApronInterface = {
    top()
  }

  override def merge(r: Replacement): ApronInterface = {

    if (r.isEmpty()) return this

    var startingState = instantiateState()
    var idsInDomain: Set[Identifier] = Set.empty
    var idsInCodomain: Set[Identifier] = Set.empty

    for ((from, to) <- r.value) {
      idsInDomain = idsInDomain.++(from)
      idsInCodomain = idsInCodomain.++(to)
      val idFromNamesList = ListBuffer.empty[String]
      for (idFrom <- from) {
        if (!startingState.getEnvironment.hasVar(idFrom.getName()))
          idFromNamesList.+=(idFrom.getName())
      }
      val newEnv = startingState.getEnvironment.add(idFromNamesList.toArray[String], new Array[String](0))
      ApronInstanceCounter.inc(0)
      startingState = startingState.changeEnvironmentCopy(domain, newEnv, false)
    }

    ApronInstanceCounter.inc(1)
    var result = new Abstract1(domain, startingState.getEnvironment, true)

    for ((from, to) <- r.value) {
      var newState = startingState
      var shallowCopyVars = List.empty[String]
      for (v <- from) {
        val vPrime: Array[String] = new Array[String](1)
        vPrime.update(0, v.getName() + "$")
        shallowCopyVars = shallowCopyVars.:+(v.getName() + "$")
        newState = newState.expandCopy(domain, v.getName(), vPrime)
      }
      // now we introduce id* into domain with values of one of the ids in "from"
      val idStar = "idStar"
      val aIdStar: Array[String] = new Array[String](1)
      aIdStar.update(0, idStar)
      newState = newState.expandCopy(domain, from.iterator.next().getName(), aIdStar)
      ApronInstanceCounter.inc(2,1)
      // now we fold shallow copies into idStar
      shallowCopyVars = idStar :: shallowCopyVars
      newState = newState.foldCopy(domain, shallowCopyVars.toArray[String])
      ApronInstanceCounter.inc(3,1)
      // now we expand id* to variables in "to"
      val idsToStringArray: Array[String] = idsToArrayOfStrings(to)
      newState = newState.expandCopy(domain, idStar, idsToStringArray)
      ApronInstanceCounter.inc(4,1)
      // now we remove the temoprary variables form the environment
      newState = newState.forgetCopy(domain, aIdStar, false)
      ApronInstanceCounter.inc(5,1)
      newState = newState.changeEnvironmentCopy(domain, newState.getEnvironment.remove(aIdStar), false)
      ApronInstanceCounter.inc(6,1)
      // we also need to extend the environment of result
      result = result.expandCopy(domain, from.iterator.next().getName(), idsToStringArray)
      ApronInstanceCounter.inc(7,1)
      result = result.joinCopy(domain, newState)
      ApronInstanceCounter.inc(8,1)
      startingState = startingState.expandCopy(domain, from.iterator.next().getName(), idsToStringArray)
      ApronInstanceCounter.inc(9,1)
    }

    for (id <- idsInDomain.--(idsInCodomain)) {
      result = result.forgetCopy(domain, id.getName(), false)
      ApronInstanceCounter.inc(10,1)
    }

    // We minimize the environment in order to achieve better performance.
    // This effects assign as there might be variable removed that are still in use.
    result = result.minimizeEnvironmentCopy(domain)
    ApronInstanceCounter.inc(11,1)
    new ApronInterface(Some(result), domain)

  }

  // TODO
  def getIds(): Set[Identifier] = Set.empty[Identifier]

  /**
   *
   * Adds a new variable to the domain (the domains environmnet). We only add variables with numerical types.
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
   */
  override def createVariable(variable: Identifier, typ: Type): ApronInterface = {
    val startingState = instantiateState()
    if (!startingState.getEnvironment.hasVar(variable.getName()) && typ.isNumericalType()) {
      val env = addToEnvironment(startingState.getEnvironment, variable.getType(), variable.getName())
      ApronInstanceCounter.inc(12,1)
      new ApronInterface(Some(startingState.changeEnvironmentCopy(domain, env, false)), domain)
    } else this
  }

  /**
   *
   * Removes a variable from the domain (the domains environment)
   *
   * @param variable the variable to be removed
   * @return the state after this action
   */
  override def removeVariable(variable: Identifier): ApronInterface = {
    state match {
      case None => this
      case Some(st) =>
        if (st.getEnvironment.hasVar(variable.getName())) {
          val v: Array[String] = new Array[String](1)
          v.update(0, variable.getName())
          val env = st.getEnvironment.remove(v)
          ApronInstanceCounter.inc(13,1)
          new ApronInterface(Some(st.changeEnvironmentCopy(domain, env, false)), domain)
        } else this
    }
  }

  override def setToTop(variable: Identifier): ApronInterface = {
    state match {
      case None => this
      case Some(st) =>
        if (st.getEnvironment.hasVar(variable.getName())) {
          ApronInstanceCounter.inc(14,1)
          new ApronInterface(Some(st.forgetCopy(domain, variable.getName(), false)), domain)
        } else this
    }
  }


  override def assign(variable: Identifier, expr: Expression): ApronInterface = {

    if (variable.getType().isNumericalType()) {

      // Create variable if it does not exist
      var newState = instantiateState()
      if (!newState.getEnvironment.hasVar(variable.getName())) {
        val env = addToEnvironment(newState.getEnvironment, variable.getType(), variable.getName())
        ApronInstanceCounter.inc(15,1)
        newState = newState.changeEnvironmentCopy(domain, env, false)
      }

      // ADDED because of minimizing the environment in merge
      // (i.e. some of the variables still in use might have been removed by merge as they were Top)
      var newEnv = newState.getEnvironment
      for (id <- Normalizer.getIdsForExpression(expr)) {
        if (!newEnv.hasVar(id.getName())) {
          newEnv = addToEnvironment(newEnv, id.getType(), id.getName())
        }
      }
      if (newEnv != newState.getEnvironment) {
        ApronInstanceCounter.inc(16,1)
        newState = newState.changeEnvironmentCopy(domain, newEnv, false)
      }
      // END of the added code

      val res = nondeterminismWrapper(expr, newState, (someExpr, someState) => {
        val expr = this.toTexpr1Intern(someExpr, someState.getEnvironment)
        if (expr.size > 1) {
          ApronInstanceCounter.inc(17,1)
          var curState = new Abstract1(domain, someState.getEnvironment, true)
          for (e <- expr) {
            ApronInstanceCounter.inc(18,2)
            curState = curState.joinCopy(domain, someState.assignCopy(domain, variable.getName(), e, null))
          }
          curState
        } else if (expr.size == 1) {
          ApronInstanceCounter.inc(19,1)
          someState.assignCopy(domain, variable.getName(), expr.head, null)
        } else {
          throw new ApronException("Empty expression set created")
        }
      })

      new ApronInterface(Some(res),domain)

    } else this
  }

  override def assume(expr: Expression): ApronInterface = {

    if (isBottom) return this

    // Check if we assume something about non-numerical values - if so, return
    val ids = Normalizer.getIdsForExpression(expr)
    for (id <- ids) {
      if (!id.getType().isNumericalType()) {
        return this
      }
    }

    // Check if we just assume if something is invalid - we dont know that here
    expr match {
      case BinaryArithmeticExpression(_,Constant("invalid",_,_),_,_) => return this
      case _ => ()
    }

    expr match {
      // Boolean variables
      case x: Identifier =>
        assume(BinaryArithmeticExpression(x, Constant("0", x.getType(), x.getProgramPoint()), ArithmeticOperator.!=, null))
      case NegatedBooleanExpression(x: Identifier) =>
        assume(BinaryArithmeticExpression(x, Constant("0", x.getType(), x.getProgramPoint()), ArithmeticOperator.==, null))
      case BinaryBooleanExpression(left, right, op, typ) => op match {
        case BooleanOperator.&& => val l = assume(left); l.glb(l, assume(right))
        case BooleanOperator.|| => val l = assume(left); l.lub(l, assume(right))
      }
      case NegatedBooleanExpression(BinaryBooleanExpression(left, right, op, typ)) => {
        val nl = NegatedBooleanExpression(left)
        val nr = NegatedBooleanExpression(right)
        val nop = op match {
          case BooleanOperator.&& => BooleanOperator.||
          case BooleanOperator.|| => BooleanOperator.&&
        }
        assume(BinaryBooleanExpression(nl, nr, nop, typ))
      }
      case _ => {

        // Create an actual instance of the current state
        var curState = instantiateState()

        // Assume the expression
        val res = nondeterminismWrapper(expr, curState, (someExpr, someState) => {

          var tmp = someState
          var expEnv = new Environment()
          for (id <- Normalizer.getIdsForExpression(someExpr)) {
            expEnv = addToEnvironment(expEnv, id.getType(), id.getName())
          }
          val unionEnv = unionOfEnvironments(tmp.getEnvironment, expEnv)
          if (!expEnv.isIncluded(tmp.getEnvironment)) {
            ApronInstanceCounter.inc(20,1)
            tmp = tmp.changeEnvironmentCopy(domain, unionEnv, false)
          }
          val constraints = this.toTcons1(someExpr, unionEnv)
          for (c <- constraints) {
            ApronInstanceCounter.inc(21,1)
            tmp = tmp.meetCopy(domain, c)
          }
          tmp
        })

        new ApronInterface(Some(res),domain)

      }
    }
  }

  override def top(): ApronInterface = {
    new ApronInterface(None, domain, isPureTop = true)
  }

  override def bottom(): ApronInterface = {
    new ApronInterface(None, domain, isPureBottom = true)
  }

  def isBottom: Boolean = {
    state match {
      case Some(s) => s.isBottom(domain)
      case None => isPureBottom
    }
  }

  def isTop: Boolean = {
    state match {
      case Some(s) => s.isTop(domain)
      case None => isPureTop
    }
  }

  override def lub(left: ApronInterface, right: ApronInterface): ApronInterface = {

    if (left == right)
      return left
    if (left.isBottom)
      return right
    if (right.isBottom)
      return left
    if (left.isTop)
      return left
    if (right.isTop)
      return right

    try {

      val leftState = left.instantiateState(Some(right))
      val rightState = right.instantiateState(Some(left))

      if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
        val env = unionOfEnvironments(leftState.getEnvironment, rightState.getEnvironment)
        val newLeft = leftState.changeEnvironmentCopy(domain, env, false)
        val newRight = rightState.changeEnvironmentCopy(domain, env, false)
        val commonVars = leftState.minimizeEnvironmentCopy(domain).getEnvironment.getVars.intersect(rightState.minimizeEnvironmentCopy(domain).getEnvironment.getVars)
        val forgotLState = newLeft.forgetCopy(domain, commonVars.toArray[String], false)
        val forgotRState = newRight.forgetCopy(domain, commonVars.toArray[String], false)
        val finalLeft = forgotLState.meetCopy(domain, newRight)
        val finalRight = forgotRState.meetCopy(domain, newLeft)
        val st = finalLeft.joinCopy(domain, finalRight)
        ApronInstanceCounter.inc(22,9)
        val res = new ApronInterface(Some(st), domain)
        res
      } else {
        ApronInstanceCounter.inc(23,1)
        val res = new ApronInterface(Some(leftState.joinCopy(domain, rightState)), domain)
        res
      }

    } catch {
      case a:apron.ApronException => {
        throw new ApronException("WARNING: incompatible environments.")
      }
    }
  }

  override def glb(left: ApronInterface, right: ApronInterface): ApronInterface = {

    if (left == right)
      return left
    if (left.isBottom)
      return left
    if (right.isBottom)
      return right
    if (left.isTop)
      return right
    if (right.isTop)
      return left

    val leftState = left.instantiateState(Some(right))
    val rightState = right.instantiateState(Some(left))

    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      val env = unionOfEnvironments(leftState.getEnvironment, rightState.getEnvironment)
      val newLeft = leftState.changeEnvironmentCopy(domain, env, false)
      val newRight = rightState.changeEnvironmentCopy(domain, env, false)
      val st = newLeft.meetCopy(domain, newRight)
      ApronInstanceCounter.inc(24,3)
      val res = new ApronInterface(Some(st), domain)
      res
    } else {
      ApronInstanceCounter.inc(25,1)
      val res = new ApronInterface(Some(leftState.meetCopy(domain, rightState)), domain)
      res
    }

  }

  override def widening(left: ApronInterface, right: ApronInterface): ApronInterface = {

    if (left == right)
      return left
    if (left.isBottom)
      return right
    if (right.isBottom)
      return left
    if (left.isTop)
      return left
    if (right.isTop)
      return right

    val leftState = left.instantiateState(Some(right))
    val rightState = right.instantiateState(Some(left))

    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      ApronInstanceCounter.inc(26,3)
      val env = unionOfEnvironments(leftState.getEnvironment, rightState.getEnvironment)
      val newLeft = leftState.changeEnvironmentCopy(domain, env, false)
      val newRight = rightState.changeEnvironmentCopy(domain, env, false)
      val res = new ApronInterface(Some(newLeft.widening(domain, newRight)), domain)
      res
    } else {
      ApronInstanceCounter.inc(27,1)
      val res = new ApronInterface(Some(leftState.widening(domain, rightState)), domain)
      res
    }

  }

  override def lessEqual(r: ApronInterface): Boolean = {

    if (this == r) return true
    if (this.isBottom) return true
    if (r.isTop) return true
    if (r.isBottom) return false
    if (this.isTop) return false

    val leftState = this.instantiateState(Some(r))
    val rightState = r.instantiateState(Some(this))

    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      val env = unionOfEnvironments(leftState.getEnvironment, rightState.getEnvironment)
      ApronInstanceCounter.inc(28,2)
      val newLeft = leftState.changeEnvironmentCopy(domain, env, false)
      val newRight = rightState.changeEnvironmentCopy(domain, env, false)
      newLeft.isIncluded(domain, newRight)
    } else {
      leftState.isIncluded(domain,rightState)
    }

  }

  override def getStringOfId(id: Identifier): String = {
    if (state == None && !isPureBottom && !isPureTop) return "Uninitialized!"
    if (isBottom) return "_|_"
    if (isTop) return "T"
    toString(this.state.get.toLincons(domain).toList.filter(constraintContains(_, id.getName())))
  }

  override def toString: String = {
    if (state == None && !isPureBottom && !isPureTop) return "Uninitialized!"
    if (isBottom) return "_|_"
    if (isTop) return "T"
    toString(this.state.get.toLincons(domain).toList)
  }

  /**
   * A custom, nicer textual representation.
   */
  private def toString(cons: List[Lincons1]): String = {
    val strs = (for (c <- cons) yield {

      val (left, right) = (for (t <- c.getLinterms) yield {
        val coeff = t.getCoefficient
        val vari = t.getVariable
        if (coeff.isEqual(1)) (Some(vari), None)
        else if (coeff.isEqual(-1)) (None, Some("-" + vari))
        else if (coeff.cmp(new DoubleScalar(0)) > 0) (Some(coeff.toString + vari), None)
        else if (coeff.cmp(new DoubleScalar(0)) < 0) (None, Some(coeff.toString + vari))
        else (None, None)
      }).unzip

      val const = c.getCst
      val (constL, constR) =
        if (const != None && !const.isZero)
          if (const.cmp(new DoubleScalar(0)) > 0) (Some(const.toString), None)
          else (None, Some(const.toString))
        else (None, None)

      val (leftX, rightX) = ((constL :: left.toList).flatten, (constR :: right.toList).flatten)
      val (leftS, rightS) = (leftX.mkString(" + "), rightX.mkString(" + ").replace("-", ""))
      val (leftR, rightR) = (if (leftS.isEmpty) "0" else leftS, if (rightS.isEmpty) "0" else rightS)

      if (leftR.compare(rightR) > 0) (leftR + opToStr(c.getKind) + rightR)
      else (rightR + opToInvStr(c.getKind) + leftR)

    })

    // This is super ugly but helps
    val a = new scala.collection.mutable.HashSet[String]
    for (str <- strs) {
      val other = str.replace("<", "?").replace(">", "<").replace("?", ">")
      if (!a.contains(other)) {
        a += str
      } else {
        val third = str.replace("<", "=").replace(">", "=").replace("==", "=")
        a.remove(other)
        a += third
      }
    }

    a.toList.sorted.mkString("\n")

  }

  private def constraintContains(c: Lincons1, variable: String): Boolean = {
    for (term <- c.getLinterms)
      if (term.getVariable.equals(variable) && !term.getCoefficient.toString.equals("0"))
        return true
    false
  }

  /**
   * This function creates an Abstract1 instance representing the current state if this is a pure bottom or pure top
   * state.
   *
   * We construct our state according to some other state
   *
   */
  def instantiateState(other:Option[ApronInterface] = None):Abstract1 = {


    state match {
      case Some(s) => s
      case None =>
        if (isPureBottom) {
          ApronInstanceCounter.inc(29,1)
          new Abstract1(domain,new Environment(),true)
        }
        else if (isPureTop) {
          ApronInstanceCounter.inc(30,1)
          new Abstract1(domain,new Environment())
        }
        else throw new ApronException("Must be bottom, top, or have an apron instance.")
    }

  }

  private def removeNondeterminism(label: String, expr: Expression): (Expression, List[(Identifier, BinaryNondeterministicExpression)]) = {
    expr match {
      case BinaryArithmeticExpression(left, right, op, typ) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (BinaryArithmeticExpression(expL, expR, op, typ), varL ::: varR)
      case BinaryBooleanExpression(left, right, op, typ) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (BinaryBooleanExpression(expL, expR, op, typ), varL ::: varR)
      case ReferenceComparisonExpression(left, right, op, typ) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (ReferenceComparisonExpression(expL, expR, op, typ), varL ::: varR)
      case NegatedBooleanExpression(left) =>
        val (expL, varL) = removeNondeterminism(label, left)
        (NegatedBooleanExpression(expL), varL)
      case UnaryArithmeticExpression(left, op, ret) =>
        val (expL, varL) = removeNondeterminism(label, left)
        (UnaryArithmeticExpression(expL, op, ret), varL)
      case BinaryNondeterministicExpression(left, right, op, returnType) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        val identifier = new VariableIdentifier(label, expr.getType(), expr.getProgramPoint())
        (identifier, varL ::: varR ::: List((identifier, BinaryNondeterministicExpression(expL, expR, op, returnType))))
      case x: Expression => (x, Nil)
    }
  }

  private def nondeterminismWrapper(expr: Expression, state: Abstract1, someFunc: (Expression, Abstract1) => Abstract1): Abstract1 = {

    // Extract all non-deterministic expressions and store them in temporary variables
    var newState = new ApronInterface(Some(state),domain)
    val (newExpr, tempAssigns) = removeNondeterminism("tmp", expr)
    for ((id, ndExpr) <- tempAssigns) {
      ndExpr.op match {
        case NondeterministicOperator.or =>
          val newStateLeft = newState.assign(id, ndExpr.left)
          val newStateRight = newState.assign(id, ndExpr.right)
          newState = lub(newStateLeft, newStateRight)
        case NondeterministicOperator.to =>
          newState = newState.
            createVariable(id, ndExpr.getType()).
            assume(BinaryArithmeticExpression(id, ndExpr.left, ArithmeticOperator.>=, ndExpr.getType())).
            assume(BinaryArithmeticExpression(id, ndExpr.right, ArithmeticOperator.<=, ndExpr.getType()))
      }
    }

    newState = new ApronInterface(Some(someFunc(newExpr, newState.instantiateState())),domain)

    // Remove all temporary variables
    for ((id, _) <- tempAssigns) {
      newState = newState.removeVariable(id)
    }

    newState.instantiateState()
  }

  private def toTexpr1Intern(e: Expression, env: apron.Environment): List[Texpr1Intern] = {
    val e1 = this.toTexpr1Node(e)
    for (e <- e1) yield new Texpr1Intern(env, e)
  }

  private def topExpression(): Texpr1Node = {
    val a = new apron.Interval(0, 0)
    val b = new DoubleScalar()
    val c = new DoubleScalar()
    b.setInfty(-1)
    c.setInfty(1)
    a.setInf(b)
    a.setSup(c)
    new Texpr1CstNode(a)
  }

  private def topConstraint(env: Environment): Tcons1 = {
    new Tcons1(Tcons1.EQ, new Texpr1Intern(env, new Texpr1CstNode(new DoubleScalar(0)))) // always true
  }

  private def bottomConstraint(env: Environment): Tcons1 = {
    new Tcons1(Tcons1.EQ, new Texpr1Intern(env, new Texpr1CstNode(new DoubleScalar(1)))) // always false
  }

  private def toTexpr1Node(e: Expression): List[Texpr1Node] = e match {
    case Constant("invalid", typ, p) =>
      List(topExpression()) // SHOULD BE: BOTTOM. NOT IMPLEMENTABLE
    case Constant("valid", typ, p) =>
      List(topExpression())
    case x: Identifier => List(new Texpr1VarNode(x.getName()))
    case setId: HeapIdSetDomain[Identifier] =>
      if (setId.isTop || setId.isBottom) List(topExpression())
      (setId.value map {
        x: Identifier => new Texpr1VarNode(x.getName())
      }).toList
    case Constant(v, typ, p) =>
      if (typ.isNumericalType())
        v match {
          case "true" => List(new Texpr1CstNode(new DoubleScalar(1)))
          case "false" => List(new Texpr1CstNode(new DoubleScalar(0)))
          case _ => List(new Texpr1CstNode(new DoubleScalar(java.lang.Double.parseDouble(v))))
        }
      else List(topExpression())
    case BinaryArithmeticExpression(left, right, op, typ) =>
      for (l <- this.toTexpr1Node(left); r <- this.toTexpr1Node(right)) yield {
        new Texpr1BinNode(this.convertArithmeticOperator(op), l, r)
      }
    case BinaryBooleanExpression(left, right, op, typ) =>
      for (l <- this.toTexpr1Node(left); r <- this.toTexpr1Node(right)) yield {
        new Texpr1BinNode(this.convertBooleanOperator(op), l, r)
      }
    case UnaryArithmeticExpression(left, op, typ) =>
      op match {
        case ArithmeticOperator.- =>
          for (l <- this.toTexpr1Node(left)) yield {
            new Texpr1UnNode(Texpr1UnNode.OP_NEG, l)
          }
      }
    case _ => throw new SemanticException("Unhandled expression type.")
  }

  private def convertArithmeticOperator(op: ArithmeticOperator.Value): Int = op match {
    case ArithmeticOperator.+ => Texpr1BinNode.OP_ADD
    case ArithmeticOperator.- => Texpr1BinNode.OP_SUB
    case ArithmeticOperator./ => Texpr1BinNode.OP_DIV
    case ArithmeticOperator.* => Texpr1BinNode.OP_MUL
  }

  /** used when we assign a boolean value, e.g. flag = flag1 && flag2 */
  private def convertBooleanOperator(op: BooleanOperator.Value): Int = op match {
    case BooleanOperator.&& => Texpr1BinNode.OP_MUL
    case BooleanOperator.|| => Texpr1BinNode.OP_ADD
  }

  private def toTcons1(e: Expression, env: Environment): List[Tcons1] = e match {
    case Constant("invalid", typ, p) =>
      List(bottomConstraint(env))
    case Constant("valid", typ, p) =>
      List(topConstraint(env))
    case BinaryArithmeticExpression(left, right, op, typ) =>
      var localOp = op
      var localLeft = left
      var localRight = right
      op match {
        case ArithmeticOperator.>= =>
        case ArithmeticOperator.== =>
        case ArithmeticOperator.!= =>
        case ArithmeticOperator.> =>
        case ArithmeticOperator.<= => localLeft = right; localRight = left; localOp = ArithmeticOperator.>=
        case ArithmeticOperator.< => localLeft = right; localRight = left; localOp = ArithmeticOperator.>
      }
      val expr1 = this.toTexpr1Node(new BinaryArithmeticExpression(localLeft, localRight, ArithmeticOperator.-, localLeft.getType()))
      localOp match {
        case ArithmeticOperator.>= => for (e <- expr1) yield new Tcons1(env, Tcons1.SUPEQ, e)
        case ArithmeticOperator.== => for (e <- expr1) yield new Tcons1(env, Tcons1.EQ, e)
        case ArithmeticOperator.!= => for (e <- expr1) yield new Tcons1(env, Tcons1.DISEQ, e)
        case ArithmeticOperator.> =>

          if (domain.isInstanceOf[Octagon]) {

            // Some domains, like Octagons have trouble representing >. In floating point mode, they will replace
            // A > B by A >= B, which can cause massive imprecision. We replace this by A >= B + EPSILON, where
            // EPSILON should be the smallest representable number. (actually, we are generating A - B - EPSILON >= 0)

            val sExpr1 = new BinaryArithmeticExpression(localLeft, localRight, ArithmeticOperator.-, localLeft.getType())
            val sExpr2 = new BinaryArithmeticExpression(sExpr1, Constant(NumericalAnalysisConstants.epsilon.toString, sExpr1.getType(), sExpr1.getProgramPoint()), ArithmeticOperator.-, sExpr1.getType())
            for (e <- this.toTexpr1Node(sExpr2)) yield {
              new Tcons1(env, Tcons1.SUPEQ, e)
            }

          } else {
            for (e <- expr1) yield new Tcons1(env, Tcons1.SUP, e)
          }

      }
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
      toTcons1(BinaryArithmeticExpression(left, right, negateOperator(op), typ), env)
    case NegatedBooleanExpression(NegatedBooleanExpression(x)) => toTcons1(x, env)
    case NegatedBooleanExpression(x) =>
      toTcons1(BinaryArithmeticExpression(x, Constant("0", x.getType(), x.getProgramPoint()), ArithmeticOperator.==, x.getType()), env)
    case x: Expression =>
      toTcons1(BinaryArithmeticExpression(x, Constant("0", x.getType(), x.getProgramPoint()), ArithmeticOperator.!=, x.getType()), env)
  }

  private def negateOperator(op: ArithmeticOperator.Value): ArithmeticOperator.Value = op match {
    case ArithmeticOperator.<= => ArithmeticOperator.>
    case ArithmeticOperator.< => ArithmeticOperator.>=
    case ArithmeticOperator.>= => ArithmeticOperator.<
    case ArithmeticOperator.== => ArithmeticOperator.!=
    case ArithmeticOperator.!= => ArithmeticOperator.==
    case ArithmeticOperator.> => ArithmeticOperator.<=
  }


  /**
   * This method returns an apron.Environment that has variables from both given environment (left, right).
   *
   * @param left - the first environment
   * @param right - the second environment
   *
   * @return an environment that has variables from both (left, right) environments.
   */
  private def unionOfEnvironments(left: Environment, right: Environment): Environment = {
    var resEnv = left
    for (v <- right.getRealVars) {
      if (!left.hasVar(v)) {
        val vA: Array[String] = new Array[String](1)
        vA.update(0, v)
        resEnv = resEnv.add(new Array[String](0), vA)
      }
    }
    for (v <- right.getIntVars) {
      if (!left.hasVar(v)) {
        val vA: Array[String] = new Array[String](1)
        vA.update(0, v)
        resEnv = resEnv.add(vA, new Array[String](0))
      }
    }
    resEnv
  }

  private def addToEnvironment(env: Environment, typ: Type, varName: String): Environment = {
    val v = new Array[String](1)
    v(0) = varName
    if (typ.isFloatingPointType()) env.add(new Array[String](0), v)
    else env.add(v, new Array[String](0))
  }

  private def opToStr(kind: Int): String = {
    kind match {
      case Lincons1.DISEQ => " != "
      case Lincons1.EQ => " = "
      case Lincons1.SUP => " > "
      case Lincons1.SUPEQ => " >= "
    }
  }

  private def opToInvStr(kind: Int): String = {
    kind match {
      case Lincons1.DISEQ => " = "
      case Lincons1.EQ => " != "
      case Lincons1.SUP => " < "
      case Lincons1.SUPEQ => " <= "
    }
  }

  private def idsToArrayOfStrings(set: Set[Identifier]): Array[String] = {
    var stringList = List.empty[String]
    for (v <- set) {
      stringList = v.getName() :: stringList
    }
    stringList.toArray[String]
  }

}


class ApronAnalysis extends SemanticAnalysis[ApronInterface] {

  var domain: Manager = null

  def getLabel(): String = "Apron numerical analysis"

  def parameters(): List[(String, Any)] = List(("Domain", List("Interval", "PPL", "Octagons", "Polka", "Linear equalities")))

  def setParameter(label: String, value: Any) {
    label match {
      case "Domain" => value match {
        case "Interval" => domain = new Box()
        case "PPL" => domain = new Polka(false) //new PplPoly(false); FIXIT: Change back to PPL
        case "Octagons" => domain = new Octagon()
        case "Polka" => domain = new Polka(false)
        case "Linear equalities" => domain = new PolkaEq()
      }
    }
  }

  def reset() {}

  def getInitialState(): ApronInterface = new ApronInterface(None, domain).bottom()

  def getProperties(): Set[Property] = Set(
    new ShowGraphProperty().asInstanceOf[Property],
    new SingleStatementProperty(DivisionByZero),
    new SingleStatementProperty(new LowerBoundedValue("y", 0)),
    new SingleStatementProperty(new BoundedValue("y", -4, 4))
  )

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil

}

class ApronException(s: String) extends Exception(s);