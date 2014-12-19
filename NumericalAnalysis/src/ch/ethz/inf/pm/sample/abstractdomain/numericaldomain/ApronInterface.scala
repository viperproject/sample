package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{BinaryBooleanExpression, BinaryNondeterministicExpression, Constant, MaybeHeapIdSetDomain, NegatedBooleanExpression, ReferenceComparisonExpression, UnaryArithmeticExpression, VariableIdentifier, _}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronTools._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._

object ApronInterface {

  /** Simple implementation of `ApronInterface`. Cannot be extended. */
  final case class Default(
                            state: Option[Abstract1],
                            domain: Manager,
                            isPureBottom: Boolean = false,
                            env: Set[Identifier])
    extends ApronInterface[Default] {

    def factory(
                 state: Option[Abstract1],
                 domain: Manager,
                 isPureBottom: Boolean = false,
                 env: Set[Identifier]) =
      Default(state, domain, isPureBottom, env)
  }

}

trait ApronInterface[T <: ApronInterface[T]]
  extends RelationalNumericalDomain[T] {
  self: T =>

  def state: Option[Abstract1]

  def domain: Manager

  def isPureBottom: Boolean

  def env: Set[Identifier]

  if (SystemParameters.DEBUG) {
    if (env.map(_.getName).size != env.size) {
      var seen: Map[String, Identifier] = Map.empty
      var badBoys: Set[(Identifier, Identifier)] = Set.empty
      for (e <- env) {
        seen.get(e.getName) match {
          case Some(x) => badBoys = badBoys + ((x, e))
          case None => seen = seen + (e.getName -> e)
        }
      }
      throw new ApronException("When constructing ApronInterface: Two different identifiers have the same getName() representation! " + badBoys)
    }
    state match {
      case Some(s) =>
        val apronEnvVars = s.getEnvironment.getVars.toSet[String]
        val fullEnvVars = env.map(_.getName)
        if (!(apronEnvVars subsetOf fullEnvVars)) {
          val varDiff = apronEnvVars diff fullEnvVars
          throw new ApronException("The set of variables in the state is not " +
            "a subset of variables in the environment. Offending variables: " + varDiff)
        }
      case None =>
    }

  }

  def factory(
               state: Option[Abstract1],
               domain: Manager,
               isPureBottom: Boolean = false,
               env: Set[Identifier]): T

  override def factory(): T = {
    top()
  }

  override def top(): T = {
    factory(None, domain, env = Set.empty)
  }

  override def bottom(): T = {
    factory(None, domain, isPureBottom = true, env = Set.empty)
  }

  override lazy val isBottom: Boolean = {
    if (isPureBottom) true
    else state match {
      case Some(s) => s.isBottom(domain)
      case None => false // top state
    }
  }

  def isTop: Boolean = {
    state match {
      case Some(s) => s.isTop(domain)
      case None => !isPureBottom
    }
  }

  /**
   * This function creates an Abstract1 instance representing the current state if this is a pure bottom or pure top
   * state. If the state is not pure top or pure bottom, this will not create a copy but return the current apron state
   */
  def instantiateState(): Abstract1 = {

    state match {
      case Some(s) => s
      case None =>
        if (isPureBottom) {
          new Abstract1(domain, new Environment(), true)
        } else {
          new Abstract1(domain, new Environment())
        }
    }

  }

  def copyState(): Abstract1 = new Abstract1(domain, instantiateState())

  def ids: Set[Identifier] = env

  /**
   *
   * Adds a new variable to the domain (the domains environmnet). We only add variables with numerical types.
   *
   * This does not modify the APRON state
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
   */
  override def createVariable(variable: Identifier, typ: Type): T = {
    factory(state, domain, env = env + variable)
  }

  /**
   * Removes a variable from the domain (the domains environment)
   */
  override def removeVariable(variable: Identifier): T = {
    state match {
      case None => factory(state, domain, env = env - variable)
      case Some(st) =>
        if (st.getEnvironment.hasVar(variable.getName)) {
          val v: Array[String] = new Array[String](1)
          v.update(0, variable.getName)
          val apronEnv = st.getEnvironment.remove(v)
          factory(Some(st.changeEnvironmentCopy(domain, apronEnv, false)), domain, env = env - variable)
        } else {
          factory(state, domain, env = env - variable)
        }
    }
  }

  // TODO: can be implemented more efficiently, not using the private method as it does unnecessary checks.
  override def removeVariables[I <: Identifier](variables: Set[I]): T = {
    removeVariables(variables.map(_.getName).toArray[String])
  }


  /**
   * Removes several variables at once
   */
  private def removeVariables(variables: Array[String]): T = {
    if (variables.isEmpty) return this
    val varSet = variables.toSet
    state match {
      case None =>
        factory(state, domain, env = env.filter({ x: Identifier => !varSet.contains(x.getName)}))
      case Some(st) =>
        val stEnvironment = st.getEnvironment
        val apronEnv = stEnvironment.remove(variables.filter(stEnvironment.hasVar))
        if (!apronEnv.equals(st.getEnvironment)) {
          factory(Some(st.changeEnvironmentCopy(domain, apronEnv, false)), domain, env = env.filter({ x: Identifier => !varSet.contains(x.getName)}))
        } else {
          factory(state, domain, env = env.filter({ x: Identifier => !varSet.contains(x.getName)}))
        }
    }
  }

  /**
   * This method substitutes variable form the list <code>form</code> to variables form the list <code>to</code>
   * so that in the resulting state a variable with name from(i) has the name to(i).
   * @param from list of old names
   * @param to corresponding list of new names
   * @return state after substituting variables from first list to variables in second list
   */
  override def rename(from: List[Identifier], to: List[Identifier]): T = {
    assert(from.size == to.size)
    assert(from.distinct.equals(from) && to.distinct.equals(to))
    if (from.isEmpty)
      return this
    if (this.isBottom)
      return this.bottom()
    if (!(from.toSet[Identifier] subsetOf this.ids))
      throw new Exception("Identifiers that should be renamed are not present.")
    state match {
      case None =>
        val newEnv = env -- from ++ to
        return factory(state, domain, env = newEnv)
      case Some(s) =>
        val stateVars = s.getEnvironment.getVars
        var index = 0
        var newFrom = List.empty[Identifier]
        var newTo = List.empty[Identifier]
        for (f <- from) {
          if (stateVars.contains(f.getName)) {
            newFrom = newFrom :+ f
            newTo = newTo :+ to(index)
          }
          index = index + 1
        }
        val newState = s.renameCopy(domain, newFrom.map(_.getName).toArray[String], newTo.map(_.getName).toArray[String])
        val newEnv = env -- from ++ to
        factory(Some(newState), domain, env = newEnv)
    }
  }

  override def setToTop(variable: Identifier): T = {
    if (SystemParameters.DEBUG) {
      assert(ids.contains(variable))
    }

    state match {
      case None => this
      case Some(st) =>
        if (st.getEnvironment.hasVar(variable.getName)) {
          factory(Some(st.forgetCopy(domain, variable.getName, false)), domain, env = env)
        } else this
    }
  }

  override def backwardAssign(oldPre: T, variable: Identifier, expr: Expression): T = {
    val varType = expr.typ

    val resultState = if (varType.isBooleanType) {
      val tru = Constant("1", variable.typ, variable.pp)
      val fal = Constant("0", variable.typ, variable.pp)
      val state1 = this.assign(variable, tru)
      val state2 = this.assign(variable, fal)
      return state1.lub(state2)
    } else if (varType.isNumericalType) {
      var post = this.instantiateState()
      var newEnv = env
      var oldApronPre = oldPre.instantiateState()
      if (!post.getEnvironment.equals(oldApronPre.getEnvironment)) {
        val env = unionOfEnvironments(post.getEnvironment, oldApronPre.getEnvironment)
        newEnv = this.env union oldPre.env
        post = post.changeEnvironmentCopy(domain, env, false)
        oldApronPre = oldApronPre.changeEnvironmentCopy(domain, env, false)
      }

      // for non-deterministic expressions, we fall back to simply forgetting the variable
      if (!isDeterministicExpr(expr)) {
        return this.setToTop(variable)
      }

      // handle multiple tree expressions (due to HeapIdSetDomain expressions)
      val texprs = toTexpr1Intern(expr, post.getEnvironment)
      val backwardAssignedState =
        if (texprs.size > 1) {
          var curState = new Abstract1(domain, oldApronPre.getEnvironment, true)
          for (e <- texprs) {
            curState = curState.joinCopy(domain, post.substituteCopy(domain, variable.getName, e, oldApronPre))
          }
          curState
        } else if (texprs.size == 1) {
          post.substituteCopy(domain, variable.getName, texprs.head, oldApronPre)
        } else {
          throw new ApronException("Empty expression set created")
        }

      // LHS summary case
      if (!variable.representsSingleVariable) {
        backwardAssignedState.join(domain, post)
      }

      // RHS summary variables case: Not very precise but sound fallback
      val rightSummaryNodes = (expr.ids filter (!_.representsSingleVariable)) - variable
      if (rightSummaryNodes.nonEmpty) {
        return this.setToTop(variable)
      }

      factory(Some(backwardAssignedState), domain, env = newEnv)
    } else this
    resultState
  }

  def boundVariable(variable: Identifier, itv: apron.Interval): T = {
    val lowBound = isIntervalBoundedBelow(itv)
    val upBound = isIntervalBoundedAbove(itv)

    val (low, up) = intervalBounds(itv)

    val s = this.addAllToApronEnv()
    val si = s.instantiateState()

    if (!lowBound && !upBound) return s

    var constraints = List[apron.Lincons1]()
    if (lowBound) {
      val linexpr = new Linexpr1(si.getEnvironment)
      val lowneg = low.copy()
      lowneg.neg()
      linexpr.setCst(lowneg)
      linexpr.setCoeff(variable.getName, 1)
      constraints = new Lincons1(Lincons1.SUPEQ, linexpr) :: constraints
    }
    if (upBound) {
      val linexpr = new Linexpr1(si.getEnvironment)
      linexpr.setCst(up)
      linexpr.setCoeff(variable.getName, -1)
      constraints = new Lincons1(Lincons1.SUPEQ, linexpr) :: constraints
    }

    val r = si.meetCopy(domain, constraints.toArray)
    factory(Some(r), domain, env = env)
  }

  override def backwardAccess(field: Identifier): T = this

  override def assign(variable: Identifier, expr: Expression): T = {

    if (variable.typ.isBooleanType && !expr.isInstanceOf[Constant]) {

      // For state S, boolean variable X and boolean expression E we compute
      //       S.assume(E).assign(X,1) |_| S.assume(!E).assign(X,0)
      // THIS IS NOT PRECISE BUT GIVES AT LEAST SOME SUPPORT FOR BOOLEANS

      val tru = Constant("1", variable.typ, variable.pp)
      val fal = Constant("0", variable.typ, variable.pp)

      val state1 = assume(expr).assign(variable, tru)
      val state2 = assume(new NegatedBooleanExpression(expr)).assign(variable, fal)

      state1.lub(state2)

    } else if (variable.typ.isNumericalType) {

      if (!ids.contains(variable)) {
        println("It is forbidden to use a non-existing identifier on the left side of an assignment! Going to bottom.")
        return bottom()
      }

      nondeterminismWrapper(expr, this, (someExpr, someState) => {

        // (1) Create left side variable if it does not exist
        var newState = someState.instantiateState()
        if (!newState.getEnvironment.hasVar(variable.getName)) {
          val env = addToEnvironment(newState.getEnvironment, variable.typ, variable.getName)
          newState = newState.changeEnvironmentCopy(domain, env, false)
        }

        // (2) Create right side variables which do not exist
        // Not all declared variables will be represented in APRON. This creates top values for all variables
        // that are currently not declared
        var newEnv = newState.getEnvironment
        for (id <- Normalizer.getIdsForExpression(someExpr)) {

          if (!someState.ids.contains(id)) {
            println("It is forbidden to use a non-existing identifier on the right side of an assignment! Going to bottom.")
            return bottom()
          }

          if (!newEnv.hasVar(id.getName)) {
            newEnv = addToEnvironment(newEnv, id.typ, id.getName)
          }

        }
        if (newEnv != newState.getEnvironment) {
          newState = newState.changeEnvironmentCopy(domain, newEnv, false)
        }

        // (3) Perform a strong update on the state
        val expr = this.toTexpr1Intern(someExpr, newState.getEnvironment)
        val assignedState =
          if (expr.size > 1) {
            var curState = new Abstract1(domain, newState.getEnvironment, true)
            for (e <- expr) {
              curState = curState.joinCopy(domain, newState.assignCopy(domain, variable.getName, e, null))
            }
            curState
          } else if (expr.size == 1) {
            newState.assignCopy(domain, variable.getName, expr.head, null)
          } else {
            throw new ApronException("Empty expression set created")
          }

        // (4) Handling of summary nodes on the left side
        // If variable is a summary node, perform weak update by computing S[x<-v] |_| S
        if (!variable.representsSingleVariable) {
          assignedState.join(domain, newState)
        }

        // (5) Handling of summary nodes on the right side
        // If the right side contains one or more summary nodes, create copies of those values by
        //   (1) Rename all appearing summary nodes in the resulting state (assignedState), call result (materializedState)
        //   (2) Remove the left side from the original state (someState), call result (summaryState)
        //   (3) Compute the least upper bound of materializedState and summaryState
        //   (4) Remove all renamed summary nodes
        // This way, we infer every thing we can about the "materialized" value

        val rightSummaryNodes = (someExpr.ids filter (!_.representsSingleVariable)) - variable
        if (rightSummaryNodes.nonEmpty) {

          val rightSummaryNodesNames = rightSummaryNodes.toList
          val newSummaryNodeNames = rightSummaryNodesNames map { x: Identifier => SimpleApronIdentifier(x.getName + "__TEMP", !x.representsSingleVariable, x.typ, x.pp)}
          val materializedState = factory(Some(assignedState), domain, env = someState.env).rename(rightSummaryNodesNames, newSummaryNodeNames)
          val summaryState = factory(Some(newState), domain, env = someState.env).removeVariable(variable)

          val resultState = materializedState.lub(summaryState)
          resultState.removeVariables(newSummaryNodeNames.map(_.getName).toArray)

        } else {

          factory(Some(assignedState), domain, env = someState.env ++ Normalizer.getIdsForExpression(someExpr) + variable)

        }
      })

    } else this
  }

  override def assume(expr: Expression): T = {

    if (isBottom) return this

    // Check if we assume something about non-numerical values - if so, return
    val ids = Normalizer.getIdsForExpression(expr)
    for (id <- ids) {
      if (!id.typ.isNumericalType) {
        return this
      }
      if (!ids.contains(id)) {
        println("It is forbidden to use a non-existing identifier in an assumption! Going to bottom.")
        return bottom()
      }
    }

    expr match {
      // Boolean variables
      case x: Identifier =>
        // ApronInterface used to assume x != 0 here, which it struggles
        // to handle properly (it is transformed to x < 0 || x > 0).
        // Assuming the value to be equal to 1 is handled much better and
        // is consistent with how ApronInterface treats boolean literals.
        assume(BinaryArithmeticExpression(x, Constant("1", x.typ, x.pp), ArithmeticOperator.==))
      case NegatedBooleanExpression(x: Identifier) =>
        assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.==))

      // Sets of boolean variables
      case ids: MaybeHeapIdSetDomain[_] =>
        if (ids.isBottom || ids.isTop) return this
        Lattice.bigLub(ids.ids map (id => assume(id)))

      // And, Or, De-Morgan, Double Negation
      case BinaryBooleanExpression(left, right, op, typ) => op match {
        case BooleanOperator.&& => assume(left).assume(right)
        case BooleanOperator.|| => assume(left).lub(assume(right))
      }
      case NegatedBooleanExpression(NegatedBooleanExpression(x)) =>
        assume(x)
      case NegatedBooleanExpression(BinaryBooleanExpression(left, right, op, typ)) =>
        val nl = NegatedBooleanExpression(left)
        val nr = NegatedBooleanExpression(right)
        val nop = op match {
          case BooleanOperator.&& => BooleanOperator.||
          case BooleanOperator.|| => BooleanOperator.&&
        }
        assume(BinaryBooleanExpression(nl, nr, nop, typ))

      // APRON fails to resolve !(a = b) and a != b. Instead, we have to specify a < b || a > b

      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, ArithmeticOperator.==, typ)) =>
        val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>, typ)
        val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<, typ)
        assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||, typ))
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=, typ) =>
        val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>, typ)
        val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<, typ)
        assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||, typ))

      case _ =>

        // Assume the expression
        summaryNodeWrapper(expr, this, (someExpr1, someState1) => {
          nondeterminismWrapper(someExpr1, someState1, (someExpr2, someState2) => {

            val expIds = Normalizer.getIdsForExpression(someExpr2)
            var tmp = someState2.instantiateState()
            var expEnv = new Environment()
            for (id <- expIds) {
              expEnv = addToEnvironment(expEnv, id.typ, id.getName)
            }
            val unionEnv = unionOfEnvironments(tmp.getEnvironment, expEnv)
            if (!unionEnv.isIncluded(tmp.getEnvironment)) {
              tmp = tmp.changeEnvironmentCopy(domain, unionEnv, false)
            }

            this.toTcons1(someExpr2, unionEnv) match {

              case x :: xs =>
                var result = tmp.meetCopy(domain, x)
                for (xMore <- xs) {
                  result = result.joinCopy(domain, tmp.meetCopy(domain, xMore))
                }
                factory(Some(result), domain, env = someState2.env ++ expIds)

              case Nil => throw new ApronException("empty set of constraints generated")

            }
          })
        })
    }
  }

  override def merge(r: Replacement): T = {

    // 1st trivial case: empty replacement, no modification
    if (r.isEmpty()) return this

    // 2nd trivial case: empty apron state, only remove / add variables
    if (state == None) {
      val newEnv = env -- r.value.map(_._1).flatten ++ r.value.map(_._2).flatten
      return factory(None, domain, isPureBottom, newEnv)
    }

    // 3rd trivial case: A simple remove
    if (r.isPureRemoving || (r.value.size == 1 && r.value.head._2.size == 0)) {
      return this.removeVariables(r.value.map(_._1.map(_.getName).toArray).toArray.flatten)
    }

    // 4th trivial case: A set of simple renames
    lazy val isRenames = r.value.foldLeft(true)((acc, m) => acc && m._1.size == 1 && m._2.size == 1)
    if (r.isPureRenaming || isRenames) {
      val unzipped = r.value.unzip
      val leftSide = unzipped._1.map { x: Set[Identifier] => x.head}.toList
      val rightSide = unzipped._2.map { x: Set[Identifier] => x.head}.toList
      return this.rename(leftSide, rightSide)
    }

    // 5th trivial case: A simple expand
    if (r.isPureExpanding || (r.value.size == 1 && r.value.head._1.size == 1 && r.value.head._2.contains(r.value.head._1.head))) {
      var cur = this.expand(r.value.head._1.head, r.value.head._2 - r.value.head._1.head)
      for ((from, to) <- r.value.tail) {
        val newVars = to -- from
        cur = cur.lub(expand(from.head, newVars))
      }
      return cur
    }



    // Filter out everything that is converting to a summary node -- we can handle this
    val simpleConversions = r.value.filter({ p: ((Set[Identifier], Set[Identifier])) =>
      p._1.size == 1 && p._2.size == 1 && p._1.head.getName.equals(p._2.head.getName)
    })
    val nextEnv = env -- simpleConversions.map(_._1).flatten ++ simpleConversions.map(_._2).flatten

    // Construct the rest of the replacement. Finish if we are already done
    val rep = new Replacement(r.value -- simpleConversions.keySet)
    if (rep.isEmpty()) return factory(state, domain, isPureBottom, nextEnv)

    var tempVersion = 0
    val tempVarName = "tempVarStar"
    var postProcessMap = Map.empty[Option[String], Array[String]]
    var foldedStates = Set.empty[T]
    var varsToRemove = Set.empty[String]
    var tempIdentifiers = Set.empty[Identifier]

    for ((from, to) <- rep.value) {
      val toVarsAsString: Array[String] = (for (v <- to) yield v.getName).toArray[String]
      if (from.nonEmpty) {

        val startingState = instantiateState()

        // Create a temporary identifier
        val tempVal = tempVarName + tempVersion
        val tempValIdent = new VariableIdentifier(tempVal)(from.head.typ, from.head.pp)
        tempIdentifiers = tempIdentifiers + tempValIdent
        tempVersion = tempVersion + 1

        // If we have a top variable (not in APRON state) on the left side
        if ((from.map(_.getName) -- startingState.getEnvironment.getVars).nonEmpty) {
          foldedStates = foldedStates + this.createVariable(tempValIdent, from.head.typ).removeVariables(from.map(_.getName).toArray)
          postProcessMap = postProcessMap + (Some(tempVal) -> toVarsAsString)
        } else {
          val fromVarsAsString: Array[String] = (for (v <- from) yield v.getName).toArray[String]
          val presentVariablesToRemove = fromVarsAsString.intersect(startingState.getEnvironment.getVars)
          varsToRemove = varsToRemove ++ fromVarsAsString ++ toVarsAsString
          if (presentVariablesToRemove.size > 0) {
            assert(fromVarsAsString.size > 0, "There should be variables in ``from'' set.")
            var tempState = startingState.expandCopy(domain, presentVariablesToRemove(0), Array(tempVal))
            val toFold = Array(tempVal) ++ presentVariablesToRemove
            tempState = tempState.foldCopy(domain, toFold)
            foldedStates = foldedStates + factory(Some(tempState), domain, env = nextEnv -- from + tempValIdent)
            postProcessMap = postProcessMap + (Some(tempVal) -> toVarsAsString)
          }
        }
      } else {
        postProcessMap = postProcessMap + (None -> toVarsAsString)
        foldedStates = foldedStates + this
      }
    }

    // Join folded states
    var result = this.bottom()
    for (s <- foldedStates) {
      result = result.lub(s.removeVariables(varsToRemove.toArray[String]))
    }

    var resultingState = result.instantiateState()
    for ((tempVar, toVars) <- postProcessMap) {
      tempVar match {
        case Some(t) =>
          if (toVars.size > 0 && resultingState.getEnvironment.hasVar(t)) {
            resultingState = resultingState.expandCopy(domain, t, toVars)
            resultingState = resultingState.changeEnvironmentCopy(domain, resultingState.getEnvironment.remove(Array(t)), false)
          }
        case None => ()
      }
    }

    val newEnvironment = result.ids -- r.value.map(_._1).flatten ++ r.value.map(_._2).flatten -- tempIdentifiers

    // TODO: Hacked in bug fix, but this it should be investigated why it occures.
    // There are cases where merge does not remove from the apron state variables that should be removed, leading to
    // the situation where the set of identifiers in the ApronInterface environment is not a superset of the
    // identifiers in the apron state (Abstract1).
    val idsToRemoveFromState = (resultingState.getEnvironment.getVars.toSet[String] -- newEnvironment.map(_.getName)).toArray[String]
    if (idsToRemoveFromState.nonEmpty) {
      println("ApronInterface.merge: The set of variables in the state is not a subset of variables in the environment. This is a bug in merge, hacked in fix is provided.")
      resultingState.changeEnvironment(domain, resultingState.getEnvironment.remove(idsToRemoveFromState), false)
    }
    // END OF HACKED FIX

    val newInterface = factory(Some(resultingState), domain, env = newEnvironment)
    newInterface
  }

  override def lub(other: T): T = {

    if (this == other)
      return this
    if (isBottom)
      return other
    if (other.isBottom)
      return this
    if (isTop)
      return factory(other.removeVariables(ids.map(_.getName).toArray).state, domain, env = ids ++ other.ids)
    if (other.isTop)
      return factory(removeVariables(other.ids.map(_.getName).toArray).state, domain, env = ids ++ other.ids)

    try {
      // NEW JOIN that supports different environments
      var leftState = instantiateState()
      var rightState = other.instantiateState()

      if (leftState == rightState) return factory(Some(leftState), domain, env = ids ++ other.ids)

      // First we compute the common variables.
      if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
        val commonVariables = ids.intersect(other.ids).map(_.getName)
        val commonRepVarsLeft: Array[String] = leftState.getEnvironment.getVars.filter(commonVariables.contains)
        val commonRepVarsRight: Array[String] = rightState.getEnvironment.getVars.filter(commonVariables.contains)
        // We need to forget the common variables in each state, otherwise we would be unsound
        val forgotLeftState = leftState.forgetCopy(domain, commonRepVarsLeft, false)
        val forgotRightState = rightState.forgetCopy(domain, commonRepVarsRight, false)
        // We assume that variables that are not in the other environment and are in the first environment are treated in the other as botom value.
        val unifiedForgotStates = forgotLeftState.unifyCopy(domain, forgotRightState)
        // The result is then LUB(Uni(uniFS, leftS), Uni(uniFS, rightS))
        leftState = leftState.unifyCopy(domain, unifiedForgotStates)
        rightState = rightState.unifyCopy(domain, unifiedForgotStates)
        // The result state is stored in the leftState in order to avoid creation of new state object.
        leftState.join(domain, rightState)

        factory(Some(leftState), domain, env = ids ++ other.ids)
      } else {
        factory(Some(leftState.joinCopy(domain, rightState)), domain, env = ids ++ other.ids)
      }
    } catch {
      case a: apron.ApronException =>
        throw new ApronException("WARNING: incompatible environments.")
    }
  }

  override def glb(other: T): T = {

    val newEnv: Set[Identifier] = ids intersect other.ids
    if (this == other)
      return this
    if (isPureBottom)
      return this
    if (other.isPureBottom)
      return other
    if (other.isTop)
      return removeVariables((ids -- other.ids).map(_.getName).toArray)
    if (isTop)
      return other.removeVariables((other.ids -- ids).map(_.getName).toArray)

    val leftState = instantiateState()
    val rightState = other.instantiateState()
    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      val leftVars = leftState.getEnvironment.getVars.toSet[String] ++ ids.map(_.getName)
      val rightVars = rightState.getEnvironment.getVars.toSet[String] ++ other.ids.map(_.getName)
      var result = leftState.unifyCopy(domain, rightState)
      val uncommonVariables: Array[String] = (((leftVars union rightVars) intersect result.getEnvironment.getVars.toSet[String]) diff (leftVars intersect rightVars)).toArray[String]
      // We remove the variables taht are not in common. (As they are bottom values in the other state)
      result = result.changeEnvironmentCopy(domain, result.getEnvironment.remove(uncommonVariables), false)
      factory(Some(result), domain, env = newEnv)
    } else {
      factory(Some(leftState.meetCopy(domain, rightState)), domain, env = newEnv)
    }
  }

  override def widening(other: T): T = {

    if (this == other)
      return this
    if (isBottom)
      return other
    if (other.isBottom)
      return this
    if (isTop)
      return factory(other.removeVariables(ids.map(_.getName).toArray).state, domain, env = ids ++ other.ids)
    if (other.isTop)
      return factory(removeVariables(other.ids.map(_.getName).toArray).state, domain, env = ids ++ other.ids)

    val leftState = instantiateState()
    val rightState = other.instantiateState()

    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      val env = unionOfEnvironments(leftState.getEnvironment, rightState.getEnvironment)
      val newLeft = leftState.changeEnvironmentCopy(domain, env, false)
      val newRight = rightState.changeEnvironmentCopy(domain, env, false)
      val res = factory(Some(newLeft.widening(domain, newRight)), domain, env = ids ++ other.ids)
      res
    } else {
      val res = factory(Some(leftState.widening(domain, rightState)), domain, env = ids ++ other.ids)
      res
    }

  }

  override def lessEqual(r: T): Boolean = {

    if (this == r)
      return true
    if (this.isBottom)
      return true
    if (r.isTop)
      return (this.ids -- r.ids).isEmpty
    if (r.isBottom)
      return false
    if (this.isTop)
      return (r.ids -- this.ids).nonEmpty

    val leftState = this.instantiateState()
    val rightState = r.instantiateState()

    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      val env = unionOfEnvironments(leftState.getEnvironment, rightState.getEnvironment)
      val newLeft = leftState.changeEnvironmentCopy(domain, env, false)
      val newRight = rightState.changeEnvironmentCopy(domain, env, false)
      newLeft.isIncluded(domain, newRight)
    } else {
      leftState.isIncluded(domain, rightState)
    }

  }

  override def getStringOfId(id: Identifier): String = {
    if (isBottom) return "⊥ (pure)"
    if (!env.contains(id)) return "⊥"
    if (isTop) return "⊤ (pure)"
    val constraints = this.state.get.toLincons(domain).toList.filter(constraintContains(_, id.getName))
    if (constraints.isEmpty) return "⊤"
    val translator = ApronInterfaceTranslator()(this)
    val exps = constraints.map(translator.translate).flatten.map(ExpPrettyPrinter)
    exps.sorted.mkString("\n")
  }

  override def toString: String = {
    if (isBottom) return "⊥"
    if (isTop) return "Environment: " + env + "\n" + "⊤"

    val exps = ApronInterfaceTranslator()(this).translateAll() map ExpPrettyPrinter
    if (exps.isEmpty) return "⊤"
    "Environment: " + env + "\n" + exps.toList.sorted.mkString("\n")
  }

  def expand(source: Identifier, target: Set[Identifier]): T = {
    if (target.nonEmpty && ids.contains(source)) {
      val newState = state match {
        case None => None
        case Some(x) =>
          if (x.getEnvironment.hasVar(source.getName))
            Some(x.expandCopy(domain, source.getName, target.map(_.getName).toArray))
          else
            Some(x)
      }
      val newEnv = env ++ {
        if (env contains source) target else Set.empty[Identifier]
      }
      return factory(newState, domain, isPureBottom, newEnv)
    } else return this
  }

  private def constraintContains(c: Lincons1, variable: String): Boolean = {
    for (term <- c.getLinterms)
      if (term.getVariable.equals(variable) && !term.getCoefficient.toString.equals("0"))
        return true
    false
  }

  private def isDeterministicExpr(expr: Expression): Boolean = {
    expr match {
      case BinaryArithmeticExpression(left, right, op, typ) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case BinaryBooleanExpression(left, right, op, typ) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case ReferenceComparisonExpression(left, right, op, typ) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case NegatedBooleanExpression(left) =>
        isDeterministicExpr(left)
      case UnaryArithmeticExpression(left, op, ret) =>
        isDeterministicExpr(left)
      case BinaryNondeterministicExpression(left, right, op, returnType) =>
        false
      case x: Expression => true
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
        val identifier = new VariableIdentifier(label)(expr.typ, expr.pp)
        (identifier, varL ::: varR ::: List((identifier, BinaryNondeterministicExpression(expL, expR, op, returnType))))
      case x: Expression => (x, Nil)
    }
  }

  private def nondeterminismWrapper(expr: Expression, state: T, someFunc: (Expression, T) => T): T = {

    // Extract all non-deterministic expressions and store them in temporary variables
    var newState = state
    val (newExpr, tempAssigns) = removeNondeterminism("tmp", expr)

    // Add all temporary variables
    for ((id, _) <- tempAssigns) {
      newState = newState.createVariable(id)
    }

    for ((id, ndExpr) <- tempAssigns) {
      ndExpr.op match {
        case NondeterministicOperator.or =>
          val newStateLeft = newState.assign(id, ndExpr.left)
          val newStateRight = newState.assign(id, ndExpr.right)
          newState = newStateLeft.lub(newStateRight)
        case NondeterministicOperator.to =>
          newState = newState.
            assume(BinaryArithmeticExpression(id, ndExpr.left, ArithmeticOperator.>=, ndExpr.typ)).
            assume(BinaryArithmeticExpression(id, ndExpr.right, ArithmeticOperator.<=, ndExpr.typ))
      }
    }

    newState = someFunc(newExpr, newState)

    // Remove all temporary variables
    for ((id, _) <- tempAssigns) {
      newState = newState.removeVariable(id)
    }

    newState
  }

  /**
   * This corresponds to the summarization technqiue
   * Gopan et al. "Numeric Domains with Summarized Dimensions"
   *
   * Materializes all summary nodes in Expression. Then calls someFunc.
   * Then, performs a clean-up.
   *
   * @param expr The expression to be expanded
   * @param state The current state
   * @param someFunc The function to be executed
   * @return The modified state, the set of temporary variables
   */
  private def summaryNodeWrapper(expr: Expression, state: T, someFunc: (Expression, T) => T): T = {

    if (expr.ids.filter(x => !x.representsSingleVariable).nonEmpty) {

      // We have a summary node.

      var temporaryCounter = 0
      val expandTemporaryVariables: Replacement = new Replacement(isPureExpanding = true)
      val removeTemporaryVariables: Replacement = new Replacement(isPureRemoving = true)

      val transformedExpression = expr.transform({
        case x: Identifier =>
          if (!x.representsSingleVariable) {
            val newIdentifier = SimpleApronIdentifier(x.getName + "__TMP" + temporaryCounter, summary = false, x.typ, x.pp)
            temporaryCounter = temporaryCounter + 1
            expandTemporaryVariables.value(Set(x)) =
              expandTemporaryVariables.value.get(Set(x)) match {
                case Some(s) => s ++ Set(x, newIdentifier)
                case None => Set(x, newIdentifier)
              }
            removeTemporaryVariables.value += (Set(newIdentifier.asInstanceOf[Identifier]) -> Set.empty[Identifier])
            newIdentifier
          } else x
        case x: Expression => x
      })
      val preState = state.merge(expandTemporaryVariables)
      val postState = someFunc(transformedExpression, preState)
      val cleanPostState = postState.merge(removeTemporaryVariables)

      cleanPostState

    } else {

      someFunc(expr, state)

    }

  }

  private def toTexpr1Intern(e: Expression, env: apron.Environment): List[Texpr1Intern] = {
    val e1 = this.toTexpr1Node(e)
    for (e <- e1) yield new Texpr1Intern(env, e)
  }

  private def topExpression(): Texpr1Node = new Texpr1CstNode(topInterval)

  private def topConstraint(env: Environment): Tcons1 = {
    new Tcons1(Tcons1.EQ, new Texpr1Intern(env, new Texpr1CstNode(new DoubleScalar(0)))) // always true
  }

  private def bottomConstraint(env: Environment): Tcons1 = {
    new Tcons1(Tcons1.EQ, new Texpr1Intern(env, new Texpr1CstNode(new DoubleScalar(1)))) // always false
  }

  private def toTexpr1Node(e: Expression): List[Texpr1Node] = e match {
    case Constant("posinfty", typ, p) =>
      val a = new DoubleScalar()
      a.setInfty(1)
      List(new Texpr1CstNode(a))
    case Constant("neginfty", typ, p) =>
      val a = new DoubleScalar()
      a.setInfty(-1)
      List(new Texpr1CstNode(a))
    case x: Identifier => List(new Texpr1VarNode(x.getName))
    case setId: HeapIdSetDomain[Identifier] =>
      if (setId.isTop || setId.isBottom) List(topExpression())
      else
        (setId.value map {
          x: Identifier => new Texpr1VarNode(x.getName)
        }).toList
    case Constant(v, typ, p) =>
      if (typ.isNumericalType)
        v match {
          case "true" => List(new Texpr1CstNode(new DoubleScalar(1)))
          case "false" => List(new Texpr1CstNode(new DoubleScalar(0)))
          case _ => List(new Texpr1CstNode(new DoubleScalar(java.lang.Double.parseDouble(v))))
        }
      else List(topExpression())
    case BinaryArithmeticExpression(left, right, op, typ) =>
      for (l <- this.toTexpr1Node(left); r <- this.toTexpr1Node(right)) yield {
        this.convertArithmeticOperator(op) match {
          case Some(x) => new Texpr1BinNode(x, l, r)
          case None => topExpression()
        }
      }
    case BinaryBooleanExpression(left, right, op, typ) =>
      for (l <- this.toTexpr1Node(left); r <- this.toTexpr1Node(right)) yield {
        this.convertBooleanOperator(op) match {
          case Some(x) => new Texpr1BinNode(x, l, r)
          case None => topExpression()
        }
      }
    case UnaryArithmeticExpression(left, op, typ) =>
      op match {
        case ArithmeticOperator.- =>
          for (l <- this.toTexpr1Node(left)) yield {
            new Texpr1UnNode(Texpr1UnNode.OP_NEG, l)
          }
      }
    case _ =>
      // Naturally, not all expressions will be supported by the numerical domain
      //println("Unhandled expression type in APRON interface (returning top expression): "+e)
      List(topExpression())
  }

  private def convertArithmeticOperator(op: ArithmeticOperator.Value): Option[Int] = op match {
    case ArithmeticOperator.+ => Some(Texpr1BinNode.OP_ADD)
    case ArithmeticOperator.- => Some(Texpr1BinNode.OP_SUB)
    case ArithmeticOperator./ => Some(Texpr1BinNode.OP_DIV)
    case ArithmeticOperator.* => Some(Texpr1BinNode.OP_MUL)
    case ArithmeticOperator.% => Some(Texpr1BinNode.OP_MOD)
    case _ => None
  }

  /** used when we assign a boolean value, e.g. flag = flag1 && flag2 */
  private def convertBooleanOperator(op: BooleanOperator.Value): Option[Int] = op match {
    case BooleanOperator.&& => Some(Texpr1BinNode.OP_MUL)
    case BooleanOperator.|| => Some(Texpr1BinNode.OP_ADD)
    case _ => None
  }

  private def toTcons1(e: Expression, env: Environment): List[Tcons1] = e match {
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
      val expr1 = this.toTexpr1Node(new BinaryArithmeticExpression(localLeft, localRight, ArithmeticOperator.-, localLeft.typ))
      localOp match {
        case ArithmeticOperator.>= => for (e <- expr1) yield new Tcons1(env, Tcons1.SUPEQ, e)
        case ArithmeticOperator.== => for (e <- expr1) yield new Tcons1(env, Tcons1.EQ, e)
        case ArithmeticOperator.!= => for (e <- expr1) yield new Tcons1(env, Tcons1.DISEQ, e)
        case ArithmeticOperator.> =>

          if (domain.isInstanceOf[Octagon]) {

            // Some domains, like Octagons have trouble representing >. In floating point mode, they will replace
            // A > B by A >= B, which can cause massive imprecision. We replace this by A >= B + EPSILON, where
            // EPSILON should be the smallest representable number. (actually, we are generating A - B - EPSILON >= 0)

            val sExpr1 = new BinaryArithmeticExpression(localLeft, localRight, ArithmeticOperator.-, localLeft.typ)
            val sExpr2 = new BinaryArithmeticExpression(sExpr1, Constant(NumericalAnalysisConstants.epsilon.toString, sExpr1.typ, sExpr1.pp), ArithmeticOperator.-, sExpr1.typ)
            for (e <- this.toTexpr1Node(sExpr2)) yield {
              new Tcons1(env, Tcons1.SUPEQ, e)
            }

          } else {
            for (e <- expr1) yield new Tcons1(env, Tcons1.SUP, e)
          }

      }
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
      toTcons1(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ), env)
    case NegatedBooleanExpression(NegatedBooleanExpression(x)) => toTcons1(x, env)
    case NegatedBooleanExpression(x) =>
      toTcons1(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.==, x.typ), env)
    case x: Expression =>
      toTcons1(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.!=, x.typ), env)
    case _ =>
      println("Unhandled constraint type in APRON interface (returning top constraint): "+e)
      List(topConstraint(env))
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

  def addAllToApronEnv(): T = {
    val state1 = this.instantiateState()
    var apronEnv = state1.getEnvironment
    for (e <- env) {
      apronEnv = addToEnvironment(apronEnv, e.typ, e.getName)
    }
    val r = state1.changeEnvironmentCopy(domain, apronEnv, false)
    factory(Some(r), domain, isPureBottom, env = env)
  }

  private def addToEnvironment(env: Environment, typ: Type, varName: String): Environment = {
    if (!env.hasVar(varName)) {
      val v = new Array[String](1)
      v(0) = varName
      if (typ.isFloatingPointType)
        env.add(new Array[String](0), v)
      else
        env.add(v, new Array[String](0))
    } else env
  }
}


class ApronAnalysis extends SemanticAnalysis[ApronInterface.Default] {

  var domain: Manager = null

  def getLabel(): String = "Apron numerical analysis"

  def parameters(): List[(String, Any)] = List(("Domain", List("Interval", "PPL", "Octagons", "Polka", "Linear equalities")))

  def setParameter(label: String, value: Any) {
    label match {
      case "Domain" => value match {
        case "Interval" => domain = new Box()
        case "PPL" => domain = new Polka(false) //new PplPoly(false); FIXIT: Change back to PPL
        case "Octagons" => domain = new OptOctagon()
        case "Polka" => domain = new Polka(false)
        case "Linear equalities" => domain = new PolkaEq()
      }
    }
  }

  def reset() {}

  def getInitialState(): ApronInterface.Default = ApronInterface.Default(None, domain, env = Set.empty).top()

  def getProperties: List[Property] = List(
    new SingleStatementProperty(DivisionByZero),
    new SingleStatementProperty(new LowerBoundedValue("y", 0)),
    new SingleStatementProperty(new BoundedValue("y", -4, 4))
  )

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil

}

class ApronException(s: String) extends Exception(s)

case class SimpleApronIdentifier(
                                  name: String,
                                  summary: Boolean,
                                  typ: Type,
                                  pp: ProgramPoint)
  extends HeapIdentifier[SimpleApronIdentifier] {

  def getName: String = name

  def representsSingleVariable: Boolean = !summary

  def getField: Option[String] = None

  override def toString: String = name
}
