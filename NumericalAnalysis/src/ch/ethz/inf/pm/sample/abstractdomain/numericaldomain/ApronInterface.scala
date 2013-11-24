package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._

import apron._
import ch.ethz.inf.pm.sample.abstractdomain.ReferenceComparisonExpression
import ch.ethz.inf.pm.sample.abstractdomain.UnaryArithmeticExpression
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.BinaryBooleanExpression
import ch.ethz.inf.pm.sample.abstractdomain.NegatedBooleanExpression
import ch.ethz.inf.pm.sample.abstractdomain.BinaryNondeterministicExpression

class ApronInterface( val state: Option[Abstract1],
                      val domain: Manager,
                      isPureBottom:Boolean = false,
                      env: Set[Identifier]
                      ) extends RelationalNumericalDomain[ApronInterface] {

  override def factory(): ApronInterface = {
    top()
  }

  override def top(): ApronInterface = {
    new ApronInterface(None, domain, env = Set.empty)
  }

  override def bottom(): ApronInterface = {
    new ApronInterface(None, domain, isPureBottom = true, env = Set.empty)
  }

  def isBottom: Boolean = {
    if (isPureBottom) return true
    state match {
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
  def instantiateState():Abstract1 = {

    state match {
      case Some(s) => s
      case None =>
        if (isPureBottom) {
          new Abstract1(domain,new Environment(),true)
        } else {
          new Abstract1(domain,new Environment())
        }
    }

  }

  def copyState():Abstract1 = new Abstract1(domain,instantiateState())

  def getIds(): Set[Identifier] = env

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
  override def createVariable(variable: Identifier, typ: Type): ApronInterface = {
    new ApronInterface(state, domain, env = env + variable)
  }

  /**
   * Removes a variable from the domain (the domains environment)
   */
  override def removeVariable(variable: Identifier): ApronInterface = {
    state match {
      case None => new ApronInterface(state, domain, env = env - variable)
      case Some(st) =>
        if (st.getEnvironment.hasVar(variable.getName())) {
          val v: Array[String] = new Array[String](1)
          v.update(0, variable.getName())
          val apronEnv = st.getEnvironment.remove(v)
          new ApronInterface(Some(st.changeEnvironmentCopy(domain, apronEnv, false)), domain, env = env - variable)
        } else {
          new ApronInterface(state, domain, env = env - variable)
        }
    }
  }

  /**
   * Removes several variables at once
   */
  def removeVariables(variables: Array[String]): ApronInterface = {
    if (variables.isEmpty) return this
    val varSet = variables.toSet
    state match {
      case None =>
        new ApronInterface(state, domain, env = env.filter({ x:Identifier => !varSet.contains(x.toString) }))
      case Some(st) =>
        val apronEnv = st.getEnvironment.remove(variables.filter({ x:String => st.getEnvironment.hasVar(x) }))
        if (!apronEnv.equals(st.getEnvironment)) {
          new ApronInterface(Some(st.changeEnvironmentCopy(domain, apronEnv, false)), domain, env = env.filter({ x:Identifier => !varSet.contains(x.toString) }))
        } else {
          new ApronInterface(state, domain, env = env.filter({ x:Identifier => !varSet.contains(x.toString) }))
        }
    }
  }

  override def setToTop(variable: Identifier): ApronInterface = {
    state match {
      case None => this
      case Some(st) =>
        if (st.getEnvironment.hasVar(variable.getName())) {
          new ApronInterface(Some(st.forgetCopy(domain, variable.getName(), false)), domain, env = env)
        } else this
    }
  }


  override def assign(variable: Identifier, expr: Expression): ApronInterface = {

    if (variable.getType().isBooleanType() && !expr.isInstanceOf[Constant]) {

      // For state S, boolean variable X and boolean expression E we compute
      //       S.assume(E).assign(X,1) |_| S.assume(!E).assign(X,0)
      // THIS IS NOT PRECISE BUT GIVES AT LEAST SOME SUPPORT FOR BOOLEANS

      val tru = Constant("1",variable.getType(),variable.getProgramPoint())
      val fal = Constant("0",variable.getType(),variable.getProgramPoint())

      val state1 = assume(expr).assign(variable,tru)
      val state2 = assume(new NegatedBooleanExpression(expr)).assign(variable,fal)

      lub(state1,state2)

    } else if (variable.getType().isNumericalType()) {

      //if (!getIds().contains(variable)) {
      //  println("It is forbidden to use a non-existing identifier on the left side of an assignment! Going to bottom.")
      //  return bottom()
      //}

      nondeterminismWrapper(expr, this, (someExpr, someState) => {

        // (1) Create left side variable if it does not exist
        var newState = someState.instantiateState()
        if (!newState.getEnvironment.hasVar(variable.getName())) {
          val env = addToEnvironment(newState.getEnvironment, variable.getType(), variable.getName())
          newState = newState.changeEnvironmentCopy(domain, env, false)
        }

        // (2) Create right side variables which do not exist
        // Not all declared variables will be represented in APRON. This creates top values for all variables
        // that are currently not declared
        var newEnv = newState.getEnvironment
        for (id <- Normalizer.getIdsForExpression(someExpr)) {

          //if (!someState.getIds().contains(id)) {
          //  println("It is forbidden to use a non-existing identifier on the right side of an assignment! Going to bottom.")
          //  return bottom()
          //}

          if (!newEnv.hasVar(id.getName())) {
            newEnv = addToEnvironment(newEnv, id.getType(), id.getName())
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
              curState = curState.joinCopy(domain, newState.assignCopy(domain, variable.getName(), e, null))
            }
            curState
          } else if (expr.size == 1) {
            newState.assignCopy(domain, variable.getName(), expr.head, null)
          } else {
            throw new ApronException("Empty expression set created")
          }

        // (4) Handling of summary nodes on the right side
        // If variable is a summary node, perform weak update by computing S[x<-v] |_| S
        if (!variable.representSingleVariable()) {
          assignedState.join(domain,newState)
        }

        // (5) Handling of summary nodes on the left side
        // If the right side contains one or more summary nodes, create copies of those values by
        //   (1) Rename all appearing summary nodes in the resulting state (assignedState), call result (materializedState)
        //   (2) Remove the left side from the original state (someState), call result (summaryState)
        //   (3) Compute the least upper bound of materializedState and summaryState
        //   (4) Remove all renamed summary nodes
        // This way, we infer every thing we can about the "materialized" value

        val rightSummaryNodes = (someExpr.identifiers() filter ( !_.representSingleVariable() )) - variable
        if (!rightSummaryNodes.isEmpty) {

          val rightSummaryNodesNames = (rightSummaryNodes map (_.toString)).toArray
          val newSummaryNodeNames = rightSummaryNodesNames map (_ + "__TEMP")
          val materializedState = assignedState.renameCopy(domain,rightSummaryNodesNames,newSummaryNodeNames)

          val summaryState = new ApronInterface(Some(newState),domain, env = env).removeVariable(variable).instantiateState()

          val resultState = lub(
            new ApronInterface(Some(materializedState),domain, env = env),
            new ApronInterface(Some(summaryState),domain, env = env))
          resultState.removeVariables(newSummaryNodeNames)

        } else {

          new ApronInterface(Some(assignedState),domain, env = env)

        }
      })

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
      //if (!getIds().contains(id)) {
      //  println("It is forbidden to use a non-existing identifier in an assumption! Going to bottom.")
      //  return bottom()
      //}
    }

    // Check if we just assume if something is invalid - we dont know that here
    // TODO: Filter everything with valid or invalid
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

      // And, Or, De-Morgan, Double Negation
      case BinaryBooleanExpression(left, right, op, typ) => op match {
        case BooleanOperator.&& => assume(left).assume(right)
        case BooleanOperator.|| => lub(assume(left), assume(right))
      }
      case NegatedBooleanExpression(NegatedBooleanExpression(x)) => {
        assume(x)
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

      // APRON fails to resolve !(a = b) and a != b. Instead, we have to specify a < b || a > b

      case NegatedBooleanExpression(BinaryArithmeticExpression(left,Constant("invalid",ctyp,cpp),ArithmeticOperator.==,typ)) =>
        assume(BinaryArithmeticExpression(left,Constant("valid",ctyp,cpp),ArithmeticOperator.==,typ))
      case BinaryArithmeticExpression(left,Constant("invalid",ctyp,cpp),ArithmeticOperator.!=,typ) =>
        assume(BinaryArithmeticExpression(left,Constant("valid",ctyp,cpp),ArithmeticOperator.==,typ))
      case NegatedBooleanExpression(BinaryArithmeticExpression(left,Constant("valid",ctyp,cpp),ArithmeticOperator.==,typ)) =>
        assume(BinaryArithmeticExpression(left,Constant("invalid",ctyp,cpp),ArithmeticOperator.==,typ))
      case BinaryArithmeticExpression(left,Constant("valid",ctyp,cpp),ArithmeticOperator.!=,typ) =>
        assume(BinaryArithmeticExpression(left,Constant("invalid",ctyp,cpp),ArithmeticOperator.==,typ))

      case NegatedBooleanExpression(BinaryArithmeticExpression(Constant("invalid",ctyp,cpp),right,ArithmeticOperator.==,typ)) =>
        assume(BinaryArithmeticExpression(Constant("valid",ctyp,cpp),right,ArithmeticOperator.==,typ))
      case BinaryArithmeticExpression(Constant("invalid",ctyp,cpp),right,ArithmeticOperator.!=,typ) =>
        assume(BinaryArithmeticExpression(Constant("valid",ctyp,cpp),right,ArithmeticOperator.==,typ))
      case NegatedBooleanExpression(BinaryArithmeticExpression(Constant("valid",ctyp,cpp),right,ArithmeticOperator.==,typ)) =>
        assume(BinaryArithmeticExpression(Constant("invalid",ctyp,cpp),right,ArithmeticOperator.==,typ))
      case BinaryArithmeticExpression(Constant("valid",ctyp,cpp),right,ArithmeticOperator.!=,typ) =>
        assume(BinaryArithmeticExpression(Constant("invalid",ctyp,cpp),right,ArithmeticOperator.==,typ))

      case NegatedBooleanExpression(BinaryArithmeticExpression(left,right,ArithmeticOperator.==,typ)) =>
        val newLeft = BinaryArithmeticExpression(left,right,ArithmeticOperator.>,typ)
        val newRight = BinaryArithmeticExpression(left,right,ArithmeticOperator.<,typ)
        assume(BinaryBooleanExpression(newLeft,newRight,BooleanOperator.||,typ))
      case BinaryArithmeticExpression(left,right,ArithmeticOperator.!=,typ) =>
        val newLeft = BinaryArithmeticExpression(left,right,ArithmeticOperator.>,typ)
        val newRight = BinaryArithmeticExpression(left,right,ArithmeticOperator.<,typ)
        assume(BinaryBooleanExpression(newLeft,newRight,BooleanOperator.||,typ))

      case _ => {

        // Assume the expression
        summaryNodeWrapper(expr, this, (someExpr1, someState1) => {
          nondeterminismWrapper(someExpr1, someState1, (someExpr2, someState2) => {

            var tmp = someState2.instantiateState()
            var expEnv = new Environment()
            for (id <- Normalizer.getIdsForExpression(someExpr2)) {
              expEnv = addToEnvironment(expEnv, id.getType(), id.getName())
            }
            val unionEnv = unionOfEnvironments(tmp.getEnvironment, expEnv)
            if (!unionEnv.isIncluded(tmp.getEnvironment)) {
              tmp = tmp.changeEnvironmentCopy(domain, unionEnv, false)
            }

            this.toTcons1(someExpr2, unionEnv) match {

              case x :: xs =>
                var result = tmp.meetCopy(domain,x)
                for (xMore <- xs) {
                  result = result.joinCopy(domain,tmp.meetCopy(domain,xMore))
                }
                new ApronInterface(Some(result),domain,env = env)

              case Nil => throw new ApronException("empty set of constraints generated")

            }
          })
        })
      }
    }
  }

  override def merge(r: Replacement): ApronInterface = {

    // 1st trivial case: empty replacement, no modification
    if (r.isEmpty()) return this

    // 2nd trivial case: empty apron state, only remove / add variables
    if (state == None) {
      val newEnv = env -- r.value.map(_._1).flatten ++ r.value.map(_._2).flatten
      return new ApronInterface(None,domain,isPureBottom,newEnv)
    }

    // 3rd trivial case: A simple remove
    if (r.isPureRemoving || (r.value.size == 1 && r.value.head._2.size == 0)) {
      return this.removeVariables(r.value.map(_._1.map(_.toString).toArray).toArray.flatten)
    }

    // 4th trivial case: A simple expand
    if (r.isPureExpanding || (r.value.size == 1 && r.value.head._1.size == 1 && r.value.head._2.contains(r.value.head._1.head))) {
      var cur = this.expand(r.value.head._1.head,r.value.head._2 - r.value.head._1.head)
      for ((from,to) <- r.value.tail) {
        val newVars = to -- from
        cur = cur.lub(cur,this.expand(from.head,newVars))
      }
      return cur
    }

    // Filter out everything that is converting to a summary node -- we can handle this
    val simpleConversions = r.value.filter ( { p:((Set[Identifier],Set[Identifier])) =>
      p._1.size == 1 && p._2.size == 1 && p._1.head.toString.equals(p._2.head.toString)
    })
    val nextEnv = env -- simpleConversions.map(_._1).flatten ++ simpleConversions.map(_._2).flatten

    // Construct the rest of the replacement. Finish if we are already done
    val rep = new Replacement(r.value -- simpleConversions.keySet)
    if (rep.isEmpty()) return new ApronInterface(state,domain,isPureBottom,nextEnv)

    var tempVersion = 0
    val tempVarName = "tempVarStar"
    var postProcessMap = Map.empty[Option[String], Array[String]]
    var foldedStates = Set.empty[ApronInterface]
    var varsToRemove = Set.empty[String]
    var tempIdentifiers = Set.empty[Identifier]

    for ((from,to) <- rep.value) {
      val toVarsAsString: Array[String] = (for (v <- to) yield v.getName()).toArray[String]
      if(!from.isEmpty) {

        val startingState = instantiateState()

        // Create a temporary identifier
        val tempVal = tempVarName + tempVersion
        val tempValIdent = new VariableIdentifier(tempVal, from.head.getType(), from.head.getProgramPoint(), new EmptyScopeIdentifier)
        tempIdentifiers = tempIdentifiers + tempValIdent
        tempVersion = tempVersion + 1

        // If we have a top variable (not in APRON state) on the left side
        if (!(from.map(_.getName()) -- startingState.getEnvironment.getVars).isEmpty) {
          foldedStates = foldedStates + this.createVariable(tempValIdent,from.head.getType()).removeVariables(from.map(_.getName()).toArray)
          postProcessMap = postProcessMap + (Some(tempVal) -> toVarsAsString)
        } else {
          val fromVarsAsString: Array[String] = (for (v <- from) yield v.getName()).toArray[String]
          val presentVariablesToRemove = fromVarsAsString.intersect(startingState.getEnvironment.getVars)
          varsToRemove = varsToRemove ++ fromVarsAsString ++ toVarsAsString
          if (presentVariablesToRemove.size > 0) {
            assert(fromVarsAsString.size > 0, "There should be variables in ``from'' set.")
            var tempState = startingState.expandCopy(domain,presentVariablesToRemove(0), Array(tempVal))
            val toFold = Array(tempVal) ++ presentVariablesToRemove
            tempState = tempState.foldCopy(domain, toFold)
            foldedStates = foldedStates + new ApronInterface(Some(tempState), domain, env = nextEnv -- from + tempValIdent)
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
      result = result.lub(result, s.removeVariables(varsToRemove.toArray[String]))
    }

    var resultingState = result.instantiateState()
    for ((tempVar, toVars) <- postProcessMap) {
      tempVar match {
        case Some(t) => {
          if (toVars.size > 0 && resultingState.getEnvironment.hasVar(t)) {
            resultingState = resultingState.expandCopy(domain, t, toVars)
            resultingState = resultingState.changeEnvironmentCopy(domain, resultingState.getEnvironment.remove(Array(t)), false)
          }
        }
        case None => ()
      }
    }

    val newEnvironment = result.getIds() -- r.value.map(_._1).flatten ++ r.value.map(_._2).flatten -- tempIdentifiers
    val newInterface = new ApronInterface(Some(resultingState), domain, env = newEnvironment)
    newInterface
  }

  override def lub(left: ApronInterface, right: ApronInterface): ApronInterface = {

    if (left == right)
      return left
    if (left.isBottom)
      return right
    if (right.isBottom)
      return left
    if (left.isTop)
      return new ApronInterface(right.removeVariables(left.getIds().map(_.toString).toArray).state, domain, env = left.getIds() ++ right.getIds())
    if (right.isTop)
      return new ApronInterface(left.removeVariables(right.getIds().map(_.toString).toArray).state, domain, env = left.getIds() ++ right.getIds())

    try {
      // NEW JOIN that supports different environments
      var leftState = left.instantiateState()
      var rightState = right.instantiateState()

      if (leftState == rightState) return new ApronInterface(Some(leftState),domain,env = left.getIds() ++ right.getIds())

      // First we compute the common variables.
      if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
        val commonVariables = left.getIds().intersect(right.getIds()).map(_.toString())
        val commonRepVarsLeft: Array[String] = leftState.getEnvironment.getVars.filter( commonVariables.contains(_) )
        val commonRepVarsRight: Array[String] = rightState.getEnvironment.getVars.filter( commonVariables.contains(_) )
        // We need to forget the common variables in each state, otherwise we would be unsound
        val forgotLeftState = leftState.forgetCopy(domain, commonRepVarsLeft, false)
        val forgotRightState = rightState.forgetCopy(domain, commonRepVarsRight, false)
        // We assume that variables that are not in the other environment and are in the first environment are treated in the other as botom value.
        val unifiedForgotStates = forgotLeftState.unifyCopy(domain, forgotRightState)
        // The result is then LUB(Uni(uniFS, leftS), Uni(uniFS, rightS))
        leftState = leftState.unifyCopy(domain, unifiedForgotStates)
        rightState = rightState.unifyCopy(domain,unifiedForgotStates)
        // The result state is stored in the leftState in order to avoid creation of new state object.
        leftState.join(domain, rightState)

        new ApronInterface(Some(leftState), domain, env = left.getIds() ++ right.getIds())
      } else {
        new ApronInterface(Some(leftState.joinCopy(domain, rightState)), domain, env = left.getIds() ++ right.getIds())
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
    if (right.isTop)
      return new ApronInterface(left.state, domain, env = left.getIds().intersect(right.getIds()))
    if (right.isBottom)
      return right
    if (left.isTop)
      return new ApronInterface(right.state, domain, env = left.getIds().intersect(right.getIds()))

    val leftState = left.instantiateState()
    val rightState = right.instantiateState()
    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      val uncommonVariables: Array[String] = ((left.getIds() -- right.getIds()) ++ (right.getIds() -- left.getIds())).map(_.toString).toArray[String]
      var result = leftState.unifyCopy(domain, rightState)
      // We remove the variables taht are not in common. (As they are bottom values in the other state)
      result = result.changeEnvironmentCopy(domain, leftState.getEnvironment.remove(uncommonVariables), false)
      new ApronInterface(Some(result), domain, env = left.getIds().intersect(right.getIds()))
    } else {
      new ApronInterface(Some(leftState.meetCopy(domain, rightState)), domain, env = left.getIds().intersect(right.getIds()))
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
      return new ApronInterface(right.removeVariables(left.getIds().map(_.toString).toArray).state, domain, env = left.getIds() ++ right.getIds())
    if (right.isTop)
      return new ApronInterface(left.removeVariables(right.getIds().map(_.toString).toArray).state, domain, env = left.getIds() ++ right.getIds())

    val leftState = left.instantiateState()
    val rightState = right.instantiateState()

    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      val env = unionOfEnvironments(leftState.getEnvironment, rightState.getEnvironment)
      val newLeft = leftState.changeEnvironmentCopy(domain, env, false)
      val newRight = rightState.changeEnvironmentCopy(domain, env, false)
      val res = new ApronInterface(Some(newLeft.widening(domain, newRight)), domain, env = left.getIds() ++ right.getIds())
      res
    } else {
      val res = new ApronInterface(Some(leftState.widening(domain, rightState)), domain, env = left.getIds() ++ right.getIds())
      res
    }

  }

  override def lessEqual(r: ApronInterface): Boolean = {

    if (this == r)
      return true
    if (this.isBottom)
      return true
    if (r.isTop)
      return (this.getIds() -- r.getIds()).isEmpty
    if (r.isBottom)
      return false
    if (this.isTop)
      return !(r.getIds() -- this.getIds()).isEmpty

    val leftState = this.instantiateState()
    val rightState = r.instantiateState()

    if (!leftState.getEnvironment.equals(rightState.getEnvironment)) {
      val env = unionOfEnvironments(leftState.getEnvironment, rightState.getEnvironment)
      val newLeft = leftState.changeEnvironmentCopy(domain, env, false)
      val newRight = rightState.changeEnvironmentCopy(domain, env, false)
      newLeft.isIncluded(domain, newRight)
    } else {
      leftState.isIncluded(domain,rightState)
    }

  }

  override def getStringOfId(id: Identifier): String = {
    if (isBottom) return "_|_ (pure)"
    if (!env.contains(id)) return "_|_"
    if (isTop) return "T (pure)"
    val constraints = this.state.get.toLincons(domain).toList.filter(constraintContains(_, id.getName()))
    if(constraints.isEmpty) return "T"
    toString(constraints)
  }

  override def toString: String = {
    if (isBottom) return "_|_"
    if (isTop) return "Environment: "+env+"\n"+"T"

    val constraints = this.state.get.toLincons(domain).toList
    if(constraints.isEmpty) return "T"
    "Environment: "+env+"\n"+toString(this.state.get.toLincons(domain).toList)
  }

  def expand(source:Identifier,target:Set[Identifier]):ApronInterface = {
    if (!target.isEmpty && getIds().contains(source)) {
      val newState = state match {
        case None => None
        case Some(x) =>
          if (x.getEnvironment.hasVar(source.toString))
            Some(x.expandCopy(domain,source.toString,target.map(_.toString).toArray))
          else
            Some(x)
      }
      val newEnv = env ++ target
      return new ApronInterface(newState,domain,isPureBottom,newEnv)
    } else return this
  }

  /**
   * A custom, nicer textual representation.
   */
  private def toString(cons: List[Lincons1]): String = {
    val strs = for (c <- cons) yield {

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

      if (leftR.compare(rightR) > 0) leftR + opToStr(c.getKind) + rightR
      else rightR + opToInvStr(c.getKind) + leftR

    }

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
        val identifier = new VariableIdentifier(label, expr.getType(), expr.getProgramPoint(), EmptyScopeIdentifier())
        (identifier, varL ::: varR ::: List((identifier, BinaryNondeterministicExpression(expL, expR, op, returnType))))
      case x: Expression => (x, Nil)
    }
  }

  private def nondeterminismWrapper(expr: Expression, state: ApronInterface, someFunc: (Expression, ApronInterface) => ApronInterface): ApronInterface = {

    // Extract all non-deterministic expressions and store them in temporary variables
    var newState = state
    val (newExpr, tempAssigns) = removeNondeterminism("tmp", expr)

    // Add all temporary variables
    for ((id, _) <- tempAssigns) {
      newState = newState.createVariable(id,id.getType())
    }

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
  private def summaryNodeWrapper(expr: Expression, state: ApronInterface, someFunc: (Expression, ApronInterface) => ApronInterface): ApronInterface = {

    if (!expr.identifiers().filter( x => !x.representSingleVariable() ).isEmpty) {

      // We have a summary node.

      var temporaryCounter = 0
      val expandTemporaryVariables: Replacement = new Replacement(isPureExpanding = true)
      val removeTemporaryVariables: Replacement = new Replacement(isPureRemoving = true)

      val transformedExpression = expr.transform({
        x:Expression => x match {
          case x:Identifier =>
            if (!x.representSingleVariable()) {
              val newIdentifier = SimpleApronIdentifier(x.toString + "__TMP" + temporaryCounter, summary = false, x.getType(), x.getProgramPoint())
              temporaryCounter = temporaryCounter + 1
              expandTemporaryVariables.value(Set(x)) =
                expandTemporaryVariables.value.get(Set(x)) match {
                  case Some(s) => s ++ Set(x,newIdentifier)
                  case None => Set(x,newIdentifier)
                }
              removeTemporaryVariables.value += (Set(newIdentifier.asInstanceOf[Identifier]) -> Set.empty[Identifier])
              newIdentifier
            } else x
          case x:Expression => x
        }
      })
      val preState = state.merge(expandTemporaryVariables)
      val postState = someFunc(transformedExpression,preState)
      val cleanPostState = postState.merge(removeTemporaryVariables)

      cleanPostState

    } else {

      someFunc(expr,state)

    }

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
    case Constant("posinfty", typ, p) =>
      val a = new DoubleScalar()
      a.setInfty(1)
      List(new Texpr1CstNode(a))
    case Constant("neginfty", typ, p) =>
      val a = new DoubleScalar()
      a.setInfty(-1)
      List(new Texpr1CstNode(a))
    case x: Identifier => List(new Texpr1VarNode(x.getName()))
    case setId: HeapIdSetDomain[Identifier] =>
      if (setId.isTop || setId.isBottom) List(topExpression())
      else
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
    case _:CollectionContainsExpression => List(topExpression())
    case _ =>
      println("Unhandled expression type in APRON interface (returning top expression): "+e)
      List(topExpression())
  }

  private def convertArithmeticOperator(op: ArithmeticOperator.Value): Option[Int] = op match {
    case ArithmeticOperator.+ => Some(Texpr1BinNode.OP_ADD)
    case ArithmeticOperator.- => Some(Texpr1BinNode.OP_SUB)
    case ArithmeticOperator./ => Some(Texpr1BinNode.OP_DIV)
    case ArithmeticOperator.* => Some(Texpr1BinNode.OP_MUL)
    case _ => None
  }

  /** used when we assign a boolean value, e.g. flag = flag1 && flag2 */
  private def convertBooleanOperator(op: BooleanOperator.Value): Option[Int] = op match {
    case BooleanOperator.&& => Some(Texpr1BinNode.OP_MUL)
    case BooleanOperator.|| => Some(Texpr1BinNode.OP_ADD)
    case _ => None
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
      toTcons1(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op).get, typ), env)
    case NegatedBooleanExpression(NegatedBooleanExpression(x)) => toTcons1(x, env)
    case NegatedBooleanExpression(x) =>
      toTcons1(BinaryArithmeticExpression(x, Constant("0", x.getType(), x.getProgramPoint()), ArithmeticOperator.==, x.getType()), env)
    case x: Expression =>
      toTcons1(BinaryArithmeticExpression(x, Constant("0", x.getType(), x.getProgramPoint()), ArithmeticOperator.!=, x.getType()), env)
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

  private def addToEnvironment(env: Environment, typ: Type, varName: String): Environment = {
    if (!env.hasVar(varName)) {
      val v = new Array[String](1)
      v(0) = varName
      if (typ.isFloatingPointType())
        env.add(new Array[String](0), v)
      else
        env.add(v, new Array[String](0))
    } else env
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

  def getInitialState(): ApronInterface = new ApronInterface(None, domain, env = Set.empty).top()

  def getProperties: List[Property] = List(
    new SingleStatementProperty(DivisionByZero),
    new SingleStatementProperty(new LowerBoundedValue("y", 0)),
    new SingleStatementProperty(new BoundedValue("y", -4, 4))
  )

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil

}

class ApronException(s: String) extends Exception(s)

case class SimpleApronIdentifier(name:String, summary:Boolean, typ:Type, override val pp:ProgramPoint) extends HeapIdentifier[SimpleApronIdentifier](typ,pp) {
  def getName(): String = name
  def representSingleVariable(): Boolean = !summary
  def getField(): Option[String] = None
  override def toString:String = name
  override def identifiers(): Set[Identifier] = Set(this)
}
