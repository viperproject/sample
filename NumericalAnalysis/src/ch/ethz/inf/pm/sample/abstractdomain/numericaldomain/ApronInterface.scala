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
;

class ApronInterface(val state : Abstract1, val domain : Manager) extends RelationalNumericalDomain[ApronInterface] {

  private def idsToArrayOfStrings(set: Set[Identifier]): Array[String] = {
    var stringList = List.empty[String]
    for (v <- set) {
      stringList = v.getName() :: stringList
    }
    return stringList.toArray[String]
  }

  override def merge(r : Replacement) : ApronInterface = {
    if(r.isEmpty) return this;

    /*
     *    ------------- VERSION 3 ---------------
     */
    var idsInDomain : Set[Identifier] = Set.empty
    var idsInCodomain : Set[Identifier] = Set.empty
    var startingState = this.state
    for ((from,to) <- r.value) {
      idsInDomain = idsInDomain.++(from)
      idsInCodomain = idsInCodomain.++(to)
      val idFromNamesList = ListBuffer.empty[String]
      for (idFrom <- from) {
        if (!startingState.getEnvironment.hasVar(idFrom.getName()))
          idFromNamesList.+=(idFrom.getName())
      }
      var newEnv = startingState.getEnvironment.add(idFromNamesList.toArray[String], new Array[String](0))
      startingState = startingState.changeEnvironmentCopy(domain, newEnv, false)
    }
    var result = new Abstract1(domain, startingState.getEnvironment, true)
    for ((from,to) <- r.value) {
      var newState = startingState
      var shallowCopyVars = List.empty[String]
      for (v <- from) {
        val vPrime : Array[String] = new Array[String](1)
			  vPrime.update(0, v.getName()+"$")
        shallowCopyVars = shallowCopyVars.:+(v.getName()+"$")
        newState = newState.expandCopy(domain, v.getName(), vPrime)
      }
      // now we introduce id* into domain with values of one of the ids in "from"
      val idStar = "idStar"
      val aIdStar: Array[String] = new Array[String](1)
			aIdStar.update(0, idStar)
      newState = newState.expandCopy(domain, from.iterator.next().getName(), aIdStar)
      // now we fold shallow copies into idStar
      shallowCopyVars = idStar :: shallowCopyVars
      newState = newState.foldCopy(domain, shallowCopyVars.toArray[String])
      // now we expand id* to variables in "to"
      val idsToStringArray: Array[String] = idsToArrayOfStrings(to)
      newState = newState.expandCopy(domain, idStar, idsToStringArray)
      // now we remove the temoprary variables form the environment
      newState = newState.forgetCopy(domain, aIdStar, false)
      newState = newState.changeEnvironmentCopy(domain, newState.getEnvironment.remove(aIdStar), false)
      // we also need to extend the environment of result
      result = result.expandCopy(domain, from.iterator.next().getName(), idsToStringArray)
      result = result.joinCopy(domain, newState)
      startingState = startingState.expandCopy(domain, from.iterator.next().getName(), idsToStringArray)
    }
    for(id <- idsInDomain.--(idsInCodomain)) {
      result=result.forgetCopy(domain, id.getName(), false)
    }
    // We minimize the environment in order to achieve better performance.
    // This effects assign as there might be variable removed that are still in use.
    result = result.minimizeEnvironmentCopy(domain)
    return new ApronInterface(result, domain)
    /*
     *    ------------- END OF VERSION 3 ---------------
     */


    /*
     *    ------------- VERSION 2 ---------------
     */
//    var idsInDomain : Set[Identifier] = Set.empty
//    var idsInCodomain : Set[Identifier] = Set.empty
//    var expandedState = this.state
//    for ((from,to) <- r.value) {
//      idsInDomain = idsInDomain.++(from)
//      idsInCodomain = idsInCodomain.++(to)
//      val idFromNamesList = ListBuffer.empty[String]
//      for (idFrom <- from) {
//        if (!expandedState.getEnvironment.hasVar(idFrom.getName()))
//          idFromNamesList.+=(idFrom.getName())
//      }
//      var newEnv = expandedState.getEnvironment.add(idFromNamesList.toArray[String], new Array[String](0))
//      expandedState = expandedState.changeEnvironmentCopy(domain, newEnv, false)
//      val idToNamesList = ListBuffer.empty[String]
//      for (idTo <- to) {
//        if (this.state.getEnvironment.getVars.contains(idTo.getName())) {
//          throw new ApronException(idTo.getName() + " already in Env and should be fresh")
//        }
//        if (!expandedState.getEnvironment.hasVar(idTo.getName()))
//          idToNamesList.+=(idTo.getName())
//        else
//          println(idTo.getName() + " is already in the environment")
//      }
//      if (!from.isEmpty) {
//        expandedState = expandedState.expandCopy(domain,from.iterator.next().getName(), idToNamesList.toArray[String])
//      } else {
//        // This means that elements in the set 'to' will be set to Top anyways
//        newEnv = expandedState.getEnvironment.add(idToNamesList.toArray[String], new Array[String](0))
//        expandedState = expandedState.changeEnvironmentCopy(domain, newEnv, false)
//      }
//    }
//    var result = new Abstract1(domain, expandedState.getEnvironment(), true)
//    for(I1 <- r.value.keySet) {
//      for(id1 : Identifier <- r.value.apply(I1)) {
//        for(id2: Identifier <- I1) {
//          val newState = expandedState;
//          val newTexpr1Intern = this.toTexpr1Intern(id1, newState.getEnvironment)
//          val temp = newState.substituteCopy(domain, id2.getName(), newTexpr1Intern, null);
//          result = result.joinCopy(domain, temp)
//        }
//      }
//    }
//    for(id <- idsInDomain.--(idsInCodomain)) {
//      result=result.forgetCopy(domain, id.getName(), false)
//    }
//    return new ApronInterface(result, domain)
    /*
     *    ------------- END OF VERSION 2 ---------------
     */


    /*
     *    ------------- VERSION 1 ---------------
     */
//    var result = new Abstract1(domain, state.getEnvironment(), true)
//		if(! result.isBottom(domain)) throw new ApronException("I'm not able to create a bottom state");
//    var idsInDomain : Set[Identifier] = Set.empty
//    var idsInCodomain : Set[Identifier] = Set.empty
//    for(I1 <- r.value.keySet) {
//      idsInDomain=idsInDomain++I1;
//      for(id1 : Identifier <- r.value.apply(I1)) {
//        idsInCodomain=idsInCodomain+(id1);
//        for(id2: Identifier <- I1) {
//          var newState = state;
//          if(! state.getEnvironment.hasVar(id2.getName()) || ! state.getEnvironment.hasVar(id1.getName())) {
//            newState = this.createVariable(id2, id2.getType()).createVariable(id1, id1.getType()).state;
//          }
//          var resEnv = unionOfEnvironments(newState.getEnvironment, result.getEnvironment);
//          newState = newState.changeEnvironmentCopy(domain, resEnv, false);
//          result = result.changeEnvironmentCopy(domain, resEnv, false);
//          var temp = newState.substituteCopy(domain, id2.getName(), this.toTexpr1Intern(id1, newState.getEnvironment), newState);
//          result = result.joinCopy(domain, temp)
//        }
//      }
//    }
//    for(id <- idsInDomain.--(idsInCodomain)) {
//      result=result.forgetCopy(domain, id.getName(), false);
//    }
//    return new ApronInterface(result, domain)
    /*
     *    ------------- END OF VERSION 1 ---------------
     */
  };

  //TODO
  def getIds : Set[Identifier] = Set.empty[Identifier];

  /**
   * A custom, nicer textual representation.
   */
  def toString(cons:List[Lincons1]):String = {

    val strs = (for (c <- cons) yield {

      val (left,right) = (for (t <- c.getLinterms) yield {
        val coeff = t.getCoefficient
        val vari = t.getVariable
        if (coeff.isEqual(1)) (Some(vari),None)
        else if (coeff.isEqual(-1)) (None,Some("-"+vari))
        else if (coeff.cmp(new DoubleScalar(0))>0) (Some(coeff.toString+vari),None)
        else if (coeff.cmp(new DoubleScalar(0))<0) (None,Some(coeff.toString+vari))
        else (None,None)
      }).unzip

      val const = c.getCst
      val (constL,constR) =
        if (const != null && !const.isZero)
          if (const.cmp(new DoubleScalar(0))>0) (Some(const.toString),None)
          else (None,Some(const.toString))
        else (None,None)

      val (leftX,rightX) = ((constL::left.toList).flatten,(constR::right.toList).flatten)
      val (leftS,rightS) = (leftX.mkString(" + "),rightX.mkString(" + ").replace("-",""))
      val (leftR,rightR) = (if (leftS.isEmpty) "0" else leftS, if (rightS.isEmpty) "0" else rightS )

      if (leftR.compare(rightR) > 0 ) (leftR + opToStr(c.getKind) + rightR)
      else (rightR + opToInvStr(c.getKind) + leftR)

    })

    val tmp = strs.toList.mkString("\n")

    // This is super ugly but helps
    val a = new scala.collection.mutable.HashSet[String]
    for (str <- strs) {
      val other = str.replace("<","?").replace(">","<").replace("?",">")
      if (!a.contains(other)) {
        a += str
      } else {
        val third = str.replace("<","=").replace(">","=").replace("==","=")
        a.remove(other)
        a += third
      }
    }

    a.toList.sorted.mkString("\n")

  }

  private def opToStr(kind:Int):String = {
    kind match {
      case Lincons1.DISEQ => " != "
      case Lincons1.EQ => " = "
      case Lincons1.SUP => " > "
      case Lincons1.SUPEQ => " >= "
    }
  }

  private def opToInvStr(kind:Int):String = {
    kind match {
      case Lincons1.DISEQ => " = "
      case Lincons1.EQ => " != "
      case Lincons1.SUP => " < "
      case Lincons1.SUPEQ => " <= "
    }
  }



	override def getStringOfId (id : Identifier) : String = {
    toString(this.state.toLincons(domain).toList.filter(constraintContains(_,id.getName())))
	}
	
	override def toString() : String = {
    toString(this.state.toLincons(domain).toList)
  }
	
	
	private def constraintContains(c : Lincons1, variable : String) : Boolean = {
		for(term <- c.getLinterms )
			if(term.getVariable().equals(variable) && !term.getCoefficient.toString.equals("0"))
				return true;
		return false;
	}

  /**
   *
   * Adds a new variable to the domain (the domains environmnet). We only add variables with numerical types.
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
   */
	override def createVariable (variable : Identifier, typ : Type) : ApronInterface = {
		if(!state.getEnvironment.hasVar(variable.getName()) && typ.isNumericalType()) {
			val v : Array[String] = new Array[String](1)
			v.update(0, variable.getName())
			val env=state.getEnvironment.add(v, new Array[String](0))
      new ApronInterface(state.changeEnvironmentCopy(domain, env, false), domain)
		} else this
	}

  /**
   *
   * Removes a variable from the domain (the domains environment)
   *
   * @param variable the variable to be removed
   * @return the state after this action
   */
	override def removeVariable(variable : Identifier) : ApronInterface = {
    if (state.getEnvironment.hasVar(variable.getName())) {
      val v : Array[String] = new Array[String](1)
      v.update(0, variable.getName())
      val env = state.getEnvironment.remove(v)
      new ApronInterface(state.changeEnvironmentCopy(domain, env, false), domain)
    } else this
	}
		
	override def setToTop(variable : Identifier) : ApronInterface = {
		val st = state.forgetCopy(domain, variable.getName, false);
		new ApronInterface(st, domain);
	}

  private def removeFloats ( expr: Expression ): Expression = { expr match {
    case Constant(s,t,p) =>
      if(s.contains(".")) {
        try {
          val fl = s.toDouble
          if (fl != Math.floor(fl))
            BinaryNondeterministicExpression(
              Constant(Math.floor(fl).toString,t,p),
              Constant(Math.ceil(fl).toString,t,p),
              NondeterministicOperator.to,t)
          else expr
        } catch {
          case e:NumberFormatException => expr
        }
      } else expr
    case BinaryArithmeticExpression(left,right,op,typ) =>
      BinaryArithmeticExpression(removeFloats(left),removeFloats(right),op,typ)
    case BinaryBooleanExpression(left,right,op,typ) =>
      BinaryBooleanExpression(removeFloats(left),removeFloats(right),op,typ)
    case ReferenceComparisonExpression(left,right,op,typ) =>
      ReferenceComparisonExpression(removeFloats(left),removeFloats(right),op,typ)
    case NegatedBooleanExpression(left) =>
      NegatedBooleanExpression(removeFloats(left))
    case UnaryArithmeticExpression(left,op,ret) =>
      UnaryArithmeticExpression(removeFloats(left),op,ret)
    case BinaryNondeterministicExpression(left,right,op,returnType) =>
      BinaryNondeterministicExpression(removeFloats(left),removeFloats(right),op,returnType)
    case e:Expression => e
  }}

  private def removeNondeterminism ( label:String, expr: Expression ): (Expression, List[(Identifier,BinaryNondeterministicExpression)]) = {
    expr match {
      case BinaryArithmeticExpression(left,right,op,typ) =>
        var (expL,varL) = removeNondeterminism(label+"L",left)
        var (expR,varR) = removeNondeterminism(label+"R",right)
        (BinaryArithmeticExpression(expL,expR,op,typ),varL:::varR)
      case BinaryBooleanExpression(left,right,op,typ) =>
        var (expL,varL) = removeNondeterminism(label+"L",left)
        var (expR,varR) = removeNondeterminism(label+"R",right)
        (BinaryBooleanExpression(expL,expR,op,typ),varL:::varR)
      case ReferenceComparisonExpression(left,right,op,typ) =>
        var (expL,varL) = removeNondeterminism(label+"L",left)
        var (expR,varR) = removeNondeterminism(label+"R",right)
        (ReferenceComparisonExpression(expL,expR,op,typ),varL:::varR)
      case NegatedBooleanExpression(left) =>
        var (expL,varL) = removeNondeterminism(label,left)
        (NegatedBooleanExpression(expL),varL)
      case UnaryArithmeticExpression(left,op,ret) =>
        var (expL,varL) = removeNondeterminism(label,left)
        (UnaryArithmeticExpression(expL,op,ret),varL)
      case BinaryNondeterministicExpression(left,right,op,returnType) =>
        var (expL,varL) = removeNondeterminism(label+"L",left)
        var (expR,varR) = removeNondeterminism(label+"R",right)
        var identifier = new VariableIdentifier(label,expr.getType(),expr.getProgramPoint())
        (identifier,varL:::varR:::List((identifier,BinaryNondeterministicExpression(expL,expR,op,returnType))))
      case x:Expression => (x,Nil)
    }
  }

  private def nondeterminismWrapper(expr:Expression, state:ApronInterface , someFunc: (Expression,ApronInterface) => ApronInterface) : ApronInterface = {

    // Extract all non-deterministic expressions and store them in temporary variables
    var newState = state
    val noFloatingPointExpr = removeFloats(expr)
    val (newExpr, tempAssigns) = removeNondeterminism("tmp",noFloatingPointExpr)
    for ((id,ndExpr) <- tempAssigns) {
      ndExpr.op match {
        case NondeterministicOperator.or =>
          val newStateLeft = newState.assign(id,ndExpr.left)
          val newStateRight = newState.assign(id,ndExpr.right)
          newState = lub(newStateLeft,newStateRight)
        case NondeterministicOperator.to =>
          newState = newState.
            createVariable(id,ndExpr.getType()).
            assume(BinaryArithmeticExpression(id,ndExpr.left,ArithmeticOperator.>=,ndExpr.getType())).
            // Instead of a <= b use a < b + 1. This only works for integers. TODO: Make sound for floating point using epsilon
            //assume(BinaryArithmeticExpression(id,
              //BinaryArithmeticExpression(ndExpr.right,Constant("1",ndExpr.right.getType(),null),ArithmeticOperator.+,ndExpr.right.getType()),
              //ArithmeticOperator.<,ndExpr.getType()))
            assume(BinaryArithmeticExpression(id,ndExpr.right,ArithmeticOperator.<=,ndExpr.getType()))
      }
    }

    newState = someFunc(newExpr,newState)

    // Remove all temporary variables
    for ((id,_) <- tempAssigns) {
      newState = newState.removeVariable(id)
    }

    newState
  }


  override def assign (variable : Identifier, expr : Expression) : ApronInterface = {

    if (variable.getType().isNumericalType()) {

      // Create variable if it does not exist
      var newState = state
      if(! state.getEnvironment.hasVar(variable.getName())) {
        newState = this.createVariable(variable, variable.getType()).state
      }

      // ADDED because of minimizing the environment in merge
      // (i.e. some of the variables still in use might have been removed by merge as they were Top)
      var newEnv = newState.getEnvironment
      for (id <- Normalizer.getIdsForExpression(expr)) {
        if(! newEnv.hasVar(id.getName())) {
          val v : Array[String] = new Array[String](1)
          v.update(0, id.getName())
          newEnv=newEnv.add(v, new Array[String](0))
        }
      }
      newState = newState.changeEnvironmentCopy(domain, newEnv, false)
      // END of the added code

      nondeterminismWrapper(expr, new ApronInterface(newState, domain), (someExpr,someState) => {
        new ApronInterface(someState.state.assignCopy(domain, variable.getName, this.toTexpr1Intern(someExpr, someState.state.getEnvironment()), null),domain)
      })

    } else this
  }

	override def assume(expr : Expression) : ApronInterface = expr match {
    // Boolean variables
    case x: Identifier =>
      assume(BinaryArithmeticExpression(x,Constant("0",x.getType(),x.getProgramPoint()),ArithmeticOperator.!=,null))
    case NegatedBooleanExpression(x:Identifier) =>
      assume(BinaryArithmeticExpression(x,Constant("0",x.getType(),x.getProgramPoint()),ArithmeticOperator.==,null))
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
      var expEnv = new Environment();
      for (id <- Normalizer.getIdsForExpression(expr)) {
        val v : Array[String] = new Array[String](1);
        v.update(0, id.getName());
        expEnv=expEnv.add(v, new Array[String](0));
      }
      val unionEnv = unionOfEnvironments(this.state.getEnvironment(), expEnv)
      val newState = state.changeEnvironmentCopy(domain, unionEnv, false);

      val res = nondeterminismWrapper(expr,new ApronInterface(newState,domain),(someExpr,someState) => {
        val unionEnv = unionOfEnvironments(someState.state.getEnvironment(), expEnv)
        val constraint = this.toTcons1(someExpr, unionEnv)
        val meetcopy = someState.state.meetCopy(domain, constraint)
        (new ApronInterface(meetcopy, domain))
      })

      res
		}
	}

	override def factory() : ApronInterface = top();
  	override def top() : ApronInterface = {
		var result = new ApronInterface(new Abstract1(domain, state.getEnvironment), domain);
  		//val result = new ApronInterface(new Abstract1(domain, new Environment()), domain);
  		if(! result.state.isTop(domain)) {
  			result = new ApronInterface(new Abstract1(domain, new Environment()), domain);
  			if(! result.state.isTop(domain)) throw new ApronException("I'm not able to create a top state");
  		}
  		return result;
  	}

	override def bottom() : ApronInterface = {
		var st = new Abstract1(domain, state.getEnvironment(), true);
		new ApronInterface(st, domain);
	}

	override def lub(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isTop(domain)) return left;
		if(right.state.isTop(domain)) return right;
		if(left.state.isBottom(domain)) return right;
		if(right.state.isBottom(domain)) return left;
		try {
      val env = unionOfEnvironments(left.state.getEnvironment, right.state.getEnvironment);
      val newLeft = new ApronInterface(left.state.changeEnvironmentCopy(left.domain, env, false), left.domain)
      val newRight = new ApronInterface(right.state.changeEnvironmentCopy(right.domain, env, false), right.domain)
      val commonVars = left.state.minimizeEnvironmentCopy(domain).getEnvironment.getVars.intersect(right.state.minimizeEnvironmentCopy(domain).getEnvironment.getVars)
      val forgotLState = newLeft.state.forgetCopy(domain, commonVars.toArray[String], false)
      val forgotRState = newRight.state.forgetCopy(domain, commonVars.toArray[String], false)
      val finalLeft = forgotLState.meetCopy(domain, newRight.state)
      val finalRight = forgotRState.meetCopy(domain, newLeft.state)
			val st = finalLeft.joinCopy(domain, finalRight);
			new ApronInterface(st, domain);
		} catch {
			case _ => {
				throw new ApronException("WARNING: incompatible environments.")
				//top()	// TODO fix this, but how?
			}
		}
	}

	override def glb(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isBottom(domain)) return left;
		if(right.state.isBottom(domain)) return right;
		if(left.state.isTop(domain)) return right;
		if(right.state.isTop(domain)) return left;
    val env = unionOfEnvironments(left.state.getEnvironment, right.state.getEnvironment);
    val newLeft = new ApronInterface(left.state.changeEnvironmentCopy(left.domain, env, false), left.domain)
    val newRight = new ApronInterface(right.state.changeEnvironmentCopy(right.domain, env, false), right.domain)
		val st = newLeft.state.meetCopy(domain, newRight.state);
		new ApronInterface(st, domain);
	}
	
	override def widening(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isTop(domain) || right.state.isTop(domain)) return top()
		if(left.state.isBottom(domain)) return right
		if(right.state.isBottom(domain)) return left

    val env = unionOfEnvironments(left.state.getEnvironment, right.state.getEnvironment)
    val newLeft = new ApronInterface(left.state.changeEnvironmentCopy(left.domain, env, false), left.domain)
    val newRight = new ApronInterface(right.state.changeEnvironmentCopy(right.domain, env, false), right.domain)
    var st = new Abstract1(domain, newLeft.state)
		st = st.widening(domain, newRight.state)
    new ApronInterface(st, domain)
	}
	
	override def lessEqual(r : ApronInterface) : Boolean = {
		if(this.state.isBottom(domain)) return true
		if(r.state.isTop(domain)) return true
		if(r.state.isBottom(domain)) return false
		if(this.state.isTop(domain)) return false
    val env = unionOfEnvironments(this.state.getEnvironment, r.state.getEnvironment)
    val newLeft = new ApronInterface(this.state.changeEnvironmentCopy(this.domain, env, false), this.domain)
    val newRight = new ApronInterface(r.state.changeEnvironmentCopy(r.domain, env, false), r.domain)
    if (newRight.state.getEnvironment.getVars.size != newLeft.state.getEnvironment.getVars.size)
      throw new ApronException("Different environments.")
		val result= newLeft.state.isIncluded(domain, newRight.state)
		result
	}

  /**
   * The method checks equivalence of two given expressions, left and right. If the method
   * returns true, then left and right are definitely equivalent. If the method returns fasle,
   * then left and right may or may not be equivalent. (e.g. left = [0,1] and right = [0,1],
   * however, it can be the case that in concrete state left = 1 and right = 0)
   *
   * @param left - first expression
   * @param right - second expression
   *
   * @return true if left and right are definitely equivalent, false otherwise
   */
  def areExpressionsEquivalent(left: Expression,  right: Expression): Boolean = {
    // if the state is originally bottom, then we can't tell whether the expressions
    // are equivalent, as we do not know where the bottom comes from.
    if (this.isBottom(this)) {
      return false
    }
    val checkState1 = this.assume(new BinaryArithmeticExpression(left, right, ArithmeticOperator.<, left.getType()))
    val checkState2 = this.assume(new BinaryArithmeticExpression(right, left, ArithmeticOperator.<, left.getType()))
    return this.isBottom(checkState1) && this.isBottom(checkState2)
  }

  /**
   * This method returns true if the given state s is bottom. Otherwise it returns false
   *
   * @param s - a state to be checked whether it is bottom
   *
   * @return true if s is bottom, otherwise return false
   */
  def isBottom(s: ApronInterface) : Boolean = return s.lessEqual(this.bottom())
	
	private def toTexpr1Intern(e : Expression, env : apron.Environment) : Texpr1Intern = {
		val e1 = this.toTexpr1Node(e);
		return new Texpr1Intern(env, e1)
	}

  private def topExpression() : Texpr1Node = {
    val a = new apron.Interval(0,0)
    val b = new DoubleScalar()
    val c = new DoubleScalar()
    b.setInfty(-1)
    c.setInfty(1)
    a.setInf(b)
    a.setSup(c)
    new Texpr1CstNode(a)
  }

  private def topConstraint(env : Environment) : Tcons1 = {
    new Tcons1(Tcons1.EQ,new Texpr1Intern(env,new Texpr1CstNode(new DoubleScalar(0)))) // always true
  }

  private def bottomConstraint(env : Environment) : Tcons1 = {
    new Tcons1(Tcons1.EQ,new Texpr1Intern(env,new Texpr1CstNode(new DoubleScalar(1)))) // always false
  }

  private def toTexpr1Node(e : Expression) : Texpr1Node = e match {
    case Constant("invalid", typ, p) =>
      topExpression() // SHOULD BE: BOTTOM. NOT IMPLEMENTABLE
    case Constant("valid", typ, p) =>
      topExpression()
		case x : Identifier => new Texpr1VarNode(x.getName);
    case setId : HeapIdSetDomain[Identifier] =>
      if (setId.isTop) topExpression()
      else if(setId.value.size!=1) throw new ApronException("Not yet supported")
      else new Texpr1VarNode(setId.value.iterator.next().getName());
		case Constant(v, typ, p) =>
      if (typ.isNumericalType())
        v match {
          case "true" => new Texpr1CstNode(new DoubleScalar(1))
          case "false" => new Texpr1CstNode(new DoubleScalar(0))
          case _ => new Texpr1CstNode(new DoubleScalar(java.lang.Double.parseDouble(v)))
        }
      else topExpression()
    case BinaryArithmeticExpression(left, right, op, typ) =>
      new Texpr1BinNode(this.convertArithmeticOperator(op), this.toTexpr1Node(left), this.toTexpr1Node(right))
		case UnaryArithmeticExpression(left, op, typ) => op match {
			case ArithmeticOperator.- => new Texpr1UnNode(Texpr1UnNode.OP_NEG, this.toTexpr1Node(left))
		}
    case _ => throw new SemanticException("Unhandled expression type.")
	}
	
	private def convertArithmeticOperator(op : ArithmeticOperator.Value) : Int = op match {
		case ArithmeticOperator.+ => Texpr1BinNode.OP_ADD
		case ArithmeticOperator.- => Texpr1BinNode.OP_SUB
		case ArithmeticOperator./ => Texpr1BinNode.OP_DIV
		case ArithmeticOperator.* => Texpr1BinNode.OP_MUL
	}
	
	private def toTcons1(e : Expression, env : Environment) : Tcons1 = e match {
    case Constant("invalid", typ, p) =>
      bottomConstraint(env)
    case Constant("valid", typ, p) =>
      topConstraint(env)
		case BinaryArithmeticExpression(left, right, op, typ) =>
			var localop = op;
			var localleft = left;
			var localright = right;
			op match {
				case ArithmeticOperator.>= =>
				case ArithmeticOperator.== =>
				case ArithmeticOperator.!= =>
				case ArithmeticOperator.> =>
				case ArithmeticOperator.<= => localleft = right; localright = left; localop = ArithmeticOperator.>=; 
				case ArithmeticOperator.< => localleft = right; localright = left; localop = ArithmeticOperator.>;
			}
			val expr1 = this.toTexpr1Node(new BinaryArithmeticExpression(localleft, localright, ArithmeticOperator.-, localleft.getType()));
			localop match {
				case ArithmeticOperator.>= => return new Tcons1(env, Tcons1.SUPEQ, expr1)
				case ArithmeticOperator.== => return new Tcons1(env, Tcons1.EQ, expr1)
				case ArithmeticOperator.!= => return new Tcons1(env, Tcons1.DISEQ, expr1)
				case ArithmeticOperator.> =>
          // TODO: FOR OCTAGONS THERE IS A BUG. THIS IS THE WORKAROUND
          // Replace a > 0 by a - 1 >= 0. Only works for integers
          val sExpr1 = new BinaryArithmeticExpression(localleft, localright, ArithmeticOperator.-, localleft.getType())
          val sExpr2 = new BinaryArithmeticExpression(sExpr1, Constant("1",sExpr1.getType(),sExpr1.getProgramPoint()), ArithmeticOperator.-, sExpr1.getType())
          return new Tcons1(env, Tcons1.SUPEQ, this.toTexpr1Node(sExpr2))
          // return new Tcons1(env, Tcons1.SUP, expr1)
			}
		case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
			return toTcons1(BinaryArithmeticExpression(left, right, negateOperator(op), typ), env)
    case NegatedBooleanExpression(NegatedBooleanExpression(x)) =>  toTcons1(x, env)
    case NegatedBooleanExpression(x) =>
      return toTcons1(BinaryArithmeticExpression(x, Constant("0",x.getType(),x.getProgramPoint()), ArithmeticOperator.==, x.getType()), env)
    case x:Expression =>
      return toTcons1(BinaryArithmeticExpression(x, Constant("0",x.getType(),x.getProgramPoint()), ArithmeticOperator.!=, x.getType()), env)
  }

	private def negateOperator(op : ArithmeticOperator.Value) : ArithmeticOperator.Value = op match {
			case ArithmeticOperator.<= => return ArithmeticOperator.>
			case ArithmeticOperator.< => return ArithmeticOperator.>=
			case ArithmeticOperator.>= => return ArithmeticOperator.<
			case ArithmeticOperator.== => return ArithmeticOperator.!=
			case ArithmeticOperator.!= => return ArithmeticOperator.==
			case ArithmeticOperator.> => return ArithmeticOperator.<=
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
    for(v <- right.getVars) {
      if(! left.hasVar(v))  {
        val vA : Array[String] = new Array[String](1)
        vA.update(0, v)
        resEnv = resEnv.add(vA, new Array[String](0))
      }
    }
    resEnv
  }
}


class ApronAnalysis extends SemanticAnalysis[ApronInterface] {
  var domain : Manager=null;
  def getLabel() : String = "Apron numerical analysis";
  def parameters() : List[(String, Any)] = List(("Domain", List("Interval", "PPL", "Octagons", "Polka", "Linear equalities")));
  def setParameter(label : String, value : Any) = label match {
    case "Domain" => value match {
      case "Interval" => domain = new Box();
        // FIXIT: Change back to PPL
      case "PPL" => domain = new Polka(false) //new PplPoly(false);
      case "Octagons" => domain = new Octagon();
      case "Polka" => domain = new Polka(false);
      case "Linear equalities" => domain = new PolkaEq();
    }
  }
  def reset() : Unit = Unit;
  def getInitialState() : ApronInterface = new ApronInterface(new Abstract1(domain, new Environment()), domain);
  def getProperties() : Set[Property] = Set(
		new ApronProperty().asInstanceOf[Property],
		new SingleStatementProperty(DivisionByZero),
		new SingleStatementProperty(new LowerBoundedValue("y", 0)),
		new SingleStatementProperty(new BoundedValue("y", -4, 4))
	);
  def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil;
}

class ApronException(s : String) extends Exception(s);