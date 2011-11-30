package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._
import sun.net.ftp.FtpProtocolException
import com.sun.org.omg.CORBA.IdentifierHelper
import apron._
;

class ApronInterface(val state : Abstract1, val domain : Manager) extends RelationalNumericalDomain[ApronInterface] {


  override def merge(r : Replacement) : ApronInterface = {
    if(r.isEmpty) return this;
    //else throw new ApronException("Not implemeneted")
    var result = new Abstract1(domain, state.getEnvironment(), true);//state.meetCopy(domain, new Lincons1(state.getEnvironment(), false));
		if(! result.isBottom(domain)) throw new ApronException("I'm not able to create a bottom state");
    var idsInDomain : Set[Identifier] = Set.empty
    var idsInCodomain : Set[Identifier] = Set.empty
    for(I1 <- r.value.keySet) {
      idsInDomain=idsInDomain++I1;
      for(id2 : Identifier <- r.value.apply(I1)) {
        idsInCodomain=idsInCodomain++I1;
        for(id1: Identifier <- I1) {
          var newState = state;
          if(! state.getEnvironment.hasVar(id2.getName()) || ! state.getEnvironment.hasVar(id1.getName())) {
            newState = this.createVariable(id2, id2.getType()).createVariable(id1, id1.getType()).state;
          }
          var resEnv = unionOfEvrinomnets(newState.getEnvironment, result.getEnvironment);
          newState = newState.changeEnvironmentCopy(domain, resEnv, false);
          result = result.changeEnvironmentCopy(domain, resEnv, false);
          var temp = newState.substituteCopy(domain, id2.getName(), this.toTexpr1Intern(id1, newState.getEnvironment), newState);
          result = result.joinCopy(domain, temp)
        }
      }
    }
    for(id <- idsInDomain.--(idsInCodomain)) {
      result=result.forgetCopy(domain, id.getName(), false);
    }
    return new ApronInterface(result, domain)
  };

  //TODO
  def getIds : Set[Identifier] = Set.empty[Identifier];

	override def getStringOfId (id : Identifier) : String = {
		var result : String = "";
		for(c <- this.state.toLincons(domain)) {
			if(this.constraintContains(c, id.getName()))
				result=result+" " + c.toString();
		}
		if(result.equals(""))
			result="T";
		return result;
	}
	
	override def toString() : String = {
    state.toString() //+ " ENV = " + state.getEnvironment.toString;
  };
	
	
	private def constraintContains(c : Lincons1, variable : String) : Boolean = {
		for(term <- c.getLinterms )
			if(term.getVariable().equals(variable) && !term.getCoefficient.toString.equals("0"))
				return true;
		return false;
	}
		
	override def createVariable (variable : Identifier, typ : Type) : ApronInterface = {
		var env = state.getEnvironment;
		if(! env.hasVar(variable.getName())) {
			val v : Array[String] = new Array[String](1);
			v.update(0, variable.getName());
			env=env.add(v, new Array[String](0));
		}
		return new ApronInterface(state.changeEnvironmentCopy(domain, env, false), domain);
	}
	
	override def removeVariable(variable : Identifier) : ApronInterface = { 
		var env = state.getEnvironment;
		val v : Array[String] = new Array[String](1);
		v.update(0, variable.getName());
		env=env.remove(v);
		return new ApronInterface(state.changeEnvironmentCopy(domain, env, false), domain);
	}
		
	override def setToTop(variable : Identifier) : ApronInterface = {
		val st = state.forgetCopy(domain, variable.getName, false);
		new ApronInterface(st, domain);
	}
	override def assign (variable : Identifier, expr : Expression) : ApronInterface = {
    if(state.isBottom(domain)) return this.bottom;
    var newState = state;
		if(! state.getEnvironment.hasVar(variable.getName())) {
      newState = this.createVariable(variable, variable.getType()).state;
    }
		val st = newState.assignCopy(domain, variable.getName, this.toTexpr1Intern(expr, newState.getEnvironment()), null);
		new ApronInterface(st, domain);
	}

	override def assume(expr : Expression) : ApronInterface = expr match {
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
      for (id <- expr.getIds(expr)) {
        val v : Array[String] = new Array[String](1);
        v.update(0, id.getName());
        expEnv=expEnv.add(v, new Array[String](0));
      }
      val newState = state.changeEnvironmentCopy(domain, unionOfEvrinomnets(this.state.getEnvironment(), expEnv), false);
			val st = newState.meetCopy(domain, this.toTcons1(expr, unionOfEvrinomnets(this.state.getEnvironment(), expEnv)));
			new ApronInterface(st, domain);
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
		var st = new Abstract1(domain, state.getEnvironment(), true);//state.meetCopy(domain, new Lincons1(state.getEnvironment(), false));
  		//if(! st.isBottom(domain)) {
  			//st = state.meetCopy(domain, new Tcons1(state.getEnvironment(), Tcons1.EQ, new Texpr1CstNode(new DoubleScalar(1.0))));
  			if(! st.isBottom(domain))throw new ApronException("I'm not able to create a bottom state");
  		//}
		new ApronInterface(st, domain);
	}
	override def lub(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isTop(domain)) return left;
		if(right.state.isTop(domain)) return right;
		if(left.state.isBottom(domain)) return right;
		if(right.state.isBottom(domain)) return left;
		try {
      val env = unionOfEvrinomnets(left.state.getEnvironment, right.state.getEnvironment);
      val newLeft = new ApronInterface(left.state.changeEnvironmentCopy(left.domain, env, false), left.domain)
      val newRight = new ApronInterface(right.state.changeEnvironmentCopy(right.domain, env, false), right.domain)
			val st = newLeft.state.joinCopy(domain, newRight.state);
			new ApronInterface(st, domain);
		} catch {
			case _ => {
				throw new ApronException("WARNING: incompatible environments.")
				top()	// TODO fix this, but how?
			}
		}
	}

	override def glb(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isBottom(domain)) return left;
		if(right.state.isBottom(domain)) return right;
		if(left.state.isTop(domain)) return right;
		if(right.state.isTop(domain)) return left;
    val env = unionOfEvrinomnets(left.state.getEnvironment, right.state.getEnvironment);
    val newLeft = new ApronInterface(left.state.changeEnvironmentCopy(left.domain, env, false), left.domain)
    val newRight = new ApronInterface(right.state.changeEnvironmentCopy(right.domain, env, false), right.domain)
		val st = newLeft.state.meetCopy(domain, newRight.state);
		new ApronInterface(st, domain);
	}
	
	override def widening(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isTop(domain) || right.state.isTop(domain)) return top();
		if(left.state.isBottom(domain)) return right;
		if(right.state.isBottom(domain)) return left;
    val env = unionOfEvrinomnets(left.state.getEnvironment, right.state.getEnvironment);
    val newLeft = new ApronInterface(left.state.changeEnvironmentCopy(left.domain, env, false), left.domain)
    val newRight = new ApronInterface(right.state.changeEnvironmentCopy(right.domain, env, false), right.domain)
    var st = new Abstract1(domain, newLeft.state);
		st = st.widening(domain, newRight.state);
		new ApronInterface(st, domain);
	}
	
	override def lessEqual(r : ApronInterface) : Boolean = {
		if(this.state.isBottom(domain)) return true;
		if(r.state.isTop(domain)) return true;
		if(r.state.isBottom(domain)) return false;
		if(this.state.isTop(domain)) return false;
    val env = unionOfEvrinomnets(this.state.getEnvironment, r.state.getEnvironment);
    val newLeft = new ApronInterface(this.state.changeEnvironmentCopy(this.domain, env, false), this.domain)
    val newRight = new ApronInterface(r.state.changeEnvironmentCopy(r.domain, env, false), r.domain)
    if (newRight.state.getEnvironment.getVars.size != newLeft.state.getEnvironment.getVars.size)
      throw new ApronException("Different environments.")
		val result= newLeft.state.isIncluded(domain, newRight.state);
		return result;
	}
	
	private def toTexpr1Intern(e : Expression, env : apron.Environment) : Texpr1Intern = {
		val e1 = this.toTexpr1Node(e);
		return new Texpr1Intern(env, e1)
	}
	
	private def toTexpr1Node(e : Expression) : Texpr1Node = e match {
		case x : Identifier => new Texpr1VarNode(x.getName);
    case setId : HeapIdSetDomain[Identifier] =>
      if(setId.value.size!=1) throw new ApronException("Not yet supported")
      new Texpr1VarNode(setId.value.iterator.next().getName());
		case Constant(v, typ, p) => new Texpr1CstNode(new DoubleScalar(java.lang.Double.parseDouble(v)))
		case BinaryArithmeticExpression(left, right, op, typ) => new Texpr1BinNode(this.convertArithmeticOperator(op), this.toTexpr1Node(left), this.toTexpr1Node(right))
		case UnaryArithmeticExpression(left, op, typ) => op match {
			case ArithmeticOperator.- => new Texpr1UnNode(Texpr1UnNode.OP_NEG, this.toTexpr1Node(left))
		}
	}
	
	private def convertArithmeticOperator(op : ArithmeticOperator.Value) : Int = op match {
		case ArithmeticOperator.+ => Texpr1BinNode.OP_ADD
		case ArithmeticOperator.- => Texpr1BinNode.OP_SUB
		case ArithmeticOperator./ => Texpr1BinNode.OP_DIV
		case ArithmeticOperator.* => Texpr1BinNode.OP_MUL
	}
	
	private def toTcons1(e : Expression, env : Environment) : Tcons1 = e match {
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
			val expr1 = this.toTexpr1Node(new BinaryArithmeticExpression(localleft, localright, ArithmeticOperator.-, null));
			localop match {
				case ArithmeticOperator.>= => return new Tcons1(env, Tcons1.SUPEQ, expr1)
				case ArithmeticOperator.== => return new Tcons1(env, Tcons1.EQ, expr1)
				case ArithmeticOperator.!= => return new Tcons1(env, Tcons1.DISEQ, expr1)
				case ArithmeticOperator.> => return new Tcons1(env, Tcons1.SUP, expr1)
			}
		case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
			return toTcons1(BinaryArithmeticExpression(left, right, negateOperator(op), typ), env)
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
  private def unionOfEvrinomnets(left: Environment, right: Environment): Environment = {
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
  def parameters() : List[(String, Any)] = List(("Domain", List("Interval", "PPL", "Octagons", "Polka")));
  def setParameter(label : String, value : Any) = label match {
    case "Domain" => value match {
      case "Interval" => domain = new Box();
        // FIXIT: Change back to PPL
      case "PPL" => domain = new Polka(false) //new PplPoly(false);
      case "Octagons" => domain = new Octagon();
      case "Polka" => domain = new Polka(false);
    }
  }
  def reset() : Unit = Unit;
  def getInitialState() : ApronInterface = new ApronInterface(new Abstract1(domain, new Environment()), domain);
  def getProperties() : Set[Property] = Set(
		new ApronProperty(),
		new SingleStatementProperty(DivisionByZero),
		new SingleStatementProperty(new LowerBoundedValue("y", 0)),
		new SingleStatementProperty(new BoundedValue("y", -4, 4))
	);
  def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil;
}

class ApronException(s : String) extends Exception(s);