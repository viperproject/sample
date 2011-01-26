package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class ApronInterface(val state : Abstract1, val domain : Manager) extends RelationalNumericalDomain[ApronInterface] {
	
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
	
	override def toString() : String = state.toString(); 
	
	
	private def constraintContains(c : Lincons1, variable : String) : Boolean = {
		for(term <- c.getLinterms )
			if(term.getVariable().equals(variable))
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
		val st = state.assignCopy(domain, variable.getName, this.toTexpr1Intern(expr, state.getEnvironment()), null);
		new ApronInterface(st, domain);
	}
	override def assume(expr : Expression) : ApronInterface = {
		val st = state.meetCopy(domain, this.toTcons1(expr, this.state.getEnvironment()));
		new ApronInterface(st, domain);
	}

	override def factory() : ApronInterface = top();
  	override def top() : ApronInterface = {
		val result = new ApronInterface(new Abstract1(domain, state.getEnvironment), domain);
  		if(! result.state.isTop(domain)) throw new ApronException("I'm not able to create a top state");
  		return result;
  	}
	override def bottom() : ApronInterface = {
		val st = state.meetCopy(domain, new Lincons1(state.getEnvironment(), false));
  		if(! st.isBottom(domain)) throw new ApronException("I'm not able to create a bottom state");
		new ApronInterface(st, domain);
	}
	override def lub(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isTop(domain)) return left;
		if(right.state.isTop(domain)) return right;
		if(left.state.isBottom(domain)) return right;
		if(right.state.isBottom(domain)) return left;
		val st = left.state.joinCopy(domain, right.state);
		new ApronInterface(st, domain);
	}
	
	override def glb(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isBottom(domain)) return left;
		if(right.state.isBottom(domain)) return right;
		if(left.state.isTop(domain)) return right;
		if(right.state.isTop(domain)) return left;
		val st = left.state.meetCopy(domain, right.state);
		new ApronInterface(st, domain);
	}
	
	override def widening(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		if(left.state.isTop(domain) || right.state.isTop(domain)) return top();
		if(left.state.isBottom(domain)) return right;
		if(right.state.isBottom(domain)) return left;
		val st = new Abstract1(domain, left.state);
		st.widening(domain, right.state);
		new ApronInterface(st, domain);
	}
	
	override def lessEqual(r : ApronInterface) : Boolean = {
		if(this.state.isBottom(domain)) return true;
		if(r.state.isTop(domain)) return true;
		if(r.state.isBottom(domain)) return false;
		if(this.state.isTop(domain)) return false;
		val result=this.state.isIncluded(domain, r.state);
		return result;
	}
	
	private def toTexpr1Intern(e : Expression, env : apron.Environment) : Texpr1Intern = {
		val e1 = this.toTexpr1Node(e);
		return new Texpr1Intern(env, e1)
	}
	
	private def toTexpr1Node(e : Expression) : Texpr1Node = e match {
		case x : Identifier => new Texpr1VarNode(x.getName);
		case Constant(v, typ) => new Texpr1CstNode(new DoubleScalar(java.lang.Double.parseDouble(v)))
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
			val expr1 = this.toTexpr1Node(new BinaryArithmeticExpression(left, right, ArithmeticOperator.-, null));
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
			case ArithmeticOperator.<= => return ArithmeticOperator.>=
			case ArithmeticOperator.< => return ArithmeticOperator.>
			case ArithmeticOperator.>= => return ArithmeticOperator.<=
			case ArithmeticOperator.== => return ArithmeticOperator.!=
			case ArithmeticOperator.!= => return ArithmeticOperator.==
			case ArithmeticOperator.> => return ArithmeticOperator.<
	}
}

class ApronException(s : String) extends Exception(s);