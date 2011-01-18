package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class ApronInterface(val state : Abstract1, val domain : Manager) extends RelationalNumericalDomain[ApronInterface] {
	
	def this(d : Manager) {
		this(new Abstract1(d, new Environment()), d);
	}
	
	override def getStringOfId (id : Identifier) : String = "TODO"
	override def setToTop(variable : Identifier) : ApronInterface = {
		val st = state.forgetCopy(domain, variable.getName, true);
		new ApronInterface(st, domain);
	}
	override def assign (variable : Identifier, expr : Expression) : ApronInterface = {
		val st = state.assignCopy(domain, variable.getName, this.toTexpr1Intern(expr, state.getEnvironment()), state);
		new ApronInterface(st, domain);
	}
	override def assume(expr : Expression) : ApronInterface = this //TODO 
	/*{
		val st = state.meetCopy(domain, this.toLincons1(expr));
		new ApronInterface(st, domain);
	}*/

	override def factory() : ApronInterface = new ApronInterface(domain);
  	override def top() : ApronInterface = {
  		val result = this.factory();
  		if(! result.state.isTop(domain)) throw new ApronException("I'm not able to create a top state");
  		return result;
  	}
	override def bottom() : ApronInterface = {
		val st = state.meetCopy(domain, new Lincons1(state.getEnvironment(), false));
  		if(! st.isBottom(domain)) throw new ApronException("I'm not able to create a bottom state");
		new ApronInterface(st, domain);
	}
	override def lub(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		val st = left.state.joinCopy(domain, right.state);
		new ApronInterface(st, domain);
	}
	
	override def glb(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		val st = left.state.meetCopy(domain, right.state);
		new ApronInterface(st, domain);
	}
	
	override def widening(left : ApronInterface, right : ApronInterface) : ApronInterface =  {
		val st = new Abstract1(domain, left.state);
		st.widening(domain, right.state);
		new ApronInterface(st, domain);
	}
	
	override def lessEqual(r : ApronInterface) : Boolean = this.state.isIncluded(domain, r.state);
	
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
	
	private def toTcons1(e : Expression) : Tcons1 = e match {
		case BinaryArithmeticExpression(left, right, op, typ) => {
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
		}
		throw new ApronException("Not yet supported");
	}
	
	private def toTcons0Operator(op : ArithmeticOperator.Value) : Int = op match {
				case ArithmeticOperator.>= => Tcons0.SUPEQ
				case ArithmeticOperator.== => Tcons0.EQ
				case ArithmeticOperator.!= => Tcons0.DISEQ
				case ArithmeticOperator.> => Tcons0.SUP
	}
}

class ApronException(s : String) extends Exception(s);