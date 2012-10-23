package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._


object SymbolicNativeMethodSemantics extends NativeMethodSemantics {

  def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = thisExpr.getType().toString() match {
    case "Int" => return new Some(state);
   
	  case "Boolean" => return new Some(state); //TODO: Potentially not sound
   
	  case className => {
	 	if(operator.equals("==") || operator.equals("!=") || operator.equals("$asInstanceOf") || operator.equals("$isInstanceOf") || operator.equals("assert")) //to avoid comparison between references and type casts
	 		return new Some(state);
    return extractMethodCall(state, thisExpr, parameters, operator, returnedtype)

	  }
	}

  private def extractMethodCall[HI <: HeapIdentifier[HI], H <: HeapDomain[H, HI], SD <: SemanticDomain[SD], S <: State[S]](state: S, thisExpr: ExpressionSet, parameters : List[ExpressionSet], method : String, retType : Type): Option[S] = {
    var result = state.bottom();
    for (exp <- thisExpr.setOfExpressions)
      exp match {
        case id: VariableIdentifier if id.getName().equals("this") =>
          val expr = new AbstractMethodCall(id, parsToVariableId(parameters), method, retType)
          result=result.lub(result, state.setExpression(new ExpressionSet(retType).add(expr)));
        case _ => throw new SemanticException("Not yet supported")
      }
    if(result.lessEqual(result.bottom())) return None;
    else return Some(result);
  }

  private def parsToVariableId[S <: State[S]](pars : List[ExpressionSet]) : List[Expression]= pars match {
    case x :: xs =>
      val exprs = x.setOfExpressions
      if(exprs.size!=1) throw new SemanticException("Not yet supported")
      val id = exprs.iterator.next();
      //if(! id.isInstanceOf[VariableIdentifier]) throw new SemanticException("Not yet supported")
      id/*.asInstanceOf[VariableIdentifier]*/ :: parsToVariableId(xs);
    case Nil => Nil;
  }

	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String, parameters : List[ExpressionSet], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] =
		throw new SemanticException("Backward semantics not yet supported");
}


case class AbstractMethodCall(val thisExpr : VariableIdentifier, val parameters : List[Expression], val calledMethod : String, val retType : Type) extends Expression(thisExpr.getProgramPoint) {
  def getType() = retType;
}
