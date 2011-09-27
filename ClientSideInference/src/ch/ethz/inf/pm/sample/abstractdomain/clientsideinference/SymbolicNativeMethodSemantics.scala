package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._


object SymbolicNativeMethodSemantics extends NativeMethodSemantics {

  def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = thisExpr.getType().toString() match {
    case "Int" => return new Some(state);
   
	  case "Boolean" => return new Some(state); //TODO: Potentially not sound
   
	  case className => {
	 	if(operator.equals("==") || operator.equals("!=") || operator.equals("$asInstanceOf") || operator.equals("$isInstanceOf")) //to avoid comparison between references and type casts
	 		return new Some(state);
    return extractMethodCall(state, thisExpr, parameters, operator, returnedtype)

	  }
	}

  private def extractMethodCall[HI <: HeapIdentifier[HI], H <: HeapDomain[H, HI], SD <: SemanticDomain[SD], S <: State[S]](state: S, thisExpr: SymbolicAbstractValue[S], parameters : List[SymbolicAbstractValue[S]], method : String, retType : Type): Option[S] = {
    var result = state.asInstanceOf[GenericAbstractState[SD, H, HI]]._1._1;
    for (exp <- thisExpr.getExpressions)
      exp match {
        case id: VariableIdentifier if id.getName().equals("this") =>
          val expr = new AbstractMethodCall(id, parsToVariableId(parameters), method, retType)
          return Some(state.setExpression(new SymbolicAbstractValue[S](expr, state)));

        case _ => throw new SemanticException("Not yet supported")
      }
    val d1 = new HeapAndAnotherDomain[SD, H, HI](result, state.asInstanceOf[GenericAbstractState[SD, H, HI]]._1._2);
    return new Some(new GenericAbstractState(d1, thisExpr.top().asInstanceOf[SymbolicAbstractValue[GenericAbstractState[SD, H, HI]]]).asInstanceOf[S])
  }

  private def parsToVariableId[S <: State[S]](pars : List[SymbolicAbstractValue[S]]) : List[Expression]= pars match {
    case x :: xs =>
      val exprs = x.getExpressions()
      if(exprs.size!=1) throw new SemanticException("Not yet supported")
      val id = exprs.iterator.next();
      //if(! id.isInstanceOf[VariableIdentifier]) throw new SemanticException("Not yet supported")
      id/*.asInstanceOf[VariableIdentifier]*/ :: parsToVariableId(xs);
    case Nil => Nil;
  }

	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, programpoint : ProgramPoint, state : S) : Option[S] = 
		throw new SemanticException("Backward semantics not yet supported");
}


case class AbstractMethodCall(val thisExpr : VariableIdentifier, val parameters : List[Expression], val calledMethod : String, val retType : Type) extends Expression(thisExpr.getProgramPoint) {
  def getType() = retType;
}
