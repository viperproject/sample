package ch.ethz.inf.pm.sample.oorepresentation.scalalang

import ch.ethz.inf.pm.sample.oorepresentation._

object RemoveGetterSetter {

  
  def cleanCFG(cfg : ControlFlowGraph) : ControlFlowGraph = {
	 var cleanedNodes : List[List[Statement]] = Nil;
  	 for(n <- cfg.nodes)
  	   cleanedNodes=cleanedNodes ::: this.cleanListStatement(n) :: Nil;
  	 val result = new ControlFlowGraph(cfg.programpoint);
  	 result.nodes=cleanedNodes;
  	 result.edges=cfg.edges;
  	 return result;
  }
  
  private def cleanStatement(st : Statement) : Statement = st match {
       case x: ControlFlowGraph => this.cleanCFG(x);
       case Assignment(pp, left, right) => return new Assignment(pp, cleanStatement(left), cleanStatement(right));
       case VariableDeclaration(pp, variable, typ, right) =>
         new VariableDeclaration(pp, variable, typ, right.map(cleanStatement))
       case FieldAccess(pp, obj, field, typ) => return new FieldAccess(pp, cleanStatement(obj), field, typ);
       case Variable(pp, id) => return st;
       case New(pp, typ) => return st;
       case ConstantStatement(pp, value, typ) => return st;
       case MethodCall(pp, method, parametricTypes, parameters, returnedType) => method.normalize() match {
         case FieldAccess(pp1, obj, field, typ) =>
           if(field.length>=2 && field.substring(field.length-2, field.length).equals("_=")) //obj.field_=expr is adopted to assign fields
           	parameters match {
           		case assigned :: Nil =>
	           		val fieldName=field.substring(0, field.length-2)
                val fieldAccess = new FieldAccess(pp1, cleanStatement(obj), fieldName, typ)
                return new Assignment(pp, fieldAccess, cleanStatement(assigned))
           		case _ => throw new MethodSemanticException("I can only assign an expression to a field!")
             }
           else parameters match {
             case Nil =>
               val cleanObj = cleanStatement(obj)
               val t = cleanObj match {
                 case v: Variable => v.id.getType
                 case fa: FieldAccess => fa.typ
                 case mc: MethodCall => mc.returnedType
                 case _ => typ.top()
               }

               if (!t.equals(t.top()))
                 for (n <- t.possibleFields) {
                   if (field.equals(n.getName))
                     return new FieldAccess(pp1, cleanObj, field, n.getType)
                 }
             case _ => 
             	//System.out.println("Look at this:\n"+st.toString);
           } 
           return new MethodCall(pp, cleanStatement(method), parametricTypes, cleanListStatement(parameters), returnedType);
       } 
       case Throw(pp, expr) => return new Throw(pp, cleanStatement(expr));
  }
  
  private def cleanListStatement(list : List[Statement]) : List[Statement] = list match {
    case x :: xs => 
    	return cleanStatement(x) :: cleanListStatement(xs);
    case Nil => return Nil;
  }
  
}