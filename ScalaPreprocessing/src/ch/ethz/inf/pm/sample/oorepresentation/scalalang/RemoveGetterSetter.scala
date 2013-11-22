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
       case VariableDeclaration(pp, variable, typ, right) => return new VariableDeclaration(pp, variable, typ, cleanStatement(right));  
       case FieldAccess(pp, objs, field, typ) => return new FieldAccess(pp, cleanListStatement(objs), field, typ);
       case Variable(pp, id) => return st;
       case New(pp, typ) => return st;
       case ConstantStatement(pp, value, typ) => return st;
       case MethodCall(pp, method, parametricTypes, parameters, returnedType) => method.normalize() match {
         case FieldAccess(pp1, objs, field, typ) =>
           if(field.length>=2 && field.substring(field.length-2, field.length).equals("_=")) //obj.field_=expr is adopted to assign fields
           	parameters match {
           		case assigned :: Nil =>
	           		val fieldName=field.substring(0, field.length-2)
	           		var fieldaccess=new FieldAccess(pp1, cleanListStatement(objs), fieldName, typ);
	           		return new Assignment(pp, fieldaccess, cleanStatement(assigned));
           		case _ => throw new MethodSemanticException("I can only assign an expression to a field!")
             }
           else parameters match {
             case Nil =>
               var t = typ.bottom();
               val cleanedObjs=cleanListStatement(objs)
               for(obj <- cleanedObjs)
                 if(obj.isInstanceOf[Variable])
                	 t=t.lub(t, obj.asInstanceOf[Variable].id.getType());
                 else if(obj.isInstanceOf[FieldAccess])
                	 t=t.lub(t, obj.asInstanceOf[FieldAccess].typ);
                 else if(obj.isInstanceOf[MethodCall])
                	 t=t.lub(t, obj.asInstanceOf[MethodCall].returnedType);
                 else t=t.top();
               if(! t.equals(t.top()))
	               for(n <- t.getPossibleFields()) {
	                 if(field.equals(n.getName()))
	                   return new FieldAccess(pp1, cleanedObjs, field, n.getType());
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