/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.scalalang

import ch.ethz.inf.pm.sample.oorepresentation._

object RemoveGetterSetter {

  
  def cleanCFG(cfg : ControlFlowGraph) : ControlFlowGraph = {
	 var cleanedNodes : List[List[Statement]] = Nil
    for(n <- cfg.nodes)
  	   cleanedNodes=cleanedNodes ::: this.cleanListStatement(n) :: Nil
    val result = new ControlFlowGraph(cfg.programpoint)
    result.nodes=cleanedNodes
    result.edges=cfg.edges
    result
  }
  
  private def cleanStatement(st : Statement) : Statement = st match {
       case x: ControlFlowGraph => this.cleanCFG(x);
       case Assignment(pp, left, right) => Assignment(pp, cleanStatement(left), cleanStatement(right));
       case VariableDeclaration(pp, variable, typ, right) =>
         VariableDeclaration(pp, variable, typ, right.map(cleanStatement))
       case FieldAccess(pp, obj, field, typ) => FieldAccess(pp, cleanStatement(obj), field, typ);
       case Variable(pp, id) => st;
       case New(pp, typ) => st;
       case ConstantStatement(pp, value, typ) => st;
       case MethodCall(pp, method, parametricTypes, parameters, returnedType, targets) => method.normalize() match {
         case FieldAccess(pp1, obj, field, typ) =>
           if(field.length>=2 && field.substring(field.length-2, field.length).equals("_=")) //obj.field_=expr is adopted to assign fields
           	parameters match {
           		case assigned :: Nil =>
	           		val fieldName=field.substring(0, field.length-2)
                val fieldAccess = FieldAccess(pp1, cleanStatement(obj), fieldName, typ)
                return Assignment(pp, fieldAccess, cleanStatement(assigned))
           		case _ => throw new MethodSemanticException("I can only assign an expression to a field!")
             }
           else parameters match {
             case Nil =>
               val cleanObj = cleanStatement(obj)
               val t = cleanObj match {
                 case v: Variable => v.id.typ
                 case fa: FieldAccess => fa.typ
                 case mc: MethodCall => mc.returnedType
                 case _ => typ.top()
               }

               if (!t.equals(t.top()))
                 for (n <- t.possibleFields) {
                   if (field.equals(n.getName))
                     return FieldAccess(pp1, cleanObj, field, n.typ)
                 }
             case _ => 
             	//System.out.println("Look at this:\n"+st.toString);
           } 
           MethodCall(pp, cleanStatement(method), parametricTypes, cleanListStatement(parameters), returnedType, targets);
       } 
       case Throw(pp, expr) => Throw(pp, cleanStatement(expr));
  }
  
  private def cleanListStatement(list : List[Statement]) : List[Statement] = list match {
    case x :: xs => 
    	cleanStatement(x) :: cleanListStatement(xs);
    case Nil => Nil;
  }
  
}