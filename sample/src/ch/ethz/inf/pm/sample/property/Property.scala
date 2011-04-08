package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import java.io.PrintStream

//TODO:Comment it
trait Property {
  def getLabel() : String;
  def check[S <: State[S]](className : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit;
  def finalizeChecking() : Unit;
}
//TODO:Comment it
trait Visitor {
  def getLabel() : String;
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit;
}
//TODO:Comment it
class SingleStatementProperty(visitor : Visitor) extends Property {
  def getLabel() : String = visitor.getLabel();
  def check[S <: State[S]](className : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
	SystemParameters.currentClass = className;
	SystemParameters.currentMethod = methodName;
    for(i <- 0 to result.nodes.size-1)
        for(k <- 0 to result.cfg.nodes.apply(i).size-1) {
          val statement=result.cfg.nodes.apply(i).apply(k);
          if(k >= result.nodes.apply(i).size-1) 
	          visitor.checkSingleStatement[S](
	            result.nodes.apply(i).apply(result.nodes.apply(i).size-1).bottom(), 
	            statement, 
	            printer
	          )
          else {
        	  val state=result.nodes.apply(i).apply(k);
        	  checkStatement(className, methodName, visitor, state, statement, printer)
          }
        }
      }
  override def finalizeChecking() : Unit = Unit;
  def checkStatement[S <: State[S]](className : Type, methodName : String, visitor : Visitor, state : S, statement : Statement, printer : OutputCollector) : Unit = statement match {
        	  	case Assignment(programpoint, left, right) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		this.checkStatement(className, methodName, visitor, state, left, printer)
        	  		this.checkStatement(className, methodName, visitor, left.forwardSemantics[S](state), right, printer)
        	  	case VariableDeclaration(programpoint, variable, typ, right) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		checkStatement(className, methodName, visitor, state, right, printer)
        	  	case Variable(programpoint, id) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  	case FieldAccess(pp, objs, field, typ) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		for(obj <- objs)
        	  			checkStatement(className, methodName, visitor, state, obj, printer)
        	  	case MethodCall(pp, method, parametricTypes, parameters, returnedType) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		checkStatement(className, methodName, visitor, state, method, printer)
        	  		var lastState=method.forwardSemantics[S](state);
        	  		for(par <- parameters)
        	  		  checkStatement(className, methodName, visitor,state, par, printer)
        	  	case New(pp, typ) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  	case NumericalConstant(pp, value, typ) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  	case Throw(programpoint, expr) =>
        	  		visitor.checkSingleStatement[S](state, statement, printer)
        	  		checkStatement(className, methodName, visitor,state, expr, printer)
        	  	case x : ControlFlowGraph =>
        	  		//This should be already there!!!
        	  		val result=new ControlFlowGraphExecution[S](x, state).forwardSemantics(state);
        	  		this.check(className, methodName, result, printer);
        	  }
  
}

//TODO:Remove it
class MatchErrorVisitor extends Visitor {

  def getLabel() : String = "Match error";
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit = statement match {
    case Throw(pp, st) => 
      st.normalize() match {
        case MethodCall(pp, method , parametricTypes, returnedType, parameters) if(method.isInstanceOf[FieldAccess] && method.asInstanceOf[FieldAccess].field.equals("this")) =>
          for(obj <- method.asInstanceOf[FieldAccess].objs) {
            obj.normalize match {
              case New(pp, typ) if(typ.getName.equals("MatchError"))=> 
              	if(state.equals(state.bottom()))
                 printer.add(new Validated(pp, SystemParameters.currentFile, "Exception MatchError is unreachable"))
               else printer.add(new NotValidated(pp, SystemParameters.currentFile, "Exception MatchError may be reachable"))
              case _ =>
            }
          }     
        case _ => 
      }
    case _ => 
  }
}
//TODO:Remove it
class CastingVisitor extends Visitor {

  def getLabel() : String = "Dynamic castings"
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit = {
    val st=statement.normalize();
    st match {
    	case MethodCall(pp, method , parametricTypes, parameters, returnedType) =>
    	if(method.isInstanceOf[FieldAccess] && method.asInstanceOf[FieldAccess].field.equals("$asInstanceOf"))
    		parameters match {
    		case Nil => parametricTypes match {
	    		case t :: Nil => 
	    		val (listObjs, conditionedState) : (List[SymbolicAbstractValue[S]], S) = UtilitiesOnStates.forwardExecuteListStatements[S](state, method.asInstanceOf[FieldAccess].objs )
	    		for(obj <- listObjs) {
	    			val result = conditionedState.setExpression(conditionedState.getExpression.createAbstractOperator(obj, Nil, parametricTypes, AbstractOperatorIdentifiers.isInstanceOf, conditionedState, st.asInstanceOf[MethodCall].returnedType));
	    			if(! (result.testFalse().equals(result.bottom))) {
	    				printer.add(new NotValidated(pp, SystemParameters.currentFile, "Unsafe casting, statement "+st.toString))
	    				return
	    			}
	    		}
	    		printer.add(new Validated(pp, SystemParameters.currentFile, "Safe casting"))
	    		case _ => throw new PropertyException("asInstanceOf must have exactly one type parameters")
    		}
    		case _ => throw new PropertyException("asInstanceOf cannot have parameters")
    	}
    	case _ => 
   }
 }
}

class PropertyException(s : String) extends Exception(s)