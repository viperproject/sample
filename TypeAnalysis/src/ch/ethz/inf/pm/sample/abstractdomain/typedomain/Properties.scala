package ch.ethz.inf.pm.sample.abstractdomain.typedomain


import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.oorepresentation._

class MatchErrorVisitor extends Visitor {

  def getLabel() : String = "Match error";
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit = statement match {
    case Throw(pp, st) =>
      st.normalize() match {
        case MethodCall(pp, method , parametricTypes, returnedType, parameters) if(method.isInstanceOf[FieldAccess] && method.asInstanceOf[FieldAccess].field.equals("this")) =>
          for(obj <- method.asInstanceOf[FieldAccess].objs) {
            obj.normalize match {
              case New(pp, typ) if(typ.getName.equals("MatchError"))=>
              	if(! state.equals(state.bottom()))
                 printer.add(new ValidatedProgramPoint(pp, "Exception MatchError is unreachable"))
               else printer.add(new WarningProgramPoint(pp, "Exception MatchError may be reachable"))
              case _ =>
            }
          }
        case _ =>
      }
    case _ =>
  }
}

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
	    				printer.add(new WarningProgramPoint(pp, "Unsafe casting, statement "+st.toString))
	    				return
	    			}
	    		}
	    		printer.add(new ValidatedProgramPoint(pp, "Safe casting"))
	    		case _ => throw new PropertyException("asInstanceOf must have exactly one type parameters")
    		}
    		case _ => throw new PropertyException("asInstanceOf cannot have parameters")
    	}
    	case _ =>
   }
 }
}