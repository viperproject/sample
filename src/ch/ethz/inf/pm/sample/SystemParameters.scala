package ch.ethz.inf.pm.sample
import ch.ethz.inf.pm.sample.property.OutputCollector
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.preprocessing.scalaprocessing._

object SystemParameters {
  val wideningLimit : Int = 1;
  var currentFile : String = "<not yet initialized>";
  val output : OutputCollector = new OutputCollector();
  val outputCasting : OutputCollector = new OutputCollector();
  val outputMatchError : OutputCollector = new OutputCollector();
  var showgraph : Boolean = false;
  var analyzedClasses : Int = 0;
  var analyzedMethods : Int = 0;
  var nativeMethodsSemantics : List[NativeMethodSemantics] = BooleanNativeMethodSemantics :: IntegerNativeMethodSemantics :: ObjectNativeMethodSemantics :: Nil;
  var currentClass : String = null;
  var currentMethod : String = null;
  var semanticsComputing : Boolean = false;
  
  var classes : List[ClassDefinition]= Nil;
  //var compiledMethods : List[Object]= Nil;
  var scalaType : Object= null;

  
  def getForwardSemantics[S <: State[S]](state : S, methodCall : MethodCall) : S = this.getSemantics(state, methodCall, true);
  
  def getBackwardSemantics[S <: State[S]](state : S, methodCall : MethodCall) : S = this.getSemantics(state, methodCall, false);
  
  private def getSemantics[S <: State[S]](state : S, methodCall : MethodCall, forward : Boolean) : S = {
	  val body : Statement = methodCall.method.normalize();
	  var result : S = state.bottom();
	  //Method call used to represent a goto statement to a while label
      if(body.isInstanceOf[Variable] && body.asInstanceOf[Variable].getName().length>=5 && body.asInstanceOf[Variable].getName().substring(0, 5).equals("while")) 
        return state;
      
	  if(! body.isInstanceOf[FieldAccess]) return state; //TODO: Sometimes it is a variable, check if $this is implicit!
	  val castedStatement : FieldAccess = body.asInstanceOf[FieldAccess]
      val calledMethod : String = castedStatement.field
      val parameters : List[Statement] = methodCall.parameters
      val typeparameters : List[Type] = methodCall.parametricTypes
      val returnedtype : Type = methodCall.returnedType
      for(obj <- castedStatement.objs) {
        result=result.lub(result, analyzeMethodCall[S](obj, calledMethod, parameters, typeparameters, returnedtype, state, forward))
      }
	  result;
	}
	
	private def analyzeMethodCall[S <: State[S]](obj : Statement, calledMethod : String, parameters : List[Statement], typeparameters : List[Type], returnedtype : Type, initialState : S, forward : Boolean) : S = {
	  val (calledExpr, resultingState) = UtilitiesOnStates.forwardExecuteStatement[S](initialState, obj);
	  val (parametersExpr, resultingState1) = UtilitiesOnStates.forwardExecuteListStatements[S](resultingState, parameters);
	  if(calledExpr.isBottom)
			  return initialState.bottom();
	  if(calledExpr.isTop)
			  return initialState.top();
	  applyNativeSemantics(calledMethod, calledExpr, parametersExpr, typeparameters, returnedtype, resultingState1, forward);
	}
 
	private def applyNativeSemantics[S <: State[S]](invokedMethod : String, thisExpr : SymbolicAbstractValue[S], parametersExpr : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S, forward : Boolean) : S = {
	  val sems=nativeMethodsSemantics
	  var result = state.top();
	  for(sem <- sems) {
		  val res : Option[S] = if(forward)
			  sem.applyForwardNativeSemantics[S](thisExpr, invokedMethod, parametersExpr, typeparameters, returnedtype, state);
          else sem.applyBackwardNativeSemantics[S](thisExpr, invokedMethod, parametersExpr, typeparameters, returnedtype, state);
	  	  if(res.isInstanceOf[Some[S]]) result=result.glb(result, res.get);
      }
	  return result;
	}
  
}

object AnalysisTimer {
	var lastValue : Option[Long] = None
	var totalTime : Long = 0;
  	
 	def start() = lastValue=Some(System.currentTimeMillis())
  
 	def stop() = lastValue match {
 	  case Some(l) => totalTime=totalTime+(System.currentTimeMillis()-l)
 	  case None => System.out.println("Timer not started before!");
    }
 	
 	def reset() = totalTime=0; lastValue=None;
}