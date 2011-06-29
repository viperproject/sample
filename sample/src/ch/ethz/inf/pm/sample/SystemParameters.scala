package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

/**
 * <code>SystemParameters</code> contains all the parameters of Sample
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object SystemParameters {

  /**
   The number of iterations after whom widening is applied
  */
  val wideningLimit : Int = 5;
  /**
   The path of the file currently analyzed
  */
  var currentFile : String = "<not yet initialized>";
  /**
   The semantics of methods defined by hand
  */
  private var nativeMethodsSemantics : List[NativeMethodSemantics] = ArrayNativeMethodSemantics :: Nil;
  /**
   The class currently analyzed
  */
  var currentClass : Type = null;
  /**
   The method currently analyzed
  */
  var currentMethod : String = null;
  //TODO:Remove it
  var semanticsComputing : Boolean = false;
  /**
   * Ir true Sample supposes that if we invoke numerical methods like + on an object of any type we are
   * performing arithmetical operations
   */
  var ignoreTypeForNumericalMethods : Boolean = false;
  /**
   The cfg currently under analysis
  */
  var currentCFG : ControlFlowGraph = null;
  /**
   The output for the window that shows the progresses of the analysis
  */
  var progressOutput : ScreenOutput = null;
  /**
   The output for the window that shows the results of the analysis
  */
  var analysisOutput : ScreenOutput = null;
  /**
   The timer that collects the amount of time spent by the heap abstraction
  */
  val heapTimer : Timer = new Timer;
  /**
   The timer that collects the amount of time spent by the semantic analysis
  */
  val domainTimer : Timer = new Timer;
  /**
   An instance of the current type system
  */
  var typ : Type = null;
  /**
   The compiler used to compile the given files
  */
  var compiler : Compiler = null;
  /**
   The checked property
  */
  var property : Property = null;

  def getType() = typ;

  def addNativeMethodsSemantics(l : List[NativeMethodSemantics]) = {
    for(s1 <- l) {
      var already = false;
      for(s2 <- nativeMethodsSemantics)
        if(s1==s2) already=true;
      if(! already)
       nativeMethodsSemantics=nativeMethodsSemantics:::s1::Nil
    }
  }

  def resetNativeMethodsSemantics() : Unit =  nativeMethodsSemantics=Nil

  def setProperty(p : Property) = property=p;
  def setCompiler(c : Compiler) = compiler=c;
  def setProgressOutput(p : ScreenOutput) = progressOutput=p;
  def setAnalysisOutput(p : ScreenOutput) = analysisOutput=p;

  //TODO:This and the following methods should not be there
  def getForwardSemantics[S <: State[S]](state : S, methodCall : MethodCall) : S = this.getSemantics(state, methodCall, true);
  
  def getBackwardSemantics[S <: State[S]](state : S, methodCall : MethodCall) : S = this.getSemantics(state, methodCall, false);
  
  private def getSemantics[S <: State[S]](state : S, methodCall : MethodCall, forward : Boolean) : S = {
	  val body : Statement = methodCall.method.normalize();
	  var result : S = state.bottom();
	  //Method call used to represent a goto statement to a while label
      if(body.isInstanceOf[Variable] && body.asInstanceOf[Variable].getName().length>=5 && body.asInstanceOf[Variable].getName().substring(0, 5).equals("while")) 
        throw new Exception("This should not appear here!");//return state;
      
	  if(! body.isInstanceOf[FieldAccess]) return state; //TODO: Sometimes it is a variable, check if $this is implicit!
	  val castedStatement : FieldAccess = body.asInstanceOf[FieldAccess]
      val calledMethod : String = castedStatement.field
      val parameters : List[Statement] = methodCall.parameters
      val typeparameters : List[Type] = methodCall.parametricTypes
      val returnedtype : Type = methodCall.returnedType
      for(obj <- castedStatement.objs) {
        result=result.lub(result, analyzeMethodCall[S](obj, calledMethod, parameters, typeparameters, returnedtype, state, methodCall.getPC(), forward))
      }
	  result;
	}
	
	private def analyzeMethodCall[S <: State[S]](obj : Statement, calledMethod : String, parameters : List[Statement], typeparameters : List[Type], returnedtype : Type, initialState : S, programpoint : ProgramPoint, forward : Boolean) : S = {
	  val (calledExpr, resultingState) = UtilitiesOnStates.forwardExecuteStatement[S](initialState, obj);
	  val (parametersExpr, resultingState1) = UtilitiesOnStates.forwardExecuteListStatements[S](resultingState, parameters);
	  if(calledExpr.isBottom)
			  return initialState.bottom();
	  if(calledExpr.isTop)
			  return initialState.top();
	  applyNativeSemantics(calledMethod, calledExpr, parametersExpr, typeparameters, returnedtype, resultingState1, programpoint, forward);
	}
 
	private def applyNativeSemantics[S <: State[S]](invokedMethod : String, thisExpr : SymbolicAbstractValue[S], parametersExpr : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S, programpoint : ProgramPoint, forward : Boolean) : S = {
	  val sems=nativeMethodsSemantics
	  var result = state.top();
	  for(sem <- sems) {
		  val res : Option[S] = if(forward)
			  sem.applyForwardNativeSemantics[S](thisExpr, invokedMethod, parametersExpr, typeparameters, returnedtype, programpoint, state);
          else sem.applyBackwardNativeSemantics[S](thisExpr, invokedMethod, parametersExpr, typeparameters, returnedtype, programpoint, state);
	  	  if(res.isInstanceOf[Some[S]]) result=result.glb(result, res.get);
      }
	  return result;
	}
  
}

//TODO:Comment the following code
trait ScreenOutput {
  def appendString(s : String);
  def getString() : String;
}

class StringCollector extends ScreenOutput {
  var s : String="";
  override def appendString(s: String): Unit = {
    this.s=this.s+"\n" + s
  }

  override def getString: String = {
    return this.s
  }
}

class Timer {
	var lastValue : Option[Long] = None
	var totalTime : Long = 0;
  	
 	def start() = lastValue=Some(System.currentTimeMillis())
  
 	def stop() = lastValue match {
 	  case Some(l) => totalTime=totalTime+(System.currentTimeMillis()-l)
 	  case None => System.out.println("Timer not started before!");
    }
 	
 	def reset() = totalTime=0; lastValue=None;
}