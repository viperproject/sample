package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.property.{OutputCollector, Property}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.util.Timer

/**
 * An <code>Analysis</code> represents the interface to provide an analysis to Sample
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
trait Analysis {

  def analyze[S <: State[S]](toAnalyze : String, entryState : S, output : OutputCollector) : List[(Type, MethodDeclaration, ControlFlowGraphExecution[S])] =
    this.analyze( toAnalyze ::Nil, entryState, output)

  def analyze[S <: State[S]](toAnalyze : List[String], entryState : S, output : OutputCollector) : List[(Type, MethodDeclaration, ControlFlowGraphExecution[S])] = {
    var res =  List.empty[(Type, MethodDeclaration, ControlFlowGraphExecution[S])]
    Timer.start;
    for (methodName <- toAnalyze) {
      val methods = SystemParameters.compiler.getMethods(methodName)
      for((c,x) <- methods) {
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.begin("Analyzing method "+x.name.toString()+" in class "+c.name.toString());
        SystemParameters.currentMethod = x.name.toString
        val s = x.forwardSemantics[S](entryState)
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.end("End of the analysis of method "+x.name.toString()+" in class "+c.name.toString());
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.begin("Checking the property over method "+x.name.toString()+" in class "+c.name.toString());
        if(SystemParameters.property!=null) {
          SystemParameters.property.check(c.name.getThisType(), x, s, output)
          res = res ::: ((c.name.getThisType(), x, s) :: Nil)
        }
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.end("End of the check of the property over method "+x.name.toString()+" in class "+c.name.toString());
        SystemParameters.currentMethod = null
      }
    }
    if(SystemParameters.property!=null) {
      SystemParameters.progressOutput.begin("Finalizing the checking the property")
      SystemParameters.property.finalizeChecking(output)
      SystemParameters.progressOutput.end("End of the checking of the property")
    }
    System.out.println(output.output()+"STATISTICS [ Property validated:"+output.validated()+", Warnings:"+output.notvalidated()+", Inferred contracts:"+output.inferredcontracts()+", Time of analyisis: " + Timer.stop+" ]")
    res
  }

  /**
   This method returns a short name for the analysis

   @return a short string containing the label of the analysis
  */
  def getLabel() : String;

  /**
   This method returns the set of the parameters of the analysis

   @return a set containing the name of the parameters and an object representing the type of the parameter.
   The supported options are: Int, Boolean, or a List of strings
  */
  def parameters() : List[(String, Any)];

  /**
  This method sets a parameter

   @param label the name of the parameter
   @param value the new value for the parameter
  */
  def setParameter(label : String, value : Any) : Unit;

  /**
   This method returns the set of the properties that can be applied to this analysis

   @return the possible properties
  */
  def getProperties : List[Property]

  /**
   This method returns the list of the semantics of "native" methods. By native methods
   we identify all the methods whose semantics is defined by hand and not through pre
   and post conditions

   @return a (possibly empty) list of methods' semantics
  */
  def getNativeMethodsSemantics() : List[NativeMethodSemantics];

  /**
   This method resets the analysis before starting it. It can be used to clean static
   fields before re-running the analysis.
  */
  def reset()
}

/**
 * An <code>Analysis</code> represents the interface to provide a semantic analysis to Sample
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
trait SemanticAnalysis[T <: SemanticDomain[T]] extends Analysis {
  /**
   This method returns the state that has to be used to start the analysis

   @return the initial state
  */
  def getInitialState() : T;
}

/**
 * An <code>Analysis</code> represents the interface to provide a heap analysis to Sample
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
trait HeapAnalysis[T <: HeapDomain[T, I], I <: HeapIdentifier[I]] extends Analysis {
  /**
   This method returns the state that has to be used to start the analysis

   @return the initial state
  */
  def getInitialState() : T;
}

/** A simple analyzer with a name. */
class SimpleAnalyzer(label:String) extends Analysis {
  def getLabel() = label
  def parameters() : List[(String, Any)] = Nil
  def setParameter(label : String, value : Any) {}
  def getProperties() : List[Property] = Nil
  def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil
  def reset() {}
}