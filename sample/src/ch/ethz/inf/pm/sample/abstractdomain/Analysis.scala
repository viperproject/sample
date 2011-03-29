package ch.ethz.inf.pm.sample.abstractdomain

/**
 * An <code>Analysis</code> represents the interface to provide an analysis to Sample
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
trait Analysis {

  /**
   This method returns a short name for the analysis

   @return a short string containing the label of the analysis
  */
  def getLabel() : String;

  /**
   This method returns the set of the parameters of the analysis

   @return a set containing the name of the parameters and a boolean
   parameter (true iff the parameter is an integer, false iff it is a boolean)
  */
  def parameters() : Set[(String, Boolean)];

  /**
   This method sets an integer parameter

   @param label the name of the parameter
   @param value the new value for the parameter
  */
  def setIntegerParameter(label : String, value : Int) : Unit;

  /**
   This method sets a boolean parameter

   @param label the name of the parameter
   @param value the new value for the parameter
  */
  def setBooleanParameter(label : String, value : Boolean) : Unit;

  /**
   This method returns the state that has to be used to start the analysis

   @return the initial state
  */
  def getInitialState[S <: SemanticDomain[S]]() : S;
}