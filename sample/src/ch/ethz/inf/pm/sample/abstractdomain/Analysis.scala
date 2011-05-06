package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.property.Property
import ch.ethz.inf.pm.sample.oorepresentation.NativeMethodSemantics

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
  def getProperties() : Set[Property];

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
  def reset() : Unit;
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