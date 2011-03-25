package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

/**
 * Constrined Polyhedra Abstract Domain.
 * 
 * If we do not want to pass any constraints we pass an empty set of
 * coefficients and 0 as a number of variables per inequality.
 * 
 * @author Milos Novacek
 *
 */
class ConstrainedPolyhedra(	val cpstate : Abstract1, 
							val cpdomain : Manager, 
							val coefficients : Set[Int],
							val numOfVariables: Int,
							val setOfIdentifiers: Set[Identifier]) extends ApronInterface(cpstate, cpdomain) {
	
	private val checkVariableSet : Boolean = !(setOfIdentifiers.isEmpty);
	private val checkCoef : Boolean = !(coefficients.isEmpty);
	private val checkNumOfVariables = !(numOfVariables < 1);
	
	
	/*
	 * **************************************************
	 * --------------------------------------------------
	 * 					PUBLIC METHODS
	 * --------------------------------------------------
	 * **************************************************
	 */

	
	override def assign (variable : Identifier, expr : Expression) : ConstrainedPolyhedra = {
		val apInterface = super.assign(variable, expr);
    for (linCons <- state.toLincons(domain)) {
      println("Lin Constraint " + linCons.getLinterms.toString);
    }
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables, this.setOfIdentifiers);
	}
	
	/**
	 * This method is the same as in ApronInterface but we only assume those expressions
	 * that satisfy the given constraints.
	 */
	override def assume(expr : Expression) : ConstrainedPolyhedra = {
		if (isExpressionAccepted(expr)) {
			assumeWithoutConstraints(expr);
		} else {
			new ConstrainedPolyhedra(this.cpstate, this.cpdomain, this.coefficients, this.numOfVariables, this.setOfIdentifiers);
		}		
	}
	
	override def bottom() : ConstrainedPolyhedra = {
		val apInterface = super.bottom();
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables, this.setOfIdentifiers);
	}
	
	override def createVariable (variable : Identifier, typ : Type) : ConstrainedPolyhedra = {
		val apInterface = super.createVariable(variable, typ);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables, this.setOfIdentifiers);
	}
	
	override def factory() : ConstrainedPolyhedra = top();
	
	override def glb(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.glb(left, right);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables, this.setOfIdentifiers);
	}
	
	override def lub(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.lub(left, right);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables, this.setOfIdentifiers);
	}
	
	override def removeVariable(variable : Identifier) : ConstrainedPolyhedra = { 
		val apInterface = super.removeVariable(variable);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables,this.setOfIdentifiers);
	}
	
	override def setToTop(variable : Identifier) : ConstrainedPolyhedra = {
		val apInterface = super.setToTop(variable);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables, this.setOfIdentifiers);
	}
	
	override def top() : ConstrainedPolyhedra = {
		val apInterface = super.top;
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables, this.setOfIdentifiers); 
	}
	
	override def widening(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.widening(left, right);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables,this.setOfIdentifiers);
	}
	
	/*
	 * **************************************************
	 * --------------------------------------------------
	 * 					PRIVATE METHODS
	 * --------------------------------------------------
	 * **************************************************
	 */
	
	/**
	 * 
	 * @param expr
	 * @return
	 */
	private def assumeWithoutConstraints(expr : Expression) : ConstrainedPolyhedra = {
		val apInterface = super.assume(expr);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefficients, this.numOfVariables, this.setOfIdentifiers);
	}
	
	/**
	 * Method for checking if the given expression satisfies constraints
	 * 
	 * @param expr Expression to be checked if it satisfies constraints
	 * @return TRUE if the expression satisfies the constrints, otherwise FALSE
	 */
	private def isExpressionAccepted(expr : Expression) : Boolean = {
		
		// In the case we haven't passed in any constraints, we accept all expressions
		if (checkVariableSet || checkCoef || checkNumOfVariables) {
			
			/* Now we check whether the expressions is within the given constraints 
			 * (coefficients and number of variables per inequality)
			 */ 
			Normalizer.conditionalExpressionToMonomes(expr) match {
			    case None => {
			    	System.out.println(expr.getClass.toString + " " + expr.toString  + "is not supproted or is not of a right type");
			    	return false;
			    }
			    case Some((monomes, constant)) => {
			    	if (checkNumOfVariables && monomes.length > numOfVariables) {
			    		
			    		//-------------------------- Comment out----
			    		//println("Number of varilables in " + expr.toString + " is bigger than " + numOfVariables);
			    		//println("Therefore, " + expr.toString + " is not accepted");
			    		//------------------------------------------
			    		
			    		return false;
			    	} else {
			    		 for(monome <- monomes) {
			    			 val (index, variable) = monome;
			    			 if (   (checkCoef && !coefficients.contains(index))  
			    				 || (checkVariableSet && !setOfIdentifiers.contains(variable))) {			    				 
			    				 //-------------------------- Comment out----
			    				 //println(index + " is not contained in " + coefficients.toString);
			    				 //println(expr.toString  + " does not satisfy given constriants.");
			    				 //------------------------------------------			    				 
			    				 return false;
			    			 }
			    		 }
			    	}
			    	return true;
			    }
			}
		} else {
			return true;
		}
	}
}