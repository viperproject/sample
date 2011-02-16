package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import scala.collection.mutable.HashSet

/**
 * Constrined Polyhedra Abstract Domain.
 * 
 * @author Milos Novacek
 *
 */
class ConstrainedPolyhedra(	val cpstate : Abstract1, 
							val cpdomain : Manager, 
							val coefitients : HashSet[Int],
							val numOfVariables: Int) extends ApronInterface(cpstate, cpdomain) {

	
	
	/*
	 * **************************************************
	 * --------------------------------------------------
	 * 					PUBLIC METHODS
	 * --------------------------------------------------
	 * **************************************************
	 */

	
	override def assign (variable : Identifier, expr : Expression) : ConstrainedPolyhedra = {
		val apInterface = super.assign(variable, expr);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
	}
	
	override def assume(expr : Expression) : ConstrainedPolyhedra = {
		if (isExpressionAccepted(expr)) {
			assumeWithoutConstraints(expr);
		} else {
			new ConstrainedPolyhedra(this.cpstate, this.cpdomain, this.coefitients, this.numOfVariables);
		}		
	}
	
	override def bottom() : ConstrainedPolyhedra = {
		val apInterface = super.bottom();
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
	}
	
	override def createVariable (variable : Identifier, typ : Type) : ConstrainedPolyhedra = {
		val apInterface = super.createVariable(variable, typ);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
	}
	
	override def factory() : ConstrainedPolyhedra = top();
	
	override def glb(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.glb(left, right);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
	}
	
	override def lub(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.lub(left, right);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
	}
	
	override def removeVariable(variable : Identifier) : ConstrainedPolyhedra = { 
		val apInterface = super.removeVariable(variable);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
	}
	
	override def setToTop(variable : Identifier) : ConstrainedPolyhedra = {
		val apInterface = super.setToTop(variable);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
	}
	
	override def top() : ConstrainedPolyhedra = {
		val apInterface = super.top;
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables); 
	}
	
	override def widening(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.widening(left, right);
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
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
		new ConstrainedPolyhedra(apInterface.state, apInterface.domain, this.coefitients, this.numOfVariables);
	}
	
	/**
	 * Method for checking if the given expression satisfies constraints
	 * 
	 * @param expr Expression to be checked if it satisfies constraints
	 * @return TRUE if the expression satisfies the constrints, otherwise FALSE
	 */
	private def isExpressionAccepted(expr : Expression) : Boolean = Normalizer.conditionalExpressionToMonomes(expr) match {
	    case None => {
	    	System.out.println(expr.getClass.toString + " " + expr.toString  + "is not supproted or is not of a right type");
	    	return false;
	    }
	    case Some((monomes, constant)) => {
	    	if (monomes.length > numOfVariables) {
	    		println("Number of varilables in " + expr.toString + " is bigger than " + numOfVariables);
	    		println("Therefore, " + expr.toString + " is not accepted");
	    		return false;
	    	} else {
	    		 for(monome <- monomes) {
	    			 val (index, variable) = monome;
	    			 if (!coefitients.contains(index)) {
	    				 //-------------------------- Comment out----
	    				 println(index + " is not contained in " + coefitients.toString);
	    				 
	    				 System.out.println(expr.toString  + " does not satisfy given constriants.");
	    				 return false;
	    			 }
	    		 }
	    	}
	    	return true;
	    }	
	}
}