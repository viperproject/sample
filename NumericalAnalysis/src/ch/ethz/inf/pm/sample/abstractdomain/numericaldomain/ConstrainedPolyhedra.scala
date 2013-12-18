package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.Property
import apron._

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
							val setOfStringOfID: Set[String],
              val setOfIdentifiers: Set[Identifier],
              cpEnv : Set[Identifier]) extends ApronInterface(Some(cpstate), cpdomain, env = cpEnv) {

	private val checkVariableSet : Boolean = !(setOfStringOfID.isEmpty) || !(setOfIdentifiers.isEmpty)
	private val checkCoef : Boolean = !(coefficients.isEmpty)
	private val checkNumOfVariables = !(numOfVariables < 1)
	
	
	/*
	 * **************************************************
	 * --------------------------------------------------
	 * 					PUBLIC METHODS
	 * --------------------------------------------------
	 * **************************************************
	 */

	
	override def assign (variable : Identifier, expr : Expression) : ConstrainedPolyhedra = {
		if (checkVariableSet) {
			val apInterface = super.assign(variable, expr)
			//new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID)
			checkAndRemoveLinConstraints(new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds))
		} else {
			//println("this should not be executed")
			new ConstrainedPolyhedra(this.cpstate, this.cpdomain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, this.getIds)
		}
	}
	
	/**
	 * This method is the same as in ApronInterface but we only assume those expressions
	 * that satisfy the given constraints.
	 */
	override def assume(expr : Expression) : ConstrainedPolyhedra = {
		val apInterface = super.assume(expr)
    val result =
		checkAndRemoveLinConstraints(new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds))
    return result
		/*
		if (isExpressionAccepted(expr)) {
			assumeWithoutConstraints(expr)
		} else {
			new ConstrainedPolyhedra(this.cpstate, this.cpdomain, this.coefficients, this.numOfVariables, this.setOfStringOfID)
		}	*/
	}
	
	override def bottom() : ConstrainedPolyhedra = {
		val apInterface = super.bottom()
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
	}
	
	override def createVariable (variable : Identifier, typ : Type) : ConstrainedPolyhedra = {
		val apInterface = super.createVariable(variable, typ)
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
	}
	
	override def factory() : ConstrainedPolyhedra = top()
	
	override def glb(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.glb(left, right)
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
	}
	
	override def lub(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.lub(left, right)
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
	}
	
	override def removeVariable(variable : Identifier) : ConstrainedPolyhedra = { 
		val apInterface = super.removeVariable(variable)
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables,this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
	}
	
	override def setToTop(variable : Identifier) : ConstrainedPolyhedra = {
		val apInterface = super.setToTop(variable)
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
	}
	
	override def top() : ConstrainedPolyhedra = {
		val apInterface = super.top
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
	}
	
	override def widening(left : ApronInterface, right : ApronInterface) : ConstrainedPolyhedra =  {
		val apInterface = super.widening(left, right)
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables,this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
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
		val apInterface = super.assume(expr)
		new ConstrainedPolyhedra(apInterface.instantiateState(), apInterface.domain, this.coefficients, this.numOfVariables, this.setOfStringOfID, this.setOfIdentifiers, apInterface.getIds)
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
					//TODO: implement this properly, uncomment the two lines below and comment the last one
			    	//System.out.println(expr.getClass.toString + " " + expr.toString  + " is not supproted or is not of a right type")
					//return false
			    	return true
			    }
			    case Some((monomes, constant)) => {
			    	if (checkNumOfVariables && monomes.length > numOfVariables) {
			    		
			    		//-------------------------- Comment out----
			    		//println("Number of varilables in " + expr.toString + " is bigger than " + numOfVariables)
			    		//println("Therefore, " + expr.toString + " is not accepted")
			    		//------------------------------------------
			    		
			    		return false
			    	} else {
			    		 for(monome <- monomes) {
			    			 val (index, variable) = monome
			    			 if (   (checkCoef && !coefficients.contains(index))  
			    				 || (checkVariableSet && !setOfStringOfID.contains(variable.getName))) {
			    				 //-------------------------- Comment out----
			    				 //println(index + " is not contained in " + coefficients.toString)
			    				 //println(expr.toString  + " does not satisfy given constriants.")
			    				 //------------------------------------------			    				 
			    				 return false
			    			 }
			    		 }
			    	}
					//println(expr.toString + " satisfies the constraints")
			    	return true
			    }
			}
		} else {
			//println("we do not have any constraints")
			return true
		}
	}

  private def isTermInIdSet[I<:HeapIdentifier[I]](linterm: Linterm1): Boolean = {
    for (id <- setOfIdentifiers) {
      id match {
        case x: I => {
          if (linterm.getVariable.contains(id.getProgramPoint.toString)) {
            return true
          }
        }
        case x: VariableIdentifier => {
          if (linterm.getVariable.equals(x.getName)) {
            return true
          }
        }
      }
    }
    return false
  }

  private def checkAndRemoveLinConstraints(cp : ConstrainedPolyhedra) : ConstrainedPolyhedra = {
//		  println("INVOKED")
		  var linCons = Set.empty[Lincons1]
		  for (linCon <- cp.copyState().toLincons(domain)) {
			  var addCon = true
			  var numOfCoef = 0
			  for (term <- linCon.getLinterms) {
				  val termCoef = term.getCoefficient.toString.toInt
				  if (termCoef != 0) {
					  numOfCoef = numOfCoef + 1
				  }
				  if (checkCoef) {
					  if (!coefficients.contains(termCoef) && termCoef != 0) {
						  addCon = false
					  }
				  }
				  if (checkVariableSet && !setOfStringOfID.contains(term.getVariable) && termCoef != 0 && !isTermInIdSet(term)) {
					  addCon = false
				  }
			  }
			  if (checkNumOfVariables && numOfCoef > numOfVariables) {
				  addCon = false
			  }
			  if (addCon) {
				  linCons.+=(linCon)
			  }
		  }
		  var newState = new Abstract1(cp.domain, cp.copyState().getEnvironment, true)
		  if (!linCons.isEmpty) {
			  newState = new Abstract1(domain, linCons.toArray)
		  }
		  new ConstrainedPolyhedra(newState, cp.cpdomain, cp.coefficients, cp.numOfVariables, cp.setOfStringOfID, this.setOfIdentifiers, cp.getIds)
	  }
}


class ConstrainedPolyhedraAnalysis extends SemanticAnalysis[ApronInterface] {
	var domain : Manager=null
	def getLabel() : String = "Constrained Polyhedra analysis"
	def parameters() : List[(String, Any)] = List(("Domain", List("Interval", "PPL", "Octagons", "Polka")))
	def setParameter(label : String, value : Any) = label match {
	case "Domain" => value match {
		case "Interval" => domain = new Box()
		case "PPL" => domain = new Polka(false)
		case "Octagons" => domain = new Octagon()
		case "Polka" => domain = new Polka(false)
		}
	}
	def reset() : Unit = Unit
	var coefSet = Set.empty[Int]
	coefSet.+=(-1)
	coefSet.+=(1)
	coefSet.+=(0)
	coefSet.+=(2)
	coefSet.+=(-2)

	def getInitialState() : ApronInterface =
    new ConstrainedPolyhedra(new Abstract1(domain, new Environment()), domain, coefSet, 2, Set.empty[String], Set.empty[Identifier], Set.empty).top()
	def getProperties() : List[Property] = Nil
	def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil
}