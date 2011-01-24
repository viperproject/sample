package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class SurelyAndMaybeContainedCharacters(protected var a1 : SurelyContainedCharacters, 
        protected var a2 : MaybeContainedCharacters)
     extends SemanticCartesianProductDomain[SurelyContainedCharacters, MaybeContainedCharacters, 
                                    SurelyAndMaybeContainedCharacters](a1,a2)
     with SimplifiedSemanticDomain[SurelyAndMaybeContainedCharacters]
 {
       def factory() : SurelyAndMaybeContainedCharacters = new SurelyAndMaybeContainedCharacters(
    		   new SurelyContainedCharacters(), new MaybeContainedCharacters()); 
       /*
       def removeVariable(variable : Identifier) : SurelyAndMaybeContainedCharacters = {
         val result : SurelyAndMaybeContainedCharacters = this.factory();
         result.d1 = d1.remove(variable);
         result.d2 = d2.remove(variable); 
         result
       }
       
       def createVariable(variable : Identifier, typ : Type) : SurelyAndMaybeContainedCharacters = 
         this;
       
       def assume(expr : Expression) : SurelyAndMaybeContainedCharacters = {
         val result : SurelyAndMaybeContainedCharacters = this.factory();
		 result.d1 = d1.assume(expr);
         result.d2 = d2.assume(expr); 
         result
       }
       
       def assign(variable : Identifier, expr : Expression) : SurelyAndMaybeContainedCharacters = {
         val result : SurelyAndMaybeContainedCharacters = this.factory();
		 result.d1 = d1.assign(variable,expr);
         result.d2 = d2.assign(variable,expr); 
         result
       }
       
       def setToTop(variable : Identifier) : SurelyAndMaybeContainedCharacters = {
         this.removeVariable(variable);
       }*/
       
       def getStringOfId(id : Identifier) : String = "{" + d1.getStringOfId(id) + "}; {" + d2.getStringOfId(id) + "}";
 }