/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._

case class SurelyAndMaybeContainedCharacters(
    _1: SurelyContainedCharacters,
    _2: MaybeContainedCharacters)
  extends SemanticCartesianProductDomain[
    SurelyContainedCharacters,
    MaybeContainedCharacters,
    SurelyAndMaybeContainedCharacters] {
      override def merge(r : Replacement) = SurelyAndMaybeContainedCharacters(this._1.merge(r), this._2.merge(r))

  def factory(a:SurelyContainedCharacters,b:MaybeContainedCharacters) = SurelyAndMaybeContainedCharacters(a, b)

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
       
       override def getStringOfId(id : Identifier) : String = "{" + _1.getStringOfId(id) + "}; {" + _2.getStringOfId(id) + "}"
}