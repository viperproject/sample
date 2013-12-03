package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._

class PrefixAndSuffix(protected var a1 : Prefix, protected var a2 : Suffix)
     extends SemanticCartesianProductDomain[Prefix, Suffix, PrefixAndSuffix](a1,a2)
     with SimplifiedSemanticDomain[PrefixAndSuffix]
 {
       def factory(a:Prefix,b:Suffix) = new PrefixAndSuffix(a,b)
       override def merge(r : Replacement) = new PrefixAndSuffix(this._1.merge(r), this._2.merge(r));
       override def getStringOfId(id : Identifier) : String = _1.getStringOfId(id) + "; " + _2.getStringOfId(id);
 }