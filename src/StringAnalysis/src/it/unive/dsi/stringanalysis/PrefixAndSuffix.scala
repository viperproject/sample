package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class PrefixAndSuffix(protected var a1 : Prefix, protected var a2 : Suffix)
     extends SemanticCartesianProductDomain[Prefix, Suffix, PrefixAndSuffix](a1,a2)
     with SimplifiedSemanticDomain[PrefixAndSuffix]
 {
       def factory() : PrefixAndSuffix = new PrefixAndSuffix(new Prefix(), new Suffix()); 
     
       def getStringOfId(id : Identifier) : String = d1.getStringOfId(id) + "; " + d2.getStringOfId(id);
 }