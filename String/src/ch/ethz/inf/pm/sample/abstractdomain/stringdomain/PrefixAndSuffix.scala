package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._

case class PrefixAndSuffix(_1: Prefix, _2: Suffix)
  extends SemanticCartesianProductDomain[Prefix, Suffix, PrefixAndSuffix] {

  def factory(a: Prefix, b: Suffix) = new PrefixAndSuffix(a, b)

  override def merge(r: Replacement) = new PrefixAndSuffix(this._1.merge(r), this._2.merge(r));

  override def getStringOfId(id: Identifier): String = _1.getStringOfId(id) + "; " + _2.getStringOfId(id);
}