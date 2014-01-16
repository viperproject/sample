package ch.ethz.inf.pm.td.analysis

import apron._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.td.domain.{InvalidAnd, StringsAnd}
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._
import ch.ethz.inf.pm.sample.abstractdomain.SemanticDomain

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 5:50 PM
 * 
 */
class TouchAnalysisWithApron[D <: NumericalDomain[D],V <:StringValueDomain[V], S <: StringDomain[V,S]]
  extends TouchAnalysis[D,V,S] {

  override def getLabel(): String = "TouchDevelop analysis with APRON"

  override def numericalDomainList = ("Domain", List("ApronInterval", "ApronOctagons", "ApronPolka", "ApronLinearEqualities"))

  override def getInitialState(): StringsAnd[InvalidAnd[D],V,S] = {
    val numericSubDomain = domain match {
      case "ApronInterval" =>
        val man = new Box()
        new ApronInterface(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronOctagons" =>
        val man = new Octagon()
        new ApronInterface(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronPolka" =>
        val man = new Polka(false)
        new ApronInterface(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronLinearEqualities" =>
        val man = new PolkaEq()
        new ApronInterface(None, man, env = Set.empty).factory().asInstanceOf[D]
    }
    val invalidAndSubDomain = new InvalidAnd(numericSubDomain)
    stringDomain match{
      case "Bricks" => new StringsAnd[InvalidAnd[D],V,S](invalidAndSubDomain, new Bricks().asInstanceOf[S])
      case _ => new StringsAnd[InvalidAnd[D],V,S](invalidAndSubDomain)
    }
  }

}
