package ch.ethz.inf.pm.td.analysis

import apron._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.td.domain.{InvalidAnd, StringsAnd}

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 5:50 PM
 * 
 */
class TouchAnalysisWithApron[D <: NumericalDomain[D]] extends TouchAnalysis[D] {

  override def getLabel(): String = "TouchDevelop analysis with APRON"

  override def parameters(): List[(String, Any)] = List(("Domain", List("Sign", "Interval", "ApronInterval", "ApronOctagons", "ApronPolka", "ApronLinearEqualities")))

  override def getInitialState(): StringsAnd[InvalidAnd[D]] = {
    new StringsAnd(new InvalidAnd(
      domain match {
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
    ))
  }

}
