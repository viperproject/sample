package ch.ethz.inf.pm.td.domain

import apron._
import ch.ethz.inf.pm.sample.abstractdomain.SemanticAnalysis
import ch.ethz.inf.pm.sample.property.{DivisionByZero, SingleStatementProperty, Property}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.oorepresentation.NativeMethodSemantics
import ch.ethz.inf.pm.td.semantics.Environment

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 5:50 PM
 * 
 */
class TouchAnalysisWithApron[D <: NumericalDomain[D]] extends TouchAnalysis[D] {

  override def getLabel(): String = "TouchDevelop analysis with APRON"

  override def parameters(): List[(String, Any)] = List(("Domain", List("Sign", "Interval", "ApronInterval", "ApronOctagons", "ApronPolka")))

  /** Initialize with some arbitrary numerical domain. Extend this to APRON later */
  override def setParameter(label: String, value: Any) { label match {
    case "Domain" => value match {
      case "ApronIntervals" => domain = new ApronInterface(new Abstract1(new Box(), new apron.Environment()), new Box()).asInstanceOf[D]
      case "ApronOctagons" => domain = new ApronInterface(new Abstract1(new Octagon(), new apron.Environment()), new Octagon()).asInstanceOf[D]
      case "ApronPolka" => domain = new ApronInterface(new Abstract1(new Polka(false), new apron.Environment()), new Polka(false)).asInstanceOf[D]
      case _ => super.setParameter(label,value)
    }
    case _ => super.setParameter(label,value)
  }}

}
