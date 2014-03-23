package ch.ethz.inf.pm.td.analysis

import apron._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._
import ch.ethz.inf.pm.td.domain.StringsAnd
import ch.ethz.inf.pm.td.domain.InvalidAnd
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.compiler.TouchCompiler

/**
 *
 * Defines an analysis, where numeric variables are packed into bins, and only those variables in the same bin
 * are related by a relational domain.
 *
 */
class TouchAnalysisWithPacking[D <: NumericalDomain[D], V <: StringValueDomain[V], S <: StringDomain[V, S]]
  extends TouchAnalysis[VariablePackingDomain[BoxedNonRelationalNumericalDomain[numericaldomain.Interval], D], V, S] {

  override def getLabel(): String = "TouchDevelop/Packing"

  override def numericalDomainList = ("Domain", List("ApronOctagons", "ApronPolka", "ApronLinearEqualities"))

  override def getInitialState(): StringsAnd[InvalidAnd[VariablePackingDomain[BoxedNonRelationalNumericalDomain[numericaldomain.Interval], D]], V, S] = {

    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]
    assert(compiler != null)

    val relationalNumericalDomain = domain match {
      case "ApronOctagons" =>
        val man = new Octagon()
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronPolka" =>
        val man = new Polka(false)
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronPolkaStrict" =>
        val man = new Polka(true)
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronLinearEqualities" =>
        val man = new PolkaEq()
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
    }
    val cheapNum = new BoxedNonRelationalNumericalDomain(new numericaldomain.Interval(0, 0))
    val packingDomain = VariablePackingDomain(cheapNum, relationalNumericalDomain, PackStorage.make(relationalNumericalDomain))

    val invalidAndSubDomain = new InvalidAnd(packingDomain)
    stringDomain match {
      case "Bricks" => new StringsAnd[InvalidAnd[VariablePackingDomain[BoxedNonRelationalNumericalDomain[numericaldomain.Interval], D]], V, S](invalidAndSubDomain, new Bricks().asInstanceOf[S])
      case _ => new StringsAnd[InvalidAnd[VariablePackingDomain[BoxedNonRelationalNumericalDomain[numericaldomain.Interval], D]], V, S](invalidAndSubDomain)
    }

  }

}