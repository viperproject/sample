package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type

trait VariablePackingClassifier {

  def classify(id:Set[Identifier]):Set[VariablePack]

  def classify(id:Identifier):Set[VariablePack] = classify(Set(id))

  def inClass(id:Identifier,c:VariablePack): Boolean = classify(id).contains(c)

  def filterClass(ids:Set[Identifier],c:VariablePack):Set[Identifier] = ids.filter(inClass(_,c))

}

object VariablePackingClassifier {

  object SimplePack extends VariablePack {
    def ids = IdentifierSet.Top
  }

  /**
   * Implements a classifier creating a single huge pack
   */
  object OnePacker extends VariablePackingClassifier {
    override def classify(id: Set[Identifier]): Set[VariablePack] = Set(SimplePack)
  }

  /**
   * Implements a classifier that does not pack anything
   */
  object EmptyPacker extends VariablePackingClassifier {
    override def classify(id: Set[Identifier]): Set[VariablePack] = Set.empty
  }

}

trait VariablePack {
  def ids:IdentifierSet
}

case class VariablePackMap[Relational <: NumericalDomain.Relational[Relational]]
    (classifier:VariablePackingClassifier,dom:Relational,map: Map[VariablePack, Relational],isPureBottom:Boolean = false, isPureTop:Boolean = false)
  extends FunctionalDomain[VariablePack,Relational,VariablePackMap[Relational]]
  with SimplifiedSemanticDomain[VariablePackMap[Relational]]{

  // TODO: As usual, handling of bottom and top is dodgy and requires a refactoring of the functional domain

  override def functionalFactory(map: Map[VariablePack, Relational], isBottom: Boolean, isTop: Boolean): VariablePackMap[Relational] = {
    VariablePackMap(classifier,dom,map,isBottom,isTop)
  }

  override def isBottom:Boolean =
    isPureBottom || map.exists(_._2.isBottom)

  override def isTop:Boolean =
    isPureTop

  override def get(key: VariablePack): Relational =
    map.getOrElse(key, dom.top())

  override def setToTop(variable: Identifier): VariablePackMap[Relational] =
    applyToPacks(variable, {x:Relational => x.setToTop(variable)})

  override def removeVariable(id: Identifier): VariablePackMap[Relational] =
    applyToPacks(id, {x:Relational => x.removeVariable(id)})

  override def ids: IdentifierSet =
    if (map.values.isEmpty) IdentifierSet.Bottom
    else {
      val xs = for ((pack,dom) <- map) yield {
        dom.ids match {
          case IdentifierSet.Top => IdentifierSet.Bottom
          case x => x
        }
      }
      Lattice.bigLub(xs)
    }

  override def assume(expr: Expression): VariablePackMap[Relational] = {
    if (expr.ids.isTop) return this
    applyToPacks(expr.ids.getNonTop, {x:Relational => x.assume(expr)})
  }

  override def createVariable(variable: Identifier, typ: Type): VariablePackMap[Relational] =
    applyToPacks(variable, {x:Relational => x.createVariable(variable,typ)})

  override def assign(variable: Identifier, expr: Expression): VariablePackMap[Relational] =
    applyToPacks(variable, {x:Relational => x.assign(variable,expr)})

  override def getStringOfId(id: Identifier): String =
    classifier.classify(id).map( get(_).getStringOfId(id) ).mkString(",")

  override def merge(f: Replacement): VariablePackMap[Relational] =
    f.value.foldLeft(this) {
      (cur:VariablePackMap[Relational],ids:(Set[Identifier],Set[Identifier])) =>
        cur.applyToPacksSep(ids._1 ++ ids._2, { (x: Relational, y: VariablePack) =>
          val from = classifier.filterClass(ids._1, y)
          val to = classifier.filterClass(ids._2, y)
          val rep = new Replacement()
          rep.value += (from -> to)
          x.merge(rep)
        })
    }

  private def applyToPacks(id: Identifier, in: Relational => Relational):VariablePackMap[Relational] =
    applyToPacks(Set(id),in)

  private def applyToPacks(ids: Set[Identifier], in: Relational => Relational):VariablePackMap[Relational] =
    classifier.classify(ids).foldLeft(this){
      (y:VariablePackMap[Relational],x:VariablePack) => y.functionalFactory(map = y.map + (x -> in(get(x))))
    }

  private def applyToPacksSep(ids: Set[Identifier], in: (Relational,VariablePack) => Relational):VariablePackMap[Relational] =
    classifier.classify(ids).foldLeft(this){
      (y:VariablePackMap[Relational],x:VariablePack) => y.functionalFactory(map = y.map + (x -> in(get(x),x)))
    }


}

/**
 * @author Lucas Brutschy
 */
case class StaticVariablePackingDomain
  [Cheap <: SemanticDomain[Cheap],Relational <: NumericalDomain.Relational[Relational]]
  (_1:Cheap,_2:VariablePackMap[Relational])
  extends SemanticCartesianProductDomain[Cheap,VariablePackMap[Relational],StaticVariablePackingDomain[Cheap,Relational]]
  with NumericalDomain[StaticVariablePackingDomain[Cheap,Relational]] {

  override def factory(a: Cheap, b: VariablePackMap[Relational]): StaticVariablePackingDomain[Cheap, Relational] =
    StaticVariablePackingDomain(a,b)

  /**
   * Returns all the knowledge we have on the given identifiers as an expression
   */
  override def getConstraints(ids: Set[Identifier]) = ???
}
