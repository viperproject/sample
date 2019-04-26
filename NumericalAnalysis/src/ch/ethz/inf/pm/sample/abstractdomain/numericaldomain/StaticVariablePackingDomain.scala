/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.util.MapUtil
import com.typesafe.scalalogging.LazyLogging

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

/**
 * @author Lucas Brutschy
 */
case class StaticVariablePackingDomain
  [Cheap <: NumericalDomain[Cheap],Relational <: NumericalDomain.Relational[Relational]]
  (cheap:Cheap,classifier:VariablePackingClassifier,dom:Relational,map: Map[VariablePack, Relational])
  extends NumericalDomain[StaticVariablePackingDomain[Cheap,Relational]]
  with SimplifiedSemanticDomain[StaticVariablePackingDomain[Cheap,Relational]]
  with LazyLogging {

  def factory(a: Cheap, b: Map[VariablePack, Relational]): StaticVariablePackingDomain[Cheap, Relational] =
    StaticVariablePackingDomain(a,classifier,dom,b)

  override def isBottom:Boolean =
    cheap.isBottom || map.exists(x => x._2.isBottom)

  override def isTop:Boolean =
    cheap.isTop && map.forall(x => x._2.isTop)

  override def setToTop(variable: Identifier): StaticVariablePackingDomain[Cheap,Relational] =
    factory(cheap.setToTop(variable),applyToPacks(variable, {x:Relational => x.setToTop(variable)}))

  override def removeVariable(id: Identifier): StaticVariablePackingDomain[Cheap,Relational] =
    factory(cheap.removeVariable(id),applyToPacks(id, {x:Relational => x.removeVariable(id)}))

  override def ids: IdentifierSet =
    cheap.ids

  override def assume(expr: Expression): StaticVariablePackingDomain[Cheap,Relational] = {
    if (expr.ids.isTop) return this
    factory(cheap.assume(expr),applyToPacks(expr.ids.getNonTop, {x:Relational => x.assume(expr)}))
  }

  override def createVariable(variable: Identifier, typ: Type): StaticVariablePackingDomain[Cheap,Relational] =
    factory(cheap.createVariable(variable,typ),applyToPacks(variable, {x:Relational => x.createVariable(variable,typ)}))

  override def assign(variable: Identifier, expr: Expression): StaticVariablePackingDomain[Cheap,Relational] =
    factory(cheap.assign(variable,expr),applyToPacks(variable, {x:Relational => x.assign(variable,expr)}))

  override def getStringOfId(id: Identifier): String = {
    val c = cheap.getStringOfId(id)
    val e = classifier.classify(id).map(getPack(_).getStringOfId(id)).mkString("\n")
    if (e.contains("\n")) {
      c + "\n\t== Constraints ==\n\t" + e.replace("\n","\n\t")
    } else {
      c + "," + e
    }
  }

  override def merge(f: Replacement): StaticVariablePackingDomain[Cheap,Relational] =
    factory(cheap.merge(f),f.value.foldLeft(map) {
      (cur:Map[VariablePack,Relational],ids:(Set[Identifier],Set[Identifier])) =>
        applyToPacksSep(cur, ids._1 ++ ids._2, { (x: Relational, y: VariablePack) =>
          val from = classifier.filterClass(ids._1, y)
          val to = classifier.filterClass(ids._2, y)
          val rep = new Replacement()
          rep.value += (from -> to)
          x.merge(rep)
        })
    })

  private def getPack(key: VariablePack): Relational = {
    getPack(map,key)
  }

  private def getPack(mapX:Map[VariablePack, Relational], key: VariablePack): Relational = {
    val vars = key.ids glb cheap.ids // glb of existing identifiers (stored by cheap domain) and pack
    if (vars.isTop)
      mapX.getOrElse(key, dom.top())
    else
      mapX.getOrElse(key, vars.getNonTop.foldLeft(dom.factory())(_ createVariable _))
  }

  private def applyToPacks(id: Identifier, in: Relational => Relational):Map[VariablePack,Relational] =
    applyToPacks(Set(id),in)

  private def applyToPacks(ids: Set[Identifier], in: Relational => Relational):Map[VariablePack,Relational] =
    classifier.classify(ids).foldLeft(map){
      (y:Map[VariablePack,Relational],x:VariablePack) => y + (x -> in(getPack(x)))
    }

  private def applyToPacksSep(mapX:Map[VariablePack,Relational], ids: Set[Identifier], in: (Relational,VariablePack) => Relational):Map[VariablePack,Relational] =
    classifier.classify(ids).foldLeft(mapX){
      (y:Map[VariablePack,Relational],x:VariablePack) => y + (x -> in(getPack(mapX,x),x))
    }

  override def getConstraints(ids: Set[Identifier]) =
    classifier.classify(ids).foldLeft(cheap.getConstraints(ids)) {
      _ ++ getPack(_).getConstraints(ids)
    }

  override def factory() = factory(cheap.factory(),Map.empty)

  override def bottom() = factory(cheap.bottom(),Map.empty)

  override def top() = {
    logger.debug("Creating a top state - this is known to cause problems")
    if (SystemParameters.DEBUG) {
      assert(false)
    }
    factory(cheap.top(),Map.empty)
  }

  override def lub(other: StaticVariablePackingDomain[Cheap, Relational]) =
    factory(cheap.lub(other.cheap),MapUtil.mergeMapsOptional(map,other.map) {
      case (None, None) => None
      case (Some(x), None) => Some(x)
      case (None, Some(x)) => Some(x)
      case (Some(x), Some(y)) => Some(x lub y)
    })

  override def glb(other: StaticVariablePackingDomain[Cheap, Relational]) =
    factory(cheap.glb(other.cheap),MapUtil.mergeMapsOptional(map,other.map) {
      case (None, None) => None
      case (Some(x), None) => Some(x)
      case (None, Some(x)) => Some(x)
      case (Some(x), Some(y)) => Some(x glb y)
    })

  override def widening(other: StaticVariablePackingDomain[Cheap, Relational]) =
    factory(cheap.widening(other.cheap),MapUtil.mergeMapsOptional(map,other.map) {
      case (None, None) => None
      case (Some(x), None) => Some(x)
      case (None, Some(x)) => Some(x)
      case (Some(x), Some(y)) => Some(x widening y)
    })

  override def lessEqual(other: StaticVariablePackingDomain[Cheap, Relational]) =
    cheap.lessEqual(other.cheap) && MapUtil.mergeMapsOtherVal(map,other.map) {
      case (None, None) => Some(true)
      case (Some(x), None) => Some(true)
      case (None, Some(x)) => Some(false)
      case (Some(x), Some(y)) => Some(x lessEqual y)
    }.values.forall{ x => x }

  override def getPossibleConstants(id: Identifier) =
    classifier.classify(id).foldLeft(cheap.getPossibleConstants(id)) {
      _ ++ getPack(_).getPossibleConstants(id)
    }

}