/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{IdentifierSet, VariableIdentifier, Identifier}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.VariablePackingClassifier
import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, ProgramPoint}
import ch.ethz.inf.pm.td.compiler.TouchException

import scala.collection.immutable.Stack

/**
 * Implements access-based localization
 *
 * Integrates with variable packing
 *
 * Currently only works for variables!
 *
 * @author Lucas Brutschy
 */
object Localization {

  private var ppToMethod:Map[ProgramPoint,MethodDeclaration] = Map.empty
  private var variablePacker:Option[VariablePackingClassifier] = None
  private var enablePruning = false
  private var currentlyCollecting:List[ProgramPoint] = List.empty
  private var readInside:Map[ProgramPoint,IdentifierSet] = Map.empty


  // Pruning functions
  def startPruning(variablePacker:Option[VariablePackingClassifier] = None):Unit = {
    enablePruning = true
    this.variablePacker = variablePacker
  }

  def matches(identifyingPP:ProgramPoint, id:Identifier): Boolean = filter(identifyingPP,Set(id)).nonEmpty

  def filter(identifyingPP:ProgramPoint, ids:Set[Identifier]): Set[Identifier] = {
    if (enablePruning) {
      readInside.get(identifyingPP) match {
        case Some(x) =>
          val simpleFilter = ids.filter(y => x.contains(TouchVariablePacking.normalize(y)))
          variablePacker match {
            case Some(z) =>
              // Make sure that variablePacks stay together
              var removed = ids -- simpleFilter
              var reAdded = Set.empty[Identifier]
              for (pack <- z.classify(simpleFilter)) {
                val toReAdd = z.filterClass(removed,pack)
                removed = removed -- toReAdd
                reAdded = reAdded ++ toReAdd
              }
              simpleFilter ++ reAdded
            case None => simpleFilter
          }
        case None => ids
      }
    } else ids
  }

  def stopPruning():Unit = {
    variablePacker = None
    enablePruning = false
  }

  def isPruning: Boolean = enablePruning

  def collectAccess(id:Identifier):Unit = collectAccess(IdentifierSet.Bottom + id)

  def collectAccess(ids:IdentifierSet):Unit = {
    if (currentlyCollecting.nonEmpty) {
      val cur = currentlyCollecting.head
      readInside = readInside + (cur -> (readInside.getOrElse(cur,IdentifierSet.Bottom) ++ ids))
    }
  }

  def enterCollectingFunction(pp:ProgramPoint,callTarget:MethodDeclaration):Unit = {
    ppToMethod = ppToMethod + (pp -> callTarget)
    currentlyCollecting = pp :: currentlyCollecting
  }

  def exitCollectingFunction(pp:ProgramPoint):Unit = {

    // update stack
    val (callee,callStack) = (currentlyCollecting.head, currentlyCollecting.tail)
    if (SystemParameters.DEBUG) assert { callee == pp }
    currentlyCollecting = callStack

    // propagate callee reads to caller
    if (currentlyCollecting.nonEmpty) {
      val caller = currentlyCollecting.head
      readInside = readInside + (caller -> (readInside.getOrElse(caller,IdentifierSet.Bottom) ++ readInside.getOrElse(callee,IdentifierSet.Bottom)))
    }

  }

  override def toString:String = {
    (for ((pp,f) <- readInside) yield {
      (ppToMethod.get(pp) match {
        case Some(m) => m.name + " at " + pp
        case None => "unknown call at " + pp
      }) + " -> "+f.toString
    }).mkString("\n")
  }

  def reset() = {
    variablePacker = None
    enablePruning = false
    currentlyCollecting = List.empty
    readInside = Map.empty
    ppToMethod = Map.empty
  }

}
