package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, Identifier}
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
  private var currentlyCollecting:Stack[ProgramPoint] = Stack.empty
  private var readInside:Map[ProgramPoint,Set[Identifier]] = Map.empty


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

  def collectAccess(id:Identifier):Unit = collectAccess(Set(id))

  def collectAccess(ids:Set[Identifier]):Unit = {
    if (currentlyCollecting.nonEmpty) {
      val cur = currentlyCollecting.head
      readInside = readInside + (cur -> (readInside.getOrElse(cur,Set.empty) ++ ids))
    }
  }

  def enterCollectingFunction(pp:ProgramPoint,callTarget:MethodDeclaration):Unit = {
    ppToMethod = ppToMethod + (pp -> callTarget)
    currentlyCollecting = currentlyCollecting.push(pp)
  }

  def exitCollectingFunction(pp:ProgramPoint):Unit = {

    // update stack
    val (callee,callStack) = currentlyCollecting.pop2
    assert { callee == pp }
    currentlyCollecting = callStack

    // propagate callee reads to caller
    if (currentlyCollecting.nonEmpty) {
      val caller = currentlyCollecting.head
      readInside = readInside + (caller -> (readInside.getOrElse(caller,Set.empty) ++ readInside.getOrElse(callee,Set.empty)))
    }

  }

  override def toString:String = {
    (for ((pp,f) <- readInside) yield {
      (ppToMethod.get(pp) match {
        case Some(m) => m.name + " at " + pp
        case None => "unknown call at " + pp
      }) + " -> "+f.collect{case v:VariableIdentifier => v}.mkString(",")
    }).mkString("\n")
  }

  def reset() = {
    variablePacker = None
    enablePruning = false
    currentlyCollecting = Stack.empty
    readInside = Map.empty
    ppToMethod = Map.empty
  }

}
