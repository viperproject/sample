package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, Identifier}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.VariablePackingClassifier
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchException

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


  private var variablePacker:Option[VariablePackingClassifier] = None
  private var enablePruning = false
  private var currentlyCollecting:Option[ProgramPoint] = None
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
    currentlyCollecting match {
      case Some(x) => readInside = readInside + (x -> (readInside.getOrElse(x,Set.empty) ++ ids))
      case None => () // Reachable for initialization code outside of any function
    }
  }

  def setCollectingFunction(pp:ProgramPoint):Unit = currentlyCollecting = Some(pp)

  override def toString:String = {
    (for ((pp,f) <- readInside) yield {
      pp+" -> "+f.collect{case v:VariableIdentifier => v}.mkString(",")
    }).mkString("\n")
  }

  def reset() = {
    variablePacker = None
    enablePruning = false
    currentlyCollecting = None
    readInside = Map.empty
  }

}
