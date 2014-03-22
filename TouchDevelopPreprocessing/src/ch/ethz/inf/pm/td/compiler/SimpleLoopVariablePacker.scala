package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, ClassDefinition}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{VariablePacker, VariablePack}
import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, Identifier}
import scala.collection.mutable
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.ProgramPointHeapIdentifier

object SimpleLoopVariablePacker {

  def make(classes: List[ClassDefinition]): SimpleLoopVariablePacker = {
    val variableMap = new mutable.HashMap[VariableIdentifier, Set[ProgramPointVariablePack]]
    val heapMap = new mutable.HashMap[ProgramPoint, Set[ProgramPointVariablePack]]
    for (clazz <- classes) {
      for (method <- clazz.methods) {
        // TODO
      }
    }
    SimpleLoopVariablePacker(variableMap.toMap, heapMap.toMap)
  }

}

case class SimpleLoopVariablePacker(
                                     variableMap: Map[VariableIdentifier, Set[ProgramPointVariablePack]],
                                     heapMap: Map[ProgramPoint, Set[ProgramPointVariablePack]]
                                     ) extends VariablePacker {

  override def classify(id: Identifier): Option[VariablePack] = {
    id match {
      case x: VariableIdentifier => variableMap.get(x).asInstanceOf[Option[VariablePack]]
      case x: ProgramPointHeapIdentifier => heapMap.get(x.pp).asInstanceOf[Option[VariablePack]]
      case _ => None
    }
  }

}

case class ProgramPointVariablePack(pp: ProgramPoint) extends VariablePack