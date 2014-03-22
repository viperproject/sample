package ch.ethz.inf.pm.td.compiler

//
//import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{VariablePack, VariablePacker}
//import ch.ethz.inf.pm.sample.abstractdomain.{Expression, VariableIdentifier, Identifier}
//import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
//import scala.collection.parallel.mutable
//
//object DynamicVariablePacker {
//
//  var map = new mutable.HashMap [ProgramPoint,Set[ProgramPointVariablePack]]()
//
//}
//
//class DynamicVariablePacker extends VariablePacker {
//
//  override def classify(id: Identifier): Option[VariablePack] = ???
//
//  override def assume(expr: Expression) {
//
//  }
//
//  override def assign(variable:Identifier,expr:Expression) {
//
//  }
//
//  override def reset() {
//    DynamicVariablePacker.map.clear()
//  }
//
//}
