package ch.ethz.inf.pm.sample.td.cost.loops

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Variable


class OldVariable(val variable : Variable) extends Variable(variable.programpoint,

  new VariableIdentifier("old_"+variable.id.getName(), variable.id.getType, variable.id.getProgramPoint, variable.id.scope)) {

}