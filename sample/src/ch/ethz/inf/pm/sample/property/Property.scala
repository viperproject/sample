/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.{CFGState, MethodAnalysisResult}
import ch.ethz.inf.pm.sample.oorepresentation._

/**
  * The check of a property
  *
  * @author Pietro Ferrara
  * @version 0.1
  */
trait Property {

  /**
    * A short label to identify and display the property
    */
  def label: String

  /**
    * Check the property over the abstract results of a single method
    *
    * @param classe     the class
    * @param methodName the name of the method
    * @param result     the abstract result
    * @param printer    the output collector that has to be used to signal warning, validate properties, or inferred contracts
    */
  def check[S <: State[S]](classe: Type, methodName: MethodDeclaration, result: CFGState[S], printer: OutputCollector): Unit


  /**
    * Check the property over the abstract results of multiple method
    *
    * @param results a list of the results, consisting of class type, method declaration and cfg
    * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
    */
  def check[S <: State[S]](results: List[(Type, MethodDeclaration, CFGState[S])], printer: OutputCollector): Unit = {
    for ((c, m, g) <- results) {
      check(c, m, g, printer)
    }
  }

}

/**
  * A visitor that checks the property over a single state
  *
  * @author Pietro Ferrara
  * @version 0.1
  */
trait Visitor {

  /**
    * A short label to identify and display the property
    */
  def label: String

  /**
    * Check the property over a single state
    *
    * @param state     the abstract state
    * @param statement the statement that was executed after the given abstract state
    * @param printer   the output collector that has to be used to signal warning, validate properties, or inferred contracts
    */
  def checkSingleStatement[S <: State[S]](state: S, statement: Statement, printer: OutputCollector): Unit
}

/**
  * The class that has to be used to check the property over single states
  *
  * @author Pietro Ferrara, Lucas Brutschy
  * @version 0.1
  */
trait SingleStatementProperty extends Property {

  def visitor: Visitor

  override def label: String = visitor.label

  override def check[S <: State[S]](className: Type, methodName: MethodDeclaration, result: CFGState[S], printer: OutputCollector): Unit = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(methodName)) {
      for (i <- result.cfg.nodes.indices)
        for (k <- result.cfg.nodes.apply(i).indices) {
          val statement = result.cfg.nodes.apply(i).apply(k)
          val blockStates = result.statesOfBlock(i)
          if (k > blockStates.size - 1)
            visitor.checkSingleStatement[S](
              blockStates.last.bottom(),
              statement,
              printer
            )
          else {
            val state = blockStates.apply(k)
            checkStatement(className, methodName, visitor, state, statement, printer)
          }
        }
    }
  }

  private def checkStatement[S <: State[S]](className: Type, methodName: MethodDeclaration, visitor: Visitor, state: S, statement: Statement, printer: OutputCollector): Unit = statement match {
    case Assignment(programPoint, left, right) =>
      visitor.checkSingleStatement[S](state, statement, printer)
      this.checkStatement(className, methodName, visitor, state, left, printer)
      this.checkStatement(className, methodName, visitor, left.forwardSemantics[S](state), right, printer)
    case VariableDeclaration(programPoint, variable, typ, right) =>
      visitor.checkSingleStatement[S](state, statement, printer)
      if (right.isDefined) checkStatement(className, methodName, visitor, state, right.get, printer)
    case Variable(programPoint, id) =>
      visitor.checkSingleStatement[S](state, statement, printer)
    case FieldAccess(pp, obj, field, typ) =>
      visitor.checkSingleStatement[S](state, statement, printer)
      checkStatement(className, methodName, visitor, state, obj, printer)
    case MethodCall(pp, method, parametricTypes, parameters, returnedType) =>
      visitor.checkSingleStatement[S](state, statement, printer)
      checkStatement(className, methodName, visitor, state, method, printer)
      for (par <- parameters)
        checkStatement(className, methodName, visitor, state, par, printer)
    case New(pp, typ) =>
      visitor.checkSingleStatement[S](state, statement, printer)
    case oorepresentation.ConstantStatement(pp, value, typ) =>
      visitor.checkSingleStatement[S](state, statement, printer)
    case Throw(programPoint, expr) =>
      visitor.checkSingleStatement[S](state, statement, printer)
      checkStatement(className, methodName, visitor, state, expr, printer)
    case x: ControlFlowGraph =>
      val result = new ControlFlowGraphExecution[S](x, state).forwardSemantics(state)
      this.check(className, methodName, result, printer);
    case _ => throw new UnsupportedOperationException("AST node not supported by visitor")
  }

}


class PropertyException(s: String) extends Exception(s)


object SingleStatementProperty {

  case class Default(visitor:Visitor) extends SingleStatementProperty

}